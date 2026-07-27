# The summarize-table path: the summaries list, its normalization, the
# generic preparer, per-column domains, scope, facet and fields.
# Spec: _blockr.design/open/summarize-table/.

sum_fixture <- function() {
  set.seed(42)
  ae <- do.call(rbind, lapply(1:5, function(i) {
    n <- 22L - 3L * i
    data.frame(
      SOC = if (i <= 3) "SOC A" else "SOC B",
      TERM = paste0("Term", i),
      USUBJID = sprintf("S%02d", seq_len(n)),
      SEV = rep(c("MILD", "MOD"), length.out = n),
      DUR = round(stats::rlnorm(n, log(2 + i), 0.5), 1),
      ARM = rep(c("Placebo", "Active"), length.out = n),
      ASTDY = seq_len(n) * 3L,
      stringsAsFactors = FALSE
    )
  }))
  ae$AENDY <- ae$ASTDY + 5L
  ae$LO <- 2
  ae$HI <- 30
  ae
}

test_that("normalization fills defaults and names the broken row", {
  s <- lane_norm_summaries(list(list(type = "dist", col = "DUR")))
  expect_identical(s[[1L]]$show, "box")
  expect_identical(s[[1L]]$scope, "cell")
  expect_identical(s[[1L]]$name, "DUR")

  # Fields are group facts: always pooled.
  s <- lane_norm_summaries(list(list(type = "field", col = "ARM",
                                     scope = "cell")))
  expect_identical(s[[1L]]$scope, "pooled")

  expect_match(lane_norm_summaries(list(list(type = "nope")))$err,
               "unknown type")
  expect_match(lane_norm_summaries(list(list(type = "dist")))$err,
               "`col` is required")
  expect_match(
    lane_norm_summaries(list(list(type = "spans", x = "ASTDY")))$err,
    "`xend` is required"
  )
})

test_that("the field join is distinct values with a fold cap, never first()", {
  expect_identical(lane_field_join(c("A", "A", "A")), "A")
  expect_identical(lane_field_join(c("B", "A", "B")), "B, A")
  expect_identical(lane_field_join(letters[1:5]), "a, b, c, +2 more")
  expect_identical(lane_field_join(letters[1:12]), "12 values")
  expect_identical(lane_field_join(c(NA, "", "X")), "X")
})

test_that("a mixed column list renders every row type through one table", {
  ae <- sum_fixture()
  S <- list(
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "USUBJID", show = "bar"),
    list(type = "dist", name = "Duration", col = "DUR", show = "box"),
    list(type = "dist", name = "Mean", col = "DUR", stat = "mean_ci95",
         show = "text"),
    list(type = "field", name = "Arms", col = "ARM"),
    list(type = "spans", name = "Episodes", x = "ASTDY", xend = "AENDY",
         color = "SEV"),
    list(type = "series", name = "Traj", x = "ASTDY", col = "DUR",
         band = c("LO", "HI")),
    list(type = "expr", name = "CV", expr = "round(sd(DUR)/mean(DUR), 2)")
  )
  p <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = S)
  expect_identical(p$kind, "flat")
  expect_identical(
    vapply(p$cols, function(c) c$kind, ""),
    c("bar", "box", "num", "num", "interval", "sparkline", "num")
  )
  # dist as TEXT: "center (lo–hi)", sorted numerically by the center.
  expect_match(p$cols[[3]]$disp[[1]], "^[0-9.]+ \\([0-9.]+–[0-9.]+\\)$")
  expect_true(is.numeric(p$cols[[3]]$v))
  # The field sees both arms (subjects span arms in this fixture): the
  # broken-constancy case is VISIBLE, not silently truncated to one row.
  expect_match(p$cols[[4]]$disp[[1]], ", ")
  # The legend comes from the spans colour.
  expect_identical(p$chrome$legend$title, "SEV")
})

test_that("per-column domains: mixed units never share a scale", {
  ae <- sum_fixture()
  ae$BIG <- ae$DUR * 1000   # a second numeric on a wildly different scale
  S <- list(
    list(type = "simple", name = "Small", func = "mean", col = "DUR",
         show = "bar"),
    list(type = "simple", name = "Big", func = "mean", col = "BIG",
         show = "bar")
  )
  p <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = S)
  # Each bar column scales to its OWN max: both hit 100 somewhere.
  expect_equal(max(as.numeric(p$cols[[1]]$w)), 100)
  expect_equal(max(as.numeric(p$cols[[2]]$w)), 100)
})

test_that("facet repeats cell rows, pooled rows and fields render once", {
  ae <- sum_fixture()
  S <- list(
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "USUBJID", show = "bar"),
    list(type = "dist", name = "Overall", col = "DUR", stat = "mean_se",
         show = "text", scope = "pooled"),
    list(type = "field", name = "Arms", col = "ARM")
  )
  p <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = S,
                          facet = "ARM")
  # Subjects repeats per level (2), Overall and Arms render once: 4 columns.
  expect_length(p$cols, 4L)
  expect_identical(vapply(p$cols, function(c) c$kind, ""),
                   c("bar", "bar", "num", "num"))
  # The facet copies share ONE scale: their union hits 100 exactly once
  # per... at least once, and no width exceeds it.
  w <- c(as.numeric(p$cols[[1]]$w), as.numeric(p$cols[[2]]$w))
  expect_equal(max(w, na.rm = TRUE), 100)
})

test_that("by_level reorders into level groups with a two-row header", {
  ae <- sum_fixture()
  S <- list(
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "USUBJID", show = "bar"),
    list(type = "dist", name = "Duration", col = "DUR", stat = "mean_se",
         show = "text"),
    list(type = "field", name = "Arms", col = "ARM")
  )
  p <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = S,
                          facet = "ARM", facet_layout = "by_level")
  # Leading field first, then per-level groups of (bar, num): 5 columns.
  expect_length(p$cols, 5L)
  expect_identical(vapply(p$cols, function(c) c$kind, ""),
                   c("num", "bar", "num", "bar", "num"))
  # The spanning header row: one colspan cell per level, label + leading
  # columns span both rows.
  expect_match(p$head, "blockr-th-group\" colspan=\"2\"")
  expect_match(p$head, "rowspan=\"2\"")
  expect_match(p$head, ">Placebo<")
  expect_match(p$head, ">Active<")
  # by_summary (the default) keeps the single header row.
  p2 <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = S,
                           facet = "ARM")
  expect_false(grepl("blockr-th-group", p2$head))
})

test_that("by nests one level: outer parent rows plus inner leaves", {
  ae <- sum_fixture()
  S <- list(list(type = "simple", name = "Rows", func = "count",
                 show = "bar"))
  p <- rank_build_payload(ae, group = NULL, by = c("SOC", "TERM"),
                          summaries = S)
  expect_true(any(as.logical(p$parent_row)))
  expect_identical(sum(as.logical(p$parent_row)), 2L)
  # Three or more grouping columns is a config error, said plainly.
  p2 <- rank_build_payload(ae, group = NULL,
                           by = c("SOC", "TERM", "USUBJID"), summaries = S)
  expect_identical(p2$kind, "html")
  expect_match(p2$html, "at most two")
})

test_that("expr rows evaluate per group and fail as a cell, not a crash", {
  ae <- sum_fixture()
  p <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "expr", name = "n2", expr = "dplyr::n() * 2")
  ))
  expect_identical(p$cols[[1]]$kind, "num")
  expect_true(is.numeric(p$cols[[1]]$v))
  # A broken expression degrades to an error cell.
  p2 <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "expr", name = "boom", expr = "no_such_fn(DUR)")
  ))
  expect_true(all(as.character(p2$cols[[1]]$disp) == "(error)"))
})

test_that("spans label/fields enrich tips and key the highlight", {
  ae <- sum_fixture()
  ae$TERM2 <- ae$TERM   # the event label column
  ae$SER <- rep(c("Y", "N"), length.out = nrow(ae))
  p <- rank_build_payload(ae, group = NULL, by = "USUBJID", summaries = list(
    list(type = "spans", x = "ASTDY", xend = "AENDY", color = "SEV",
         label = "TERM2", fields = "SER", size = "lg")
  ))
  c1 <- p$cols[[1]]
  # The tooltip headlines the event, then level, span, field pairs.
  expect_match(c1$tips[[1]][[1]], "^Term[0-9] · (MILD|MOD) · ")
  expect_match(c1$tips[[1]][[1]], "SER: (Y|N)")
  # The 4th segment slot carries the escaped label (data-l, the highlight
  # key); without `label` it is absent.
  expect_length(c1$segs[[1]][[1]], 4L)
  expect_match(c1$segs[[1]][[1]][[4L]], "^Term")
  expect_true(isTRUE(c1$lg))
  p2 <- rank_build_payload(ae, group = NULL, by = "USUBJID", summaries = list(
    list(type = "spans", x = "ASTDY", xend = "AENDY", color = "SEV")
  ))
  expect_length(p2$cols[[1]]$segs[[1]][[1]], 3L)
  expect_null(p2$cols[[1]]$lg)
})

test_that("a series ref computes a pooled line, mean_sd adds the band", {
  ae <- sum_fixture()
  p <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "series", x = "ASTDY", col = "DUR", ref = "mean")
  ))
  c1 <- p$cols[[1]]
  expect_true(is.numeric(c1$rc))
  expect_null(c1$rby)                      # mean = line only
  p2 <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "series", x = "ASTDY", col = "DUR", ref = "mean_sd")
  ))
  c2 <- p2$cols[[1]]
  expect_true(is.numeric(c2$rby) && is.numeric(c2$rbh) && c2$rbh > 0)
  # The reference rides INSIDE the svg: band can exceed the observed range,
  # so the y-domain must have grown to keep it on canvas.
  expect_true(c2$rby >= 0)
  # No ref -> no coordinates shipped.
  p3 <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "series", x = "ASTDY", col = "DUR")
  ))
  expect_null(p3$cols[[1]]$rc)
})

test_that("identity rides through simple rows: the value as-is", {
  d <- data.frame(g = c("a", "b", "c"), v = c(3, 9, 6))
  p <- rank_build_payload(d, group = NULL, by = "g", summaries = list(
    list(type = "simple", name = "V", func = "identity", col = "v",
         show = "bar"),
    list(type = "simple", name = "Vn", func = "identity", col = "v",
         show = "number")
  ))
  expect_identical(vapply(p$cols, function(c) c$kind, ""), c("bar", "num"))
  expect_identical(as.character(p$label), c("b", "c", "a"))   # ranked as-is
  expect_equal(max(as.numeric(p$cols[[1]]$w)), 100)
  expect_identical(trimws(as.character(p$cols[[2]]$disp)),
                   c("9", "6", "3"))
})

test_that("sort_by a summary's name orders by that column", {
  ae <- sum_fixture()
  S <- list(
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "USUBJID", show = "bar"),
    list(type = "dist", name = "Duration", col = "DUR", show = "box")
  )
  p <- rank_build_payload(ae, group = NULL, by = "TERM", summaries = S,
                          sort_by = "Duration")
  meds <- as.numeric(p$cols[[2]]$v)
  expect_true(all(diff(meds) <= 0))
})

test_that("the constructor round-trips the summaries list", {
  S <- list(list(type = "dist", name = "D", col = "DUR", show = "box"))
  blk <- new_summarize_table_block(by = "TERM", summaries = S)
  expect_s3_class(blk, "summarize_table_block")
  fmls <- names(formals(new_summarize_table_block))
  expect_true(all(c("summaries", "by", "facet_layout") %in% fmls))
})

test_that("sort_by data honors factor levels, else first appearance", {
  visits <- c("Baseline", "Week 2", "Week 10")
  d <- data.frame(
    AVISIT = factor(rep(rev(visits), each = 3L), levels = visits),
    AVAL = 1:9,
    stringsAsFactors = FALSE
  )
  S <- list(list(type = "simple", name = "N", func = "count", show = "bar"))

  p <- lane_prepare_summaries(d, by = "AVISIT", summaries = S,
                              sort_by = "data", sort_dir = "asc")
  expect_identical(p$rows$.label, visits)
  # The direction still applies -- reverse chronological is a real ask.
  p <- lane_prepare_summaries(d, by = "AVISIT", summaries = S,
                              sort_by = "data", sort_dir = "desc")
  expect_identical(p$rows$.label, rev(visits))
  # Alphabetical is what this exists to avoid.
  p <- lane_prepare_summaries(d, by = "AVISIT", summaries = S,
                              sort_by = "label", sort_dir = "asc")
  expect_identical(p$rows$.label, c("Baseline", "Week 10", "Week 2"))

  # No factor: the order the rows arrive in (ADaM is visit-sorted).
  d$AVISIT <- as.character(d$AVISIT)
  p <- lane_prepare_summaries(d, by = "AVISIT", summaries = S,
                              sort_by = "data", sort_dir = "asc")
  expect_identical(p$rows$.label, rev(visits))

  # Nested: parents in data order, each parent's children too.
  d$ARM <- rep(c("Placebo", "Active"), length.out = nrow(d))
  p <- lane_prepare_summaries(d, by = c("ARM", "AVISIT"), summaries = S,
                              sort_by = "data", sort_dir = "asc")
  expect_identical(p$rows$.label[p$rows$.is_parent], c("Placebo", "Active"))
  expect_identical(p$rows$.label[!p$rows$.is_parent], rep(rev(visits), 2L))
})

test_that("the bar path takes the same data order", {
  ae <- sum_fixture()
  ae$TERM <- factor(ae$TERM, levels = paste0("Term", 5:1))
  p <- rank_prepare(ae, group = "TERM", func = "count", sort_by = "data",
                    sort_dir = "asc")
  expect_identical(p$rows$.label, paste0("Term", 5:1))
})

test_that("sort_by a raw numeric column orders by the group minimum", {
  # The visit case first appearance cannot solve: one subject discontinues
  # after Week 4, so "End of Treatment" appears in the rows before Week 8.
  d <- data.frame(
    AVISIT = c("Baseline", "Week 4", "End of Treatment",
               "Baseline", "Week 4", "Week 8", "End of Treatment"),
    AVISITN = c(0, 4, 99, 0, 4, 8, 99),
    AVAL = c(1, 2, 3, 4, 5, 6, 7),
    USUBJID = c("a", "a", "a", "b", "b", "b", "b"),
    stringsAsFactors = FALSE
  )
  S <- list(list(type = "simple", name = "N", func = "count", show = "bar"))
  p <- lane_prepare_summaries(d, by = "AVISIT", summaries = S,
                              sort_by = "data", sort_dir = "asc")
  expect_identical(p$rows$.label,
                   c("Baseline", "Week 4", "End of Treatment", "Week 8"))
  p <- lane_prepare_summaries(d, by = "AVISIT", summaries = S,
                              sort_by = "AVISITN", sort_dir = "asc")
  expect_identical(p$rows$.label,
                   c("Baseline", "Week 4", "Week 8", "End of Treatment"))
  # A summary column of the same name still wins the keyword race.
  p <- lane_prepare_summaries(d, by = "AVISIT", summaries = S,
                              sort_by = "N", sort_dir = "desc")
  expect_identical(p$rows$.label[[1L]], "Baseline")
})
