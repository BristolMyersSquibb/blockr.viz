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
  # The house default is the dot style with both ranges on; `show` is the
  # legacy mirror of the style, not the axis itself.
  expect_identical(s[[1L]]$style, "dot")
  expect_identical(s[[1L]]$inner, "median_q1_q3")
  expect_identical(s[[1L]]$outer, "tukey")
  expect_identical(s[[1L]]$show, "pointrange")
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

test_that("box and dot are ONE mark: style over centre + inner + outer", {
  dist <- function(...) {
    lane_norm_summaries(list(list(type = "dist", col = "DUR", ...)))[[1L]]
  }
  # The style picks the drawing, never which numbers exist: both carry the
  # same three pieces.
  for (st in LANE_DIST_STYLES) {
    s <- dist(style = st)
    expect_identical(s$inner, "median_q1_q3")
    expect_identical(s$outer, "tukey")
  }
  # Either range switches off, which is where the degenerate family lives.
  expect_identical(dist(outer = "none")$outer, "none")
  expect_identical(dist(inner = "none")$inner, "none")
  # A centre is never absent: with the inner range off, the outer range's
  # centre stands in; with both off, the median.
  expect_identical(lane_dist_centre_stat(dist(inner = "none",
                                              outer = "min_max")),
                   "min_max")
  expect_identical(lane_dist_centre_stat(dist(inner = "none",
                                              outer = "none")),
                   "median_q1_q3")
  # Unknown values fall back rather than throwing: a bad style is a dot.
  expect_identical(dist(style = "banana")$style, "dot")
  expect_identical(dist(inner = "banana")$inner, "median_q1_q3")
  expect_identical(dist(outer = "banana")$outer, "tukey")
})

test_that("legacy `show` still restores the mark it always drew", {
  dist <- function(...) {
    lane_norm_summaries(list(list(type = "dist", col = "DUR", ...)))[[1L]]
  }
  # A saved board that said "pointrange" drew a centre and ONE range -- so it
  # restores as the dot style with no outer, not as the new default.
  pr <- dist(show = "pointrange")
  expect_identical(pr$style, "dot")
  expect_identical(pr$outer, "none")
  # "box" kept its whiskers, and the legacy stat fields still seed the axes.
  bx <- dist(show = "box", stat = "mean_sd", whiskers = "min_max")
  expect_identical(bx$style, "box")
  expect_identical(bx$inner, "mean_sd")
  expect_identical(bx$outer, "min_max")
  # An explicit axis wins over the sugar.
  expect_identical(dist(show = "pointrange", outer = "tukey")$outer, "tukey")
  expect_identical(dist(show = "box", style = "dot")$style, "dot")
})

test_that("the ground follows the mark: only a bare mark keeps a hairline", {
  d <- sum_fixture()
  plan1 <- function(...) {
    p <- lane_prepare_summaries(
      d, by = "TERM",
      summaries = list(list(type = "dist", col = "DUR", ...))
    )
    p$plan[[1L]]
  }
  # An outer range IS the rail, so the lane is not bare -- in either style.
  expect_false(plan1()$bare)
  expect_false(plan1(style = "box")$bare)
  # Nothing spanning the cell: the hairline comes back.
  expect_true(plan1(outer = "none")$bare)
  expect_true(plan1(inner = "none", outer = "none")$bare)
  # The leaf columns follow the pieces that are on.
  expect_false("wl" %in% names(plan1(outer = "none")$cols))
  expect_false("bl" %in% names(plan1(inner = "none")$cols))
  expect_true(all(c("bl", "bh", "wl", "wh") %in% names(plan1()$cols)))
})

test_that("the colour dimension reaches every lane mark, dot included", {
  ae <- sum_fixture()
  S <- list(
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "USUBJID", show = "dot"),
    list(type = "simple", name = "Bar", func = "count_distinct",
         col = "USUBJID", show = "bar"),
    list(type = "dist", name = "Duration", col = "DUR")
  )
  p <- lane_prepare_summaries(ae, by = "TERM", summaries = S, color = "ARM")
  lv <- rank_levels(ae$ARM)
  # The dot splits like the distribution does: one glyph per level, named by
  # the same geometry key the cell builder reads.
  expect_identical(p$plan[[1L]]$levels, lv)
  expect_length(p$plan[[1L]]$lcols, length(lv))
  expect_identical(names(p$plan[[1L]]$lcols[[1L]]), "bc")
  expect_true(all(unlist(p$plan[[1L]]$lcols) %in% names(p$rows)))
  # A bar takes the same dimension as a COMPOSITION: stacked segments, one
  # per level, still totalling the group's value -- not a second glyph in
  # the cell.
  expect_identical(p$plan[[2L]]$kind, "barsplit")
  expect_null(p$plan[[2L]]$lcols)
  expect_identical(p$plan[[2L]]$series, lv)
  expect_true(all(paste0(p$plan[[2L]]$prefix, lv) %in% names(p$rows)))
  expect_identical(p$plan[[3L]]$levels, lv)
  # Every level of a split column is read against ONE scale.
  m <- rank_cells(p)
  expect_true(isTRUE(m$cols[[1L]]$multi))
  expect_length(m$cols[[1L]]$lv, length(lv))
  # Nothing spans the cell, so the dot keeps its hairline in every level.
  expect_true(all(vapply(m$cols[[1L]]$lv, function(g) isTRUE(g$bare),
                         logical(1L))))
  # The split bar scales on its own column max, not the prep-level bar_max
  # (which is 0 on this path) -- otherwise every segment ships width 0.
  expect_true(max(unlist(m$cols[[2L]]$seg), na.rm = TRUE) > 0)
  # Segments carry the level colours, and they sum to the bar's value.
  expect_identical(m$cols[[2L]]$fills, p$plan[[2L]]$fills)
  expect_equal(Reduce(`+`, m$cols[[2L]]$segv), m$cols[[2L]]$v)

  # A group whose rows are all ONE level draws that level alone: grouping by
  # subject (every subject has one SEX) is a single bar in its own colour,
  # not a stack with an empty half.
  one <- lane_prepare_summaries(ae, by = "USUBJID", summaries = S,
                                color = "ARM")
  segv <- rank_cells(one)$cols[[2L]]$segv
  nonzero <- vapply(seq_along(segv[[1L]]), function(i) {
    sum(vapply(segv, function(x) x[[i]] > 0, logical(1L)))
  }, integer(1L))
  expect_true(all(nonzero <= 1L))
})

test_that("the column axis prints the domain once, on glyph columns only", {
  # Nice steps, and never a tick outside the domain it labels.
  expect_identical(rank_axis_ticks(0, 100), c(0, 25, 50, 75, 100))
  expect_identical(rank_axis_ticks(2, 9), c(2.5, 5, 7.5))
  expect_length(rank_axis_ticks(1, 1), 0L)
  expect_length(rank_axis_ticks(NA_real_, 1), 0L)

  strip <- function(p) as.character(rank_axis_strip(p))
  expect_match(strip(list(kind = "box", dmin = 0, dmax = 100)),
               "blockr-rank-axis")
  # A bar's domain is a share of the column max, which the value beside it
  # already says: no axis.
  expect_null(rank_axis_strip(list(kind = "bar", dmin = 0, dmax = 100)))
  expect_null(rank_axis_strip(list(kind = "box", dmin = 1, dmax = 1)))
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

test_that("a distribution's domain is its data, not zero and not n", {
  d <- data.frame(
    G = rep(c("a", "b"), each = 40L),
    # Diastolic-BP-shaped: far from zero, narrow spread, and 80 rows -- the
    # group size must never reach the value axis.
    V = c(rnorm(40L, 76, 4), rnorm(40L, 72, 4)),
    stringsAsFactors = FALSE
  )
  box <- lane_prepare_summaries(
    d, by = "G",
    summaries = list(list(type = "dist", name = "V", col = "V", show = "box"))
  )$plan[[1L]]
  expect_gt(box$dmin, 40)          # not anchored at zero
  expect_lt(box$dmax, 100)         # and nowhere near n = 40
  # The whiskers fit, with only the padding to spare.
  span <- box$dmax - box$dmin
  expect_lt(span, (max(d$V) - min(d$V)) * 1.2)

  # A simple row's dot IS a length from zero (a bar with less ink), so it
  # keeps the zero anchor.
  dot <- lane_prepare_summaries(
    d, by = "G",
    summaries = list(list(type = "simple", name = "N", func = "count",
                          show = "dot"))
  )$plan[[1L]]
  expect_identical(dot$dmin, 0)
})

test_that("a distribution splits by colour: one glyph per level, one scale", {
  ae <- sum_fixture()
  p <- lane_prepare_summaries(
    ae, by = "TERM",
    summaries = list(list(type = "dist", name = "Duration", col = "DUR",
                          show = "box", color = "SEV"))
  )
  e <- p$plan[[1L]]
  expect_identical(e$levels, c("MILD", "MOD"))
  expect_length(e$lcols, 2L)
  expect_length(e$fills, 2L)
  # Every level is read against the SAME axis, or the split lies.
  expect_true(is.finite(e$dmin) && is.finite(e$dmax))
  lo <- min(p$rows[[e$lcols[[1L]][["wl"]]]], p$rows[[e$lcols[[2L]][["wl"]]]],
            na.rm = TRUE)
  hi <- max(p$rows[[e$lcols[[1L]][["wh"]]]], p$rows[[e$lcols[[2L]][["wh"]]]],
            na.rm = TRUE)
  expect_lte(e$dmin, lo)
  expect_gte(e$dmax, hi)

  # The pooled glyph survives as the sort value, and the colour dimension
  # gets the legend (identity never rides on colour alone).
  expect_true(all(is.finite(p$rows$.v)))
  expect_identical(p$series, c("MILD", "MOD"))
  expect_identical(p$color, "SEV")

  m <- rank_cells(p)
  cell <- m$cols[[1L]]
  expect_true(isTRUE(cell$multi))
  expect_length(cell$lv, 2L)
  expect_match(cell$lv[[1L]]$tip[[1L]], "^MILD")

  # A level with no rows in a group draws no lane at all -- the per-subject
  # case ("this subject is female") is one coloured glyph, not one plus a gap.
  one <- ae[ae$SEV == "MILD", , drop = FALSE]
  p1 <- lane_prepare_summaries(
    one, by = "TERM",
    summaries = list(list(type = "dist", col = "DUR", show = "box",
                          color = "SEV"))
  )
  html <- rank_cells_html(rank_cells(p1))
  expect_equal(lengths(regmatches(html, gregexpr("blockr-rank-lv", html))),
               nrow(p1$rows))
})

test_that("the colour split takes the board's fixed colours", {
  skip_if_not_installed("blockr.theme")
  ae <- sum_fixture()
  map <- list(SEV = list(color = c(MILD = "#123456", MOD = "#654321")))
  p <- lane_prepare_summaries(
    ae, by = "TERM",
    summaries = list(list(type = "dist", col = "DUR", show = "box",
                          color = "SEV")),
    scale_map = map
  )
  expect_identical(p$plan[[1L]]$fills, c("#123456", "#654321"))
})

test_that("colour is a table-level dimension every glyph column inherits", {
  ae <- sum_fixture()
  S <- list(
    list(type = "dist", name = "Duration", col = "DUR", show = "box"),
    list(type = "dist", name = "Mean", col = "DUR", stat = "mean_ci95",
         show = "pointrange"),
    list(type = "spans", name = "Episodes", x = "ASTDY", xend = "AENDY",
         color = "SEV")
  )
  p <- lane_prepare_summaries(ae, by = "TERM", summaries = S, color = "ARM")
  # One pick splits every glyph column that can carry it...
  expect_identical(p$plan[[1L]]$levels, c("Active", "Placebo"))
  expect_identical(p$plan[[2L]]$levels, c("Active", "Placebo"))
  # ...but a summary naming its own colour keeps it: on a swimlane the
  # colour describes the events, not the table's series.
  expect_identical(p$plan[[3L]]$levels, c("MILD", "MOD"))
  # One legend, for the table's dimension.
  expect_identical(p$color, "ARM")
  expect_identical(p$series, c("Active", "Placebo"))

  # The role reaches the preparer through the block's own `color` slot.
  p2 <- rank_prepare(ae, group = "TERM", by = "TERM", summaries = S[1],
                     color = "ARM")
  expect_identical(p2$plan[[1L]]$levels, c("Active", "Placebo"))
})

test_that("a numeric grouping column orders numerically, not as text", {
  # Study days: no factor to carry the order, so first appearance used to
  # scatter them (2, 10, 1, 100) and A-Z sorted the digits ("10" < "2").
  d <- data.frame(ADY = c(2, 10, 1, 2, 10, 1, 100, 100), V = 1:8)
  S <- list(list(type = "simple", name = "N", func = "count", show = "bar"))
  for (sb in c("data", "label")) {
    p <- lane_prepare_summaries(d, by = "ADY", summaries = S, sort_by = sb,
                                sort_dir = "asc")
    expect_identical(p$rows$.label, c("1", "2", "10", "100"), info = sb)
    p <- lane_prepare_summaries(d, by = "ADY", summaries = S, sort_by = sb,
                                sort_dir = "desc")
    expect_identical(p$rows$.label, c("100", "10", "2", "1"), info = sb)
  }

  # The same for a numeric PARENT, a facet and a colour split: every level
  # list runs through rank_levels().
  d$G <- rep(c("b", "a"), 4)
  p <- lane_prepare_summaries(d, by = c("ADY", "G"), summaries = S,
                              sort_by = "data", sort_dir = "asc")
  expect_identical(p$rows$.label[p$rows$.is_parent], c("1", "2", "10", "100"))
  expect_identical(rank_levels(c(2, 10, 1, 100)), c("1", "2", "10", "100"))
  expect_identical(rank_levels(as.Date(c("2024-01-10", "2024-01-02"))),
                   c("2024-01-02", "2024-01-10"))
})
