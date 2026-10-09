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
  lv <- summarize_levels(ae$ARM)
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
  m <- summarize_cells(p)
  expect_true(isTRUE(m$cols[[1L]]$multi))
  expect_length(m$cols[[1L]]$lv, length(lv))
  # Nothing spans the cell, so the dot keeps its hairline in every level.
  expect_true(all(vapply(m$cols[[1L]]$lv, function(g) isTRUE(g$bare),
                         logical(1L))))
  # The split bar scales on its own column max, not the prep-level bar_max
  # (which is 0 on this path) -- otherwise every segment ships width 0.
  expect_true(max(unlist(m$cols[[2L]]$seg), na.rm = TRUE) > 0)
  # Segments carry the level colours, and they sum to the bar's count (the
  # sort value is its percentage of N: a count column carries a denominator).
  expect_identical(m$cols[[2L]]$fills, p$plan[[2L]]$fills)
  expect_equal(Reduce(`+`, m$cols[[2L]]$segv),
               as.numeric(p$rows[[p$plan[[2L]]$key]]))
  expect_equal(m$cols[[2L]]$v, round(as.numeric(p$rows[[p$plan[[2L]]$key]]) /
                                       p$plan[[2L]]$denom * 100, 4L))

  # A group whose rows are all ONE level draws that level alone: grouping by
  # subject (every subject has one SEX) is a single bar in its own colour,
  # not a stack with an empty half.
  one <- lane_prepare_summaries(ae, by = "USUBJID", summaries = S,
                                color = "ARM")
  segv <- summarize_cells(one)$cols[[2L]]$segv
  nonzero <- vapply(seq_along(segv[[1L]]), function(i) {
    sum(vapply(segv, function(x) x[[i]] > 0, logical(1L)))
  }, integer(1L))
  expect_true(all(nonzero <= 1L))
})

test_that("the hover card names the colour column by its label", {
  ae <- sum_fixture()
  attr(ae$ARM, "label") <- "Treatment Arm"
  S <- list(
    list(type = "simple", name = "Bar", func = "count_distinct",
         col = "USUBJID", show = "bar"),
    list(type = "dist", name = "Duration", col = "DUR")
  )
  p <- lane_prepare_summaries(ae, by = "TERM", summaries = S, color = "ARM")
  expect_identical(p$plan[[1L]]$cvar, "Treatment Arm")
  expect_identical(p$plan[[2L]]$cvar, "Treatment Arm")
  expect_identical(summarize_legend_spec(p)$groups[[1L]]$title, "Treatment Arm")
  # No label: the column name, as before.
  attr(ae$ARM, "label") <- NULL
  p <- lane_prepare_summaries(ae, by = "TERM", summaries = S, color = "ARM")
  expect_identical(p$plan[[1L]]$cvar, "ARM")
})

test_that("hover text names columns by their labels", {
  ae <- sum_fixture()
  attr(ae$SEV, "label") <- "Severity"
  attr(ae$ARM, "label") <- "Treatment Arm"
  attr(ae$ASTDY, "label") <- "Start Day"
  attr(ae$AENDY, "label") <- "End Day"
  S <- list(
    list(type = "spans", name = "Episodes", x = "ASTDY", xend = "AENDY",
         color = "SEV", fields = "ARM"),
    list(type = "pair", name = "Onset to end", from = "ASTDY",
         from_func = "mean", to = "AENDY", to_func = "mean")
  )
  m <- summarize_cells(summarize_prepare(ae, by = "TERM", summaries = S))
  expect_match(m$cols[[1L]]$tips[[1L]][[1L]],
               "^Severity (MILD|MOD) .* Treatment Arm: ")
  expect_match(m$cols[[2L]]$tt$sub, "Start Day.*End Day")
  # The plain summarize table: the bar header and card name the value label.
  p <- summarize_prepare(ae, group = "TERM", value = "ASTDY", func = "mean")
  expect_identical(p$plan[[1L]]$meas, "Mean Start Day")
})

test_that("a split bar only stacks an additive measure", {
  ae <- sum_fixture()
  S <- list(list(type = "simple", name = "Mean duration", func = "mean",
                 col = "DUR", show = "bar"))
  p <- lane_prepare_summaries(ae, by = "TERM", summaries = S, color = "ARM")
  e <- p$plan[[1L]]
  # The parts of a mean are not a composition of it: they sit side by side,
  # and the column's domain reaches the widest of them (scaling them against
  # the pooled mean instead would clamp every larger arm at full width).
  expect_identical(e$kind, "barsplit")
  expect_identical(e$mode, "grouped")
  segs <- unlist(p$rows[, paste0(e$prefix, e$series)])
  expect_equal(e$dmax, max(c(p$rows[[e$key]], segs), na.rm = TRUE))
  segw <- unlist(summarize_cells(p)$cols[[1L]]$seg)
  expect_true(max(segw, na.rm = TRUE) <= 100)

  # A count is a composition, so it keeps stacking on the group's total.
  cs <- list(list(type = "simple", name = "Events", func = "count",
                  col = "DUR", show = "bar"))
  pc <- lane_prepare_summaries(ae, by = "TERM", summaries = cs, color = "ARM")
  expect_identical(pc$plan[[1L]]$mode, "stacked")
  # Its domain is in percent of N, the unit its bars are drawn in.
  expect_equal(pc$plan[[1L]]$dmax,
               max(pc$rows[[pc$plan[[1L]]$key]]) / pc$plan[[1L]]$denom * 100)
})

test_that("the column axis prints the domain once, on glyph columns only", {
  # Nice steps, and never a tick outside the domain it labels.
  expect_identical(summarize_axis_ticks(0, 100), c(0, 25, 50, 75, 100))
  expect_identical(summarize_axis_ticks(2, 9), c(2.5, 5, 7.5))
  expect_length(summarize_axis_ticks(1, 1), 0L)
  expect_length(summarize_axis_ticks(NA_real_, 1), 0L)

  strip <- function(p, ...) as.character(summarize_axis_strip(p, ...))
  expect_match(strip(list(kind = "box", dmin = 0, dmax = 100)),
               "blockr-summarize-axis")
  # EVERY mark on a scale gets one, each against the domain its own geometry
  # was computed from: a bar from zero to the column max...
  expect_match(strip(list(kind = "bar", dmin = 40, dmax = 100)),
               "left:0%\">0</span>")
  # ...a 100% split over the percentage itself, with the unit named once...
  expect_match(strip(list(kind = "barsplit", mode = "percent", dmax = 7)),
               "100%</span>")
  # ...and a difference bar zero-centred, the comparator in the middle.
  expect_match(strip(list(kind = "bardiv", dmax = 10)), "left:50%\">0</span>")
  # A swimlane / sparkline axis is the x domain, printed as DATES when the x
  # column is one (the tooltip's rule).
  span <- as.numeric(as.Date(c("2023-02-11", "2023-06-02")))
  expect_match(strip(list(kind = "interval", dom = span, dom_date = TRUE)),
               "Mar 2023")
  expect_match(strip(list(kind = "sparkline", dom = c(0, 90))),
               "blockr-summarize-axis")
  # No scale, no strip -- and a degenerate domain draws nothing either.
  expect_null(summarize_axis_strip(list(kind = "num")))
  expect_null(summarize_axis_strip(list(kind = "box", dmin = 1, dmax = 1)))
  # The prep-level scale stands in where the plan entry carries none (the
  # flat ranked-bar path, where one bar_max covers every bar column).
  expect_match(strip(list(kind = "bar"), NULL, list(bar_max = 100)),
               "blockr-summarize-axis")
})

test_that("axis = FALSE drops every strip", {
  ae <- sum_fixture()
  S <- list(list(type = "dist", name = "Duration", col = "DUR", show = "box"))
  prep <- lane_prepare_summaries(ae, by = "ARM", summaries = S)
  expect_match(summarize_cells(prep)$thead, "blockr-summarize-axis")
  expect_false(grepl("blockr-summarize-axis",
                     summarize_cells(prep, cfg = list(axis = FALSE))$thead))
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
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S)
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
  # The legend comes from the spans colour: one group, titled by its column.
  expect_length(p$chrome$legend$groups, 1L)
  expect_identical(p$chrome$legend$groups[[1L]]$title, "SEV")
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
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S)
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
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S,
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
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S,
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
  p2 <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S,
                                facet = "ARM")
  expect_false(grepl("blockr-th-group", p2$head))
})

test_that("by_level with one summary per level keeps a single header row", {
  ae <- sum_fixture()
  S <- list(list(type = "simple", name = "Subjects", func = "count_distinct",
                 col = "USUBJID", show = "bar"))
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = S,
                         facet = "ARM", facet_layout = "by_level")
  expect_null(p$facet_spans)
  # Headed by the level over its N, never by the summary name.
  expect_identical(vapply(p$plan, function(x) x$label, ""),
                   c("Active", "Placebo"))
  expect_match(p$plan[[1L]]$sub_label, "^N = ")
})

test_that("colour is the SUMMARY's mapping: one column splits, the next does not", {
  ae <- sum_fixture()
  S <- list(
    list(type = "dist", name = "Split", col = "DUR", color = "SEV"),
    list(type = "dist", name = "Plain", col = "DUR")
  )
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = S)
  # Only the mapped column carries per-level geometry.
  expect_identical(p$plan[[1L]]$levels, c("MILD", "MOD"))
  expect_null(p$plan[[2L]]$levels)
  # One legend group, titled by the column it decodes.
  expect_length(p$color_groups, 1L)
  expect_identical(p$color_groups[[1L]]$column, "SEV")
})

test_that("two colour columns give two titled legend groups", {
  ae <- sum_fixture()
  S <- list(
    list(type = "dist", name = "By severity", col = "DUR", color = "SEV"),
    list(type = "dist", name = "By arm", col = "DUR", color = "ARM")
  )
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S)
  expect_identical(
    vapply(p$chrome$legend$groups, function(g) g$title, ""),
    c("SEV", "ARM")
  )
  expect_length(p$chrome$legend$groups[[1L]]$items, 2L)
})

test_that("a colour column with BLANK values still builds its payload", {
  # Adding a colour field on real data used to abort the whole render with
  # "subscript out of bounds": `pal[[""]]` finds nothing, because the empty
  # string matches no name in R.
  ae <- sum_fixture()
  ae$SEV[seq(1L, nrow(ae), by = 5L)] <- ""
  S <- list(list(type = "dist", name = "By severity", col = "DUR",
                 color = "SEV"))
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S)
  expect_identical(
    vapply(p$chrome$legend$groups[[1L]]$items, `[[`, character(1L), "label"),
    c("(Missing)", "MILD", "MOD")
  )
})

test_that("facet is the SUMMARY's mapping: only mapped columns repeat", {
  ae <- sum_fixture()
  S <- list(
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "USUBJID", show = "bar", facet = "ARM"),
    list(type = "simple", name = "Rows", func = "count", show = "bar")
  )
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = S)
  # Two copies of the faceted column, one of the plain one.
  expect_length(p$plan, 3L)
  # With one facet column across the table the level alone labels a copy.
  # A count's sub-label is its N (the arm's subjects), as on the ranked-bar
  # surface's facets.
  expect_identical(vapply(p$plan, function(x) x$label, ""),
                   c("Active", "Placebo", "Rows"))
  expect_identical(p$plan[[1L]]$sub_label,
                   paste0("N = ", dplyr::n_distinct(
                     ae$USUBJID[ae$ARM == "Active"])))
  # One shared facet column, so the by-level reading is still available.
  expect_identical(p$facet, "ARM")
})

test_that("columns may facet by DIFFERENT columns; the header names them", {
  ae <- sum_fixture()
  S <- list(
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "USUBJID", show = "bar", facet = "ARM"),
    list(type = "dist", name = "Duration", col = "DUR", stat = "mean_se",
         show = "text", facet = "SEV")
  )
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = S,
                         facet_layout = "by_level")
  expect_identical(vapply(p$plan, function(x) x$label, ""),
                   c("ARM: Active", "ARM: Placebo", "SEV: MILD", "SEV: MOD"))
  # No shared facet column, so the by-level reading has nothing to span and
  # the layout stays by_summary.
  expect_null(p$facet_spans)
  expect_null(p$facet)
  expect_identical(p$facet_levels, character())
  expect_identical(p$layout, "facet")
})

test_that("a one-level facet column draws one copy labelled with the level", {
  # A population filter that leaves one arm narrows the facet to one level.
  ae <- sum_fixture()
  ae$ONE <- "only"
  S <- list(list(type = "simple", name = "Rows", func = "count",
                 show = "bar", facet = "ONE"))
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = S)
  expect_null(p$err)
  expect_identical(vapply(p$plan, function(x) x$label, ""), "only")
})

test_that("the retired table-level pair fans down onto the rows it applied to", {
  ae <- sum_fixture()
  S <- lane_norm_summaries(list(
    list(type = "dist", name = "Split", col = "DUR"),
    list(type = "dist", name = "Text", col = "DUR", show = "text"),
    list(type = "simple", name = "Overall", func = "count", show = "bar",
         scope = "pooled"),
    list(type = "field", name = "Arms", col = "ARM")
  ))
  m <- lane_migrate_globals(S, color = "SEV", facet = "ARM")
  # Colour reaches every column that can draw a split, never a text cell;
  # the facet reaches the cell-scoped rows, never a pooled one or a field.
  expect_identical(vapply(m, function(s) s$color %||% "", ""),
                   c("SEV", "", "SEV", ""))
  expect_identical(vapply(m, function(s) s$facet %||% "", ""),
                   c("ARM", "ARM", "", ""))
  # A row that names its own keeps it.
  own <- lane_migrate_globals(
    lane_norm_summaries(list(list(type = "dist", col = "DUR",
                                  color = "ARM", facet = "SEV"))),
    color = "SEV", facet = "ARM"
  )
  expect_identical(own[[1L]]$color, "ARM")
  expect_identical(own[[1L]]$facet, "SEV")
})

test_that("a high-cardinality colour or facet is refused, naming the column", {
  ae <- sum_fixture()
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "dist", name = "Duration", col = "DUR", color = "USUBJID")
  ))
  expect_match(p$err, "colour column \"USUBJID\" has 19 levels")
  # Facet has the same ceiling for a different reason: one column per level.
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "dist", name = "Duration", col = "DUR", facet = "USUBJID")
  ))
  expect_match(p$err, "would repeat 19 times")
  # The gear seeds a mapping from the level counts it ships, so the pick it
  # offers is one that passes.
  cols <- summarize_gear_cols(ae)
  n <- vapply(cols, function(c) c$n_lev %||% NA_integer_, integer(1L))
  expect_identical(vapply(cols, function(c) c$name, "")[!is.na(n) & n <= 15L],
                   c("SOC", "TERM", "SEV", "ARM"))
  # USUBJID is exactly the column the seed must skip.
  expect_identical(n[vapply(cols, function(c) c$name, "") == "USUBJID"], 19L)
})

test_that("the ctor migrates the retired pair into the block's STATE", {
  b <- new_summarize_table_block(
    by = "TERM", color = "SEV", facet = "ARM",
    summaries = list(list(type = "dist", col = "DUR"),
                     list(type = "simple", func = "count", show = "bar",
                          scope = "pooled"))
  )
  st <- blockr.core::blockr_ser(b)$payload
  expect_identical(st$summaries[[1L]]$color, "SEV")
  expect_identical(st$summaries[[1L]]$facet, "ARM")
  # The pooled row was the "Overall" column: colour, but no facet.
  expect_identical(st$summaries[[2L]]$color, "SEV")
  expect_null(st$summaries[[2L]]$facet)
  # Cleared, so a mapping the user then removes from a column stays removed
  # instead of being re-applied on the next render.
  expect_null(st$color)
  expect_null(st$facet)
})

test_that("the ctor moves a ranked-bar grouping into `by`", {
  # summarize_prepare() falls back to group/parent when `by` is unset, so such a
  # board DRAWS -- while the summarize gear's required "Group by" row shows
  # empty. The state has to say what the table is doing.
  b <- new_summarize_table_block(group = "TERM", parent = "SOC",
                                 summaries = list(list(type = "dist", col = "DUR")))
  st <- blockr.core::blockr_ser(b)$payload
  expect_identical(st$by, c("SOC", "TERM"))
  # Cleared, so clearing "Group by" in the gear cannot resurrect them.
  expect_null(st$group)
  expect_null(st$parent)

  # The ranked-bar surface (no column list) keeps its own slots untouched.
  st <- blockr.core::blockr_ser(
    new_summarize_table_block(group = "TERM")
  )$payload
  expect_identical(st$group, "TERM")
  expect_null(st$by)
})

test_that("by nests one level: outer parent rows plus inner leaves", {
  ae <- sum_fixture()
  S <- list(list(type = "simple", name = "Rows", func = "count",
                 show = "bar"))
  p <- summarize_build_payload(ae, group = NULL, by = c("SOC", "TERM"),
                               summaries = S)
  expect_true(any(as.logical(p$parent_row)))
  expect_identical(sum(as.logical(p$parent_row)), 2L)
  # Three or more grouping columns is a config error, said plainly.
  p2 <- summarize_build_payload(ae, group = NULL,
                                by = c("SOC", "TERM", "USUBJID"), summaries = S)
  expect_identical(p2$kind, "html")
  expect_match(p2$html, "at most two")
})

test_that("expr rows evaluate per group and fail as a cell, not a crash", {
  ae <- sum_fixture()
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "expr", name = "n2", expr = "dplyr::n() * 2")
  ))
  expect_identical(p$cols[[1]]$kind, "num")
  expect_true(is.numeric(p$cols[[1]]$v))
  # A broken expression degrades to an error cell.
  p2 <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "expr", name = "boom", expr = "no_such_fn(DUR)")
  ))
  expect_true(all(as.character(p2$cols[[1]]$disp) == "(error)"))
})

test_that("spans label/fields enrich tips and key the highlight", {
  ae <- sum_fixture()
  ae$TERM2 <- ae$TERM   # the event label column
  ae$SER <- rep(c("Y", "N"), length.out = nrow(ae))
  p <- summarize_build_payload(ae, group = NULL, by = "USUBJID",
                               summaries = list(
    list(type = "spans", x = "ASTDY", xend = "AENDY", color = "SEV",
         label = "TERM2", fields = "SER", size = "lg")
  ))
  c1 <- p$cols[[1]]
  # The tooltip headlines the event, then level, span, field pairs.
  expect_match(c1$tips[[1]][[1]], "^Term[0-9] · SEV (MILD|MOD) · ")
  expect_match(c1$tips[[1]][[1]], "SER: (Y|N)")
  # The 4th segment slot carries the escaped label (data-l, the highlight
  # key); without `label` it is absent.
  expect_length(c1$segs[[1]][[1]], 4L)
  expect_match(c1$segs[[1]][[1]][[4L]], "^Term")
  expect_true(isTRUE(c1$lg))
  p2 <- summarize_build_payload(ae, group = NULL, by = "USUBJID",
                                summaries = list(
    list(type = "spans", x = "ASTDY", xend = "AENDY", color = "SEV")
  ))
  expect_length(p2$cols[[1]]$segs[[1]][[1]], 3L)
  expect_null(p2$cols[[1]]$lg)
})

test_that("a series ref computes a pooled line, mean_sd adds the band", {
  ae <- sum_fixture()
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "series", x = "ASTDY", col = "DUR", ref = "mean")
  ))
  c1 <- p$cols[[1]]
  expect_true(is.numeric(c1$rc))
  expect_null(c1$rby)                      # mean = line only
  p2 <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "series", x = "ASTDY", col = "DUR", ref = "mean_sd")
  ))
  c2 <- p2$cols[[1]]
  expect_true(is.numeric(c2$rby) && is.numeric(c2$rbh) && c2$rbh > 0)
  # The reference rides INSIDE the svg: band can exceed the observed range,
  # so the y-domain must have grown to keep it on canvas.
  expect_true(c2$rby >= 0)
  # No ref -> no coordinates shipped.
  p3 <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = list(
    list(type = "series", x = "ASTDY", col = "DUR")
  ))
  expect_null(p3$cols[[1]]$rc)
})

test_that("identity rides through simple rows: the value as-is", {
  d <- data.frame(g = c("a", "b", "c"), v = c(3, 9, 6))
  p <- summarize_build_payload(d, group = NULL, by = "g", summaries = list(
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
  p <- summarize_build_payload(ae, group = NULL, by = "TERM", summaries = S,
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
  p <- summarize_prepare(ae, group = "TERM", func = "count", sort_by = "data",
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

  m <- summarize_cells(p)
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
  html <- summarize_cells_html(summarize_cells(p1))
  expect_equal(lengths(regmatches(html, gregexpr("blockr-summarize-lv", html))),
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
  p2 <- summarize_prepare(ae, group = "TERM", by = "TERM", summaries = S[1],
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
  # list runs through summarize_levels().
  d$G <- rep(c("b", "a"), 4)
  p <- lane_prepare_summaries(d, by = c("ADY", "G"), summaries = S,
                              sort_by = "data", sort_dir = "asc")
  expect_identical(p$rows$.label[p$rows$.is_parent], c("1", "2", "10", "100"))
  expect_identical(summarize_levels(c(2, 10, 1, 100)), c("1", "2", "10", "100"))
  expect_identical(summarize_levels(as.Date(c("2024-01-10", "2024-01-02"))),
                   c("2024-01-02", "2024-01-10"))
})

test_that("a count column carries its N and draws no row for a missing group", {
  ae <- sum_fixture()
  # The population join's shape: one row per subject without a record, the
  # grouping column NA.
  extra <- ae[1:2, ]
  extra$TERM <- NA
  extra$USUBJID <- c("NEW-1", "NEW-2")
  extra$ARM <- c("Active", "Placebo")
  pop <- rbind(ae, extra)
  S <- list(
    list(type = "simple", name = "Total", func = "count_distinct",
         col = "USUBJID", show = "bar"),
    list(type = "simple", name = "Patients", func = "count_distinct",
         col = "USUBJID", show = "bar", facet = "ARM")
  )
  p <- summarize_prepare(pop, group = NULL, by = "TERM", summaries = S)
  expect_false(anyNA(p$rows$.label))
  n_all <- dplyr::n_distinct(pop$USUBJID)
  expect_identical(p$plan[[1L]]$sub_label, paste0("N = ", n_all))
  expect_equal(p$plan[[1L]]$val_denom, n_all)
  expect_equal(p$plan[[2L]]$val_denom,
               dplyr::n_distinct(pop$USUBJID[pop$ARM == "Active"]))
  # The label reads "n (x%)".
  m <- summarize_cells(p)
  expect_true(all(grepl("^\\(\\d+%\\)$", m$cols[[1L]]$pct[nzchar(m$cols[[1L]]$pct)])))
  # Two faceted columns keep their names beside N.
  S2 <- c(S[2L], list(modifyList(S[[2L]], list(name = "Again"))))
  p2 <- summarize_prepare(pop, group = NULL, by = "TERM", summaries = S2)
  expect_match(p2$plan[[1L]]$sub_label, "^Patients · N = ")
})

test_that("a table whose every row lacks a group says so instead of failing", {
  ae <- sum_fixture()
  ae$TERM <- NA
  S <- list(list(type = "simple", name = "n", func = "count_distinct",
                 col = "USUBJID", show = "bar"))
  p <- summarize_prepare(ae, group = NULL, by = "TERM", summaries = S)
  expect_identical(p$err, "No rows to display")
  p2 <- summarize_prepare(ae, group = NULL, by = c("ARM", "TERM"),
                          summaries = S)
  expect_false(is.null(p2$err) && anyNA(p2$rows$.label))
  # the payload the block builds carries the message, it does not throw
  expect_silent(summarize_build_payload(ae, by = "TERM", summaries = S))
})

test_that("facet columns with nothing to draw are left out, as the chart does", {
  ae <- sum_fixture()
  # a level kept by the factor after its rows were filtered away, and a level
  # whose rows carry no record (the population join's shape)
  ae$ARM <- factor(ae$ARM, levels = c(sort(unique(ae$ARM)), "Gone", "Empty"))
  extra <- ae[1:2, ]
  extra$TERM <- NA
  extra$USUBJID <- c("E-1", "E-2")
  extra$ARM <- factor("Empty", levels = levels(ae$ARM))
  pop <- rbind(ae, extra)

  S <- list(list(type = "simple", name = "n", func = "count_distinct",
                 col = "USUBJID", show = "bar", facet = "ARM"))
  p <- summarize_prepare(pop, group = NULL, by = "TERM", summaries = S)
  labels <- vapply(p$plan, function(x) x$label, "")
  expect_false(any(c("Gone", "Empty") %in% labels))
  expect_setequal(labels, c("Active", "Placebo"))

  # a distribution column: a level with no values goes too
  D <- list(list(type = "dist", name = "Dur", col = "DUR", show = "box",
                 facet = "ARM"))
  pd <- summarize_prepare(pop, group = NULL, by = "TERM", summaries = D)
  expect_false(any(c("Gone", "Empty") %in%
                     vapply(pd$plan, function(x) x$label, "")))

  # the ranked-bar surface the same
  pf <- summarize_prepare(pop, group = "TERM", facet = "ARM",
                          func = "count_distinct", id_var = "USUBJID")
  expect_false(any(c("Gone", "Empty") %in%
                     vapply(pf$plan, function(x) x$label, "")))
  expect_false(any(c("Gone", "Empty") %in% pf$facet_levels))
})

test_that("every facet copy carries its level's N, counted in subjects", {
  d <- data.frame(
    USUBJID = c("S1", "S1", "S2", "S3", "S3", "S4", "S5"),
    ARM = c("A", "A", "A", "B", "B", "B", "B"),
    TERM = c("x", "y", NA, "x", "x", "y", NA),
    AGE = c(30, 30, 40, 50, 50, 60, 70)
  )
  S <- list(
    list(type = "simple", name = "Patients", func = "count_distinct",
         col = "USUBJID", show = "bar", facet = "ARM"),
    list(type = "dist", name = "Age", col = "AGE", show = "box",
         facet = "ARM")
  )
  p <- summarize_prepare(d, by = "TERM", summaries = S, subject = "USUBJID")
  subs <- vapply(p$plan, function(x) x$sub_label, "")
  # rows with no TERM (the population join's) are in the N
  expect_identical(subs, c("Patients · N = 2", "Patients · N = 3",
                           "Age · N = 2", "Age · N = 3"))

  # no subject column: the level's rows
  p0 <- summarize_prepare(d, by = "TERM", summaries = S[2])
  expect_identical(vapply(p0$plan, function(x) x$sub_label, ""),
                   c("Age · N = 3", "Age · N = 4"))

  # by_level: the N moves up into the level's span
  pl <- summarize_prepare(d, by = "TERM", summaries = S,
                          facet_layout = "by_level", subject = "USUBJID")
  expect_identical(vapply(pl$facet_spans$groups, function(g) g$pop, 1L),
                   c(2L, 3L))
  expect_null(pl$plan[[2L]]$sub_label)
  expect_match(summarize_thead(pl), "N = 3</span>", fixed = TRUE)

  # the ranked-bar surface: a mean's facet N is subjects too
  pr <- summarize_prepare(d, group = "TERM", func = "mean", value = "AGE",
                          facet = "ARM", subject = "USUBJID")
  expect_identical(vapply(pr$plan, function(x) x$sub_label, ""),
                   c("N = 2", "N = 3"))
})
