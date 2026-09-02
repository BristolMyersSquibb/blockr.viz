# R/gg-chart.R :: the static ggplot renderer for chart-block state.
#
# Numbers are delegated to dd_table_aggregate (golden-tested against the JS
# engine elsewhere), so these tests pin the gg-side semantics: the identity
# mode, category ordering, count labels, level colors, sizing attributes,
# and the fallback contract.

skip_if_not_installed("ggplot2")

gg_iris <- function() {
  transform(datasets::iris, Grp = rep(c("A", "B"), 75))
}

built <- function(p) ggplot2::ggplot_build(p)

test_that("every covered family builds a ggplot with pptx size attrs", {
  d <- gg_iris()

  cases <- list(
    static_chart(d, "bar", group = "Species", color = "Grp"),
    static_chart(d, "bar", group = "Species", color = "Grp",
             bar_mode = "grouped"),
    static_chart(d, "bar", group = "Species", color = "Grp",
             bar_mode = "percent", orientation = "vertical"),
    static_chart(d, "boxplot", group = "Species", value = "Sepal.Width",
             color = "Grp", box_points = "all"),
    static_chart(d, "scatter", x = "Sepal.Length", y = "Sepal.Width",
             color = "Species", smoother = "lm", identity_line = TRUE),
    static_chart(d, "line", x = "Petal.Length", y = "Petal.Width",
             series = "Species", color = "Species", step = "middle",
             lo = "Sepal.Width", hi = "Sepal.Length")
  )

  for (p in cases) {
    expect_s3_class(p, "gg")
    expect_true(is.numeric(attr(p, "pptx_width")))
    expect_true(is.numeric(attr(p, "pptx_height")))
    expect_no_error(built(p))
  }
})

test_that("bar counts match the shared aggregation engine", {
  d <- gg_iris()
  p <- static_chart(d, "bar", group = "Species")
  bars <- built(p)$data[[1L]]
  # 50 rows per species; horizontal bars carry the value on x.
  expect_setequal(bars$x, c(50, 50, 50))
})

test_that("count_distinct counts distinct values per cell", {
  d <- gg_iris()
  p <- static_chart(d, "bar", group = "Species", value = "Grp",
                func = "count_distinct")
  bars <- built(p)$data[[1L]]
  expect_setequal(bars$x, c(2, 2, 2)) # A and B in every species
})

test_that("identity plots values as-is, first row per category wins", {
  d <- data.frame(k = c("a", "b", "a"), n = c(3, 9, 100))
  p <- static_chart(d, "bar", group = "k", value = "n", func = "identity")
  bars <- built(p)$data[[1L]]
  # duplicate "a" collapses to its FIRST row (3), not 100 and not 103.
  expect_setequal(bars$x, c(3, 9))
})

test_that("category order mirrors the canvas orderGroups", {
  d <- data.frame(
    k = rep(c("small", "big", "mid"), c(1L, 5L, 3L))
  )

  # An unset bar sort normalizes to value DESCENDING, largest at the TOP
  # of a horizontal chart (_ensureFamilyDefaults; the canvas draws with
  # inverse: true, ggplot puts the LAST level of a discrete y axis on top,
  # so the level vector is reversed).
  def <- static_chart(d, "bar", group = "k")
  expect_identical(rev(levels(built(def)$plot$data$k)),
                   c("big", "mid", "small"))

  asc <- static_chart(d, "bar", group = "k", sort_by = "value",
                  sort_dir = "asc")
  expect_identical(rev(levels(built(asc)$plot$data$k)),
                   c("small", "mid", "big"))

  alpha <- static_chart(d, "bar", group = "k", sort_by = "alpha",
                    sort_dir = "asc")
  expect_identical(rev(levels(built(alpha)$plot$data$k)),
                   c("big", "mid", "small"))

  # "data" = first-seen order in the raw rows.
  dat <- static_chart(d, "bar", group = "k", sort_by = "data",
                  sort_dir = "asc")
  expect_identical(rev(levels(built(dat)$plot$data$k)),
                   c("small", "big", "mid"))
})

test_that("count labels annotate the category axis", {
  d <- gg_iris()
  p <- static_chart(d, "bar", group = "Species", count_on = "axis",
                count_col = "Grp")
  b <- built(p)
  labs <- b$layout$panel_params[[1L]]$y$get_labels()
  expect_true(all(grepl("\\(2\\)$", labs))) # 2 distinct Grp per species
})

test_that("level colors cycle the shared palette over sorted levels", {
  d <- gg_iris()
  cols <- gg_level_colors(NULL, "Species", d)
  expect_identical(names(cols), c("setosa", "versicolor", "virginica"))
  expect_identical(unname(cols), dd_palette()[1:3])
})

test_that("titles resolve templates, auto tier and suppress", {
  d <- gg_iris()

  # The auto tier (NULL) is the data's display attribute shown verbatim --
  # the same contract the canvas receives; no attribute, no title band.
  none <- static_chart(d, "bar", group = "Species", color = "Grp")
  expect_null(none$labels$title)

  attr(d, "label") <- "Iris rows"
  auto <- static_chart(d, "bar", group = "Species", color = "Grp")
  expect_identical(auto$labels$title, "Iris rows")

  tpl <- static_chart(d, "bar", group = "Species", title = "All {n} rows")
  expect_identical(tpl$labels$title, "All 150 rows")

  off <- static_chart(d, "bar", group = "Species", title = "")
  expect_null(off$labels$title)
})

test_that("horizontal bar height follows the 28px row geometry", {
  many <- data.frame(k = as.character(seq_len(12L)))
  few <- data.frame(k = c("a", "b"))
  h_many <- attr(static_chart(many, "bar", group = "k"), "pptx_height")
  h_few <- attr(static_chart(few, "bar", group = "k"), "pptx_height")
  expect_gt(h_many, h_few)
  expect_lte(h_many, 5.6) # capped to the slide body
})

test_that("an uncovered type falls back to the aggregated data", {
  d <- gg_iris()
  expect_warning(
    out <- static_chart(d, "pie", group = "Species"),
    "cannot draw"
  )
  expect_s3_class(out, "data.frame")
  expect_setequal(out$.value, c(50, 50, 50))
})

test_that("a state column missing from the data degrades, not errors", {
  d <- gg_iris()
  # facet/color dropped upstream: chart still renders without them.
  p <- static_chart(d, "bar", group = "Species", color = "GONE", facet = "ALSO")
  expect_s3_class(p, "gg")
  # the group itself gone -> no chart to draw -> data fallback.
  expect_warning(out <- static_chart(d, "bar", group = "GONE"), "cannot draw")
  expect_s3_class(out, "data.frame")
})

test_that("facet_scales reaches facet_wrap, fixed by default", {
  d <- gg_iris()

  fixed <- static_chart(d, "bar", group = "Species", facet = "Grp")
  free <- static_chart(d, "bar", group = "Species", facet = "Grp",
                       facet_scales = "free")

  # facet_wrap() stores the pick on the facet params (free is a per-axis
  # list, so "fixed" is both FALSE and "free" both TRUE).
  expect_false(fixed$facet$params$free$x)
  expect_false(fixed$facet$params$free$y)
  expect_true(free$facet$params$free$x)
  expect_true(free$facet$params$free$y)

  # free_y frees the value axis only -- the canvas' meaning of the word.
  free_y <- static_chart(d, "bar", group = "Species", facet = "Grp",
                         facet_scales = "free_y")
  expect_false(free_y$facet$params$free$x)
  expect_true(free_y$facet$params$free$y)

  # A bad value is a bug in the caller, not something to render around.
  expect_error(
    static_chart(d, "bar", group = "Species", facet = "Grp",
                 facet_scales = "loose")
  )
})

# --- na_group / pct_distinct parity with the canvas -------------------------
# The exported chart and the one on screen must say the same thing. This
# package already learned that the hard way with downloads (a slide and a
# download were two renderings of one chart), so the two additions that change
# what a bar MEANS get parity tests rather than trust.

pop_df <- data.frame(
  AEDECOD = c("Diarrhoea", "Diarrhoea", "Nausea", NA),
  USUBJID = c("S1", "S2", "S3", "S4"),
  stringsAsFactors = FALSE
)

test_that("na_group = 'drop' removes the category from the static chart too", {
  keep <- blockr.viz:::gg_agg(pop_df, "AEDECOD", NULL, NULL, "USUBJID",
                              "count_distinct", na_group = "level")
  drop <- blockr.viz:::gg_agg(pop_df, "AEDECOD", NULL, NULL, "USUBJID",
                              "count_distinct", na_group = "drop")
  expect_true(anyNA(keep$AEDECOD))
  expect_false(anyNA(drop$AEDECOD))
  expect_equal(nrow(drop), 2L)
})

test_that("pct_distinct divides by the panel, matching the JS engine", {
  out <- blockr.viz:::gg_agg(pop_df, "AEDECOD", NULL, NULL, "USUBJID",
                             "pct_distinct", na_group = "drop")
  v <- stats::setNames(out$.value, out$AEDECOD)
  # 2 of the 4 SUBJECTS, not 2 of the 3 with a term -- S4 is dropped from the
  # categories and kept in the denominator, which is the whole point.
  expect_equal(unname(v[["Diarrhoea"]]), 2 / 4)
  expect_equal(unname(v[["Nausea"]]), 1 / 4)
})

test_that("each facet gets its own denominator in the static chart", {
  d <- rbind(
    transform(pop_df, TRT = "A"),
    data.frame(AEDECOD = c("Diarrhoea", NA), USUBJID = c("T1", "T2"),
               TRT = "B", stringsAsFactors = FALSE)
  )
  out <- blockr.viz:::gg_agg(d, "AEDECOD", NULL, "TRT", "USUBJID",
                             "pct_distinct", na_group = "drop")
  v <- stats::setNames(out$.value, paste(out$TRT, out$AEDECOD))
  expect_equal(unname(v[["A Diarrhoea"]]), 2 / 4)
  expect_equal(unname(v[["B Diarrhoea"]]), 1 / 2)
})

test_that("pct_distinct with no value column draws nothing, like the canvas", {
  expect_null(blockr.viz:::gg_agg(pop_df, "AEDECOD", NULL, NULL, NULL,
                                  "pct_distinct"))
})

test_that("pct_of picks the same denominator in the static chart", {
  # Export parity. The canvas numbers are guarded in test-agg-pct-distinct.R;
  # this pins that a PNG says the same thing as the screen, which is the
  # failure this package already had once with downloads.
  d <- rbind(
    data.frame(USUBJID = sprintf("U%02d", 1:10), COUNTRY = "USA",
               ACTION = c(rep("CHANGED", 4), rep(NA, 6)),
               stringsAsFactors = FALSE),
    data.frame(USUBJID = sprintf("J%02d", 1:5), COUNTRY = "JPN",
               ACTION = c(rep("CHANGED", 3), rep(NA, 2)),
               stringsAsFactors = FALSE)
  )
  by_group <- blockr.viz:::gg_agg(d, "COUNTRY", NULL, "ACTION", "USUBJID",
                                  "pct_distinct", na_group = "drop",
                                  pct_of = "group")
  v <- stats::setNames(by_group$.value, by_group$COUNTRY)
  expect_equal(unname(v[["USA"]]), 4 / 10)
  expect_equal(unname(v[["JPN"]]), 3 / 5)

  by_facet <- blockr.viz:::gg_agg(d, "COUNTRY", NULL, "ACTION", "USUBJID",
                                  "pct_distinct", na_group = "drop",
                                  pct_of = "facet")
  w <- stats::setNames(by_facet$.value, by_facet$COUNTRY)
  expect_equal(unname(w[["USA"]]), 4 / 7)
  expect_equal(unname(w[["JPN"]]), 3 / 7)
})

# The canvas turns a vertical layout's category labels when they do not fit
# their column, and truncates the turned ones at a cap (chart.js
# _xAxisLabels). A slide has the same problem and less room to grow.
test_that("long category labels turn 90 degrees and cut at the cap", {

  long <- paste0("GROUP", LETTERS[1:6],
                 " BMS-986507 2.0mg+Pumitamig 1500 or 1200mg")
  d <- data.frame(k = factor(rep(long, each = 4L), levels = long),
                  v = rep(c(1, 5, 9, 4, 6, 3), each = 4L))

  p <- static_chart(d, "boxplot", group = "k", value = "v")
  expect_identical(p$theme$axis.text.x$angle, 90)

  drawn <- built(p)$layout$panel_params[[1L]]$x$get_labels()
  expect_true(all(grepl("\u2026$", drawn)))
  # Cut, not collapsed: the arms are still told apart.
  expect_identical(length(unique(drawn)), 6L)

  short <- data.frame(k = factor(rep(c("A", "B", "C"), each = 4L)),
                      v = rep(c(1, 5, 9), each = 4L))
  flat <- static_chart(short, "boxplot", group = "k", value = "v")
  expect_null(flat$theme$axis.text.x$angle)
  expect_identical(built(flat)$layout$panel_params[[1L]]$x$get_labels(),
                   c("A", "B", "C"))

  # Horizontal reads them left to right, so nothing turns and nothing is cut.
  h <- static_chart(d, "boxplot", group = "k", value = "v",
                    orientation = "horizontal")
  expect_null(h$theme$axis.text.x$angle)
  expect_identical(built(h)$layout$panel_params[[1L]]$y$get_labels(),
                   rev(long))
})
