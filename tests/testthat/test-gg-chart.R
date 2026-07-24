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
    gg_chart(d, "bar", group = "Species", color = "Grp"),
    gg_chart(d, "bar", group = "Species", color = "Grp",
             bar_mode = "grouped"),
    gg_chart(d, "bar", group = "Species", color = "Grp",
             bar_mode = "percent", orientation = "vertical"),
    gg_chart(d, "boxplot", group = "Species", value = "Sepal.Width",
             color = "Grp", box_points = "all"),
    gg_chart(d, "scatter", x = "Sepal.Length", y = "Sepal.Width",
             color = "Species", smoother = "lm", identity_line = TRUE),
    gg_chart(d, "line", x = "Petal.Length", y = "Petal.Width",
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
  p <- gg_chart(d, "bar", group = "Species")
  bars <- built(p)$data[[1L]]
  # 50 rows per species; horizontal bars carry the value on x.
  expect_setequal(bars$x, c(50, 50, 50))
})

test_that("count_distinct counts distinct values per cell", {
  d <- gg_iris()
  p <- gg_chart(d, "bar", group = "Species", value = "Grp",
                func = "count_distinct")
  bars <- built(p)$data[[1L]]
  expect_setequal(bars$x, c(2, 2, 2)) # A and B in every species
})

test_that("identity plots values as-is, first row per category wins", {
  d <- data.frame(k = c("a", "b", "a"), n = c(3, 9, 100))
  p <- gg_chart(d, "bar", group = "k", value = "n", func = "identity")
  bars <- built(p)$data[[1L]]
  # duplicate "a" collapses to its FIRST row (3), not 100 and not 103.
  expect_setequal(bars$x, c(3, 9))
})

test_that("value sort puts the largest category on top of a horizontal bar", {
  d <- data.frame(
    k = rep(c("small", "big", "mid"), c(1L, 5L, 3L))
  )
  p <- gg_chart(d, "bar", group = "k")
  lv <- levels(built(p)$plot$data$k)
  # ggplot draws the LAST level of a discrete y axis at the top.
  expect_identical(lv[[length(lv)]], "big")
  expect_identical(lv[[1L]], "small")
})

test_that("count labels annotate the category axis", {
  d <- gg_iris()
  p <- gg_chart(d, "bar", group = "Species", count_on = "axis",
                count_col = "Grp")
  b <- built(p)
  labs <- b$layout$panel_params[[1L]]$y$get_labels()
  expect_true(all(grepl("\\(2\\)$", labs))) # 2 distinct Grp per species
})

test_that("level colors cycle the shared palette over sorted levels", {
  d <- gg_iris()
  cols <- gg_level_colors(NULL, "Species", d)
  expect_identical(names(cols), c("setosa", "versicolor", "virginica"))
  expect_identical(unname(cols), DD_BLOCKR_PALETTE[1:3])
})

test_that("titles resolve templates, auto-compose and suppress", {
  d <- gg_iris()

  auto <- gg_chart(d, "bar", group = "Species", color = "Grp")
  expect_identical(auto$labels$title, "Count by Species and Grp")

  tpl <- gg_chart(d, "bar", group = "Species", title = "All {n} rows")
  expect_identical(tpl$labels$title, "All 150 rows")

  off <- gg_chart(d, "bar", group = "Species", title = "")
  expect_null(off$labels$title)
})

test_that("horizontal bar height follows the 28px row geometry", {
  many <- data.frame(k = as.character(seq_len(12L)))
  few <- data.frame(k = c("a", "b"))
  h_many <- attr(gg_chart(many, "bar", group = "k"), "pptx_height")
  h_few <- attr(gg_chart(few, "bar", group = "k"), "pptx_height")
  expect_gt(h_many, h_few)
  expect_lte(h_many, 5.6) # capped to the slide body
})

test_that("an uncovered type falls back to the aggregated data", {
  d <- gg_iris()
  expect_warning(
    out <- gg_chart(d, "pie", group = "Species"),
    "cannot draw"
  )
  expect_s3_class(out, "data.frame")
  expect_setequal(out$.value, c(50, 50, 50))
})

test_that("a state column missing from the data degrades, not errors", {
  d <- gg_iris()
  # facet/color dropped upstream: chart still renders without them.
  p <- gg_chart(d, "bar", group = "Species", color = "GONE", facet = "ALSO")
  expect_s3_class(p, "gg")
  # the group itself gone -> no chart to draw -> data fallback.
  expect_warning(out <- gg_chart(d, "bar", group = "GONE"), "cannot draw")
  expect_s3_class(out, "data.frame")
})
