# R/chart-expr.R :: chart-block state compiled to plain dplyr + ggplot2 code.
#
# The contract has two halves: the emitted code must EVALUATE to the same
# chart static_chart() renders from the same state (parity), and it must
# carry no blockr vocabulary at all (independence).

skip_if_not_installed("ggplot2")

ce_iris <- function() {
  transform(datasets::iris, Grp = rep(c("A", "B"), 75))
}

# Evaluate an emitted expression the way a rendered document would: the
# qualified form needs nothing attached beyond base.
ce_eval <- function(ex, d, var = "chart1") {
  env <- new.env(parent = baseenv())
  assign(var, d, envir = env)
  eval(ex, env)
}

built <- function(p) ggplot2::ggplot_build(p)

test_that("every family compiles, evaluates bare, and builds", {
  d <- ce_iris()

  cases <- list(
    chart_expr("chart1", "bar", group = "Species", color = "Grp",
               data = d, qualify = TRUE),
    chart_expr("chart1", "bar", group = "Species", color = "Grp",
               bar_mode = "grouped", data = d, qualify = TRUE),
    chart_expr("chart1", "bar", group = "Species", color = "Grp",
               bar_mode = "percent", orientation = "vertical",
               data = d, qualify = TRUE),
    chart_expr("chart1", "boxplot", group = "Species", value = "Sepal.Width",
               color = "Grp", box_points = "all", data = d, qualify = TRUE),
    chart_expr("chart1", "scatter", x = "Sepal.Length", y = "Sepal.Width",
               color = "Species", smoother = "lm", identity_line = TRUE,
               data = d, qualify = TRUE),
    chart_expr("chart1", "line", x = "Petal.Length", y = "Petal.Width",
               series = "Species", color = "Species", step = "middle",
               lo = "Sepal.Width", hi = "Sepal.Length",
               data = d, qualify = TRUE)
  )

  for (ex in cases) {
    expect_true(is.call(ex))
    p <- ce_eval(ex, d)
    expect_s3_class(p, "gg")
    expect_no_error(built(p))
  }
})

test_that("emitted code carries no blockr vocabulary", {
  d <- ce_iris()
  for (qualify in c(TRUE, FALSE)) {
    txt <- chart_code(chart_expr(
      "chart1", "bar", group = "Species", color = "Grp",
      count_on = "axis", data = d, qualify = qualify
    ))
    expect_no_match(txt, "blockr", fixed = TRUE)
    expect_no_match(txt, "static_chart", fixed = TRUE)
  }
})

test_that("bar counts and level order match static_chart", {
  d <- ce_iris()

  p <- ce_eval(chart_expr("chart1", "bar", group = "Species", color = "Grp",
                          data = d, qualify = TRUE), d)
  ps <- static_chart(d, "bar", group = "Species", color = "Grp")

  seg <- function(p) {
    b <- built(p)$data[[1L]]
    sort(round(b$xmax - b$xmin))
  }
  expect_identical(seg(p), seg(ps))

  # Same fills: the emitted scale bakes the palette static_chart cycles.
  fills <- function(p) sort(unique(built(p)$data[[1L]]$fill))
  expect_identical(fills(p), fills(ps))
})

test_that("value sort puts the largest category on top, both directions", {
  d <- data.frame(k = rep(c("small", "big", "mid"), c(1L, 5L, 3L)))

  lv_of <- function(...) {
    p <- ce_eval(chart_expr("chart1", "bar", group = "k", ...,
                            qualify = TRUE), d)
    b <- built(p)
    b$layout$panel_params[[1L]]$y$get_labels()
  }

  # ggplot lists discrete y labels bottom-up: largest last = largest on top.
  lv <- lv_of()
  expect_identical(lv[[length(lv)]], "big")

  lv_asc <- lv_of(sort_dir = "asc")
  expect_identical(lv_asc[[length(lv_asc)]], "small")
})

test_that("identity heights collapse duplicates like the canvas", {
  d <- data.frame(k = c("a", "b", "a"), n = c(3, 9, 100))
  p <- ce_eval(chart_expr("chart1", "bar", group = "k", value = "n",
                          func = "identity", qualify = TRUE), d)
  bars <- built(p)$data[[1L]]
  # duplicate "a" collapses to its FIRST row (3), not 100 and not 103.
  expect_setequal(round(bars$xmax - bars$xmin), c(3, 9))
})

test_that("count_distinct counts distinct values per cell", {
  d <- ce_iris()
  p <- ce_eval(chart_expr("chart1", "bar", group = "Species", value = "Grp",
                          func = "count_distinct", qualify = TRUE), d)
  bars <- built(p)$data[[1L]]
  expect_setequal(round(bars$xmax - bars$xmin), c(2, 2, 2))
})

test_that("count labels bake with a snapshot and recompute without", {
  d <- ce_iris()

  labs_of <- function(ex) {
    b <- built(ce_eval(ex, d))
    b$layout$panel_params[[1L]]$y$get_labels()
  }

  baked <- chart_expr("chart1", "bar", group = "Species",
                      count_on = "axis", count_col = "Grp",
                      data = d, qualify = TRUE)
  expect_true(all(grepl("\\(2\\)$", labs_of(baked))))
  # Baked = a literal named vector, no runtime lookup.
  expect_match(chart_code(baked), "setosa (2)", fixed = TRUE)

  live <- chart_expr("chart1", "bar", group = "Species",
                     count_on = "axis", count_col = "Grp", qualify = TRUE)
  expect_true(all(grepl("\\(2\\)$", labs_of(live))))
  # Live = a label function over the data variable.
  expect_match(chart_code(live), "chart1$Grp", fixed = TRUE)
})

test_that("titles: templates bake with a snapshot, auto tier is the label", {
  d <- ce_iris()

  tpl <- ce_eval(chart_expr("chart1", "bar", group = "Species",
                            title = "All {n} rows", data = d,
                            qualify = TRUE), d)
  expect_identical(tpl$labels$title, "All 150 rows")

  # The auto tier (NULL) is the snapshot's display attribute shown
  # verbatim, the same contract the canvas renders; no attribute (or no
  # snapshot), no title band.
  none <- ce_eval(chart_expr("chart1", "bar", group = "Species",
                             color = "Grp", qualify = TRUE), d)
  expect_null(none$labels$title)

  dl <- d
  attr(dl, "label") <- "Iris rows"
  auto <- ce_eval(chart_expr("chart1", "bar", group = "Species",
                             color = "Grp", data = dl, qualify = TRUE), dl)
  expect_identical(auto$labels$title, "Iris rows")

  off <- ce_eval(chart_expr("chart1", "bar", group = "Species", title = "",
                            qualify = TRUE), d)
  expect_null(off$labels$title)
})

test_that("facet_scales and facets reach the emitted facet_wrap", {
  d <- ce_iris()

  p <- ce_eval(chart_expr("chart1", "bar", group = "Species", facet = "Grp",
                          facet_scales = "free", qualify = TRUE), d)
  expect_true(p$facet$params$free$x)
  expect_true(p$facet$params$free$y)

  fixed <- chart_expr("chart1", "bar", group = "Species", facet = "Grp",
                      qualify = TRUE)
  expect_no_match(chart_code(fixed), "scales", fixed = TRUE)
})

test_that("uncompilable state returns NULL", {
  expect_null(chart_expr("chart1", "pie", group = "Species"))
  expect_null(chart_expr("chart1", "bar")) # no group
  expect_null(chart_expr("chart1", "bar", group = "k", func = "mean")) # no value
})

test_that("a state column missing from the snapshot degrades, not errors", {
  d <- ce_iris()
  ex <- chart_expr("chart1", "bar", group = "Species", color = "GONE",
                   facet = "ALSO", data = d, qualify = TRUE)
  expect_s3_class(ce_eval(ex, d), "gg")
  expect_no_match(chart_code(ex), "GONE", fixed = TRUE)
})

test_that("chart_code renders the pipe form, one layer per line", {
  ex <- chart_expr("chart1", "bar", group = "Species", color = "Grp")
  txt <- chart_code(ex)
  lines <- strsplit(txt, "\n")[[1L]]

  expect_identical(lines[[1L]], "chart1 |>")
  expect_match(lines[[2L]], "^  count\\(.*\\|>$")
  expect_match(lines[[3L]], "^  ggplot\\(aes\\(")
  # No orphaned operators on their own line.
  expect_no_match(txt, "\n\\s*[+|]")
})

test_that("the emitted pipeline round-trips through parse", {
  ex <- chart_expr("chart1", "bar", group = "Species", color = "Grp",
                   data = ce_iris(), qualify = TRUE)
  txt <- chart_code(ex)
  reparsed <- parse(text = txt, keep.source = FALSE)[[1L]]
  p <- ce_eval(reparsed, ce_iris())
  expect_s3_class(p, "gg")
})
