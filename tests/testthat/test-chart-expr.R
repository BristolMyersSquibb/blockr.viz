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

# The tick labels of one axis, in ggplot's own order (discrete axes list
# bottom-up / left-to-right).
ce_cats <- function(p, axis) {
  built(p)$layout$panel_params[[1L]][[axis]]$get_labels()
}

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

# The canvas resolves unset state per family: a bar lies horizontal and
# sorts by value, a boxplot stands vertical and keeps the data's own order
# (_ensureDistributionMetric). A compiler that skips that emits a picture
# the screen never showed.
test_that("an unset boxplot stands vertical in data order, like static_chart", {
  d <- data.frame(
    k = factor(rep(c("20 mg", "5 mg", "10 mg"), each = 4L),
               levels = c("5 mg", "10 mg", "20 mg")),
    v = rep(c(9, 1, 5), each = 4L)
  )

  p <- ce_eval(chart_expr("chart1", "boxplot", group = "k", value = "v",
                          data = d, qualify = TRUE), d)
  # Categories on x, in the factor's levels -- not reordered by the median.
  expect_identical(ce_cats(p, "x"), levels(d$k))
  expect_identical(
    ce_cats(p, "x"),
    ce_cats(static_chart(d, "boxplot", group = "k", value = "v"), "x")
  )

  h <- ce_eval(chart_expr("chart1", "boxplot", group = "k", value = "v",
                          orientation = "horizontal", data = d,
                          qualify = TRUE), d)
  # Horizontal puts them on y, first level at the top = last in ggplot's
  # bottom-up listing.
  expect_identical(ce_cats(h, "y"), rev(levels(d$k)))
  expect_identical(
    ce_cats(h, "y"),
    ce_cats(static_chart(d, "boxplot", group = "k", value = "v",
                         orientation = "horizontal"), "y")
  )
})

test_that("the data order recomputes from the column with no snapshot", {
  fct <- data.frame(k = factor(c("b", "a"), levels = c("b", "a")), v = c(1, 2))
  p <- ce_eval(chart_expr("chart1", "boxplot", group = "k", value = "v",
                          qualify = TRUE), fct)
  expect_identical(ce_cats(p, "x"), c("b", "a"))

  # A character column has no levels: first seen, not alphabetical.
  chr <- data.frame(k = c("b", "a"), v = c(1, 2))
  q <- ce_eval(chart_expr("chart1", "boxplot", group = "k", value = "v",
                          qualify = TRUE), chr)
  expect_identical(ce_cats(q, "x"), c("b", "a"))
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

test_that("the compiled chart turns and cuts the same labels", {

  long <- paste0("GROUP", LETTERS[1:6],
                 " BMS-986507 2.0mg+Pumitamig 1500 or 1200mg")
  d <- data.frame(k = factor(rep(long, each = 4L), levels = long),
                  v = rep(c(1, 5, 9, 4, 6, 3), each = 4L))

  p <- ce_eval(chart_expr("chart1", "boxplot", group = "k", value = "v",
                          data = d, qualify = TRUE), d)
  expect_identical(p$theme$axis.text.x$angle, 90)

  # Both renderers turn and cut; each measures at the size IT draws axis
  # text (8.25pt here, 8.8pt in the compiled theme), so the cut lands a
  # character apart. What has to agree is the order and the arms staying
  # distinguishable.
  drawn <- as.character(ce_cats(p, "x"))
  expect_true(all(grepl("\u2026$", drawn)))
  expect_identical(length(unique(drawn)), 6L)
  expect_identical(sub("\u2026$", "", drawn),
                   substr(long, 1L, nchar(sub("\u2026$", "", drawn[[1L]]))))

  # Without a snapshot there is nothing to measure, so the labels stay flat
  # and whole rather than being cut on a guess.
  live <- chart_expr("chart1", "boxplot", group = "k", value = "v",
                     qualify = TRUE)
  expect_no_match(chart_code(live), "angle", fixed = TRUE)

  short <- data.frame(k = factor(rep(c("A", "B", "C"), each = 4L)),
                      v = rep(c(1, 5, 9), each = 4L))
  flat <- chart_expr("chart1", "boxplot", group = "k", value = "v",
                     data = short, qualify = TRUE)
  expect_no_match(chart_code(flat), "angle", fixed = TRUE)
})
