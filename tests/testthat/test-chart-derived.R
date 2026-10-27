# The chart's derived pieces from the state under the public names
# (R/chart-derived.R). The computations themselves are tested in
# test-band-series.R and test-smoother-series.R.

derived_df <- function() {
  set.seed(1)
  d <- data.frame(
    day = rep(1:30, 4),
    val = round(stats::rnorm(120, 10, 2), 2),
    id = rep(sprintf("S%d", 1:4), each = 30),
    arm = rep(c("A", "B"), each = 60),
    hi = rep(c(14, 15), 60),
    lo = 6,
    txt = "a"
  )
  attr(d$val, "label") <- "Value"
  d
}

band_state <- function(...) {
  utils::modifyList(list(
    type = "band", x = "day", y = "val", id = "id", band_window = "fixed",
    band_size = 3, band_min_n = 2
  ), list(...))
}

test_that("chart_band_args reads the public names and the defaults", {
  d <- derived_df()
  a <- chart_band_args(band_state(color = "arm", facet = "nope"), d)
  expect_identical(a$x_col, "day")
  expect_identical(a$color_by, "arm")
  expect_null(a$facet_by)
  expect_identical(a$id_col, "id")
  expect_identical(a$window, "fixed")
  expect_identical(a$summary, "median_q1_q3")
  expect_identical(a$whiskers, "p10_p90")
  expect_setequal(chart_args_cols(a, d), c("day", "val", "arm", "id"))

  a <- chart_band_args(list(type = "band", x = "day", y = "val", id = "gone"),
                       d)
  expect_identical(a[c("window", "window_size", "min_n")],
                   list(window = "adaptive", window_size = 45, min_n = 12))
  expect_null(a$id_col)
})

test_that("chart_band gives the series and the reference lines", {
  d <- derived_df()
  b <- chart_band(band_state(ref_max = "hi", ref_min = "lo"), d)
  expect_identical(names(b$series), "__all__")
  expect_identical(names(b$series$`__all__`), "__all__")
  expect_identical(b$series,
                   compute_band_series(d, "day", "val", NULL, NULL,
                                       window = "fixed", window_size = 3,
                                       min_n = 2, id_col = "id"))
  expect_identical(names(b$refs), c("hi", "lo"))
  expect_equal(b$refs$hi$value, 14.5)
  expect_equal(b$refs$hi[c("lo", "hi", "n_distinct")],
               list(lo = 14, hi = 15, n_distinct = 2L))
  expect_null(b$note)
})

test_that("chart_band explains an empty band and leaves other types alone", {
  d <- derived_df()
  b <- chart_band(band_state(band_min_n = 50), d)
  expect_null(b$series)
  expect_match(b$note, "cut-off of 50 subjects")
  expect_match(chart_band(band_state(x = "txt"), d)$note, "not numeric")
  expect_null(chart_band(band_state(ref_max = "txt"), d)$refs)
  expect_null(chart_band(band_state(type = "line"), d))
  expect_null(chart_band(band_state(), NULL))
})

test_that("chart_smoother fits on the scatter only, in log10 where asked", {
  d <- derived_df()
  st <- list(type = "scatter", x = "day", y = "val", smoother = "lm",
             color = "arm", x_transform = "identity", y_transform = "log10")
  a <- chart_smoother_args(st, d)
  expect_false(a$x_log)
  expect_true(a$y_log)
  s <- chart_smoother(st, d)
  expect_identical(names(s), c("A", "B"))
  expect_identical(s, compute_smoother_series(d, "lm", "day", "val", "arm",
                                              NULL, y_log = TRUE))
  expect_null(chart_smoother(utils::modifyList(st, list(smoother = "none")),
                             d))
  expect_null(chart_smoother(utils::modifyList(st, list(type = "line")), d))
  expect_null(chart_smoother(utils::modifyList(st, list(smoother = NULL)), d))
})

test_that("chart_smoother turns a failed fit into a warning and no line", {
  d <- derived_df()
  st <- list(type = "scatter", x = "day", y = "val", smoother = "lm")
  local_mocked_bindings(compute_smoother_series = function(...) stop("bad"))
  expect_warning(out <- chart_smoother(st, d), "smoother computation failed")
  expect_null(out)
})

test_that("chart_hist bins a numeric x and labels the bins as x", {
  d <- derived_df()
  h <- chart_hist(list(type = "histogram", x = "val", bins = 5), d)
  expect_identical(h$col, "val_bin")
  expect_identical(attr(h$bin, "label"), "Value")
  expect_length(h$bin, nrow(d))
  expect_identical(levels(h$bin), h$edges$label)
  expect_true(h$edges$last[nrow(h$edges)])
  out <- chart_hist_data(d, h)
  expect_identical(out$val_bin, h$bin)
  expect_identical(chart_hist_data(d, NULL), d)

  expect_null(chart_hist(list(type = "histogram", x = "txt", bins = 5), d))
  expect_null(chart_hist(list(type = "bar", x = "val", bins = 5), d))
  expect_null(chart_hist(list(type = "histogram", x = NULL, bins = 5), d))
})

test_that("a click on bins filters x inside them, the last bin closed", {
  d <- derived_df()
  h <- chart_hist(list(type = "histogram", x = "day", bins = 3), d)
  last <- utils::tail(h$edges$label, 1L)
  cond <- chart_bin_cond("day", c(h$edges$label[1L], last), h$edges)
  hit <- dplyr::filter(d, !!cond)
  e <- h$edges
  expect_true(all(hit$day < e$hi[1L] | hit$day >= e$lo[nrow(e)]))
  expect_true(30 %in% hit$day)
  expect_null(chart_bin_cond("day", "nope", h$edges))
  expect_null(chart_bin_cond("day", "x", NULL))
})

test_that("chart_band turns a failed computation into a warning and a note", {
  d <- derived_df()
  local_mocked_bindings(compute_band_series = function(...) stop("bad"))
  expect_warning(b <- chart_band(band_state(), d), "band computation failed")
  expect_null(b$series)
  expect_type(b$note, "character")
})
