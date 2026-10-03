# The scatter smoother overlay: what it is fit on.
#
# compute_smoother_series() is the R half of an R-computes / JS-renders pair
# (inst/js/chart/option-points.js draws the points it is handed, see
# smootherLine()). The
# thing worth pinning here is the SPLIT: which rows go into which fit. A fit
# over the wrong rows still draws a perfectly plausible line, so nothing on
# screen says it is wrong -- only a test does.

# Simpson's paradox in eight rows: within each panel the relationship rises,
# pooled across panels it falls. Any fit that is not per panel shows a
# NEGATIVE slope in a panel whose own points climb.
simpson_df <- function() {
  data.frame(
    g = rep(c("a", "b"), each = 4L),
    x = c(1, 2, 3, 4, 11, 12, 13, 14),
    y = c(10, 11, 12, 13, 1, 2, 3, 4)
  )
}

slope_of <- function(fit) {
  x <- unlist(fit$x)
  y <- unlist(fit$y)
  (y[length(y)] - y[1L]) / (x[length(x)] - x[1L])
}

test_that("a faceted fit is per panel, not one pooled line", {
  d <- simpson_df()

  res <- compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                                 series_by = NULL, facet_by = "g")

  # Keyed by facet level first, then by group.
  expect_named(res, c("a", "b"))
  expect_named(res$a, "__all__")
  expect_named(res$b, "__all__")

  # Each panel rises, though the pooled fit falls. This is the whole bug:
  # before the facet split both panels were drawn the pooled negative line.
  expect_gt(slope_of(res$a$`__all__`), 0)
  expect_gt(slope_of(res$b$`__all__`), 0)

  pooled <- compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                                    series_by = NULL, facet_by = NULL)
  expect_lt(slope_of(pooled$`__all__`), 0)
})

test_that("a panel's line spans that panel's x range, not the pooled one", {
  d <- simpson_df()

  res <- compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                                 series_by = NULL, facet_by = "g")

  # Panel "a" holds x 1:4. A pooled fit would have run it out to 14 --
  # the overshooting line that showed up on screen.
  expect_equal(range(unlist(res$a$`__all__`$x)), c(1, 4))
  expect_equal(range(unlist(res$b$`__all__`$x)), c(11, 14))
})

test_that("each panel's fit matches lm() on that panel's rows", {
  d <- simpson_df()

  res <- compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                                 series_by = NULL, facet_by = "g")

  for (lvl in c("a", "b")) {
    panel <- d[d$g == lvl, , drop = FALSE]
    coefs <- stats::coef(stats::lm(panel$y ~ panel$x))
    xs <- unlist(res[[lvl]]$`__all__`$x)
    expect_equal(unlist(res[[lvl]]$`__all__`$y),
                 unname(coefs[1L] + coefs[2L] * xs))
  }
})

test_that("facet and color split together, panel first", {
  d <- rbind(
    transform(simpson_df(), col = "p"),
    transform(simpson_df(), col = "q", y = simpson_df()$y * 2)
  )

  res <- compute_smoother_series(d, "lm", "x", "y", color_by = "col",
                                 series_by = NULL, facet_by = "g")

  expect_named(res, c("a", "b"))
  expect_named(res$a, c("p", "q"))
  # The q rows are the p rows with y doubled, so within a panel q is twice
  # as steep -- i.e. the two groups really were fit apart inside the panel.
  expect_equal(slope_of(res$a$q), 2 * slope_of(res$a$p))
})

test_that("an unfaceted chart keeps the flat one-level shape", {
  d <- simpson_df()

  res <- compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                                 series_by = NULL)

  expect_named(res, "__all__")
  expect_named(res$`__all__`, c("x", "y"))

  # A facet column that is not in the data is ignored rather than fatal:
  # the renderer then reads the top level, which is what it gets.
  expect_equal(
    compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                            series_by = NULL, facet_by = "nope"),
    res
  )
})

test_that("a panel too small to fit is dropped, and the others survive", {
  d <- rbind(
    simpson_df(),
    data.frame(g = "tiny", x = c(1, 2), y = c(5, 6))
  )

  res <- compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                                 series_by = NULL, facet_by = "g")

  # Two points are an interpolation, not an estimate (fit_one wants 3+).
  # The panel gets no entry at all, so the renderer draws no line there --
  # rather than borrowing a neighbouring panel's fit.
  expect_named(res, c("a", "b"))
  expect_null(res$tiny)
})

test_that("no panel can be fit gives NULL, not an empty list", {
  d <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 1, 2),
                  y = c(1, 2, 3, 4))

  expect_null(compute_smoother_series(d, "lm", "x", "y", color_by = NULL,
                                      series_by = NULL, facet_by = "g"))
})

test_that("a loess fit at or below loess_max is the exact fit", {
  set.seed(11)
  d <- data.frame(x = runif(300, 0, 10))
  d$y <- sin(d$x) + rnorm(300, 0, 0.3)
  res <- compute_smoother_series(d, "loess", "x", "y", color_by = NULL,
                                 series_by = NULL)
  xs <- seq(min(d$x), max(d$x), length.out = 100L)
  fit <- stats::loess(y ~ x, d, span = 0.75,
                      control = stats::loess.control(surface = "direct"))
  expect_equal(unlist(res$`__all__`$y),
               as.numeric(stats::predict(fit, data.frame(x = xs))))
})

test_that("a loess fit above loess_max is binned and stays on the exact line", {
  set.seed(12)
  d <- data.frame(x = sample(1:200, 4000, TRUE))
  d$y <- 10 * sin(d$x / 30) + rnorm(4000, 0, 5)
  exact <- compute_smoother_series(d, "loess", "x", "y", NULL, NULL,
                                   loess_max = Inf)
  binned <- compute_smoother_series(d, "loess", "x", "y", NULL, NULL,
                                    loess_max = 1000L)
  expect_identical(binned$`__all__`$x, exact$`__all__`$x)
  ye <- unlist(exact$`__all__`$y)
  yb <- unlist(binned$`__all__`$y)
  expect_length(yb, 100L)
  expect_true(all(is.finite(yb)))
  # Well inside the noise of the exact fit itself (sd 5 on 4000 rows).
  expect_lt(max(abs(yb - ye)), 0.1 * diff(range(ye)))
})

test_that("a loess fit on 100k rows is bounded in time", {
  set.seed(13)
  n <- 100000L
  d <- data.frame(x = sample(1:400, n, TRUE), g = sample(c("a", "b"), n, TRUE))
  d$y <- sin(d$x / 60) + rnorm(n)
  for (i in 1:20) d[[paste0("pad", i)]] <- rnorm(n)
  t <- system.time(
    res <- compute_smoother_series(d, "loess", "x", "y", color_by = "g",
                                   series_by = NULL)
  )[["elapsed"]]
  expect_named(res, c("a", "b"))
  expect_lt(t, 3)
})
