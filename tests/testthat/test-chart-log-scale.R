test_that("y_scale and x_scale are linear or log", {
  blk <- new_chart_block(type = "line", x = "t", y = "v", y_scale = "log")
  expect_identical(chart_scale_state(NULL), "linear")
  expect_identical(chart_scale_state(list("log")), "log")
  expect_identical(chart_scale_state(NA), "linear")
  expect_error(new_chart_block(y_scale = "sqrt"), "\"linear\" or \"log\"")
  expect_error(new_chart_block(x_scale = TRUE), "`x_scale`")
  # The gear's value, or the current scale when it sends something else.
  expect_identical(chart_scale_msg("log", "linear"), "log")
  expect_identical(chart_scale_msg("sqrt", "log"), "log")
  expect_identical(chart_scale_msg(list(), "linear"), "linear")
})

test_that("only scatter, line and band take a log axis", {
  expect_identical(chart_log_axes("scatter", "log", "log"),
                   list(y = TRUE, x = TRUE))
  expect_identical(chart_log_axes("line", "log", "log"),
                   list(y = TRUE, x = FALSE))
  expect_identical(chart_log_axes("band", "log", "linear"),
                   list(y = TRUE, x = FALSE))
  for (type in c("bar", "area", "histogram", "boxplot", "gantt")) {
    expect_identical(chart_log_axes(type, "log", "log"),
                     list(y = FALSE, x = FALSE))
  }
})

test_that("the smoother is fitted on the scale the axis draws", {
  d <- data.frame(gdp = c(-1, 0, 500, 1000, 5000, 20000, 80000))
  d$life <- 30 + 10 * log10(pmax(d$gdp, 1))
  lin <- compute_smoother_series(d, "lm", "gdp", "life", NULL, NULL)
  lg <- compute_smoother_series(d, "lm", "gdp", "life", NULL, NULL,
                                x_log = TRUE)
  x <- unlist(lg$`__all__`$x)
  y <- unlist(lg$`__all__`$y)
  # The rows at or below zero are left out; x comes back in raw units.
  expect_equal(range(x), c(500, 80000))
  expect_equal(y, 30 + 10 * log10(x))
  # Fitted on raw x, the same rows give another line.
  expect_false(isTRUE(all.equal(unlist(lin$`__all__`$y)[100], 30 + 10 * log10(80000))))

  # A log y: fitted on log10(y), returned in raw units.
  d2 <- data.frame(t = 1:10, v = 10^(0.2 * (1:10)))
  fit <- compute_smoother_series(d2, "lm", "t", "v", NULL, NULL, y_log = TRUE)
  t <- unlist(fit$`__all__`$x)
  expect_equal(unlist(fit$`__all__`$y), 10^(0.2 * t))
})

test_that("the gear sets the scales and the browser reads them", {
  df <- data.frame(gx = c(100, 1000, 10000), gy = c(60, 70, 80))
  blk <- new_chart_block(type = "scatter", x = "gx", y = "gy",
                         x_scale = "log", smoother = "lm")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- new.env(parent = emptyenv())
      sent$msgs <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
        invisible(NULL)
      }
      session$flushReact()
      last_cfg <- function() {
        m <- Filter(function(m) identical(m$type, "drilldown-data"), sent$msgs)
        m[[length(m)]]$message$config
      }
      act <- function(...) {
        session$makeScope("expr")$setInputs(drilldown_block_action = list(
          action = "config", chart_type = "scatter", x = "gx", y = "gy",
          smoother = "lm", ...
        ))
        session$flushReact()
      }
      st <- session$returned$state
      expect_identical(st$x_scale(), "log")
      expect_identical(st$y_scale(), "linear")
      cfg <- last_cfg()
      expect_identical(cfg$x_scale, "log")
      expect_identical(cfg$y_scale, "linear")
      # The fit on log x is a straight line there: equal steps in log10(x)
      # give equal steps in y.
      sm <- cfg$smoother_series$`__all__`
      lx <- log10(unlist(sm$x))
      expect_equal(diff(unlist(sm$y)) / diff(lx),
                   rep(10, length(lx) - 1L))

      # A message without the fields keeps them.
      act(legend = "top")
      expect_identical(st$x_scale(), "log")

      act(x_scale = "linear", y_scale = "log")
      expect_identical(st$x_scale(), "linear")
      expect_identical(st$y_scale(), "log")
      expect_identical(last_cfg()$y_scale, "log")

      # A value the gear does not offer changes nothing.
      act(y_scale = "sqrt", x_scale = "linear")
      expect_identical(st$y_scale(), "log")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})
