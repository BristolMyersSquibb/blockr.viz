# Line features: several ymin/ymax intervals, linetype, x_ranges,
# direct_labels and the area type.

line_df <- function() {
  data.frame(
    d = as.Date("2024-01-01") + 0:5, v = c(1, 2, 3, 4, 5, 6),
    type = c("actual", "actual", "actual", "forecast", "forecast", "forecast"),
    l80 = c(NA, NA, NA, 3.5, 4, 4.5), h80 = c(NA, NA, NA, 4.5, 6, 7.5),
    l95 = c(NA, NA, NA, 3, 3, 3), h95 = c(NA, NA, NA, 5, 7, 9),
    stringsAsFactors = FALSE
  )
}

line_cfg <- function(blk, data = line_df) {
  cfg <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "drilldown-data")) sent[[length(sent) + 1L]] <<- message
      }
      session$flushReact()
      cfg <<- sent[[length(sent)]]$config
    },
    args = list(x = blk, data = list(data = data))
  )
  cfg
}

test_that("several intervals, a linetype, ranges and labels reach the browser", {
  blk <- new_chart_block(
    type = "line", x = "d", y = "v",
    ymin = c("l95", "l80"), ymax = c("h95", "h80"), linetype = "type",
    x_ranges = list(Forecast = c("2024-01-04", "2024-01-06")),
    direct_labels = TRUE
  )
  cfg <- line_cfg(blk)
  expect_equal(unlist(cfg$lo), c("l95", "l80"))
  expect_equal(unlist(cfg$hi), c("h95", "h80"))
  expect_equal(cfg$linetype, "type")
  expect_equal(cfg$x_ranges,
               list(list(name = "Forecast", from = "2024-01-04",
                         to = "2024-01-06")))
  expect_equal(cfg$direct_labels, "on")

  payload <- blockr.core::blockr_ser(blk)$payload
  expect_equal(payload$ymin, c("l95", "l80"))
  expect_equal(payload$x_ranges,
               list(Forecast = c("2024-01-04", "2024-01-06")))
  expect_error(new_chart_block(ymin = c("a", "b"), ymax = "c"),
               "same number")
})

test_that("x_ranges take dates and unnamed stretches, and drop junk", {
  r <- chart_ranges_state(list(
    A = as.Date(c("2020-01-01", "2020-06-30")), c(1, 5), "junk"
  ))
  expect_equal(names(r), c("A", "range2"))
  expect_equal(r$A, c("2020-01-01", "2020-06-30"))
  expect_equal(chart_ranges_json(r)[[2L]]$name, "")
  # what the browser sends back
  expect_equal(
    chart_ranges_state(list(list(name = "B", from = 1, to = 2))),
    list(B = c(1, 2))
  )
})

test_that("an area is an individual type, and the gear writes the new rows", {
  blk <- new_chart_block(type = "area", x = "d", y = "v")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      session$makeScope("expr")$setInputs(drilldown_block_action = list(
        action = "config", chart_type = "area", x = "d", y = "v",
        lo = list("l95", "l80"), hi = list("h95", "h80"),
        linetype = "type", direct_labels = "on"
      ))
      session$flushReact()
      st <- session$returned$state
      expect_equal(st$type(), "area")
      expect_equal(st$ymin(), c("l95", "l80"))
      expect_equal(st$linetype(), "type")
      expect_true(st$direct_labels())
    },
    args = list(x = blk, data = list(data = line_df))
  )
})

test_that("a name in y_lines / x_lines labels its line", {
  cfg <- line_cfg(new_chart_block(type = "line", x = "d", y = "v",
                                  y_lines = c(Target = 4, 6), x_lines = 19725))
  expect_equal(unlist(cfg$value_lines), c(4, 6))
  expect_equal(unlist(cfg$value_line_names), c("Target", ""))
  expect_length(cfg$x_line_names, 0L)
})
