test_that("highlight state is one column and its values", {
  expect_null(chart_highlight_state(NULL))
  expect_null(chart_highlight_state(list()))
  expect_identical(
    chart_highlight_state(list(country = "Switzerland")),
    list(country = "Switzerland")
  )
  # A JSON array arrives as a list; NA and "" are no values.
  expect_identical(
    chart_highlight_state(list(country = list("CH", "DE", NA, ""))),
    list(country = c("CH", "DE"))
  )
  expect_identical(
    chart_highlight_state(c(country = "CH", country = "DE")),
    list(country = c("CH", "DE"))
  )
  expect_null(chart_highlight_state(list(country = character())))
  expect_error(chart_highlight_state("CH"), "names a column")
  expect_error(
    chart_highlight_state(list(country = "CH", region = "Europe")),
    "one column"
  )
})

test_that("the gear's column and values make the state", {
  expect_identical(chart_highlight_msg("country", list("CH")),
                   list(country = "CH"))
  expect_null(chart_highlight_msg("country", list()))
  expect_null(chart_highlight_msg("", list("CH")))
})

test_that("the browser gets the column and the values apart", {
  expect_identical(
    chart_highlight_json(list(country = c("CH", "DE"))),
    list(by = "country", values = list("CH", "DE"))
  )
  expect_identical(chart_highlight_json(NULL), list(by = NULL, values = NULL))
  # A shape that is no highlight draws none instead of erroring.
  expect_identical(chart_highlight_json("CH"), list(by = NULL, values = NULL))
})

test_that("{highlight} prints the values in a title", {
  expect_identical(chart_highlight_text(list(country = "CH")), "CH")
  expect_identical(chart_highlight_text(list(country = c("CH", "DE"))),
                   "CH and DE")
  expect_identical(chart_highlight_text(list(country = c("CH", "DE", "JP"))),
                   "CH, DE and JP")
  expect_identical(chart_highlight_text(NULL), "")

  d <- data.frame(country = c("CH", "DE"))
  attr(d, "blockr_highlight") <- "CH"
  expect_identical(
    resolve_block_title("GDP per head[, {highlight} highlighted]", d),
    "GDP per head, CH highlighted"
  )
  attr(d, "blockr_highlight") <- ""
  expect_identical(
    resolve_block_title("GDP per head[, {highlight} highlighted]", d),
    "GDP per head"
  )
  # Without the attribute it is a column name like any other.
  d2 <- data.frame(highlight = "a")
  expect_identical(resolve_block_title("{highlight}", d2), "a")
})

test_that("the gear sets, keeps and clears the highlight", {
  df <- data.frame(country = c("CH", "DE", "JP"), region = c("E", "E", "A"),
                   gx = 1:3, gy = c(2, 4, 3))
  blk <- new_chart_block(type = "scatter", x = "gx", y = "gy",
                         highlight = list(country = "CH"))
  expect_error(new_chart_block(highlight = "CH"), "names a column")
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
        m[[length(m)]]$message
      }
      act <- function(...) {
        session$makeScope("expr")$setInputs(drilldown_block_action = list(
          action = "config", chart_type = "scatter", x = "gx", y = "gy", ...
        ))
        session$flushReact()
      }
      st <- session$returned$state
      expect_identical(st$highlight(), list(country = "CH"))
      msg <- last_cfg()
      expect_identical(msg$config$highlight_by, "country")
      expect_identical(msg$config$highlight, list("CH"))
      # The rows carry the column, though nothing maps it.
      expect_match(msg$data, "country", fixed = TRUE)

      # A message without the fields keeps it.
      act(legend = "top")
      expect_identical(st$highlight(), list(country = "CH"))

      # A column without values is no highlight yet, but the browser gets
      # the column's values to offer.
      act(highlight_by = "region", highlight = list())
      expect_null(st$highlight())
      expect_identical(last_cfg()$config$highlight_by, "region")
      expect_match(last_cfg()$data, "region", fixed = TRUE)

      act(highlight_by = "region", highlight = list("E"))
      expect_identical(st$highlight(), list(region = "E"))

      act(highlight_by = "", highlight = list())
      expect_null(st$highlight())
      expect_null(last_cfg()$config$highlight_by)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("the subtitle says what is highlighted", {
  df <- data.frame(country = c("CH", "DE"), gx = 1:2, gy = 1:2)
  blk <- new_chart_block(type = "scatter", x = "gx", y = "gy",
                         highlight = list(country = c("CH", "DE")),
                         subtitle = "GDP[, {highlight} highlighted]")
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
    args = list(x = blk, data = list(data = function() df))
  )
  expect_identical(cfg$subtitle_resolved, "GDP, CH and DE highlighted")
})
