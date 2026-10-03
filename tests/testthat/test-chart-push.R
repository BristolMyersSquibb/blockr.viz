# What the chart block's server sends, and when it leaves the rows out.

push_df <- function() {
  data.frame(
    PARAMCD = rep(c("SYSBP", "DIABP"), each = 3),
    AVAL = c(120, 118, 122, 80, 78, 82),
    stringsAsFactors = FALSE
  )
}

# Records every custom message the session sends. Install before the first
# flush: the push observer fires on it.
spy_messages <- function(session) {
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  root <- session$rootScope()
  root$sendCustomMessage <- function(type, message) {
    sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
    invisible(NULL)
  }
  function(type = "drilldown-data") {
    lapply(Filter(function(m) identical(m$type, type), sent$msgs),
           `[[`, "message")
  }
}

test_that("a config edit is pushed without rows, a data change with them", {
  src <- new.env(parent = emptyenv())
  src$d <- push_df()
  tick <- shiny::reactiveVal(0L)
  dat <- shiny::reactive({
    tick()
    src$d
  })
  blk <- new_chart_block(chart_type = "bar", group = "PARAMCD")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      msgs <- spy_messages(session)
      session$flushReact()
      expect_length(msgs(), 1L)
      first <- msgs()[[1L]]
      expect_false(is.null(first[["data"]]))

      expr <- session$makeScope("expr")
      expr$setInputs(drilldown_block_action = list(
        action = "config", sort_dir = "asc"
      ))
      session$flushReact()
      expect_length(msgs(), 2L)
      second <- msgs()[[2L]]
      expect_null(second[["data"]])
      expect_identical(second$data_rev, first$data_rev)
      expect_equal(second$config$sort_dir, "asc")

      src$d$PARAMCD[1L] <- "MAP"
      tick(1L)
      session$flushReact()
      expect_length(msgs(), 3L)
      third <- msgs()[[3L]]
      expect_false(is.null(third[["data"]]))
      expect_false(identical(third$data_rev, first$data_rev))
    },
    args = list(x = blk, data = list(data = dat))
  )
})

test_that("a _need request is answered with the whole last message", {
  blk <- new_chart_block(chart_type = "bar", group = "PARAMCD")
  df <- push_df()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      msgs <- spy_messages(session)
      session$flushReact()
      expr <- session$makeScope("expr")
      expr$setInputs(drilldown_block_action = list(
        action = "config", sort_dir = "asc"
      ))
      session$flushReact()
      expect_null(msgs()[[2L]][["data"]])

      expr$setInputs(drilldown_block_need = msgs()[[2L]]$data_rev)
      session$flushReact()
      expect_length(msgs(), 3L)
      full <- msgs()[[3L]]
      expect_false(is.null(full[["data"]]))
      expect_identical(full$data_rev, msgs()[[1L]]$data_rev)
      expect_identical(full[["data"]], msgs()[[1L]][["data"]])
      expect_equal(full$config$sort_dir, "asc")

      # The client holds the rows again, so the next edit leaves them out.
      expr$setInputs(drilldown_block_action = list(
        action = "config", sort_dir = "desc"
      ))
      session$flushReact()
      expect_null(msgs()[[4L]][["data"]])
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("a capture after a config edit still carries the rows", {
  blk <- new_chart_block(chart_type = "bar", group = "PARAMCD")
  df <- push_df()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      msgs <- spy_messages(session)
      session$flushReact()
      expr <- session$makeScope("expr")
      expr$setInputs(drilldown_block_action = list(
        action = "config", sort_dir = "asc"
      ))
      session$flushReact()
      expect_null(msgs()[[2L]][["data"]])

      ids <- chart_capture_ids(session)
      expect_length(ids, 1L)
      chart_capture_request(ids, 800, 400, session = session)
      cap <- msgs("drilldown-capture")
      expect_length(cap, 1L)
      expect_identical(cap[[1L]][["data"]], msgs()[[1L]][["data"]])
      expect_equal(cap[[1L]]$config$sort_dir, "asc")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("_ready is answered with the rev, not the whole message", {
  blk <- new_chart_block(chart_type = "bar", group = "PARAMCD")
  df <- push_df()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      msgs <- spy_messages(session)
      session$flushReact()
      expect_length(msgs(), 1L)

      expr <- session$makeScope("expr")
      expr$setInputs(drilldown_block_ready = 1)
      session$flushReact()
      expect_length(msgs(), 1L)
      ready <- msgs("drilldown-ready")
      expect_length(ready, 1L)
      expect_named(ready[[1L]], c("id", "data_rev"))
      expect_identical(ready[[1L]]$data_rev, msgs()[[1L]]$data_rev)
      expect_match(ready[[1L]]$id, "drilldown_block$")
    },
    args = list(x = blk, data = list(data = function() df))
  )
})
