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

test_that("an equal frame or an edit that keeps the column set serializes nothing", {
  calls <- new.env(parent = emptyenv())
  calls$json <- 0L
  calls$meta <- 0L
  real_json <- chart_data_json
  real_meta <- dd_col_meta
  testthat::local_mocked_bindings(
    chart_data_json = function(...) {
      calls$json <- calls$json + 1L
      real_json(...)
    },
    dd_col_meta = function(...) {
      calls$meta <- calls$meta + 1L
      real_meta(...)
    }
  )

  src <- new.env(parent = emptyenv())
  src$d <- push_df()
  tick <- shiny::reactiveVal(0L)
  # A new object holding the same rows each time, as a panel visit delivers.
  dat <- shiny::reactive({
    tick()
    src$d[seq_len(nrow(src$d)), , drop = FALSE]
  })
  blk <- new_chart_block(chart_type = "bar", group = "PARAMCD")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      msgs <- spy_messages(session)
      session$flushReact()
      expect_equal(calls$json, 1L)
      expect_equal(calls$meta, 1L)

      tick(1L)
      session$flushReact()
      expr <- session$makeScope("expr")
      expr$setInputs(drilldown_block_action = list(
        action = "config", subtitle = "Records", sort_by = "alpha"
      ))
      session$flushReact()
      expect_equal(calls$json, 1L)
      expect_equal(calls$meta, 1L)
      expect_length(msgs(), 2L)

      # A new mapped column is a new payload.
      expr$setInputs(drilldown_block_action = list(
        action = "config", color = "PARAMCD", value = "AVAL", func = "mean"
      ))
      session$flushReact()
      expect_equal(calls$json, 2L)
    },
    args = list(x = blk, data = list(data = dat))
  )
})

test_that("the scale map is resolved once per column, not per push", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  real <- dd_scales_config
  testthat::local_mocked_bindings(dd_scales_config = function(...) {
    calls$n <- calls$n + 1L
    real(...)
  })
  df <- data.frame(TRT = c("A", "A", "B"), SEX = c("F", "M", "F"))
  blk <- new_chart_block(chart_type = "bar", group = "SEX", color = "TRT")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$userData$board_options <- list(
        scale_map = shiny::reactiveVal(list(TRT = list(color = list(A = "#006400"))))
      )
      msgs <- spy_messages(session)
      session$flushReact()
      expect_equal(calls$n, 1L)
      expr <- session$makeScope("expr")
      for (dir in c("asc", "desc", "asc")) {
        expr$setInputs(drilldown_block_action = list(
          action = "config", sort_dir = dir
        ))
        session$flushReact()
      }
      expect_length(msgs(), 4L)
      expect_equal(calls$n, 1L)
      expect_identical(msgs()[[4L]]$config$scales, msgs()[[1L]]$config$scales)

      expr$setInputs(drilldown_block_action = list(
        action = "config", color = "SEX"
      ))
      session$flushReact()
      expect_equal(calls$n, 2L)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("a pre-serialized config entry reaches the browser as Shiny would write it", {
  d <- data.frame(day = rep(1:40, 5), val = sin(1:200), id = rep(1:5, each = 40),
                  arm = rep(c("A", "B"), 100))
  band <- compute_band_series(d, "day", "val", "arm", NULL, id_col = "id",
                              window = "fixed", window_size = 5, min_n = 2)
  expect_false(is.null(band))
  expect_identical(
    shiny:::toJSON(list(config = list(band_series = chart_config_json(band)))),
    shiny:::toJSON(list(config = list(band_series = band)))
  )
  expect_null(chart_config_json(NULL))
})

test_that("a panel visit or a gear edit does not refit the band or the smoother", {
  calls <- new.env(parent = emptyenv())
  calls$band <- 0L
  calls$smooth <- 0L
  real_band <- compute_band_series
  real_smooth <- compute_smoother_series
  testthat::local_mocked_bindings(
    compute_band_series = function(...) {
      calls$band <- calls$band + 1L
      real_band(...)
    },
    compute_smoother_series = function(...) {
      calls$smooth <- calls$smooth + 1L
      real_smooth(...)
    }
  )
  src <- new.env(parent = emptyenv())
  src$d <- data.frame(day = rep(1:40, 5), val = sin(1:200),
                      id = rep(1:5, each = 40))
  tick <- shiny::reactiveVal(0L)
  dat <- shiny::reactive({
    tick()
    src$d[seq_len(nrow(src$d)), , drop = FALSE]
  })
  check <- function(blk, field, count) {
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", blk),
      {
        msgs <- spy_messages(session)
        session$flushReact()
        expect_equal(calls[[count]], 1L)
        expect_false(is.null(msgs()[[1L]]$config[[field]]))
        tick(isolate(tick()) + 1L)
        session$flushReact()
        expr <- session$makeScope("expr")
        expr$setInputs(drilldown_block_action = list(
          action = "config", title = "Over time"
        ))
        session$flushReact()
        expect_length(msgs(), 2L)
        expect_equal(calls[[count]], 1L)
      },
      args = list(x = blk, data = list(data = dat))
    )
  }
  check(new_chart_block(chart_type = "band", x = "day", y = "val",
                        band_window = "fixed", band_size = 5, band_min_n = 2),
        "band_series", "band")
  check(new_chart_block(chart_type = "scatter", x = "day", y = "val",
                        smoother = "loess"),
        "smoother_series", "smooth")
})

test_that("only a scatter fits the smoother", {
  d <- data.frame(day = rep(1:40, 5), val = sin(1:200))
  for (type in c("scatter", "line")) {
    blk <- new_chart_block(chart_type = type, x = "day", y = "val",
                           smoother = "loess")
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", blk),
      {
        msgs <- spy_messages(session)
        session$flushReact()
        if (type == "scatter") {
          expect_false(is.null(msgs()[[1L]]$config$smoother_series))
        } else {
          expect_null(msgs()[[1L]]$config$smoother_series)
        }
      },
      args = list(x = blk, data = list(data = function() d))
    )
  }
})
