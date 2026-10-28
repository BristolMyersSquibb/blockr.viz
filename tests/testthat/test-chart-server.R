# Rules of the chart server that the recordings do not pin: what travels
# when, what is computed again, and what the emitted code holds on to.

srv_df <- function() {
  data.frame(day = rep(1:40, 5), val = round(sin(1:200), 3),
             id = rep(sprintf("S%d", 1:5), each = 40),
             arm = rep(c("A", "B"), 100))
}

# The block server with a data source the test can re-fire with an equal
# frame (a dock panel visit). `code(session, expr, visit)` runs inside it.
# A payload the kit does not send again is a cancelled render, which the
# test session reports as an error; `srv_payload()` reads it as NULL.
srv_payload <- function(expr) {
  tryCatch(expr$output$chart, error = function(e) NULL)
}

srv_run <- function(blk, code) {
  src <- new.env(parent = emptyenv())
  src$d <- srv_df()
  tick <- shiny::reactiveVal(0L)
  dat <- shiny::reactive({
    tick()
    src$d[seq_len(nrow(src$d)), , drop = FALSE]
  })
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      visit <- function() {
        tick(shiny::isolate(tick()) + 1L)
        session$flushReact()
      }
      session$flushReact()
      code(session, expr, visit)
    },
    args = list(x = blk, data = list(data = dat))
  )
}

test_that("a setting goes out without rows, changed data with them", {
  blk <- new_chart_block(type = "bar", category = "arm")
  srv_run(blk, function(session, expr, visit) {
    first <- srv_payload(expr)
    expect_false(is.null(first[["data"]]))
    chart_input_set(expr, position = "dodge")
    session$flushReact()
    second <- srv_payload(expr)
    expect_null(second[["data"]])
    expect_identical(second$data_rev, first$data_rev)
    expect_match(second$config, "\"position\":\"dodge\"", fixed = TRUE)
  })
})

test_that("a panel visit sends nothing new and keeps the expr object", {
  blk <- new_chart_block(type = "bar", category = "arm", drill = "arm")
  srv_run(blk, function(session, expr, visit) {
    chart_input_click(expr, arm = "A")
    session$flushReact()
    ex <- session$returned$expr()
    visit()
    expect_null(srv_payload(expr))
    expect_identical(rlang::obj_address(session$returned$expr()),
                     rlang::obj_address(ex))
  })
})

test_that("a panel visit or a setting does not refit the band or the smoother", {
  calls <- new.env(parent = emptyenv())
  calls$band <- 0L
  calls$smooth <- 0L
  real_band <- compute_band_series
  real_smooth <- compute_smoother_series
  local_mocked_bindings(
    compute_band_series = function(...) {
      calls$band <- calls$band + 1L
      real_band(...)
    },
    compute_smoother_series = function(...) {
      calls$smooth <- calls$smooth + 1L
      real_smooth(...)
    }
  )
  srv_run(new_chart_block(type = "band", x = "day", y = "val", id = "id",
                          band_min_n = 2), function(session, expr, visit) {
    expect_equal(calls$band, 1L)
    visit()
    chart_input_set(expr, title = "Band")
    session$flushReact()
    expect_equal(calls$band, 1L)
    chart_input_set(expr, band_size = 10)
    session$flushReact()
    expect_equal(calls$band, 2L)
  })
  srv_run(new_chart_block(type = "scatter", x = "day", y = "val",
                          smoother = "lm"), function(session, expr, visit) {
    expect_equal(calls$smooth, 1L)
    visit()
    chart_input_set(expr, point_size = 2)
    session$flushReact()
    expect_equal(calls$smooth, 1L)
  })
})

test_that("a highlight is one column and its values", {
  blk <- new_chart_block(highlight = list())
  expect_null(blockr.core:::initial_block_state(blk)$highlight)
  blk <- new_chart_block(type = "bar", category = "arm",
                         highlight = list(arm = "A"))
  expect_identical(blockr.core:::initial_block_state(blk)$highlight,
                   list(arm = "A"))
  expect_error(new_chart_block(highlight = "A"), "highlight")
  expect_error(new_chart_block(highlight = list(arm = "A", id = "S1")),
               "at most 1")
  # A saved board with a bad highlight restores without it.
  expect_warning(
    blk <- blockr.core::blockr_deser(
      structure(list(), class = "chart_block"),
      list(payload = list(type = "bar", category = "arm",
                          highlight = list(arm = "A", id = "S1")),
           constructor = list(version = "0.3.0"))
    ),
    "highlight"
  )
  expect_null(blockr.core:::initial_block_state(blk)$highlight)
})

test_that("a click on a histogram bin filters x inside the bin", {
  blk <- new_chart_block(type = "histogram", x = "day", bins = 4,
                         drill = "auto")
  srv_run(blk, function(session, expr, visit) {
    h <- chart_hist(list(type = "histogram", x = "day", bins = 4), srv_df())
    chart_input_click(expr, day_bin = h$edges$label[[1L]])
    session$flushReact()
    res <- session$returned$result()
    expect_true(all(res$day >= h$edges$lo[[1L]] & res$day < h$edges$hi[[1L]]))
    expect_gt(nrow(res), 0L)
  })
})

test_that("a drill target gets every click and the chart holds none", {
  blk <- new_chart_block(type = "bar", category = "arm", drill = "arm",
                         drill_target = "vf1")
  sends <- new.env(parent = emptyenv())
  sends$n <- 0L
  srv_run(blk, function(session, expr, visit) {
    session$userData$blockr_ctrl_send <- function(target, args, author) {
      sends$n <- sends$n + 1L
      TRUE
    }
    chart_input_click(expr, arm = "A", nonce = 1L)
    session$flushReact()
    chart_input_click(expr, arm = "A", nonce = 2L)
    session$flushReact()
    expect_equal(sends$n, 2L)
    expect_null(session$returned$state$selection())
    expect_equal(nrow(session$returned$result()), 200L)
  })
})

# The control channel with a recording update, so a test sees what reaches the
# target: each entry is the `state` one update set on `vf1`.
srv_channel <- function(session) {
  log <- new.env(parent = emptyenv())
  log$sent <- list()
  install_ctrl_send(function(x) {
    log$sent <- c(log$sent, list(x$blocks$mod$vf1$state))
  }, session = session)
  log
}

test_that("a clear with a drill target releases the chart's claim", {
  blk <- new_chart_block(type = "bar", category = "arm", drill = "arm",
                         drill_target = "vf1")
  srv_run(blk, function(session, expr, visit) {
    log <- srv_channel(session)
    chart_input_click(expr, arm = "A", nonce = 1L)
    session$flushReact()
    expect_length(log$sent, 1L)
    expect_identical(log$sent[[1L]]$columns[[1L]]$values, "A")
    chart_input_click(expr, nonce = 2L)
    session$flushReact()
    expect_length(log$sent, 2L)
    expect_identical(log$sent[[2L]], list(columns = list()))
  })
})

test_that("a range with a drill target sends nothing and clears nothing", {
  blk <- new_chart_block(type = "scatter", x = "day", y = "val", drill = "arm",
                         drill_target = "vf1")
  srv_run(blk, function(session, expr, visit) {
    log <- srv_channel(session)
    chart_input_click(expr, arm = "B", nonce = 1L)
    session$flushReact()
    expect_length(log$sent, 1L)
    expr$setInputs(chart_select = viz_read_selection(list(
      selection = list(type = "range", x = "day", y = "val",
                       x_range = list(1, 10), y_range = list(-1, 1)),
      nonce = 2L
    )))
    session$flushReact()
    expect_length(log$sent, 1L)
    # The claim is still this chart's, so its clear releases it.
    chart_input_click(expr, nonce = 3L)
    session$flushReact()
    expect_length(log$sent, 2L)
    expect_identical(log$sent[[2L]], list(columns = list()))
  })
})

test_that("a clear with a drill target leaves another sender's claim", {
  blk <- new_chart_block(type = "bar", category = "arm", drill = "arm",
                         drill_target = "vf1")
  srv_run(blk, function(session, expr, visit) {
    log <- srv_channel(session)
    session$userData$blockr_ctrl_send("vf1", list(state = list(columns = list(
      list(name = "id", mode = "multi", values = "S1")
    ))), author = "other-block")
    chart_input_click(expr, nonce = 1L)
    session$flushReact()
    expect_length(log$sent, 1L)
  })
})

test_that("the default_caption option captions a new chart that sets none", {
  withr::local_options(blockr.viz.default_caption = "{filters}")
  st <- blockr.core:::initial_block_state(new_chart_block(type = "bar"))
  expect_identical(st$caption, "{filters}")
  st <- blockr.core:::initial_block_state(
    new_chart_block(type = "bar", caption = "Own")
  )
  expect_identical(st$caption, "Own")
})

test_that("a click on one point filters its x and y", {
  blk <- new_chart_block(type = "scatter", x = "day", y = "val")
  srv_run(blk, function(session, expr, visit) {
    expr$setInputs(chart_select = viz_read_selection(list(
      selection = list(type = "range", x = "day", y = "val",
                       x_range = list(3, 3), y_range = list(0.141, 0.141)),
      nonce = 1L
    )))
    session$flushReact()
    res <- session$returned$result()
    expect_true(nrow(res) > 0L)
    expect_true(all(res$day == 3 & res$val == 0.141))
  })
})

test_that("the facet_cols option sets the panels per row of a new chart", {
  withr::local_options(blockr.viz.facet_cols = 3L)
  st <- blockr.core:::initial_block_state(new_chart_block(type = "bar"))
  expect_identical(st$facet_cols, 3L)
  st <- blockr.core:::initial_block_state(
    new_chart_block(type = "bar", facet_cols = 2L)
  )
  expect_identical(st$facet_cols, 2L)
})

test_that("a deck asks a live chart for a picture and collects the reply", {
  skip_if_not_installed("png")
  f <- withr::local_tempfile(fileext = ".png")
  png::writePNG(array(1, c(4, 6, 3)), f)
  url <- paste0("data:image/png;base64,",
                gsub("\n", "", jsonlite::base64_enc(readBin(f, "raw",
                                                            file.size(f)))))
  blk <- new_chart_block(type = "bar", category = "arm")
  srv_run(blk, function(session, expr, visit) {
    root <- session$rootScope()
    sent <- list()
    root$sendCustomMessage <- function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
    ids <- chart_capture_ids(session)
    expect_length(ids, 1L)
    expect_null(chart_capture_for("not_a_block", session))
    expect_error(chart_capture_request("nope", 600, 400, session),
                 "no chart capture registered")

    tok <- chart_capture_request(ids, 600, 400, session)
    expect_match(tok, "^cap-")
    req <- Filter(function(m) identical(m$type, "blockr-viz:capture"), sent)
    expect_length(req, 1L)
    expect_identical(req[[1L]]$message$req, tok)
    expect_identical(req[[1L]]$message$width, 600)

    expect_null(chart_capture_collect(tok, session))
    root$setInputs(blockr_viz_capture_result = list(
      req = tok, png = url, width = 600, height = 400
    ))
    cap <- chart_capture_collect(tok, session)
    expect_s3_class(cap, "chart_capture")
    expect_equal(c(cap$width, cap$height), c(600, 400) / 96)

    tok2 <- chart_capture_request(ids, 300, 200, session)
    root$setInputs(blockr_viz_capture_result = list(req = tok2,
                                                    error = "no canvas"))
    expect_error(chart_capture_collect(tok2, session), "no canvas")

    skip_if_not_installed("officer")
    img <- chart_capture_img(cap, max_width = 3)
    expect_equal(attr(img, "pptx_width"), 3)
    expect_equal(attr(img, "pptx_height"), 2)
    # Without a box the picture grows to at most twice its size.
    expect_equal(attr(chart_capture_img(cap), "pptx_width"), 2 * cap$width)
  })
})

test_that("the board's ECharts theme option reaches the chart", {
  opt <- new_drilldown_theme_option("dark")
  expect_identical(blockr.core::board_option_id(opt), "echart_theme")
  ui <- as.character(blockr.core::board_option_ui(opt, id = "opts"))
  expect_match(ui, "ECharts Theme", fixed = TRUE)
  expect_match(ui, '<option value="dark" selected>Dark</option>',
               fixed = TRUE)
  expect_match(ui, "purple-passion", fixed = TRUE)

  blk <- new_chart_block(type = "bar", category = "arm")
  srv_run(blk, function(session, expr, visit) {
    expect_identical(srv_payload(expr)$derived$theme, "default")
  })
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$userData$board_options <- list(
        echart_theme = shiny::reactiveVal("dark")
      )
      session$flushReact()
      expect_identical(srv_payload(session$makeScope("expr"))$derived$theme,
                       "dark")
    },
    args = list(x = blk, data = list(data = shiny::reactive(srv_df())))
  )
})
