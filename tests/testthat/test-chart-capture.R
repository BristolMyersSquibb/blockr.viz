# The capture service (R/chart-capture.R): the registry blockr.outline decks
# ask for pictures through, and the helpers the picture's writers share.

cap_module <- function(id, el, send = function(...) NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    register_chart_capture(el, send)
  })
}

test_that("removing the first chart leaves the registry to the others", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    cap_module("a", "el_a")
    session$destroy("a")
    expect_no_error(cap_module("b", "el_b"))
    expect_identical(chart_capture_ids(session), "el_b")
  })
})

test_that("a removed chart's entry goes, a later one under its id stays", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    cap_module("a", "el_x")
    cap_module("b", "el_x", send = function(...) "b")
    session$destroy("a")
    expect_identical(chart_capture_ids(session), "el_x")
    send <- shiny::isolate(capture_registry(session)$el_x)
    expect_identical(send(), "b")
  })
})

test_that("a deck's request carries the box and the pixel ratio", {
  withr::local_options(blockr.viz.canvas_capture_ratio = 4)
  session <- shiny::MockShinySession$new()
  sent <- NULL
  session$sendCustomMessage <- function(type, message) sent <<- message
  shiny::withReactiveDomain(session, {
    chart_capture_service(session, "el_c", function() list(config = list()))
    token <- chart_capture_request("el_c", 600, 400, session = session)
  })
  expect_identical(sent$req, token)
  expect_identical(c(sent$width, sent$height), c(600, 400))
  expect_identical(sent$ratio, 4)
})

test_that("a capture message decodes once into a chart_capture", {
  png <- as.raw(c(0x89, 0x50, 0x4e, 0x47))
  msg <- list(png = png_data_uri(png), width = 192, height = 96)
  cap <- chart_capture_read(msg)
  expect_s3_class(cap, "chart_capture")
  expect_identical(cap$png, png)
  expect_identical(c(cap$width, cap$height), c(2, 1))
  expect_null(chart_capture_read(list(png = "javascript:x", width = 1,
                                      height = 1)))
  expect_null(chart_capture_read(NULL))
})

test_that("a capture fits its box and grows at most twice", {
  cap <- new_chart_capture(as.raw(1), 192, 96)
  # no box: the full two times
  expect_identical(capture_fit(cap), 2)
  expect_identical(capture_fit(cap, max_width = 1), 0.5)
  expect_identical(capture_fit(cap, max_width = 10, max_height = 10), 2)
})
