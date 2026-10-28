it_html <- function(x) as.character(htmltools::renderTags(x)$html)
it_payload <- function(x) static_payloads(it_html(x))[[1L]]

it_ae <- function() {
  data.frame(
    .group1 = "SOC", .group1_level = c("Cardiac", "Cardiac", "Skin"),
    .variable = "PT", .variable_level = c("Palp", "Tachy", "Rash"),
    .label = c("Palp", "Tachy", "Rash"),
    Placebo = c("3", "1", "8"), Active = c("7", "2", "12")
  )
}

test_that("interactive_table() draws the records view, live and without a gear", {
  tag <- interactive_table(it_ae(), drill_target = "pt", elem_id = "x")
  h <- it_html(tag)
  expect_match(h, 'id="x"', fixed = TRUE)
  expect_match(h, 'data-viz-gear="off"', fixed = TRUE)
  expect_match(h, 'data-viz-still="off"', fixed = TRUE)
  expect_match(h, "#x .blockr-table-wrapper { max-height: 600px; }",
               fixed = TRUE)
  p <- it_payload(tag)
  expect_identical(p$config$layout, "records")
  expect_identical(p$config$drill, "auto")
  expect_identical(p$config$drill_target, "pt")
  expect_true(length(p$data$cols) > 0L)

  cfg <- function(...) {
    it_payload(interactive_table(head(mtcars), elem_id = "y", ...))$config
  }
  expect_identical(cfg()$drill, "mpg")
  expect_null(cfg(drill = FALSE)$drill)
  expect_identical(cfg(drill = "cyl")$drill, "cyl")
  expect_identical(cfg(drill = FALSE, drill_target = "pt")$drill_target, "")
  expect_false(cfg(search = FALSE)$search)

  empty <- interactive_table(NULL, elem_id = "z", max_height = NULL)
  expect_identical(it_payload(empty)$derived$empty$kind, "no_data")
  expect_no_match(it_html(empty), "max-height", fixed = TRUE)
})

test_that("update_interactive_table() sends the table and counts up", {
  sent <- list()
  session <- list(
    userData = new.env(),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  update_interactive_table(session, "z", it_ae())
  update_interactive_table(session, "z", htmltools::tags$p("cannot draw"))
  update_interactive_table(session, "z", NULL)
  expect_identical(sent[[1L]]$type, "blockr-viz-static")
  expect_identical(sent[[1L]]$message$id, "z")
  expect_equal(vapply(sent, function(s) s$message$payload$data_rev,
                      numeric(1L)), 1:3)
  expect_true(length(sent[[1L]]$message$payload$data$cols) > 0L)
  expect_identical(sent[[2L]]$message$payload$derived$empty$message,
                   "cannot draw")
  expect_identical(sent[[3L]]$message$payload$derived$empty$kind, "no_data")
})
