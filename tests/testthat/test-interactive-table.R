it_html <- function(x) as.character(htmltools::renderTags(x)$html)

it_ae <- function() {
  data.frame(
    .group1 = "SOC", .group1_level = c("Cardiac", "Cardiac", "Skin"),
    .variable = "PT", .variable_level = c("Palp", "Tachy", "Rash"),
    .label = c("Palp", "Tachy", "Rash"),
    Placebo = c("3", "1", "8"), Active = c("7", "2", "12")
  )
}

test_that("an annotated frame draws structured, with its drill target", {
  h <- it_html(interactive_table(it_ae(), drill_target = "pt", elem_id = "x"))
  expect_match(h, 'data-dt-structured="1"', fixed = TRUE)
  expect_match(h, 'data-dt-ctrl-target="pt"', fixed = TRUE)
  expect_match(h, 'data-dt-gear="0"', fixed = TRUE)
})

test_that("a plain frame drills on its first column by default", {
  h <- it_html(interactive_table(head(mtcars), elem_id = "x"))
  expect_match(h, 'data-dt-onclick-col="mpg"', fixed = TRUE)
  h <- it_html(interactive_table(head(mtcars), drill = FALSE, elem_id = "x"))
  expect_no_match(h, "data-dt-onclick-col=\"mpg\"", fixed = TRUE)
})

test_that("no drill target without a drill", {
  h <- it_html(interactive_table(it_ae(), drill = FALSE, drill_target = "pt"))
  expect_no_match(h, "data-dt-ctrl-target", fixed = TRUE)
})

test_that("x = NULL draws an empty slot to fill later", {
  h <- it_html(interactive_table(NULL, elem_id = "z", scroll = "page",
                                 search = FALSE))
  expect_match(h, "dt-table-slot", fixed = TRUE)
  expect_match(h, "dt-scroll-page", fixed = TRUE)
  expect_no_match(h, '<input type="search"', fixed = TRUE)
})

test_that("update_interactive_table() pushes the body and counts up", {
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
  expect_equal(sent[[1]]$type, "blockr-viz-table-data")
  expect_equal(vapply(sent, function(s) s$message$rev, numeric(1)), 1:3)
  p1 <- jsonlite::fromJSON(sent[[1]]$message$payload)
  p2 <- jsonlite::fromJSON(sent[[2]]$message$payload)
  p3 <- jsonlite::fromJSON(sent[[3]]$message$payload)
  expect_equal(p1$kind, "html")
  expect_match(p1$html, 'data-dt-structured="1"', fixed = TRUE)
  expect_match(p2$html, "cannot draw", fixed = TRUE)
  expect_equal(p3$html, "")
})
