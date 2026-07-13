test_that("ctrl_receipt() renders a chip per sent column", {

  cols <- list(
    list(name = "SEX", table = "adsl", mode = "multi", values = "F"),
    list(name = "ARM", table = "adsl", mode = "multi", values = c("A", "B"))
  )

  html <- as.character(htmltools::renderTags(ctrl_receipt(cols, "Cohort"))$html)

  expect_match(html, "Sent to")
  expect_match(html, "Cohort")
  expect_match(html, "SEX")
  expect_match(html, "A, B", fixed = TRUE)
  chips <- gregexpr("class=\"ctrl-receipt-chip\"", html, fixed = TRUE)
  expect_equal(lengths(regmatches(html, chips)), 2L)
  expect_no_match(html, "ctrl-receipt--idle")
})

test_that("ctrl_receipt() renders an idle state when nothing was sent", {

  html <- as.character(htmltools::renderTags(ctrl_receipt(list(), "Cohort"))$html)

  expect_match(html, "ctrl-receipt--idle")
  expect_match(html, "Nothing sent")
  expect_no_match(html, "ctrl-receipt-chip")
})

test_that("ctrl_receipt() carries its stylesheet dependency", {

  dep <- htmltools::renderTags(ctrl_receipt(list()))$dependencies[[1L]]
  expect_true(file.exists(file.path(dep$src$file, "ctrl-receipt.css")))
})
