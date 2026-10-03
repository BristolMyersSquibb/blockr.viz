test_that("the Excel sheet of an aggregated chart holds one row per cell", {
  d <- data.frame(
    term = c("Nausea", "Nausea", "Rash", "Rash", "Rash"),
    sev = c("MILD", "MILD", "MILD", "SEVERE", "SEVERE"),
    arm = c("A", "B", "A", "A", "B"),
    aval = c(1, 2, 3, 4, 5)
  )
  out <- chart_xlsx_frame(d, "bar", group = "term", color = "sev")
  expect_equal(nrow(out), 3L)
  expect_true(all(c("term", "sev") %in% names(out)))

  faceted <- chart_xlsx_frame(d, "bar", group = "term", facet = "arm")
  expect_equal(nrow(faceted), 4L)

  summed <- chart_xlsx_frame(d, "pie", group = "term", func = "sum",
                             value = "aval")
  expect_equal(nrow(summed), 2L)
})

test_that("charts that draw rows put the rows in the Excel sheet", {
  d <- data.frame(x = 1:3, y = c(2, 4, 6), g = c("a", "b", "a"))
  expect_identical(chart_xlsx_frame(d, "scatter", group = "g"), d)
  expect_identical(chart_xlsx_frame(d, "boxplot", group = "g", value = "y"), d)
  expect_identical(chart_xlsx_frame(d, "bar"), d)
  expect_null(chart_xlsx_frame(NULL, "bar", group = "g"))
})
