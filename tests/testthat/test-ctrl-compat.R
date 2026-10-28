# The names blockr.viz 0.3.0 exported stay callable.

test_that("dd_col_claims() reshapes a column click into conditions", {
  keys <- list(list(column = "ARM", values = "Placebo"),
               list(column = "SEX", values = c("F", "M")),
               list(column = "", values = "x"),
               list(column = "AGE", values = character()))
  expect_identical(
    dd_col_claims(keys),
    list(list(name = "ARM", mode = "multi", values = "Placebo"),
         list(name = "SEX", mode = "multi", values = c("F", "M")))
  )
  expect_identical(dd_col_claims(keys[1L], table = "adsl")[[1L]]$table,
                   "adsl")
  expect_identical(dd_col_claims(list()), list())
})

test_that("the dd_ctrl_ names call the current functions", {
  exports <- getNamespaceExports("blockr.viz")
  expect_true(all(c("dd_ctrl_claims", "dd_ctrl_sender", "dd_ctrl_subject",
                    "dd_col_claims") %in% exports))
})
