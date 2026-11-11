# A date control without a date: "automatic" until one is set.

test_that("an empty date control prints its placeholder and clears to NA", {
  script <- paste(
    "cutoff <- as.Date(NA)  #| date(label = \"Cutoff\", placeholder = \"automatic\")",
    "data",
    sep = "\n"
  )
  specs <- cb_specs(cb_parse(script), data.frame(x = 1))
  expect_identical(specs[[1]]$kind, "date")
  expect_true(is.na(specs[[1]]$default))
  expect_identical(script_title_args(list(), specs)$cutoff, "automatic")
  set <- viz_script_values(list(cutoff = "2024-03-01"), specs)
  expect_identical(set$cutoff, as.Date("2024-03-01"))
  expect_identical(script_title_args(list(), specs, set)$cutoff,
                   as.Date("2024-03-01"))
  back <- viz_script_values(list(cutoff = ""), specs, set)
  expect_true(is.na(back$cutoff))
  # A garbled date keeps the current value.
  expect_identical(viz_script_values(list(cutoff = "nope"), specs, set), set)
  # The band and the word read no date as null.
  expect_true(is.na(script_control_values(specs)$sv_cutoff))
})

test_that("a select can take its choices from the data through a helper line", {
  script <- paste(
    "lv <- sort(unique(data$drug))",
    "drugs <- factor(lv, levels = lv)  #| select(label = \"Drugs\")",
    "data[data$drug %in% drugs, , drop = FALSE]",
    sep = "\n"
  )
  d <- data.frame(drug = c("B", "A", "B"))
  specs <- cb_specs(cb_parse(script), d)
  expect_identical(specs[[1]]$choices, c("A", "B"))
  expect_true(specs[[1]]$multiple)
  r <- dd_prepare_run(d, cb_parse(script), specs, list(drugs = "A"))
  expect_identical(r$data$drug, "A")
  # The sentence's word: all, none, or the picks.
  expect_identical(script_title_args(list(), specs)$drugs, "all")
  expect_identical(
    script_title_args(list(), specs, list(drugs = "A"))$drugs, "A"
  )
  expect_identical(
    script_title_args(list(), specs, list(drugs = character()))$drugs, "none"
  )
})
