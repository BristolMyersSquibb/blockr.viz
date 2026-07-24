# R/report-call.R :: how a block states its printed form for a document.

test_that("chart blocks emit a self-qualified gg_chart call", {
  b <- new_chart_block(
    chart_type = "bar", group = "AGEGR1", color = "ARM", facet = "SEX",
    func = "count_distinct", value = "USUBJID",
    count_on = "axis", count_col = "USUBJID",
    title = "Enrollment: {n} records"
  )

  cl <- report_call(b, "chart1")
  expect_true(is.call(cl))

  txt <- paste(deparse(cl), collapse = " ")
  expect_match(txt, "^blockr.viz::gg_chart\\(chart1", perl = TRUE)
  expect_match(txt, "group = \"AGEGR1\"", fixed = TRUE)
  expect_match(txt, "func = \"count_distinct\"", fixed = TRUE)
  expect_match(txt, "title = \"Enrollment: {n} records\"", fixed = TRUE)

  # Interaction-only state never reaches the document.
  expect_no_match(txt, "drill|tt_fields|filter_")
})

test_that("defaults are omitted so the call stays readable", {
  b <- new_chart_block(chart_type = "scatter", x = "AGE", y = "BMIBL")
  txt <- paste(deparse(report_call(b, "sc")), collapse = " ")
  expect_identical(
    txt,
    "blockr.viz::gg_chart(sc, chart_type = \"scatter\", x = \"AGE\", y = \"BMIBL\")"
  )
})

test_that("the emitted call evaluates to the same plot as direct state", {
  d <- transform(datasets::iris, Grp = rep(c("A", "B"), 75))
  b <- new_chart_block(chart_type = "bar", group = "Species", color = "Grp")

  env <- new.env(parent = globalenv())
  assign("chart1", d, envir = env)
  p <- eval(report_call(b, "chart1"), envir = env)

  expect_s3_class(p, "gg")
  bars <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_setequal(round(bars$x), c(25, 50)) # stacked segments of 25 each
})

test_that("non-viz blocks print bare", {
  expect_null(report_call(blockr.core::new_head_block(), "h"))
})
