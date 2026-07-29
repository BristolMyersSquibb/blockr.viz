# R/report-call.R :: how a block states its printed form for a document.

with_report_style <- function(style, code) {
  old <- options(blockr.viz.report_style = style)
  on.exit(options(old))
  force(code)
}

test_that("chart blocks emit plain ggplot2 code by default", {
  b <- new_chart_block(
    chart_type = "bar", group = "AGEGR1", color = "ARM",
    title = "Enrollment: {n} records"
  )

  cl <- report_call(b, "chart1")
  expect_true(is.call(cl))

  txt <- paste(deparse(cl), collapse = " ")
  # No blockr vocabulary: the document reproduces the chart from dplyr +
  # ggplot2 alone (self-qualified, so nothing needs attaching).
  expect_no_match(txt, "blockr|static_chart")
  expect_match(txt, "ggplot2::ggplot", fixed = TRUE)
  expect_match(txt, "dplyr::count(chart1, AGEGR1, ARM", fixed = TRUE)
})

test_that("an uncompilable chart type falls back to the static call", {
  b <- new_chart_block(chart_type = "pie", group = "ARM")
  txt <- paste(deparse(report_call(b, "c1")), collapse = " ")
  expect_match(txt, "^blockr.viz::static_chart\\(c1")
})

test_that("the static style emits a self-qualified static_chart call", {
  with_report_style("static", {
    b <- new_chart_block(
      chart_type = "bar", group = "AGEGR1", color = "ARM", facet = "SEX",
      func = "count_distinct", value = "USUBJID",
      count_on = "axis", count_col = "USUBJID",
      title = "Enrollment: {n} records"
    )

    cl <- report_call(b, "chart1")
    expect_true(is.call(cl))

    txt <- paste(deparse(cl), collapse = " ")
    expect_match(txt, "^blockr.viz::static_chart\\(chart1", perl = TRUE)
    expect_match(txt, "group = \"AGEGR1\"", fixed = TRUE)
    expect_match(txt, "func = \"count_distinct\"", fixed = TRUE)
    expect_match(txt, "title = \"Enrollment: {n} records\"", fixed = TRUE)

    # Interaction-only state never reaches the document.
    expect_no_match(txt, "drill|tt_fields|filter_")
  })
})

test_that("static style: defaults are omitted so the call stays readable", {
  with_report_style("static", {
    b <- new_chart_block(chart_type = "scatter", x = "AGE", y = "BMIBL")
    # deparse() wraps long calls; the emitted chunk keeps the line breaks
    # (blockr.outline joins with "\n"), so compare the squished form.
    txt <- gsub(
      "\\s+", " ", paste(deparse(report_call(b, "sc")), collapse = " ")
    )
    expect_identical(
      txt,
      paste0(
        "blockr.viz::static_chart(sc, chart_type = \"scatter\", ",
        "x = \"AGE\", y = \"BMIBL\")"
      )
    )
  })
})

test_that("the emitted call evaluates to the same plot as direct state", {
  skip_if_not_installed("ggplot2")

  d <- transform(datasets::iris, Grp = rep(c("A", "B"), 75))
  b <- new_chart_block(chart_type = "bar", group = "Species", color = "Grp")

  for (style in c("code", "static")) {
    with_report_style(style, {
      env <- new.env(parent = globalenv())
      assign("chart1", d, envir = env)
      p <- eval(report_call(b, "chart1"), envir = env)

      expect_s3_class(p, "gg")
      bars <- ggplot2::ggplot_build(p)$data[[1L]]
      expect_setequal(round(bars$x), c(25, 50)) # stacked segments of 25 each
    })
  }
})

test_that("non-viz blocks print bare", {
  expect_null(report_call(blockr.core::new_head_block(), "h"))
})

test_that("free panel scales reach the document; the fixed default does not", {
  fixed <- new_chart_block(chart_type = "bar", group = "AGEGR1", facet = "SEX")
  free <- new_chart_block(chart_type = "bar", group = "AGEGR1", facet = "SEX",
                          facet_scales = "free")

  with_report_style("static", {
    expect_no_match(
      paste(deparse(report_call(fixed, "c1")), collapse = " "), "facet_scales"
    )
    expect_match(
      paste(deparse(report_call(free, "c1")), collapse = " "),
      "facet_scales = \"free\"", fixed = TRUE
    )
  })

  # Code style: the pick reaches facet_wrap()'s scales argument.
  expect_no_match(
    paste(deparse(report_call(fixed, "c1")), collapse = " "), "scales"
  )
  expect_match(
    paste(deparse(report_call(free, "c1")), collapse = " "),
    "scales = \"free\"", fixed = TRUE
  )
})
