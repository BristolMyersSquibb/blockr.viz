test_that("download filenames use study, kind, title and timestamp", {
  d <- data.frame(x = 1)
  attr(d, "study_id") <- "CA-244-0001"

  expect_identical(
    viz_download_filename("table", "xlsx", "Demographic Characteristics",
                          data = d,
                          time = as.POSIXct("2026-09-29 12:34:56", tz = "UTC")),
    "ca_244_0001_table_demographic_characteristics_20260929t123456.xlsx"
  )
})

test_that("download filenames accept user patterns and data tokens", {
  d <- data.frame(ARM = c("Placebo", "Placebo"))

  expect_identical(
    viz_download_filename("plot", "png", "Most Frequent AEs", data = d,
                          study = "CA-244-0001",
                          pattern = "{study}_{ARM}_{kind}_{title}",
                          time = as.POSIXct("2026-09-29 12:34:56", tz = "UTC")),
    "ca_244_0001_placebo_plot_most_frequent_aes.png"
  )
})

test_that("download filenames fall back safely", {
  expect_identical(
    viz_download_filename("table", "html", title = "", pattern = "",
                          time = as.POSIXct("2026-09-29 12:34:56", tz = "UTC")),
    "study_table_20260929t123456.html"
  )
})
