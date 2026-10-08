t0 <- as.POSIXct("2026-10-02 14:32:07")

test_that("the name is dataset, title, time", {
  d <- data.frame(x = 1)
  attr(d, "blockr_provenance") <- list(dataset = "AQ-001")
  expect_identical(
    dl_filename(dl_dataset(d), "Ozone by month", "chart", "xlsx", t0),
    "AQ-001_Ozone_by_month_2026-10-02_1432.xlsx"
  )
})

test_that("no dataset, no prefix; no title, the fallback word", {
  d <- data.frame(x = 1)
  expect_null(dl_dataset(d))
  expect_identical(
    dl_filename(dl_dataset(d), "Ozone by month", "chart", "png", t0),
    "Ozone_by_month_2026-10-02_1432.png"
  )
  expect_identical(dl_filename(NULL, NULL, "table", "pptx", t0),
                   "table_2026-10-02_1432.pptx")
  expect_identical(dl_filename(NULL, "  ", "table", "pptx", t0),
                   "table_2026-10-02_1432.pptx")
})

test_that("titles lose markup and punctuation, keep case and words", {
  expect_identical(dl_name_part("Ozone ± SD — <b>May</b> to Sep."),
                   "Ozone_SD_May_to_Sep")
  expect_identical(dl_name_part("Mean: 4/5 (n = 3)"), "Mean_4_5_n_3")
  expect_identical(dl_name_part("AQ-001"), "AQ-001")
  expect_null(dl_name_part("—"))
  expect_identical(nchar(dl_name_part(strrep("word ", 30), max = 60L)) <= 60L,
                   TRUE)
})
