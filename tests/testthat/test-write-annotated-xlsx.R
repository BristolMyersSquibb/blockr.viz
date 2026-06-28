# write_annotated_xlsx(): annotated data frame -> styled .xlsx.

xlsx_part <- function(file, part) {
  td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(file, files = part, exdir = td)
  paste(readLines(file.path(td, part), warn = FALSE), collapse = "")
}

test_that("write_annotated_xlsx() writes indent + bold + values", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    .label  = c("Age (years)", "Mean (SD)", "Median"),
    .indent = c(0L, 1L, 1L),
    .strong = c(TRUE, FALSE, FALSE),
    Placebo = c("", "75.2 (8.6)", "76.0"),
    check.names = FALSE
  )
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)
  expect_identical(write_annotated_xlsx(df, f, title = "Demographics"), f)
  expect_true(file.exists(f) && file.info(f)$size > 0)

  flat <- unlist(openxlsx::read.xlsx(f, colNames = FALSE), use.names = FALSE)
  expect_true("Age (years)" %in% flat)
  expect_true("75.2 (8.6)" %in% flat)

  styles <- xlsx_part(f, "xl/styles.xml")
  expect_true(grepl("<b/>", styles, fixed = TRUE))       # bold (.strong + title)
  expect_true(grepl("indent=\"1\"", styles, fixed = TRUE))
})

test_that("write_annotated_xlsx() merges spanner cells for || columns", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    .label = c("N", "Mean"),
    `Placebo||CAT1` = c("86", "75.2"),
    `Placebo||CAT2` = c("72", "73.8"),
    check.names = FALSE
  )
  attr(df$`Placebo||CAT1`, "label") <- "CAT1"
  attr(df$`Placebo||CAT2`, "label") <- "CAT2"
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)
  write_annotated_xlsx(df, f)

  sheet <- xlsx_part(f, "xl/worksheets/sheet1.xml")
  merges <- regmatches(sheet, gregexpr("<mergeCell ref=\"[^\"]+\"", sheet))[[1]]
  expect_true(any(grepl(":", merges)))   # at least one spanner merge
})
