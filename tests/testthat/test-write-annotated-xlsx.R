# write_annotated_xlsx(): annotated data frame -> styled .xlsx.

xlsx_part <- function(file, part) {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
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
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  expect_identical(write_annotated_xlsx(df, f, title = "Demographics"), f)
  expect_true(file.exists(f) && file.info(f)$size > 0)

  flat <- unlist(openxlsx::read.xlsx(f, colNames = FALSE), use.names = FALSE)
  expect_true("Age (years)" %in% flat)
  expect_true("75.2 (8.6)" %in% flat)

  styles <- xlsx_part(f, "xl/styles.xml")
  expect_true(grepl("<b/>", styles, fixed = TRUE))       # bold (.strong + title)
  expect_true(grepl("indent=\"1\"", styles, fixed = TRUE))
})

test_that("write_annotated_xlsx() renders row-side groups as header rows, not columns", {
  skip_if_not_installed("openxlsx")
  # A sectioned Table-1 frame as the table block's download handler passes it
  # in (fmt_to_wide(ann_data()) keeps the dotted structure columns).
  df <- data.frame(
    .group1_level = c("Screening", "Screening", "Treatment", "Treatment"),
    .label  = c("Age", "Sex", "Dose", "Weight"),
    .indent = c(0L, 0L, 0L, 0L),
    .strong = c(FALSE, FALSE, FALSE, FALSE),
    Placebo = c("64.1", "43 F", "0 mg", "71.3"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(df$.group1_level, "label") <- "Visit"
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  write_annotated_xlsx(df, f)

  cells <- openxlsx::read.xlsx(f, colNames = FALSE, skipEmptyRows = FALSE,
                               skipEmptyCols = FALSE)
  flat <- unlist(cells, use.names = FALSE)
  # The structure column never leaks as a dotted-headed data column ...
  expect_false(any(grepl("^\\.group", flat[!is.na(flat)])))
  expect_identical(ncol(cells), 2L)   # stub + Placebo only
  # ... and each section restart gets a bold header row (label-prefixed, like
  # the HTML renderer's section rows).
  stub <- cells[[1L]]
  expect_identical(sum(stub == "Visit: Screening", na.rm = TRUE), 1L)
  expect_identical(sum(stub == "Visit: Treatment", na.rm = TRUE), 1L)
  expect_lt(which(stub == "Visit: Screening"), which(stub == "Age")[1L])

  wb <- openxlsx::loadWorkbook(f)
  hdr_rows <- which(stub %in% c("Visit: Screening", "Visit: Treatment"))
  bold_stub_rows <- unlist(lapply(wb$styleObjects, function(s) {
    if ("BOLD" %in% s$style$fontDecoration && all(s$cols == 1L)) s$rows
  }))
  expect_true(all(hdr_rows %in% bold_stub_rows))
})

test_that("write_annotated_xlsx() on a stub-only frame leaves column 2 alone", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    .label  = c("Group", "A", "B"),
    .indent = c(0L, 1L, 1L),
    .strong = c(TRUE, FALSE, FALSE),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  write_annotated_xlsx(df, f)

  sheet <- xlsx_part(f, "xl/worksheets/sheet1.xml")
  # 2L:n_col used to reverse to 2:1 here and style/widen a phantom column 2
  # (clobbering the stub width): only column 1 may carry a custom width, and
  # no cell may live outside column A.
  cols <- regmatches(sheet, gregexpr("<col [^/]*/>", sheet))[[1]]
  expect_identical(length(cols), 1L)
  expect_true(grepl("min=\"1\" max=\"1\" width=\"34", cols))
  refs <- regmatches(sheet, gregexpr("<c r=\"[A-Z]+[0-9]+\"", sheet))[[1]]
  expect_true(all(grepl("r=\"A", refs)))
})

test_that("write_annotated_xlsx() batched styles match the per-row layout", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    .label  = c("Age (years)", "Mean (SD)", "Median", "Sex", "F"),
    .indent = c(0L, 1L, 2L, 0L, 1L),
    .strong = c(TRUE, FALSE, FALSE, TRUE, FALSE),
    Placebo = c("", "75.2 (8.6)", "76.0", "", "43 (50%)"),
    Active  = c("", "74.8 (9.0)", "75.0", "", "40 (48%)"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  write_annotated_xlsx(df, f)

  # Read the workbook back and flatten styleObjects to per-cell facts.
  wb <- openxlsx::loadWorkbook(f)
  facts <- do.call(rbind, lapply(wb$styleObjects, function(s) {
    # rows/cols are stored PAIRED per styled cell (already grid-expanded).
    data.frame(row = s$rows, col = s$cols,
               bold = "BOLD" %in% s$style$fontDecoration,
               indent = s$style$indent %||% 0L,
               halign = s$style$halign %||% "",
               stringsAsFactors = FALSE)
  }))
  cell <- function(row, col) facts[facts$row == row + 1L & facts$col == col, ]

  for (i in seq_len(nrow(df))) {
    st <- cell(i, 1L)                    # stub: indent + bold, left
    expect_identical(nrow(st), 1L)
    expect_identical(as.integer(st$indent), df$.indent[i])
    expect_identical(st$bold, df$.strong[i])
    expect_identical(st$halign, "left")
    for (j in 2:3) {                     # data cells: right, bold on .strong
      dc <- cell(i, j)
      expect_identical(nrow(dc), 1L)
      expect_identical(dc$bold, df$.strong[i])
      expect_identical(dc$halign, "right")
    }
  }
  # Values round-trip.
  cells <- openxlsx::read.xlsx(f, colNames = FALSE)
  expect_true(all(c("75.2 (8.6)", "40 (48%)", "Median") %in%
                    unlist(cells, use.names = FALSE)))
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
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  write_annotated_xlsx(df, f)

  sheet <- xlsx_part(f, "xl/worksheets/sheet1.xml")
  merges <- regmatches(sheet, gregexpr("<mergeCell ref=\"[^\"]+\"", sheet))[[1]]
  expect_true(any(grepl(":", merges)))   # at least one spanner merge
})

test_that("write_annotated_xlsx() writes subtitle and caption rows", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    .label  = c("Age (years)", "Mean (SD)"),
    .indent = c(0L, 1L),
    Placebo = c("", "75.2 (8.6)"),
    check.names = FALSE
  )
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  write_annotated_xlsx(
    df, f,
    title = "Demographics", subtitle = "Treatment: Placebo",
    caption = "N = 86 subjects"
  )
  flat <- unlist(openxlsx::read.xlsx(f, colNames = FALSE), use.names = FALSE)
  expect_true("Demographics" %in% flat)
  expect_true("Treatment: Placebo" %in% flat)
  expect_true("N = 86 subjects" %in% flat)
  # Order: title above subtitle above body; caption after the body.
  idx <- match(c("Demographics", "Treatment: Placebo", "Mean (SD)",
                 "N = 86 subjects"), flat)
  expect_false(anyNA(idx))
  expect_true(all(diff(idx) > 0))
})

test_that("subtitle alone and caption alone still write", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(.label = "Age", Placebo = "75", check.names = FALSE)
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  write_annotated_xlsx(df, f, subtitle = "Sub only", caption = "Cap only")
  flat <- unlist(openxlsx::read.xlsx(f, colNames = FALSE), use.names = FALSE)
  expect_true(all(c("Sub only", "Cap only") %in% flat))
})
