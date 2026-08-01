dl_frame <- function(n = 60L) {
  set.seed(7)
  data.frame(
    grp = rep(c("A", "B", "C"), each = n / 3L),
    arm = rep(c("P", "X"), n / 2L),
    val = stats::rnorm(n, 10, 3),
    day = rep(seq_len(n / 3L), 3L),
    id = paste0("S", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

dl_exhibit <- function(...) {
  static_summarize_table(
    dl_frame(), by = "grp",
    summaries = list(
      list(type = "simple", func = "count_distinct", col = "id", show = "bar",
           facet = "arm", name = "Subjects"),
      list(type = "dist", col = "val", style = "box",
           inner = "median_q1_q3", outer = "p10_p90", name = "Value")
    ),
    title = "T", ...
  )
}

test_that("the export frame is the numbers behind the marks", {
  df <- rank_export_df(dl_exhibit()$prep)

  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 3L)
  # A faceted bar names its measure AND its level: "Subjects" alone would not
  # say which arm, "P" alone would not say what was counted.
  expect_true(all(c("Subjects · P", "Subjects · X") %in% names(df)))
  # Every statistic the box was drawn from, named the way its own header is.
  expect_true("Value · Median" %in% names(df))
  expect_true(any(grepl("Q1", names(df), fixed = TRUE)))
  expect_true(any(grepl("P10", names(df), fixed = TRUE)))
  expect_true("Value · n" %in% names(df))
  expect_type(df[["Value · Median"]], "double")
})

test_that("a mark with no scalar behind it is dropped, and says so", {
  ex <- static_summarize_table(
    dl_frame(), by = "grp",
    summaries = list(
      list(type = "simple", func = "count", show = "number", name = "Events"),
      list(type = "spans", name = "Episodes", x = "day", xend = "day")
    )
  )
  df <- rank_export_df(ex$prep)

  # A swimlane is a set of intervals per row, which is not a cell.
  expect_identical(attr(df, "dropped"), "Episodes")
  expect_false("Episodes" %in% names(df))
  expect_true("Events" %in% names(df))
})

test_that("a nested export carries its indentation", {
  ex <- static_summarize_table(
    dl_frame(), by = c("arm", "grp"),
    summaries = list(list(type = "simple", func = "count", show = "number",
                          name = "n"))
  )
  df <- rank_export_df(ex$prep)

  expect_true(".indent" %in% names(df))
  expect_true(any(df$.indent > 0L))
})

test_that("the HTML download is one self-contained file that still works", {
  f <- withr::local_tempfile(fileext = ".html")
  write_exhibit_html(dl_exhibit(), f, title = "T")

  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  # The marks are in the markup, not a picture of them.
  expect_true(grepl("blockr-rank-table", txt, fixed = TRUE))
  expect_true(grepl("lane-box", txt, fixed = TRUE))
  # Nothing the file does not carry: a linked script or stylesheet is a
  # promise the recipient's machine cannot keep.
  expect_false(grepl("<script[^>]+src=", txt))
  expect_false(grepl("<link[^>]+href=", txt))
  # ... and the sorting script IS carried, inlined off disk.
  expect_true(grepl("blockr-rank-row", txt, fixed = TRUE))
  expect_gt(file.size(f), 50000)
})

test_that("a dependency that cannot be inlined is refused, not shipped broken", {
  tags <- htmltools::tagList(
    htmltools::tags$div("x"),
    htmltools::htmlDependency("cdn-thing", "1.0",
                              src = c(href = "https://example.com/lib"),
                              script = "thing.js")
  )
  expect_error(exhibit_inline_deps(htmltools::renderTags(tags)$dependencies),
               "self-contained")
})

test_that("the pptx download paints, the png download is one image", {
  skip_if_not_installed("officer")
  skip_if_not(rank_paint_ready())

  ex <- dl_exhibit()

  f <- withr::local_tempfile(fileext = ".pptx")
  write_exhibit_pptx(ex, f, title = "T")
  expect_true(any(grepl("^ppt/media/.*\\.png$", utils::unzip(f, list = TRUE)$Name)))

  g <- withr::local_tempfile(fileext = ".png")
  write_exhibit_png(ex, g)
  expect_gt(file.size(g), 5000)
  # A file has no fixed box, so the image is not paged: one file, one table.
  expect_identical(length(list.files(dirname(g), pattern = basename(g))), 1L)
})

test_that("the block carries the download toggle through state", {
  blk <- new_summarize_table_block(by = "grp", download = TRUE)

  expect_true(isTRUE(
    get0("download", envir = environment(blk[["expr_server"]]))))
  # Every constructor formal needs a state entry, or the value is lost on
  # save / restore.
  expect_true("download" %in% names(formals(new_summarize_table_block)))
})
