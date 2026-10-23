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
  df <- summarize_export_df(dl_exhibit()$prep)

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
  df <- summarize_export_df(ex$prep)

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
  df <- summarize_export_df(ex$prep)

  expect_true(".indent" %in% names(df))
  expect_true(any(df$.indent > 0L))
})

test_that("the HTML download is one self-contained file that still works", {
  f <- withr::local_tempfile(fileext = ".html")
  write_exhibit_html(dl_exhibit(), f, title = "T")

  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  # The marks are in the markup, not a picture of them.
  expect_true(grepl("blockr-summarize-table", txt, fixed = TRUE))
  expect_true(grepl("lane-box", txt, fixed = TRUE))
  # Nothing the file does not carry: a linked script or stylesheet is a
  # promise the recipient's machine cannot keep.
  expect_false(grepl("<script[^>]+src=", txt))
  expect_false(grepl("<link[^>]+href=", txt))
  # ... and the sorting script IS carried, inlined off disk.
  expect_true(grepl("blockr-summarize-row", txt, fixed = TRUE))
  expect_gt(file.size(f), 50000)
})

test_that("the HTML download does not re-resolve subtitle templates", {
  ex <- dl_exhibit(
    subtitle = "By: {@rows}[ Display AEs with at least {@min_share} patients]",
    .title_args = list(rows = "SOC and PT", min_share = "1")
  )
  f <- withr::local_tempfile(fileext = ".html")
  write_exhibit_html(ex, f, title = ex$title, subtitle = ex$subtitle)

  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_true(grepl("By: SOC and PT", txt, fixed = TRUE))
  expect_false(grepl(">By:<", txt, fixed = TRUE))
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
  skip_if_not(summarize_paint_ready())

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

test_that("a new table drops the picture the browser took of the old one", {
  skip_if_not(summarize_paint_ready())
  f <- withr::local_tempfile(fileext = ".png")
  grDevices::png(f, width = 7, height = 5)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  shot <- readBin(f, "raw", file.size(f))
  url <- paste0("data:image/png;base64,", jsonlite::base64_enc(shot))

  blk <- new_summarize_table_block(
    by = "grp", download = TRUE,
    summaries = list(list(type = "simple", func = "count", show = "bar"))
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      scope <- session$makeScope("expr")
      session$flushReact()
      scope$setInputs(summarize_table_block_capture = list(
        png = url, width = 7, height = 5
      ))
      session$flushReact()
      # The picture the browser took is what the image download writes ...
      expect_identical(readBin(scope$output$dl_png, "raw", 1e6), shot)
      scope$setInputs(summarize_table_block_action = list(
        action = "config", param = "sort_dir", value = "asc"
      ))
      session$flushReact()
      # ... until the table changes: then the current table is painted.
      expect_false(identical(readBin(scope$output$dl_png, "raw", 1e6), shot))
    },
    args = list(x = blk, data = list(data = function() dl_frame()))
  )
})
