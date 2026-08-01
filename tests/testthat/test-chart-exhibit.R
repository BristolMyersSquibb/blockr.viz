# The chart block's downloads and the deck's slide must be ONE rendering.
# Before this they were two: the block captured the live ECharts canvas, the
# deck evaluated the compiled ggplot pipeline.

chart_state <- function(...) {
  c(list(chart_type = "bar", group = "Species", func = "count"), list(...))
}

test_that("the exhibit is the chart a report would print", {
  p <- chart_static_exhibit(datasets::iris, chart_state(title = "Species"))

  expect_s3_class(p, "gg")
  # The aggregated frame, one row per mark -- not the 150 input rows.
  d <- chart_exhibit_data(p)
  expect_identical(nrow(d), 3L)
  expect_true(all(c("Species", "n") %in% names(d)))
})

test_that("both report styles produce a chart, and the download follows the deck", {
  # The deck's report call honours this option; so does the download, so the
  # two cannot disagree about which renderer drew the picture.
  withr::with_options(list(blockr.viz.report_style = "code"), {
    expect_s3_class(chart_static_exhibit(datasets::iris, chart_state()), "gg")
  })
  withr::with_options(list(blockr.viz.report_style = "static"), {
    expect_s3_class(chart_static_exhibit(datasets::iris, chart_state()), "gg")
  })
})

test_that("a chart with no data is no exhibit, rather than an error", {
  expect_null(chart_static_exhibit(datasets::iris[0L, ], chart_state()))
  expect_null(chart_static_exhibit(NULL, chart_state()))
})

test_that("a chart renders to every target through the shared writers", {
  p <- chart_static_exhibit(datasets::iris, chart_state(title = "Species"))

  # png
  f <- withr::local_tempfile(fileext = ".png")
  write_exhibit_png(p, f)
  expect_gt(file.size(f), 5000)

  # html: an image in the exhibit document shell, carrying nothing external
  g <- withr::local_tempfile(fileext = ".html")
  write_exhibit_html(p, g, title = "Species")
  txt <- paste(readLines(g, warn = FALSE), collapse = "\n")
  expect_true(grepl("<img", txt, fixed = TRUE))
  expect_true(grepl("data:image/png;base64", txt, fixed = TRUE))
  expect_false(grepl("<script[^>]+src=", txt))

  skip_if_not_installed("officer")

  # pptx: one slide with a real picture on it
  h <- withr::local_tempfile(fileext = ".pptx")
  write_exhibit_pptx(p, h, title = "Species")
  files <- utils::unzip(h, list = TRUE)$Name
  expect_true(any(grepl("^ppt/media/", files)))
  expect_identical(length(officer::read_pptx(h)), 1L)
})

test_that("a plot keeps its aspect on a slide rather than filling it", {
  p <- chart_static_exhibit(datasets::iris, chart_state())
  size <- gg_exhibit_size(p)

  expect_gt(size$width, 0)
  expect_gt(size$height, 0)
  # Scaled down to a box, never stretched: the ratio survives.
  half <- gg_exhibit_size(p, max_width = size$width / 2)
  expect_equal(half$width / half$height, size$width / size$height)
})

test_that("the chart block carries the download toggle", {
  blk <- new_chart_block(chart_type = "bar", group = "Species")

  # A chart has always been takeable, so downloads default ON -- what changed
  # is what they write.
  expect_true(isTRUE(
    get0("download", envir = environment(blk[["expr_server"]]))))

  off <- new_chart_block(chart_type = "bar", group = "Species",
                         download = FALSE)
  expect_false(isTRUE(
    get0("download", envir = environment(off[["expr_server"]]))))
})
