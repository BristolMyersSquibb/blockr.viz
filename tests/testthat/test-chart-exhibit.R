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

test_that("a chart fills the slide it is placed on", {
  skip_if_not_installed("officer")

  p <- chart_static_exhibit(datasets::iris, chart_state())
  own <- gg_exhibit_size(p)

  doc <- officer::read_pptx()
  doc <- pptx_add_exhibit(doc, p, title = "Species")
  f <- withr::local_tempfile(fileext = ".pptx")
  print(doc, target = f)

  d <- withr::local_tempdir()
  utils::unzip(f, exdir = d)
  xml <- paste(readLines(file.path(d, "ppt", "slides", "slide1.xml"),
                         warn = FALSE), collapse = "")
  ext <- regmatches(xml, gregexpr('<a:ext cx="[0-9]+" cy="[0-9]+"', xml))[[1L]]
  pic <- ext[[length(ext)]]
  w <- as.numeric(sub('.*cx="([0-9]+)".*', "\\1", pic)) / 914400
  h <- as.numeric(sub('.*cy="([0-9]+)".*', "\\1", pic)) / 914400

  # Bigger than the size the plot asks to be read at: the slide's space is
  # there to be used, and an 8in figure centred on a 12.5in slide reads as
  # unfinished.
  expect_gt(w, own$width)
  # ... and at the same aspect. A plot stretched to a slide is another chart.
  expect_equal(w / h, own$width / own$height, tolerance = 1e-6)
})

test_that("a title is printed once, and only the duplicate is dropped", {
  skip_if_not_installed("officer")

  slide_text <- function(p, title) {
    doc <- pptx_add_exhibit(officer::read_pptx(), p, title = title)
    f <- tempfile(fileext = ".pptx")
    on.exit(unlink(f), add = TRUE)
    print(doc, target = f)
    d <- file.path(tempdir(), basename(tempfile()))
    utils::unzip(f, exdir = d)
    xml <- paste(readLines(file.path(d, "ppt", "slides", "slide1.xml"),
                           warn = FALSE), collapse = "")
    gsub("</?a:t>", "", regmatches(xml, gregexpr("<a:t>[^<]*</a:t>", xml))[[1L]])
  }

  p <- chart_static_exhibit(datasets::iris,
                            chart_state(title = "Species counts"))
  expect_identical(gg_title(p), "Species counts")

  # The placeholder and the plot's band would say the same thing twice, in two
  # sizes. The placeholder keeps it: it is the slide's title, and an editor
  # can retype it.
  expect_identical(slide_text(p, "Species counts"), "Species counts")

  # A deck titles its slides with the BLOCK NAME, which says something else --
  # so both survive, and the informative one is not thrown away. (The plot's
  # own title is inside the picture, so the slide's text is just the
  # placeholder's.)
  expect_identical(slide_text(p, "3. Chart"), "3. Chart")
  expect_identical(gg_title(p), "Species counts")
})

test_that("a colour scale never runs short of levels on the deck path", {
  # The deck builds a report call from the BLOCK alone -- blockr.outline has
  # no data at projection time -- so the emitted palette has to fit a level
  # count nobody knows yet. `scale_*_manual(values = <unnamed>)` does not
  # cycle: it raises "Insufficient values in manual scale" the moment the
  # data has more levels than the pool, and it did so only on a slide,
  # because the block's own download passes a snapshot and emits NAMED
  # values. That is the worst place to find out.
  set.seed(1)
  levels_n <- function(k) {
    data.frame(
      x = stats::rnorm(60), y = stats::rnorm(60),
      grp = sample(paste0("L", seq_len(k)), 60, TRUE),
      stringsAsFactors = FALSE
    )
  }
  render <- function(expr, data) {
    p <- eval(expr, list(data = data), baseenv())
    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    grDevices::png(f, width = 400, height = 300)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    print(p)
    TRUE
  }

  state <- list(chart_type = "scatter", x = "x", y = "y", color = "grp")

  # More levels than the house palette holds (7).
  for (k in c(3L, 9L, 14L)) {
    d <- levels_n(k)
    deck <- do.call(chart_expr, c(list(var = "data", qualify = TRUE), state))
    expect_true(render(deck, d))
  }

  # A snapshot still pins each level to its colour, and a level the snapshot
  # never saw does not fail the render.
  snap <- do.call(chart_expr,
                  c(list(var = "data", data = levels_n(4L), qualify = TRUE),
                    state))
  expect_true(render(snap, levels_n(5L)))
})
