# A ggplot through the exhibit writers (R/gg-exhibit.R). Other packages'
# blocks hand these a ggplot; the chart block exports its captured picture.

species_plot <- function(title = NULL) {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(datasets::iris, ggplot2::aes(Species)) +
    ggplot2::geom_bar()
  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }
  attr(p, "pptx_width") <- 6
  attr(p, "pptx_height") <- 4
  p
}

test_that("a plot renders to every target through the shared writers", {
  p <- species_plot(title = "Species")

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

test_that("a titled plot's HTML omits the document title block", {
  p <- species_plot(title = "Demographic Distribution")

  f <- withr::local_tempfile(fileext = ".html")
  write_exhibit_html(p, f, title = "Demographic Distribution", subtitle = "by")
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(html, "<title>Demographic Distribution</title>", fixed = TRUE)
  expect_true(grepl("<img", html, fixed = TRUE))
  expect_false(grepl("<h1>Demographic Distribution</h1>", html,
                     fixed = TRUE))
})

test_that("bare ggplot HTML output keeps the document subtitle", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(datasets::iris, ggplot2::aes(Species, Sepal.Length)) +
    ggplot2::geom_boxplot()

  f <- withr::local_tempfile(fileext = ".html")
  write_exhibit_html(p, f, title = "Demographic Distribution",
                     subtitle = "Safety population")
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(html, "<p class=\"blockr-exhibit-subtitle\">Safety population</p>",
               fixed = TRUE)
})

test_that("a plot keeps its aspect on a slide rather than filling it", {
  p <- species_plot()
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

test_that("a plot fills the slide it is placed on", {
  skip_if_not_installed("officer")

  p <- species_plot()
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

  p <- species_plot(title = "Species counts")
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
