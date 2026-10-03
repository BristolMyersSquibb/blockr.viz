# A browser picture cut into slide pages (inst/js/capture-pages.js) goes out
# one page per slide, all at one scale, later pages titled "(k of n)".

png_url <- function(w, h) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  grDevices::png(f, width = w, height = h)
  grid::grid.rect(gp = grid::gpar(fill = "grey"))
  grDevices::dev.off()
  paste0("data:image/png;base64,",
         jsonlite::base64_enc(readBin(f, "raw", file.size(f))))
}

page_msg <- function(n_pages = 3) {
  page <- list(png = png_url(40, 30), width = 700, height = 500)
  list(png = png_url(40, 90), width = 700, height = 1400,
       pages = rep(list(page), n_pages), title = "Adverse events")
}

test_that("a capture message keeps its pages and title", {
  cap <- chart_capture_from_msg(page_msg(3))
  expect_s3_class(cap, "chart_capture")
  expect_length(cap$pages, 3L)
  expect_s3_class(cap$pages[[1]], "chart_capture")
  expect_equal(cap$pages[[1]]$width, 700 / 96)
  expect_identical(cap$title, "Adverse events")

  one <- page_msg(0)
  one$pages <- list()
  expect_null(chart_capture_from_msg(one)$pages)
})

test_that("a paged capture writes one slide per page at one scale", {
  skip_if_not_installed("officer")
  cap <- chart_capture_from_msg(page_msg(3))
  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f))
  write_exhibit_pptx(cap, f)

  doc <- officer::read_pptx(f)
  expect_length(doc, 3L)
  s <- officer::pptx_summary(doc)
  titles <- s$text[nzchar(s$text)]
  expect_true(all(c("Adverse events (2 of 3)", "Adverse events (3 of 3)") %in%
                    titles))
  expect_false(any(grepl("(1 of 3)", titles, fixed = TRUE)))

  widths <- vapply(seq_len(3), function(k) {
    x <- officer::slide_summary(doc, index = k)
    x$cx[x$type != "title"][1]
  }, numeric(1))
  expect_equal(length(unique(round(widths, 4))), 1L)
})

test_that("an unpaged capture still goes on one slide", {
  skip_if_not_installed("officer")
  one <- page_msg(0)
  one$pages <- list()
  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f))
  write_exhibit_pptx(chart_capture_from_msg(one), f)
  expect_length(officer::read_pptx(f), 1L)
})

test_that("the page box is the slide less its margins, with the font floor", {
  skip_if_not_installed("officer")
  box <- capture_page_box()
  expect_named(box, c("w", "h", "minPt"))
  expect_gt(box$w, 5)
  expect_gt(box$h, 3)
  expect_equal(box$minPt, exhibit_min_font_size())
})
