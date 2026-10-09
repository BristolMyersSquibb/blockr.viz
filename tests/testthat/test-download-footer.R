t0 <- as.POSIXct("2026-10-09 14:32:07")

footer_data <- function() {
  d <- data.frame(x = 1:3)
  attr(d, "blockr_provenance") <- list(
    dataset = "CA-244-0001", path = "/data/ca-244-0001",
    extraction = "2026-09-01",
    note = "CA-244-0001 · extract 2026-09-01"
  )
  attr(d, "blockr_filters") <- c(a = "SEX = F", b = "TRTEMFL")
  d
}

test_that("the footer fills in user, time, filters and provenance fields", {
  expect_identical(
    download_footer_text(
      "Exported by {user} on {time} · {note} · {filters}",
      footer_data(), user = "jdoe", time = t0
    ),
    paste("Exported by jdoe on 2026-10-09 14:32 ·",
          "CA-244-0001 · extract 2026-09-01 · SEX = F; TRTEMFL")
  )
  expect_identical(
    download_footer_text("{dataset} from {path}", footer_data()),
    "CA-244-0001 from /data/ca-244-0001"
  )
})

test_that("unknown names and missing data print nothing", {
  expect_identical(
    download_footer_text("By {user}{nope}.", data.frame(), user = NULL),
    "By ."
  )
  expect_identical(download_footer_text("Internal", NULL), "Internal")
})

test_that("an empty footer is no footer", {
  expect_null(download_footer_text(NULL, footer_data()))
  expect_null(download_footer_text("", footer_data()))
  expect_null(download_footer_text("  ", footer_data()))
  expect_null(download_footer_text("{nope}", footer_data()))
})

test_that("the footer is a band under the picture, at its density", {
  skip_if_not_installed("png")
  skip_if_not_installed("ragg")

  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 800, height = 400, units = "px")
  graphics::plot(1:10)
  grDevices::dev.off()
  raw <- readBin(f, "raw", file.size(f))

  # drawn at 2x: 400 CSS px wide
  out <- png_add_footer(raw, "Exported by jdoe on 2026-10-09 14:32",
                        css_width = 400)
  img <- png::readPNG(out$png)
  expect_identical(dim(img)[2L], 800L)
  expect_identical(dim(img)[1L], 400L + as.integer(out$band * 2))
  expect_gt(out$band, 0)

  # a long text wraps to more lines, so a taller band
  long <- png_add_footer(raw, strrep("For internal use only. ", 20),
                         css_width = 400)
  expect_gt(long$band, out$band)
})

test_that("the stamp grows the capture and leaves it alone without a footer", {
  skip_if_not_installed("png")
  skip_if_not_installed("ragg")

  f <- tempfile(fileext = ".png")
  ragg::agg_png(f, width = 400, height = 200, units = "px")
  graphics::plot(1:10)
  grDevices::dev.off()
  cap <- new_chart_capture(readBin(f, "raw", file.size(f)), 400, 200)

  session <- list(user = "jdoe")
  local_mocked_bindings(board_download_footer = function(...) "By {user}")
  out <- download_footer_stamp(cap, footer_data(), session)
  expect_gt(out$height, cap$height)
  expect_false(identical(out$png, cap$png))

  local_mocked_bindings(board_download_footer = function(...) NULL)
  expect_identical(download_footer_stamp(cap, footer_data(), session), cap)
  expect_null(download_footer_stamp(NULL, footer_data(), session))
})

test_that("the option takes its default from the deployment", {
  withr::local_options(blockr.viz.download_footer = "Internal · {user}")
  opt <- new_download_footer_option()
  expect_identical(blockr.core::board_option_default(opt),
                   "Internal · {user}")
  expect_identical(attr(opt, "category"), "Downloads")
})

test_that("blocks with picture downloads put the option on the board", {
  ids <- function(blk) {
    vapply(blockr.core::board_options(blk), blockr.core::board_option_id,
           character(1L))
  }
  expect_true("download_footer" %in% ids(new_chart_block()))
  expect_true("download_footer" %in% ids(new_summarize_table_block()))
  expect_true("download_footer" %in% suppressWarnings(ids(new_heatmap_block())))
  expect_false("download_footer" %in% ids(new_table_block()))
})
