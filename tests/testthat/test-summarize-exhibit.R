demo_frame <- function(n = 100L) {
  set.seed(42)
  data.frame(
    grp = rep(c("A", "B", "C", "D"), each = n / 4L),
    arm = rep(c("Placebo", "Active"), n / 2L),
    val = stats::rnorm(n, 10, 3),
    id = paste0("S", sprintf("%03d", seq_len(n))),
    stringsAsFactors = FALSE
  )
}

demo_exhibit <- function(data = demo_frame(), ...) {
  static_summarize_table(
    data, by = "grp",
    summaries = list(
      list(type = "simple", func = "count_distinct", col = "id",
           show = "bar", facet = "arm", name = "Subjects"),
      list(type = "dist", col = "val", style = "box",
           inner = "median_q1_q3", outer = "p10_p90", name = "Value")
    ),
    ...
  )
}

test_that("static_summarize_table() carries the cell model", {
  ex <- demo_exhibit(title = "T")

  expect_s3_class(ex, "summarize_exhibit")
  expect_s3_class(ex, "blockr_exhibit")
  expect_identical(ex$cells$n, 4L)
  expect_identical(
    vapply(ex$cells$cols, function(c) c$kind, character(1L)),
    c("bar", "bar", "box")
  )
  expect_identical(ex$title, "T")
})

test_that("the HTML renderer draws the same marks the app does", {
  ex <- demo_exhibit()
  html <- as.character(htmltools::renderTags(html_exhibit(ex))$html)

  expect_true(grepl("blockr-rank-table", html, fixed = TRUE))
  expect_true(grepl("lane-box", html, fixed = TRUE))
  expect_true(grepl("blockr-rank-fill", html, fixed = TRUE))
  # An exhibit is printed, not searched. (The class name still appears in the
  # inlined CSS, so the test asks for the INPUT.)
  expect_false(grepl("<input type=\"search\"", html, fixed = TRUE))
})

test_that("a nested export opens every row", {
  data <- demo_frame()
  ex <- static_summarize_table(
    data, by = c("arm", "grp"),
    summaries = list(list(type = "simple", func = "count", show = "number",
                          name = "n"))
  )
  html <- as.character(htmltools::renderTags(html_exhibit(ex))$html)

  # The dashboard hides child rows behind a chevron; a printed table cannot.
  # Both class names appear in the inlined CSS, so the test reads the ROWS.
  rows <- regmatches(html, gregexpr("<tr class=\"[^\"]*\"", html))[[1L]]
  expect_true(any(grepl("is-child", rows, fixed = TRUE)))
  expect_false(any(grepl("collapsed-hidden", rows, fixed = TRUE)))
  expect_false(any(grepl(" collapsed\"", rows, fixed = TRUE)))
})

test_that("the block's report call rebuilds its own table", {
  blk <- new_summarize_table_block(
    by = "grp",
    summaries = list(list(type = "simple", func = "count", show = "bar",
                          name = "n")),
    title = "AEs"
  )
  cl <- report_call(blk, "block1")

  expect_true(is.call(cl))
  txt <- paste(deparse(cl), collapse = " ")
  expect_true(grepl("blockr.viz::static_summarize_table(block1", txt,
                    fixed = TRUE))
  expect_true(grepl("by = \"grp\"", txt, fixed = TRUE))
  expect_true(grepl("title = \"AEs\"", txt, fixed = TRUE))
  # Interaction-only state is not part of a printed table.
  expect_false(grepl("drill", txt, fixed = TRUE))
  expect_false(grepl("ctrl_target", txt, fixed = TRUE))
  expect_false(grepl("max_height", txt, fixed = TRUE))

  # ... and the emitted call evaluates to an exhibit.
  block1 <- demo_frame()
  expect_s3_class(eval(cl), "summarize_exhibit")
})

test_that("the row slicer keeps per-row and per-column fields apart", {
  ex <- demo_exhibit()
  m <- ex$cells
  cut <- rp_slice(m, c(1L, 3L))

  expect_identical(cut$n, 2L)
  expect_identical(cut$label, m$label[c(1L, 3L)])
  # per row
  expect_identical(cut$cols[[3L]]$bc, m$cols[[3L]]$bc[c(1L, 3L)])
  # per column: the label slot width and the palette are the WHOLE table's,
  # so pages keep the same tracks and the same colours
  expect_identical(cut$cols[[1L]]$dw, m$cols[[1L]]$dw)
  expect_identical(cut$cols[[1L]]$fill, m$cols[[1L]]$fill)
})

test_that("the pager terminates, covers every row and never orphans a group", {
  # 3 groups of 5: a parent row and four children each
  n <- 15L
  m <- list(
    n = n,
    level = rep(c(0L, 1L, 1L, 1L, 1L), 3L),
    parent_row = rep(c(TRUE, FALSE, FALSE, FALSE, FALSE), 3L),
    label = as.character(seq_len(n)),
    cols = list()
  )

  for (per in c(1L, 2L, 3L, 4L, 5L, 7L, 20L)) {
    pages <- rp_page_rows(m, per)
    rows <- as.integer(unlist(pages))

    # every row placed
    expect_setequal(unique(rows), seq_len(n))
    # only parents are ever repeated (as carried-over headings)
    body <- rows[!(rows %in% which(m$parent_row))]
    expect_false(any(duplicated(body)))
    # No page ends on a parent row, which would strand its children at the
    # top of the next one. Unless the page holds a single row: at that budget
    # there is nothing to shrink into.
    for (p in pages) {
      last <- as.integer(p)[length(p)]
      expect_true(!m$parent_row[[last]] || last == n || length(p) == 1L)
    }
  }
})

test_that("a page never exceeds the height it was given", {
  data <- demo_frame(240L)
  data$grp <- paste0("term ", sprintf("%02d", seq_len(nrow(data)) %% 30L))
  ex <- static_summarize_table(
    data, by = "grp",
    summaries = list(list(type = "simple", func = "count", show = "bar",
                          name = "n")),
    title = "T")
  pages <- rank_paint_pages(ex$cells, ex$prep, width_in = 12.5,
                            max_height = 2.2, title = "T")

  expect_gt(length(pages), 1L)
  for (p in pages) {
    expect_lte(p$height, 2.2)
    expect_equal(p$width, 12.5)
  }
})

test_that("pptx_add_exhibit() paints one slide per page", {
  skip_if_not_installed("officer")
  skip_if_not(rank_paint_ready())

  ex <- demo_exhibit(title = "T")
  doc <- officer::read_pptx()
  doc <- pptx_add_exhibit(doc, ex, title = "Deck")

  expect_identical(length(doc), 1L)

  f <- withr::local_tempfile(fileext = ".pptx")
  print(doc, target = f)
  files <- utils::unzip(f, list = TRUE)$Name

  # The exhibit is a picture, which is what the format forces: a DrawingML
  # table cell holds text runs only, so a glyph cannot live in one.
  expect_true(any(grepl("^ppt/media/.*\\.png$", files)))
})

test_that("a long table is paged rather than overflowing the slide", {
  skip_if_not_installed("officer")
  skip_if_not(rank_paint_ready())

  data <- demo_frame(600L)
  data$grp <- paste0("term ", sprintf("%03d", seq_len(nrow(data)) %% 60L))
  ex <- static_summarize_table(
    data, by = "grp",
    summaries = list(list(type = "simple", func = "count", show = "bar",
                          name = "n"))
  )
  doc <- officer::read_pptx()
  doc <- pptx_add_exhibit(doc, ex, title = "Long")

  expect_gt(length(doc), 1L)
})
