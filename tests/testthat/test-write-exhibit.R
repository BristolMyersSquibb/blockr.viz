# write_exhibit_html() / write_exhibit_pptx(): the annotated data frame as a
# self-contained page and as a one-slide deck -- the two downloads that sit
# next to write_annotated_xlsx() on the table block's toolbar.

demo_df <- function() {
  data.frame(
    .label  = c("Age (years)", "Mean (SD)", "Median"),
    .indent = c(0L, 1L, 1L),
    .strong = c(TRUE, FALSE, FALSE),
    Placebo = c("", "75.2 (8.6)", "76.0"),
    Drug    = c("", "74.1 (9.2)", "74.5"),
    check.names = FALSE
  )
}

pptx_part <- function(file, part) {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(file, files = part, exdir = td)
  paste(readLines(file.path(td, part), warn = FALSE), collapse = "")
}

# ---- HTML -----------------------------------------------------------------

test_that("write_exhibit_html() writes one file that needs no other file", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  expect_identical(write_exhibit_html(demo_df(), f), f)

  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(html, "^<!DOCTYPE html>")
  expect_match(html, "</html>\\s*$")
  # The table, its styling and its behaviour, all in the one file.
  expect_match(html, "<table", fixed = TRUE)
  expect_match(html, "75.2 (8.6)", fixed = TRUE)
  expect_match(html, "<style", fixed = TRUE)
  expect_match(html, "<script", fixed = TRUE)
  # Nothing to fetch: no stylesheet link, no external script, no sidecar
  # lib/ folder. (blockr.ui's CSS does carry a `.shiny-html-output` selector;
  # it is an inert rule, not a dependency -- nothing outside the file is
  # referenced.)
  expect_false(grepl("<link[^>]+href", html))
  expect_false(grepl("<script[^>]+src=", html))
  expect_length(list.files(dirname(f), pattern = "_files$"), 0L)
})

test_that("write_exhibit_html() carries title, subtitle and caption", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  write_exhibit_html(
    demo_df(), f,
    title = "Demographics", subtitle = "Safety population",
    caption = "N = 200 subjects"
  )
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(html, "<title>Demographics</title>", fixed = TRUE)
  expect_match(html, "<h1>Demographics</h1>", fixed = TRUE)
  expect_match(html, "Safety population", fixed = TRUE)
  expect_match(html, "N = 200 subjects", fixed = TRUE)
  # The heading says the title; a <caption> repeating it two lines below is a
  # rendering accident, so the exhibit's own title slot stays empty.
  expect_equal(
    lengths(regmatches(html, gregexpr("Demographics", html, fixed = TRUE))),
    2L  # <title> + <h1>
  )
})

test_that("write_exhibit_html() omits the heading block when there is none", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  write_exhibit_html(demo_df(), f, title = "", subtitle = "")
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_false(grepl("<h1>", html, fixed = TRUE))
  # The CLASS is in the stylesheet either way; what must not be there is a
  # paragraph carrying it.
  expect_false(grepl("<p class=\"blockr-exhibit-subtitle\"", html,
                     fixed = TRUE))
})

test_that("write_exhibit_html() keeps the section structure it was given", {
  df <- data.frame(
    .group1_level = c("Screening", "Screening", "Treatment"),
    .label = c("Age", "Sex", "Dose"),
    Placebo = c("75", "F", "10mg"),
    check.names = FALSE
  )
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  write_exhibit_html(df, f)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(html, "blockr-section-header", fixed = TRUE)
  expect_match(html, "Screening", fixed = TRUE)
  expect_match(html, "Treatment", fixed = TRUE)
})

# A tall nested table: html_exhibit()'s height rule would open this one at its
# section rows (>24 rows, and the .indent nesting gives it something to
# collapse into). The download must not inherit that -- a page scrolls.
tall_df <- function(n = 40L) {
  data.frame(
    .label  = c("Age (years)", paste("Stat", seq_len(n))),
    .indent = c(0L, rep(1L, n)),
    Placebo = c("", rep("1.0", n)),
    check.names = FALSE
  )
}

test_that("write_exhibit_html() opens every section, however tall the table", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  # The auto rule this replaces would collapse this table.
  expect_false(html_exhibit_expanded(tall_df()))

  write_exhibit_html(tall_df(), f)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(html, "data-initial-expanded=\"1\"", fixed = TRUE)

  # ... and the caller can still ask for the collapsed opening.
  write_exhibit_html(tall_df(), f, default_expanded = FALSE)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(html, "data-initial-expanded=\"0\"", fixed = TRUE)
})

test_that("write_exhibit_html() honours collapsible = FALSE", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  df <- data.frame(
    .group1_level = c("Screening", "Screening", "Treatment"),
    .label = c("Age", "Sex", "Dose"),
    Placebo = c("75", "F", "10mg"),
    check.names = FALSE
  )

  write_exhibit_html(df, f, collapsible = FALSE)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  # Static section labels: no toggle button, no chevron, and the script is
  # told to leave the rows alone.
  expect_match(html, "blockr-section-btn-static", fixed = TRUE)
  expect_false(grepl("<button class=\"blockr-section-btn\"", html, fixed = TRUE))
  expect_match(html, "data-dt-collapsible=\"0\"", fixed = TRUE)
  # Nothing can unfold a table with no chevrons, so it cannot start folded.
  expect_match(html, "data-initial-expanded=\"1\"", fixed = TRUE)
})

test_that("write_exhibit_html() honours sortable = FALSE", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  # The class name is in the inlined stylesheet either way; what must go is
  # the header that carries it (the hook the inline script binds to) and its
  # sort arrow.
  write_exhibit_html(demo_df(), f)
  expect_match(paste(readLines(f, warn = FALSE), collapse = "\n"),
               "<th class=\"blockr-col-header leaf blockr-sortable\"",
               fixed = TRUE)

  write_exhibit_html(demo_df(), f, sortable = FALSE)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_false(grepl("<th class=\"blockr-col-header leaf blockr-sortable\"",
                     html, fixed = TRUE))
  expect_false(grepl("blockr-sort-icon\"", html, fixed = TRUE))
})

# ---- pptx -----------------------------------------------------------------

test_that("write_exhibit_pptx() writes one slide holding a NATIVE table", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  expect_identical(write_exhibit_pptx(demo_df(), f), f)

  parts <- utils::unzip(f, list = TRUE)$Name
  expect_equal(sum(grepl("^ppt/slides/slide[0-9]+\\.xml$", parts)), 1L)

  xml <- pptx_part(f, "ppt/slides/slide1.xml")
  # Real DrawingML table cells, not a picture of a table: the point of the
  # officer route is that every cell stays editable text.
  expect_gt(lengths(regmatches(xml, gregexpr("<a:tc>", xml))), 0L)
  expect_match(xml, "75.2 (8.6)", fixed = TRUE)
  expect_false(grepl("<a:blip", xml, fixed = TRUE))
})

test_that("write_exhibit_pptx() prints the title once, not twice", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  write_exhibit_pptx(demo_df(), f, title = "Demographics",
                     subtitle = "Safety population",
                     caption = "N = 200 subjects")
  xml <- pptx_part(f, "ppt/slides/slide1.xml")

  expect_equal(
    lengths(regmatches(xml, gregexpr("Demographics", xml, fixed = TRUE))),
    1L
  )
  expect_match(xml, "Safety population", fixed = TRUE)
  expect_match(xml, "N = 200 subjects", fixed = TRUE)
})

test_that("write_exhibit_pptx() strips the template's example slides", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  # A template is a deck WITH slides -- that is how pandoc learns its styles.
  # Opening rather than reading it is what used to prepend them to the export.
  tpl <- tempfile(fileext = ".pptx")
  on.exit(unlink(tpl), add = TRUE)
  demo <- officer::add_slide(officer::read_pptx(),
                             layout = "Title and Content", master = "Office Theme")
  demo <- officer::ph_with(
    demo, "EXAMPLE SLIDE",
    location = officer::ph_location_type(type = "title")
  )
  print(demo, target = tpl)

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)
  write_exhibit_pptx(demo_df(), f, template = tpl)

  parts <- utils::unzip(f, list = TRUE)$Name
  expect_equal(sum(grepl("^ppt/slides/slide[0-9]+\\.xml$", parts)), 1L)
  expect_false(grepl("EXAMPLE SLIDE", pptx_part(f, "ppt/slides/slide1.xml"),
                     fixed = TRUE))
})

test_that("an app-declared exhibit font beats the template's theme font", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  withr::with_options(
    list(blockr.viz.ft_font = "Comic Sans MS"),
    write_exhibit_pptx(demo_df(), f)
  )

  expect_match(pptx_part(f, "ppt/slides/slide1.xml"), "Comic Sans MS",
               fixed = TRUE)
})

# ---- pagination -----------------------------------------------------------

# A table long enough that no font step saves it, in the sectioned shape, so
# the section headers are there to be repeated.
long_df <- function(n = 60L, sections = TRUE) {
  out <- data.frame(
    .label = sprintf("Preferred term %d", seq_len(n)),
    .indent = 0L,
    Placebo = sprintf("%d (%.1f%%)", seq_len(n), seq_len(n) / 2),
    Drug = sprintf("%d (%.1f%%)", seq_len(n), seq_len(n) / 3),
    check.names = FALSE
  )
  if (sections) {
    out$.group1 <- "SOC"
    out$.group1_level <- rep(c("Section one", "Section two", "Section three"),
                             length.out = n)[order(rep(1:3, length.out = n))]
  }
  attr(out, "label") <- "A very long table"
  out
}

test_that("a table too tall for one slide is carried onto the next", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  write_exhibit_pptx(long_df(), f, title = "Adverse events")
  doc <- officer::read_pptx(f)
  expect_gt(length(doc), 1L)

  # Every slide repeats the header band, and says which page it is.
  for (i in seq_along(doc)) {
    xml <- pptx_part(f, sprintf("ppt/slides/slide%d.xml", i))
    expect_match(xml, "Placebo", fixed = TRUE)
    expect_match(xml, sprintf("(%d of %d)", i, length(doc)), fixed = TRUE)
  }

  # ... and no row is lost or printed twice.
  seen <- unlist(lapply(seq_along(doc), function(i) {
    xml <- pptx_part(f, sprintf("ppt/slides/slide%d.xml", i))
    grep("Preferred term", regmatches(
      xml, gregexpr("Preferred term [0-9]+", xml)
    )[[1L]], value = TRUE)
  }))
  expect_setequal(seen, long_df()$.label)
})

test_that("max_rows = NULL keeps the old one-slide overflow", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  write_exhibit_pptx(long_df(), f, title = "Adverse events", max_rows = NULL)
  expect_length(officer::read_pptx(f), 1L)
})

test_that("shrinking to fit beats splitting", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  # 20 rows overflow at 13pt and fit once the font steps down. The count is
  # deliberate: 21 lands on the boundary and steps to 11pt under a wider face
  # (DejaVu Sans, which the runners substitute for Inter), while 20 measures
  # the same under Inter, DejaVu Sans, Arial, Helvetica and Courier New.
  write_exhibit_pptx(long_df(20L, sections = FALSE), f, title = "AE")
  expect_length(officer::read_pptx(f), 1L)
  xml <- pptx_part(f, "ppt/slides/slide1.xml")
  expect_false(grepl("sz=\"1300\"", xml, fixed = TRUE))
  expect_match(xml, "sz=\"1200\"", fixed = TRUE)

  # Denied the step, the same table splits instead.
  g <- tempfile(fileext = ".pptx")
  on.exit(unlink(g), add = TRUE)
  write_exhibit_pptx(long_df(20L, sections = FALSE), g, title = "AE",
                     min_font_size = 13)
  expect_gt(length(officer::read_pptx(g)), 1L)
})

test_that("a section carried across a break repeats its heading", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  x <- long_df(60L)
  ft <- static_table(x, title = "", fit_width = 12)
  breaks <- pptx_page_breaks(ft, 6)
  expect_gt(length(breaks), 1L)
  expect_equal(utils::tail(breaks, 1L), nrow(x))
  expect_true(all(diff(breaks) > 0))

  # The continuation page opens with the section it is continuing, marked.
  page <- pptx_slice_rows(x, (breaks[[1L]] + 1L):breaks[[2L]])
  cont <- static_table(page, title = "", fit_width = 12, continued = TRUE)
  expect_match(cont$body$dataset[[1L]][[1L]], "(continued)", fixed = TRUE)
  # ... and the same page without the flag does not claim to be one.
  plain <- static_table(page, title = "", fit_width = 12)
  expect_false(grepl("(continued)", plain$body$dataset[[1L]][[1L]],
                     fixed = TRUE))
})

test_that("a table sectioned by .variable_label alone still pages", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  # The composer shape: no `.group1_level` axis, the sections come from runs
  # of `.variable_label`, and the column the headers are named from
  # (`.variable_block`) exists only on the structure view. Reading it off the
  # input instead gave an empty key and the second page asked it for a row it
  # did not have -- so the download handler threw and a deck lost its
  # pagination silently.
  x <- long_df(60L, sections = FALSE)
  x$.variable_label <- rep(c("Age", "Race", "Sex"), length.out = nrow(x))[
    order(rep(1:3, length.out = nrow(x)))
  ]

  expect_length(pptx_section_key(x), nrow(x))

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  # Sixty rows page, and a table that pages says so (exhibit_split_note).
  # What this test is about is that nothing on that path throws or warns.
  expect_no_warning(
    expect_message(
      write_exhibit_pptx(x, f, title = "Demographics"),
      class = "blockr_exhibit_split"
    )
  )
  doc <- officer::read_pptx(f)
  expect_gt(length(doc), 1L)

  seen <- unlist(lapply(seq_along(doc), function(i) {
    xml <- pptx_part(f, sprintf("ppt/slides/slide%d.xml", i))
    regmatches(xml, gregexpr("Preferred term [0-9]+", xml))[[1L]]
  }))
  expect_setequal(seen, x$.label)
})

test_that("breaks do not strand the tail of a section on the next slide", {
  key <- rep(c("a", "b"), c(10L, 12L))
  # A break at 21 would leave one row of "b" alone: the whole section moves.
  expect_equal(pptx_hold_sections(c(21L, 22L), key), c(10L, 22L))
  # A break with enough of the section left over is kept.
  expect_equal(pptx_hold_sections(c(15L, 22L), key), c(15L, 22L))
  # No sections, nothing to hold.
  expect_equal(pptx_hold_sections(c(21L, 22L), NULL), c(21L, 22L))
})

test_that("row heights are measured, not counted", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  x <- demo_df()
  x$.label[[1L]] <- paste(rep("A very long label indeed", 4L), collapse = " ")

  wide <- static_table(x, title = "", fit_width = 12)
  narrow <- static_table(x, title = "", col_widths = c(1, 1, 1))

  # The same row is taller when its column cannot hold it on one line.
  expect_gt(ft_part_heights(narrow)[[1L]], ft_part_heights(wide)[[1L]])
  expect_equal(ft_line_count("one two three", 10, "Arial", 12), 1L)
  expect_gt(ft_line_count("one two three", 0.4, "Arial", 12), 1L)
  # A hard break is honoured.
  expect_equal(ft_line_count("a\nb", 10, "Arial", 12), 2L)
})

# ---- too many columns -----------------------------------------------------

# The shape that broke in production: six arms times six toxicity grades, so
# 36 data columns on a 12.5in slide.
grade_df <- function(n_arm = 6L, n_row = 6L, cell = "1") {
  arms <- paste0(c("Placebo", "300mg", "600mg", "900mg", "1500mg", "1200mg",
                   "1800mg", "2400mg"), " (N=20)")
  stats <- c("Any Grade\nN=20", paste0("Grade ", 1:5, "\nN=20"))
  out <- data.frame(.label = paste("Dictionary derived term", seq_len(n_row)),
                    .indent = 0L, check.names = FALSE)
  for (a in arms[seq_len(n_arm)]) {
    for (s in stats) {
      out[[paste0(a, "||", s)]] <- structure(rep(cell, n_row), label = s)
    }
  }
  out
}

test_that("a table that still will not fit gets tighter padding, then says so", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  # Counts, not a bare "1": a single digit is the narrowest cell there is, and
  # in a face as tight as Inter 48 of them still clear the default padding --
  # so the relief correctly does not fire and the test was asserting the
  # metrics of whichever wider face the machine happened to substitute. A real
  # grade table holds "n (pct)", which no face fits at 5pt of padding a side.
  ft <- static_table(grade_df(8L, cell = "143 (41.2%)"), title = "",
                     fit_width = 12.53)
  expect_equal(attr(ft, "layout_plan")$cell_padding, TIGHT_PAD)
  expect_lt(ft$body$styles$pars$padding.left$data[[1L, 2L]], 5)

  # One arm holding twenty columns of "143 (41.2%)": too wide for a slide and
  # nowhere to cut it, since a spanner group is never dealt in half. Cut off
  # rather than dropped, but never silently.
  wide <- data.frame(.label = c("Nausea", "Vomiting"), .indent = 0L,
                     check.names = FALSE)
  for (i in 1:20) {
    wide[[sprintf("Arm A||Week %d", i)]] <-
      structure(rep("143 (41.2%)", 2L), label = sprintf("Week %d", i))
  }

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)
  expect_warning(write_exhibit_pptx(wide, f, title = "Wide"), "more columns")
})

test_that("columns too wide for one slide are dealt over several", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)

  # 36 grade columns of counts do not fit a widescreen slide at any font the
  # exporter is allowed to use, so the arms are dealt over two sets of slides.
  #
  # Counts rather than the fixture's bare "1": a single digit fits 36 columns
  # comfortably and only the HEADERS run out of room, and a header running out
  # of room is no longer a reason to deal the columns sideways. A table read by
  # flipping between two sets of slides is a high price, and wrapped headers do
  # not justify it -- see pptx_width_broken().
  wide <- grade_df(6L, cell = "12 (60.0)")
  expect_message(
    write_exhibit_pptx(wide, f, title = "Grades"),
    "dealt over"
  )
  doc <- officer::read_pptx(f)
  expect_gt(length(doc), 1L)

  arms_on <- function(i) {
    xml <- pptx_part(f, sprintf("ppt/slides/slide%d.xml", i))
    unique(regmatches(xml, gregexpr("[0-9a-zA-Z]+mg \\(N=20\\)|Placebo \\(N=20\\)",
                                    xml))[[1L]])
  }
  first <- arms_on(1L)
  last <- arms_on(length(doc))
  # Every slide carries the stub and a whole number of arms, and no arm is on
  # two slides at once.
  expect_lt(length(first), 6L)
  expect_length(intersect(first, last), 0L)

  # Nothing is lost: every arm appears somewhere.
  seen <- unique(unlist(lapply(seq_along(doc), arms_on)))
  expect_length(seen, 6L)

  # Told a column count, it uses that instead.
  g <- tempfile(fileext = ".pptx")
  on.exit(unlink(g), add = TRUE)
  write_exhibit_pptx(wide, g, title = "Grades", max_cols = 6L)
  expect_gte(length(officer::read_pptx(g)), 6L)

  # Told not to, it does not.
  h <- tempfile(fileext = ".pptx")
  on.exit(unlink(h), add = TRUE)
  expect_silent(suppressWarnings(
    write_exhibit_pptx(wide, h, title = "Grades", max_cols = NULL)
  ))
})

test_that("a table only splits sideways once the headers are unreadable", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")
  # This table is meant to sit right on the boundary -- it fits, but only just
  # -- and where that boundary falls is a property of the deck's typeface. A
  # runner that substitutes a wider face deals the same 14 columns over two
  # sets of slides, which says nothing about the tolerance under test.
  skip_if_font_substituted()

  # Seven arms of counts: it fits, but only with the stub over two lines and
  # the headers breaking inside a word. That is a table to read down, not one
  # to deal over two sets of slides.
  ae <- data.frame(
    .label = c("Gastrooesophageal reflux disease",
               "Upper respiratory tract infection", "Nausea"),
    .indent = 0L, check.names = FALSE
  )
  for (a in LETTERS[1:7]) {
    ae[[sprintf("Arm %s (N=143)||n (%%)", a)]] <-
      structure(c("143 (100.0%)", "88 (61.5%)", "12 (8.4%)"), label = "n (%)")
    ae[[sprintf("Arm %s (N=143)||Events", a)]] <-
      structure(c("212", "104", "17"), label = "Events")
  }

  f <- tempfile(fileext = ".pptx")
  on.exit(unlink(f), add = TRUE)
  expect_no_message(write_exhibit_pptx(ae, f, title = "Adverse events"))

  # The tolerance is what decides it: hold the headers to their full words and
  # the same table comes apart.
  g <- tempfile(fileext = ".pptx")
  on.exit(unlink(g), add = TRUE)
  withr::local_options(blockr.viz.ft_header_break_tol = 1)
  expect_message(write_exhibit_pptx(ae, g, title = "Adverse events"),
                 "dealt over")
})

test_that("column sets never cut an arm in half", {
  x <- data.frame(.label = "a", check.names = FALSE)
  for (a in c("A", "B", "C")) {
    for (s in c("n", "pct")) x[[paste0(a, "||", s)]] <- "1"
  }
  expect_equal(pptx_col_groups(x),
               list(c("A||n", "A||pct"), c("B||n", "B||pct"),
                    c("C||n", "C||pct")))
  # ... and a frame without spanners splits anywhere.
  flat <- data.frame(.label = "a", p = "1", q = "2", check.names = FALSE)
  expect_equal(pptx_col_groups(flat), list("p", "q"))

  # Whole groups, contiguous, in order.
  expect_equal(pptx_deal(6L, 2L), list(1:3, 4:6))
  expect_equal(pptx_deal(5L, 2L), list(1:2, 3:5))
})

test_that("a squeezed table gives the stub back to the columns", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  # Short stub labels: holding 1.2in for them while the data columns are cut
  # off is the wrong trade.
  short <- grade_df(8L)
  short$.label <- paste("T", seq_len(nrow(short)))
  expect_lt(static_table(short, title = "", fit_width = 12.53)$body$colwidths[[1L]], 1.2)
})

test_that("the table starts below the title, however many lines it takes", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("systemfonts")

  top_of <- function(file) {
    xml <- pptx_part(file, "ppt/slides/slide1.xml")
    off <- regmatches(xml, gregexpr("<a:off x=\"-?[0-9]+\" y=\"-?[0-9]+\"/>",
                                    xml))[[1L]]
    y <- as.numeric(sub(".*y=\"(-?[0-9]+)\".*", "\\1", off))
    y[[length(y)]] / 914400
  }

  short <- tempfile(fileext = ".pptx")
  long <- tempfile(fileext = ".pptx")
  on.exit(unlink(c(short, long)), add = TRUE)

  write_exhibit_pptx(demo_df(), short, title = "Demographics")
  write_exhibit_pptx(demo_df(), long, title = paste(
    "Number of Subjects with Treatment-Emergent Adverse Events by highest",
    "Standard Toxicity Grade, System Organ Class, and Dictionary Derived Term"
  ))

  # Never above the exporter's own floor, and pushed down by the wrap.
  expect_gte(top_of(short), 1.1)
  expect_gt(top_of(long), top_of(short) + 0.3)
})

# ---- template resolution --------------------------------------------------

test_that("pptx_template() takes the first path that actually exists", {
  real <- tempfile(fileext = ".pptx")
  file.create(real)
  on.exit(unlink(real), add = TRUE)

  expect_identical(pptx_template(real), real)

  withr::with_options(
    list(blockr.viz.pptx_template = real),
    expect_identical(pptx_template(NULL), real)
  )

  # A stale path is not a template. State saved on one machine and opened on
  # another carries absolute paths that resolve nowhere, and styling silently
  # against nothing is not something to diagnose from a slide.
  withr::with_options(
    list(blockr.viz.pptx_template = real),
    expect_identical(pptx_template("/nowhere/deck.pptx"), real)
  )
})

# ---- the availability probe ----------------------------------------------

test_that("the format probe reports what is installed", {
  expect_true(dt_pkg_installed("stats"))
  expect_true(dt_pkg_installed("stats", "utils"))
  expect_false(dt_pkg_installed("notAPackageThatExists"))
  # All of them, not any: the pptx entry needs officer AND flextable.
  expect_false(dt_pkg_installed("stats", "notAPackageThatExists"))
})

test_that("the format probe does not LOAD what it finds", {
  skip_if_not_installed("callr")
  skip_if_not_installed("flextable")

  # The probe runs when the download control renders, not when a download is
  # clicked. requireNamespace() would pull officer + flextable into the process
  # (0.49s, 53MB) just to decide whether to grey out a menu entry, so enabling
  # the PowerPoint pill has to stay free. Checked in a fresh process, because
  # this one has flextable loaded already.
  loaded <- callr::r(
    function(probe) {
      found <- probe("officer", "flextable")
      c(found = found,
        officer = "officer" %in% loadedNamespaces(),
        flextable = "flextable" %in% loadedNamespaces())
    },
    args = list(probe = dt_pkg_installed)
  )

  expect_true(loaded[["found"]])
  expect_false(loaded[["officer"]])
  expect_false(loaded[["flextable"]])
})

test_that("a page stops above whatever the layout puts at the foot", {
  skip_if_not_installed("officer")

  doc <- officer::read_pptx()
  layouts <- officer::layout_summary(doc)
  lay <- layouts$layout[[1L]]
  mas <- layouts$master[[1L]]

  bottom <- pptx_body_bottom(doc, lay, mas, 7.5)

  # Whatever the layout says, a page never runs past the old constant.
  expect_lte(bottom, 7.5 - 0.4)

  # And where there is a footer band it stops above THAT, with a gap: the
  # limit used to be the constant, so a full page of rows printed over the
  # company line on a template whose footer sits higher than 0.4in.
  ph <- officer::layout_properties(doc, layout = lay, master = mas)
  foot <- ph$offy[ph$type %in% c("ftr", "dt", "sldNum")]
  foot <- foot[is.finite(foot) & foot > 7.5 / 2]
  if (length(foot)) {
    expect_lt(bottom, min(foot))
  }

  # Unreadable geometry is not a reason to fail an export.
  expect_identical(pptx_body_bottom(doc, "no such layout", mas, 7.5), 7.5 - 0.4)
})
