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
