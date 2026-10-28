# dt_download_control(): the download menu other packages put on a table
# (blockr.sandbox draws a composer table with it).

dtc_png <- function() {
  f <- withr::local_tempfile(fileext = ".png", .local_envir = parent.frame())
  png::writePNG(array(1, c(4, 6, 3)), f)
  new_chart_capture(readBin(f, "raw", file.size(f)), 600, 400)
}

dtc_server <- function(id, exhibit, enabled = NULL, picture = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    slot <- dt_download_control(session, exhibit, enabled = enabled,
                                picture = picture)
    # A second call in the same session hands back the slot alone.
    again <- dt_download_control(session, exhibit)
    list(slot = slot, again = again)
  })
}

dtc_exhibit <- function() {
  d <- data.frame(arm = c("A", "B"), n = c(1.234, 2.5))
  attr(d, "label") <- "Counts"
  list(data = d, title = "Counts by arm", subtitle = "All subjects",
       caption = "A note", digits = 1L, dataset = "adsl")
}

test_that("the menu offers what is installed, and every format writes", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("officer")
  on <- shiny::reactiveVal(TRUE)
  shiny::testServer(dtc_server, args = list(exhibit = dtc_exhibit,
                                            enabled = on), {
    expect_s3_class(session$returned$slot, "shiny.tag")
    expect_identical(session$returned$again, session$returned$slot)
    menu <- paste(as.character(output$dt_download$html), collapse = "")
    for (lab in c("Excel", "Web page", "PowerPoint")) {
      expect_match(menu, lab, fixed = TRUE)
    }
    expect_no_match(menu, "Image")

    xlsx <- output$dl_xlsx
    expect_true(file.exists(xlsx))
    expect_identical(openxlsx::read.xlsx(xlsx, colNames = FALSE)[1L, 1L],
                     "Counts by arm")
    html <- paste(readLines(output$dl_html, warn = FALSE), collapse = "")
    expect_match(html, "All subjects", fixed = TRUE)
    expect_match(html, "1.2", fixed = TRUE)
    expect_gt(file.size(output$dl_pptx), 0)

    # A format the menu does not offer still answers: no picture, an error.
    expect_error(output$dl_png, "picture is not ready")

    on(FALSE)
    session$flushReact()
    expect_null(output$dt_download$html %||% NULL)
  })
})

test_that("with a picture the menu adds Image, and the files carry it", {
  skip_if_not_installed("png")
  skip_if_not_installed("officer")
  pic <- dtc_png()
  shiny::testServer(dtc_server, args = list(exhibit = dtc_exhibit,
                                            picture = function() pic), {
    menu <- paste(as.character(output$dt_download$html), collapse = "")
    expect_match(menu, "Image", fixed = TRUE)
    expect_identical(readBin(output$dl_png, "raw", 8L), pic$png[1:8])
    html <- paste(readLines(output$dl_html, warn = FALSE), collapse = "")
    expect_match(html, "data:image/png;base64", fixed = TRUE)
    expect_gt(file.size(output$dl_pptx), 0)
  })
})

test_that("an exhibit with cards writes the cards", {
  skip_if_not_installed("officer")
  ex <- function() {
    e <- dtc_exhibit()
    e$cards <- e$data
    e
  }
  shiny::testServer(dtc_server, args = list(exhibit = ex), {
    html <- paste(readLines(output$dl_html, warn = FALSE), collapse = "")
    expect_match(html, "Counts by arm", fixed = TRUE)
    expect_gt(file.size(output$dl_pptx), 0)
  })
})

test_that("one installed format is a tool button, none is no control", {
  local_mocked_bindings(dt_has_openxlsx = function() FALSE,
                        dt_has_officer = function() FALSE)
  shiny::testServer(dtc_server, args = list(exhibit = dtc_exhibit), {
    html <- paste(as.character(output$dt_download$html), collapse = "")
    expect_match(html, "Download as Web page (.html)", fixed = TRUE)
  })
  expect_null(dl_control_ui(identity, list()))
})
