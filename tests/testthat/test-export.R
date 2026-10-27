tiny_png <- function() {
  f <- withr::local_tempfile(fileext = ".png", .local_envir = parent.frame())
  png::writePNG(array(1, c(4, 6, 3)), f)
  readBin(f, "raw", file.size(f))
}

exhibit_table <- function(...) {
  df <- data.frame(arm = c("A", "B"), n = c(1.2345, 2.5))
  new_viz_exhibit(df, title = "Counts", subtitle = "By arm", caption = "Note",
                  meta = list(digits = 1L), ...)
}

test_that("an exhibit is read through its accessors", {
  x <- exhibit_table()
  expect_s3_class(x, "viz_exhibit")
  expect_identical(exhibit_titles(x),
                   list(title = "Counts", subtitle = "By arm",
                        caption = "Note"))
  expect_identical(exhibit_kind(x), "table")
  expect_null(exhibit_picture(x))
  expect_null(exhibit_render(x))
  expect_identical(exhibit_meta(x)$digits, 1L)
  expect_identical(exhibit_data(x)$n, c(1.2345, 2.5))
  expect_identical(exhibit_data(x, display = TRUE)$n, c("1.2", "2.5"))
})

test_that("new_viz_exhibit() refuses what it cannot hold", {
  expect_error(new_viz_exhibit(1), "`data` must be a data frame")
  expect_error(new_viz_exhibit(data.frame(), picture = "x"), "chart_capture")
  expect_error(new_viz_exhibit(data.frame(), meta = list(1)), "named list")
  expect_error(new_viz_exhibit(data.frame(), kind = "map"))
})

test_that("formats are registered, replaced and ordered", {
  old <- as.list(viz_exporter_env)
  withr::defer({
    rm(list = ls(viz_exporter_env), envir = viz_exporter_env)
    list2env(old, viz_exporter_env)
  })
  rm(list = ls(viz_exporter_env), envir = viz_exporter_env)
  register_viz_exporter("b", "B", function(x, f) NULL, order = 2)
  register_viz_exporter("a", "A", function(x, f) NULL, order = 1)
  register_viz_exporter("c", "C", function(x, f) NULL, needs = "notapkg9")
  register_viz_exporter("d", "D", function(x, f) NULL,
                        accepts = function(x) stop("boom"))
  expect_named(viz_exporters(), c("a", "b", "c", "d"))
  expect_named(viz_export_formats(exhibit_table()), c("a", "b"))
  register_viz_exporter("a", "A2", function(x, f) NULL, order = 9)
  expect_named(viz_exporters(), c("b", "a", "c", "d"))
  expect_error(register_viz_exporter("A B", "x", identity))
})

test_that("viz registers Excel, Web page, Image and PowerPoint", {
  expect_true(all(c("xlsx", "html", "png", "pptx") %in% ls(viz_exporter_env)))
  table_formats <- names(viz_export_formats(exhibit_table()))
  expect_false("png" %in% table_formats)
  expect_true("html" %in% table_formats)
  chart <- new_viz_exhibit(data.frame(a = 1), kind = "chart")
  expect_true("png" %in% names(viz_export_formats(chart)))
})

test_that("the web page writes the rows, or the picture", {
  f <- withr::local_tempfile(fileext = ".html")
  export_html(exhibit_table(), f)
  page <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(page, "Counts")
  expect_match(page, "1.2")

  pic <- new_chart_capture(tiny_png(), 600, 400)
  g <- withr::local_tempfile(fileext = ".html")
  export_html(new_viz_exhibit(data.frame(a = 1), picture = pic,
                              title = "Chart", kind = "chart"), g)
  expect_match(paste(readLines(g, warn = FALSE), collapse = ""),
               "data:image/png;base64")
})

test_that("the image writes the picture and asks for one without", {
  pic <- new_chart_capture(tiny_png(), 600, 400)
  f <- withr::local_tempfile(fileext = ".png")
  export_png(new_viz_exhibit(data.frame(a = 1), picture = pic,
                             kind = "chart"), f)
  expect_identical(dim(png::readPNG(f))[1:2], c(4L, 6L))
  expect_error(export_png(new_viz_exhibit(data.frame(a = 1), kind = "chart"),
                          f), "not ready yet")
})

test_that("the spreadsheet keeps the values", {
  skip_if_not_installed("openxlsx")
  f <- withr::local_tempfile(fileext = ".xlsx")
  export_xlsx(exhibit_table(), f)
  wb <- openxlsx::read.xlsx(f, colNames = FALSE)
  expect_true(any(vapply(wb, function(col) any(col %in% "Counts"),
                         logical(1L))))
  expect_true(any(vapply(wb, function(col) {
    any(suppressWarnings(as.numeric(col)) == 1.2345, na.rm = TRUE)
  }, logical(1L))))
})

test_that("the slides write the rows, or the picture", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  f <- withr::local_tempfile(fileext = ".pptx")
  export_pptx(exhibit_table(), f)
  expect_gt(length(officer::read_pptx(f)), 0L)
  pic <- new_chart_capture(tiny_png(), 600, 400)
  g <- withr::local_tempfile(fileext = ".pptx")
  export_pptx(new_viz_exhibit(data.frame(a = 1), picture = pic,
                              kind = "chart"), g)
  expect_gt(length(officer::read_pptx(g)), 0L)
})

dl_server <- function(id, exhibit, enabled) {
  shiny::moduleServer(id, function(input, output, session) {
    list(menu = viz_downloads(exhibit, enabled, session))
  })
}

test_that("viz_downloads() lists the formats and writes them on click", {
  on <- shiny::reactiveVal(TRUE)
  shiny::testServer(dl_server, args = list(exhibit = exhibit_table,
                                           enabled = on), {
    menu <- session$returned$menu()
    expect_true(all(c("Web page") %in% vapply(menu, `[[`, "", "label")))
    expect_false("png" %in% vapply(menu, `[[`, "", "ext"))
    html <- menu[[which(vapply(menu, `[[`, "", "ext") == "html")]]
    expect_identical(html$id, session$ns("dl_html"))
    path <- output$dl_html
    expect_match(paste(readLines(path, warn = FALSE), collapse = ""),
                 "Counts")
    on(FALSE)
    session$flushReact()
    expect_identical(session$returned$menu(), list())
  })
})

test_that("an exhibit that fails to build gives an empty menu", {
  shiny::testServer(dl_server, args = list(
    exhibit = function() stop("no data"), enabled = function() TRUE
  ), {
    expect_identical(session$returned$menu(), list())
  })
})

# The PowerPoint files move to blockr.outline as they are (design.md
# section 5), so they may call only their own functions, the exhibit
# accessors and blockr.viz's exports.
test_that("the PowerPoint files reach nothing internal", {
  paths <- Sys.glob(test_path("..", "..", "R", "export-pptx-*.R"))
  skip_if_not(length(paths) > 0L, "R/ sources are not in the package")
  ns <- asNamespace("blockr.viz")
  own <- unlist(lapply(paths, function(p) {
    env <- new.env()
    sys.source(p, envir = env, keep.source = FALSE, toplevel.env = ns)
    ls(env, all.names = TRUE)
  }))
  # %||% is rlang's (and base R's from 4.4); viz's own copy only shadows it.
  allowed <- c(own, getNamespaceExports(ns), "%||%")
  reached <- unique(unlist(lapply(own, function(nm) {
    f <- get(nm, envir = ns)
    if (is.function(f)) codetools::findGlobals(f) else character()
  })))
  internal <- setdiff(intersect(reached, ls(ns, all.names = TRUE)), allowed)
  expect_identical(sort(internal), character())
})
