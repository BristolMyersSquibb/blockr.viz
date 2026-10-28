#' Write an Exhibit to a Self-Contained HTML File
#'
#' The HTML sibling of [write_annotated_xlsx()] and [write_exhibit_pptx()]. The
#' table is drawn by [html_exhibit()] inside a minimal document shell that
#' carries the title, subtitle and caption.
#'
#' The file is self-contained: no Shiny, no CDN, no sidecar folder. The
#' display table inlines its own `<style>` and `<script>`, and the
#' dependencies of a renderer such as the summarize table are read off disk
#' and written into the document. Section collapse and column sorting work in
#' the saved file, offline.
#'
#' @param x A data frame, an [as_annotated_df()]-coercible table object, or an
#'   exhibit that renders itself, such as a `summarize_exhibit` from
#'   [static_summarize_table()].
#' @param file Path to write the `.html` to.
#' @param title,subtitle Document heading and its muted second line. `NULL` or
#'   `""` omits each.
#' @param caption Trailing footnote, rendered under the table.
#' @param default_expanded Logical. `TRUE` (default) opens every section, since
#'   the page scrolls and has no height to budget. `FALSE` opens the table at its section rows.
#' @param collapsible,sortable Logical. Whether the saved page keeps the
#'   collapse chevrons and the click-to-sort headers (both `TRUE` by default).
#' @param ... Passed to [html_exhibit()].
#'
#' @return `file`, invisibly.
#' @seealso [write_exhibit_pptx()], [write_annotated_xlsx()], [html_exhibit()]
#' @examples
#' f <- tempfile(fileext = ".html")
#' write_exhibit_html(
#'   summary_table(iris, vars = "Sepal.Length", by = "Species"),
#'   f,
#'   title = "Sepal length by species"
#' )
#' unlink(f)
#' @export
write_exhibit_html <- function(x, file, title = NULL, subtitle = NULL,
                               caption = NULL, default_expanded = TRUE,
                               collapsible = TRUE, sortable = TRUE, ...) {
  # The exhibit's own title slot stays empty: the document heading says it.
  # default_expanded is passed so html_exhibit()'s slide height rule does not
  # apply to a file that scrolls.
  exhibit <- html_exhibit(
    x, title = "", subtitle = "", caption = caption,
    default_expanded = default_expanded, collapsible = collapsible,
    sortable = sortable, ...
  )
  rendered <- htmltools::renderTags(exhibit)
  head_extra <- exhibit_inline_deps(rendered$dependencies)

  txt <- function(x) {
    if (is.character(x) && length(x) == 1L && nzchar(x)) x else NULL
  }

  body_title <- exhibit_doc_text(x, title)
  subtitle <- exhibit_doc_text(x, subtitle)
  head_title <- txt(title) %||% "Table"

  writeLines(
    c(
      "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"utf-8\">",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
      paste0("<title>", htmltools::htmlEscape(head_title), "</title>"),
      paste0("<style>", exhibit_doc_css(), "</style>"),
      head_extra,
      as.character(rendered$head),
      "</head>",
      "<body>",
      "<main class=\"blockr-exhibit-doc\">",
      as.character(htmltools::tagList(
        if (!is.null(txt(body_title))) htmltools::tags$h1(body_title),
        if (!is.null(txt(subtitle))) {
          htmltools::tags$p(class = "blockr-exhibit-subtitle", subtitle)
        }
      )),
      as.character(rendered$html),
      "</main>",
      "</body>",
      "</html>"
    ),
    file
  )

  invisible(file)
}

# A chart is already a picture of the fully titled chart, so a visible heading
# above it would print the title twice. Table-like exhibits keep their heading.
exhibit_doc_text <- function(x, text) {
  if (inherits(x, "chart_capture") || (inherits(x, "gg") && gg_has_title_band(x))) {
    return(NULL)
  }
  text
}

gg_has_title_band <- function(x) {
  labels <- tryCatch(x$labels, error = function(e) NULL)
  if (!is.list(labels)) return(FALSE)

  any(vapply(
    labels[c("title", "subtitle", "caption")],
    function(label) is.character(label) && length(label) == 1L && nzchar(label),
    logical(1)
  ))
}

# The document shell's own CSS: the page around the table. Tabular figures
# keep columns of counts aligned.
exhibit_doc_css <- function() {
  paste(
    ":root {",
    "  --blockr-color-text-primary: #111827;",
    "  --blockr-color-text-muted: #6b7280;",
    "  --blockr-color-text-subtle: #9ca3af;",
    "}",
    "body {",
    "  margin: 0;",
    "  background: #ffffff;",
    "  color: var(--blockr-color-text-primary);",
    "  font-family: Inter, \"Inter var\", system-ui, -apple-system,",
    "    \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif;",
    "  font-variant-numeric: tabular-nums;",
    "  -webkit-font-smoothing: antialiased;",
    "}",
    ".blockr-exhibit-doc {",
    "  max-width: 1100px;",
    "  margin: 0 auto;",
    "  padding: 40px 32px 64px;",
    "}",
    ".blockr-exhibit-doc h1 {",
    "  font-size: 1.35rem;",
    "  font-weight: 600;",
    "  letter-spacing: -0.01em;",
    "  line-height: 1.25;",
    "  margin: 0 0 0.3em;",
    "}",
    ".blockr-exhibit-doc .blockr-exhibit-subtitle {",
    "  color: var(--blockr-color-text-muted);",
    "  font-size: 0.9rem;",
    "  line-height: 1.45;",
    "  max-width: 46em;",
    "  margin: 0 0 1.4em;",
    "}",
    # The screen margins waste a printed page.
    "@media print {",
    "  .blockr-exhibit-doc { max-width: none; padding: 0; }",
    "}",
    sep = "\n"
  )
}

# HTML dependencies written into the document as `<style>` / `<script>`
# blocks, in dependency order. A dependency with no readable local file (a CDN
# href) is an error, since the file would not be self-contained.
exhibit_inline_deps <- function(deps) {
  out <- character()
  for (dep in deps) {
    dir <- dep$src$file

    if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) {
      stop("write_exhibit_html() cannot inline the HTML dependency '",
           dep$name %||% "?", "'; the file would not be self-contained.",
           call. = FALSE)
    }

    read_one <- function(f, tag) {
      path <- file.path(dir, f)
      if (!file.exists(path)) {
        stop("write_exhibit_html() cannot inline '", f, "' from the '",
             dep$name %||% "?", "' dependency.", call. = FALSE)
      }
      c(paste0("<", tag, ">"),
        readLines(path, warn = FALSE),
        paste0("</", tag, ">"))
    }

    for (f in dep$stylesheet %||% character()) {
      out <- c(out, read_one(f, "style"))
    }
    for (f in dep$script %||% character()) {
      out <- c(out, read_one(f, "script"))
    }
  }
  out
}

# The web page download: the picture when the browser drew one, else what
# the block's render object draws, else the rows as the screen rounds them.
export_html <- function(x, file) {
  t <- exhibit_titles(x)
  p <- exhibit_picture(x)
  if (!is.null(p)) {
    return(write_exhibit_html(p, file, title = t$title))
  }
  obj <- exhibit_render(x) %||% exhibit_data(x, display = TRUE)
  m <- exhibit_meta(x)
  write_exhibit_html(obj, file, title = t$title, subtitle = t$subtitle,
                     caption = t$caption,
                     collapsible = !identical(m$collapsible, FALSE),
                     sortable = !identical(m$sortable, FALSE))
}
