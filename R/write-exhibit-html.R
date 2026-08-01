#' Write an Exhibit to a Self-Contained HTML File
#'
#' The HTML sibling of [write_annotated_xlsx()] and [write_exhibit_pptx()]: one
#' annotated data frame, one file, nothing to unpack. The table is drawn by
#' [html_exhibit()] -- the same markup, CSS and collapse script the app itself
#' shows -- wrapped in a minimal document shell that carries the title,
#' subtitle and caption.
#'
#' The file is self-contained, always. [html_table()] inlines its own `<style>`
#' and `<script>` next to the table and talks to nothing outside them (no
#' Shiny, no CDN, no sidecar `lib/` folder), so for a display table there is
#' nothing to bundle. A renderer that does carry dependencies -- the summarize
#' table's shared script bundle -- has them read off disk and written into the
#' document. Section collapse and column sorting keep working in the saved
#' file, offline, which is the difference between this and a screenshot.
#'
#' @param x A data frame, an [as_annotated_df()]-coercible table object, or an
#'   exhibit that renders itself -- a `summarize_exhibit` from
#'   [static_summarize_table()], whose glyphs exist only in this markup.
#' @param file Path to write the `.html` to.
#' @param title,subtitle Document heading and its muted second line. `NULL` or
#'   `""` omits each.
#' @param caption Trailing footnote, rendered under the table.
#' @param default_expanded Logical. `TRUE` (default) opens every section, which
#'   is what a downloaded file is for: the page scrolls, so there is no height
#'   to budget, and a reader who asked for the table wants the numbers, not
#'   nine headers to click. `FALSE` opens it at its section rows.
#' @param collapsible,sortable Logical. Whether the saved page keeps the
#'   collapse chevrons and the click-to-sort headers (both `TRUE` by default).
#'   The table block passes its own display toggles through here, so a table
#'   shown without collapsing downloads without it.
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

  # The exhibit's own title slot stays empty: the document heading below says
  # it once, and a <caption> repeating it two lines further down reads as a
  # rendering accident rather than a design.
  #
  # default_expanded is passed rather than left to html_exhibit()'s height
  # rule. That rule exists for the slide: a sixty-row table printed onto a
  # reveal.js slide has a fixed box to fit and trades rows for reach. A
  # downloaded file has no such box -- it scrolls, and it prints -- so the
  # rule would only cost the reader nine clicks on a demographics table.
  exhibit <- html_exhibit(
    x,
    title = "",
    caption = caption,
    default_expanded = default_expanded,
    collapsible = collapsible,
    sortable = sortable,
    ...
  )

  rendered <- htmltools::renderTags(exhibit)

  # Self-contained or nothing. `html_table()` inlines its own style and script
  # and so arrives here with no dependencies at all; the summarize table's
  # renderer carries the shared bundle (Blockr.Select, the gear engine, the
  # rank script), and those are read off disk and written INTO the file, the
  # same way blockr.outline's HTML deck carries them. What cannot be inlined
  # -- a CDN href, a file that has gone -- is a dependency the download would
  # not carry, so it is a refusal rather than a silently broken artifact.
  head_extra <- character()

  if (length(rendered$dependencies)) {
    head_extra <- exhibit_inline_deps(rendered$dependencies)
  }

  txt <- function(x) {
    if (is.character(x) && length(x) == 1L && nzchar(x)) x else NULL
  }

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
        if (!is.null(txt(title))) htmltools::tags$h1(title),
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

# The document shell's own CSS -- the page around the table, never the table.
# Deliberately short: everything the exhibit needs it brings with it, so what
# is left is the page's typeface, its measure and its heading block.
#
# The face and the greys are the deck theme's (inst/revealjs/blockr.scss in
# blockr.outline), so a table downloaded as HTML and the same table on a slide
# are recognisably the same artifact. Tabular figures for the same reason they
# are set there: a column of counts that does not align on the digit is what
# makes a clinical table look unfinished.
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
    # Printing the download is a first-class use of it (the reason a reviewer
    # asks for HTML rather than xlsx), and the screen margins waste a page.
    "@media print {",
    "  .blockr-exhibit-doc { max-width: none; padding: 0; }",
    "}",
    sep = "\n"
  )
}

# An HTML dependency written INTO the document rather than linked beside it.
#
# A download is a file somebody mails on; a `<link href="lib/...">` beside it
# is a promise the recipient's machine cannot keep. So each stylesheet and
# script is read off disk and emitted as a `<style>` / `<script>` block, in
# dependency order. Anything with no readable local file -- a CDN href, a
# package that has moved -- is an error here rather than a table that arrives
# unstyled on somebody else's laptop.
#
# blockr.outline's deck writer does the same job for the same reason
# (deck_inline_deps); it drops what it cannot inline because a deck of twenty
# exhibits should not fail over one widget. A single-table download has no
# such trade: there is nothing else in the file.
#' @noRd
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
