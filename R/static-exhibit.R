#' Static Renderer for Any Block Result
#'
#' The single print routine a rendered report or deck applies to a block's
#' result, whatever block produced it. Objects that already print as
#' themselves (a flextable, a gt table, a ggplot, an htmlwidget, ready-made
#' HTML) come back untouched; a data frame or any object with an
#' [as_annotated_df()] method (a composer table, a gtsummary table, ...) is
#' rendered through [static_table()]. Anything else comes back untouched too,
#' so wrapping a result in `static_exhibit()` is never worse than printing it
#' bare.
#'
#' This is what makes the interactive table block optional in a static
#' document. `new_table_block()` draws an annotated data frame as the
#' searchable / sortable / drillable widget, which a report cannot use; the
#' STRUCTURE that makes the table a table lives in the annotated data frame
#' itself, not in the block. So a function block that emits a composer table
#' (or its annotated data frame) prints in a report exactly as it would with a
#' table block spliced in front of it -- the render block is a dashboard
#' component, not a report requirement.
#'
#' Very large frames fall back to the bare print: a thousand-row data block is
#' a data source, not an exhibit, and typesetting it as a flextable is slow and
#' unreadable. The threshold is `getOption("blockr.viz.static_exhibit_max_rows")`
#' (2000). Display tables are far smaller, so this never touches a real
#' exhibit.
#'
#' @param x A block result: a data frame, an [as_annotated_df()]-coercible
#'   table object, or anything else.
#' @param ... Passed to [static_table()] when `x` is rendered as one.
#'
#' @return A flextable, or `x` unchanged.
#'
#' @examplesIf requireNamespace("flextable", quietly = TRUE)
#' # an annotated data frame becomes the styled static table ...
#' static_exhibit(summary_table(iris, vars = "Sepal.Length", by = "Species"))
#'
#' # ... anything else is returned as-is
#' identical(static_exhibit(1:3), 1:3)
#' @seealso [static_table()], [static_chart()], [as_annotated_df()]
#' @export
static_exhibit <- function(x, ...) {

  # Objects with a print / knit_print method of their own are already the
  # exhibit. gt stays gt on purpose: it renders in HTML documents, and
  # rewriting it as a flextable here would change what existing reports show.
  passthrough <- c(
    "flextable", "ft_group",
    "gt_tbl", "gt_group",
    "gg", "ggplot", "patchwork", "trellis",
    "htmlwidget", "knitr_kable",
    "shiny.tag", "shiny.tag.list", "html"
  )

  if (inherits(x, passthrough)) {
    return(x)
  }

  if (!can_coerce_annotated_df(x)) {
    return(x)
  }

  max_rows <- getOption("blockr.viz.static_exhibit_max_rows", 2000L)

  if (is.data.frame(x) && is.numeric(max_rows) && nrow(x) > max_rows) {
    return(x)
  }

  # An HTML target gets the HTML table: same markup, same CSS, same column
  # estimator as the app's own display table, whereas flextable exists here
  # for the ONE thing HTML does not need (real OpenXML tables in pptx and
  # docx). The route is decided from the output format rather than plumbed in
  # from the caller, so a document that renders to both gets the right table
  # in each without the report generator knowing this seam exists.
  if (exhibit_html_output()) {
    out <- tryCatch(html_exhibit(x, ...), error = function(e) NULL)
    if (!is.null(out)) {
      return(out)
    }
  }

  # static_table() needs flextable; without it, the bare print (kable, under
  # quarto's df-print) is the graceful outcome.
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(x)
  }

  # A method that exists but refuses this particular value (composer's paged
  # listings, say) must not take the whole document down with it -- the bare
  # print still says something.
  out <- tryCatch(static_table(x, ...), error = function(e) NULL)

  if (is.null(out)) x else out
}

# TRUE when the exhibit is being printed into an HTML document. knitr is a
# Suggests (and static_exhibit() is called outside knitr entirely by the
# officer deck path, which evaluates the board's code in-process), so a
# missing knitr means "not HTML" rather than an error. is_html_output() is
# TRUE for html, revealjs and every other html-based format, FALSE for pptx,
# docx and pdf -- exactly the split that matters here.
exhibit_html_output <- function() {

  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(FALSE)
  }

  isTRUE(tryCatch(knitr::is_html_output(), error = function(e) FALSE))
}

#' Static HTML Renderer for a Display Table
#'
#' The HTML counterpart of [static_table()]: the same annotated data frame,
#' rendered as the hand-rolled HTML display table the app itself draws, with
#' its styles and its collapse / search script inlined. Nothing in the
#' output talks to Shiny, so the result is a self-contained exhibit that
#' survives being written to a standalone HTML file.
#'
#' Called by [static_exhibit()] when the render target is HTML; exported so a
#' document can ask for it explicitly.
#'
#' @param x A data frame or [as_annotated_df()]-coercible table object.
#' @param title,caption Table title / footnote. `NULL` (default) takes them
#'   from the annotated data frame's display attributes, `""` switches them
#'   off -- the same title-tier convention as [static_table()].
#' @param max_height CSS max-height of the scroll container, or `NULL`
#'   (default) for none. A report exhibit is printed rather than scrolled.
#' @param ... Passed to the underlying renderer.
#'
#' @return A [htmltools::tagList()].
#' @seealso [static_exhibit()], [static_table()]
#' @export
html_exhibit <- function(x, title = NULL, caption = NULL, max_height = NULL,
                         ...) {

  # Same entry sequence as static_table() and gt_table(): coerce, then spread
  # summary_table()'s long internals to the wide display grid (a no-op on
  # already-wide input).
  data <- fmt_to_wide(as_annotated_df(x))

  if (is.null(title))   title   <- attr(data, "label")
  if (is.null(caption)) caption <- attr(data, "caption")

  html_table(
    data,
    title = title,
    caption = caption,
    max_height = max_height,
    # A printed exhibit cannot be searched, so it does not offer to be.
    search = FALSE,
    ...
  )
}
