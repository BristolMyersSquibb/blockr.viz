#' Static Renderer for Any Block Result
#'
#' The print routine a rendered report or deck applies to a block's result.
#' Objects that already print as themselves (a flextable, a gt table, a
#' ggplot, an htmlwidget, ready-made HTML) come back untouched; a data frame
#' or any object with an [as_annotated_df()] method (a composer table, a
#' gtsummary table, ...) is rendered through [static_table()]. Anything else
#' comes back untouched too.
#'
#' The table structure lives in the annotated data frame, so a function block
#' that emits a composer table prints in a report as it would with a table
#' block in front of it.
#'
#' Data frames above `getOption("blockr.viz.static_exhibit_max_rows")` (2000)
#' rows fall back to the bare print.
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
#' @seealso [static_table()], [as_annotated_df()]
#' @export
static_exhibit <- function(x, ...) {
  # Objects with a print / knit_print method of their own are already the
  # exhibit.
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

  # An HTML target gets the HTML table; flextable serves pptx and docx.
  if (exhibit_html_output()) {
    out <- tryCatch(html_exhibit(x, ...), error = function(e) NULL)
    if (!is.null(out)) {
      return(out)
    }
  }

  # static_table() needs flextable; without it the bare print is returned.
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(x)
  }

  # A method that refuses this value falls back to the bare print.
  out <- tryCatch(static_table(x, ...), error = function(e) NULL)

  if (is.null(out)) x else out
}

# TRUE when the exhibit is printed into an HTML document. knitr is a Suggests,
# so a missing knitr means "not HTML".
exhibit_html_output <- function() {

  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(FALSE)
  }

  isTRUE(tryCatch(knitr::is_html_output(), error = function(e) FALSE))
}

#' Static HTML Renderer for a Display Table
#'
#' The HTML counterpart of [static_table()]: the same annotated data frame,
#' rendered as the HTML display table the app draws, with its styles and its
#' collapse / search script inlined. Nothing in the output talks to Shiny, so
#' it can be written to a standalone HTML file.
#'
#' Called by [static_exhibit()] when the render target is HTML. It is a
#' generic: an exhibit that carries its own renderer (the summarize table's
#' glyph model) answers for itself, and everything else falls to the default,
#' an annotated data frame drawn as the display table.
#'
#' @param x A data frame or [as_annotated_df()]-coercible table object.
#' @param title,subtitle,caption Table title / subtitle / footnote. `NULL`
#'   (default) takes them from the annotated data frame's display attributes,
#'   `""` switches them off: the same title-tier convention as
#'   [static_table()]. The default method ignores `subtitle`.
#' @param max_height CSS max-height of the scroll container, or `NULL`
#'   (default) for none. A report exhibit is printed rather than scrolled.
#' @param default_expanded Logical, or `NULL` (default) to decide from the
#'   table's height: a table that fits opens expanded, one that does not opens
#'   at its section rows. The threshold is
#'   `getOption("blockr.viz.html_exhibit_expanded_max_rows")` (24).
#' @param sortable,collapsible Logical. Whether the default method's table
#'   keeps its click-to-sort headers and its fold chevrons.
#' @param ... Passed to the method.
#'
#' @return A [htmltools::tagList()].
#' @seealso [static_exhibit()], [static_table()]
#' @export
html_exhibit <- function(x, title = NULL, subtitle = NULL, caption = NULL,
                         max_height = NULL, default_expanded = NULL, ...) {
  UseMethod("html_exhibit")
}

#' @rdname html_exhibit
#' @export
html_exhibit.default <- function(x, title = NULL, subtitle = NULL,
                                 caption = NULL,
                                 max_height = NULL, default_expanded = NULL,
                                 sortable = TRUE, collapsible = TRUE, ...) {

  # Same entry as static_table(): coerce, then spread to the wide grid.
  data <- fmt_to_wide(as_annotated_df(x))

  if (is.null(title))   title   <- attr(data, "label")
  if (is.null(caption)) caption <- attr(data, "caption")

  if (is.null(default_expanded)) {
    default_expanded <- html_exhibit_expanded(data)
  }

  # The table view draws it (table_static()), as the block's records layout
  # does. A printed exhibit cannot be searched or downloaded from.
  spec <- viz_block_spec("table")
  v <- viz_check(spec, list(layout = "records", sortable = isTRUE(sortable),
                            collapsible = isTRUE(collapsible), search = FALSE,
                            download = FALSE))
  flat <- !table_is_structured(data)
  shown <- if (flat) data[!grepl(ANNOTATION_PAINT_RE, names(data))] else data
  built <- table_frame_view(v, shown, list(scale_map = function() NULL),
                            "records")
  model <- built$model
  # A frame's own paint (`.bg:<col>`, the block's shading as exported).
  if (!is.null(model) && flat) {
    for (k in seq_along(model$cols)) {
      p <- annotation_cell_paint(data, model$cols[[k]]$name)
      if (!is.null(p)) model$cols[[k]][c("bg", "fg")] <- list(I(p$bg), I(p$fg))
    }
  }
  # Nothing can unfold a table with no chevrons, so it cannot start folded.
  if (!is.null(model) && !isTRUE(default_expanded) && isTRUE(collapsible)) {
    model$collapsed <- TRUE
  }
  table_static(model, v,
               list(titles = table_static_titles(title, NULL, caption),
                    empty = built$empty),
               max_height = max_height)
}

# Should a printed table open expanded? Decided from height: a table of at
# most `blockr.viz.html_exhibit_expanded_max_rows` rows stays expanded,
# since collapsing a short table hides its numbers. A taller one opens at its
# section rows if it has something to collapse.
html_exhibit_expanded <- function(data) {

  view <- tryCatch(annotated_structure_view(data), error = function(e) NULL)

  if (is.null(view)) {
    return(TRUE)
  }

  max_rows <- getOption("blockr.viz.html_exhibit_expanded_max_rows", 24L)

  if (!is.numeric(max_rows) || !length(max_rows)) {
    return(TRUE)
  }

  if (nrow(view$data) <= max_rows[[1L]]) {
    return(TRUE)
  }

  # A flat table stays expanded whatever its height. Collapsible structures
  # are section headers (section_cols) and rows that parent deeper rows
  # through `.indent`.
  !html_table_collapsible(view$data, view$section_cols)
}

# Does this display grid render any collapsible row? The structured table's
# two rules: a non-empty section axis, or a row immediately followed
# by a more deeply indented one.
html_table_collapsible <- function(data, section_cols = character()) {

  if (length(section_cols)) {
    return(TRUE)
  }

  if (!".indent" %in% names(data) || nrow(data) < 2L) {
    return(FALSE)
  }

  ind <- suppressWarnings(as.integer(data[[".indent"]]))
  ind[is.na(ind) | ind < 0L] <- 0L

  any(ind[-1L] > ind[-length(ind)])
}
