# The exhibit: everything a block's downloads write, in one object.
#
# A block hands its download menu a function that returns an exhibit; each
# exporter (R/export-registry.R) reads it through the accessors below and
# nothing else of the block. That is what lets an exporter live in another
# package: blockr.outline registers PowerPoint the same way viz registers
# Excel.

#' A block's exhibit
#'
#' What a block's downloads write: its rows, and when the browser drew the
#' block, the picture.
#'
#' @param data An annotated data frame ([as_annotated_df()]): the rows as the
#'   block shows them, with their cell formats, colours and groups.
#' @param picture A `chart_capture`: the block as the browser drew it, or
#'   `NULL`.
#' @param render An object that [html_exhibit()] and [pptx_add_exhibit()]
#'   draw better than `data` alone (the groups table with its marks, cards),
#'   or `NULL`.
#' @param title,subtitle,caption The resolved text, or `NULL`.
#' @param kind `"table"` or `"chart"`.
#' @param meta A named list: `dataset` (the first part of the file name),
#'   `digits` (the number format the screen rounds to), `collapsible` and
#'   `sortable` (for the web page), `source` (the frame the download
#'   footer's provenance is read from, when `data` was built for the export).
#' @return A `viz_exhibit`.
#' @seealso [register_viz_exporter()]
#' @export
new_viz_exhibit <- function(data, picture = NULL, render = NULL, title = NULL,
                            subtitle = NULL, caption = NULL,
                            kind = c("table", "chart"), meta = list()) {
  kind <- match.arg(kind)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.null(picture) && !inherits(picture, "chart_capture")) {
    stop("`picture` must be a chart_capture or NULL.", call. = FALSE)
  }
  if (!is.list(meta) || (length(meta) && is.null(names(meta)))) {
    stop("`meta` must be a named list.", call. = FALSE)
  }
  structure(
    list(data = data, picture = picture, render = render,
         titles = list(title = title, subtitle = subtitle, caption = caption),
         kind = kind, meta = meta),
    class = "viz_exhibit"
  )
}

#' Read an exhibit
#'
#' The only way an exporter reads an exhibit.
#'
#' @param x A `viz_exhibit`.
#' @param display For `exhibit_data()`: `TRUE` rounds the numbers to the
#'   exhibit's `digits` as text, the way the screen shows them (for a web
#'   page or a slide); `FALSE` keeps the values (for a spreadsheet, which
#'   rounds with a number format instead).
#' @return `exhibit_data()` a data frame; `exhibit_picture()` a
#'   `chart_capture` or `NULL`; `exhibit_render()` the render object or
#'   `NULL`; `exhibit_titles()` a list of `title`, `subtitle` and `caption`;
#'   `exhibit_kind()` `"table"` or `"chart"`; `exhibit_meta()` the named list.
#' @name exhibit-accessors
#' @export
exhibit_data <- function(x, display = FALSE) {
  stopifnot(inherits(x, "viz_exhibit"))
  if (display) dt_format_digits(x$data, x$meta$digits) else x$data
}

#' @rdname exhibit-accessors
#' @export
exhibit_picture <- function(x) {
  stopifnot(inherits(x, "viz_exhibit"))
  x$picture
}

#' @rdname exhibit-accessors
#' @export
exhibit_render <- function(x) {
  stopifnot(inherits(x, "viz_exhibit"))
  x$render
}

#' @rdname exhibit-accessors
#' @export
exhibit_titles <- function(x) {
  stopifnot(inherits(x, "viz_exhibit"))
  x$titles
}

#' @rdname exhibit-accessors
#' @export
exhibit_kind <- function(x) {
  stopifnot(inherits(x, "viz_exhibit"))
  x$kind
}

#' @rdname exhibit-accessors
#' @export
exhibit_meta <- function(x) {
  stopifnot(inherits(x, "viz_exhibit"))
  x$meta
}

# The exhibit with the board's download footer (R/download-footer.R) as the
# caption's last line and, on a picture, drawn under it. Done once, before
# any exporter sees the exhibit.
exhibit_footed <- function(x, session) {
  src <- x$meta$source %||% x$data
  x$titles$caption <- download_footer_caption(x$titles$caption, src, session)
  if (!is.null(x$picture)) {
    x$picture <- download_footer_stamp(x$picture, src, session)
  }
  x
}
