# Taking a summarize table away ------------------------------------------------
#
# xlsx gets the numbers behind the marks, one column per statistic; html the
# table itself; pptx painted pages (R/paint-table.R); png one picture,
# however tall it needs to be. The spreadsheet is no picture: someone who opens
# it came to pivot.

# The numbers behind the marks: the row stub, then one column per statistic
# each mark is drawn from (a bar's value, a box's centre, inner range and
# whiskers). Marks with no scalar behind them (a swimlane's intervals, a
# sparkline's series) are left out and listed in `attr(, "dropped")`. A nested
# table gets an `.indent` column.
#' @noRd
summarize_export_df <- function(prep) {
  rows <- prep$rows
  out <- list()
  # A table without `by` has a blank stub header; the column needs a name.
  head <- summarize_label_header(prep)
  out[[if (nzchar(head)) head else " "]] <- as.character(rows$.label)
  dropped <- character()
  for (p in prep$plan) {
    if ((p$kind %||% "num") %in% c("interval", "sparkline")) {
      dropped <- c(dropped, summarize_export_label(p))
      next
    }
    cols <- summarize_export_cols(p, rows)
    for (nm in names(cols)) out[[nm]] <- cols[[nm]]
  }
  df <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(rows$.level) && any(rows$.level > 0L)) {
    df$.indent <- as.integer(rows$.level)
  }
  attr(df, "dropped") <- dropped
  df
}

# A plan item's columns for the spreadsheet, as a named list.
#' @noRd
summarize_export_cols <- function(p, rows) {
  cols <- list()
  if (length(p$cols)) {
    # A distribution: every statistic it was drawn from, in reading order.
    for (role in names(p$cols)) {
      key <- p$cols[[role]]
      if (!is.null(rows[[key]])) {
        cols[[paste0(summarize_export_label(p), " \u00b7 ",
                     summarize_export_role(p, role))]] <- rows[[key]]
      }
    }
    return(cols)
  }
  key <- p$key
  if (is.null(key) || is.null(rows[[key]])) {
    return(cols)
  }
  if (identical(p$kind, "tile")) {
    # A heatmap cell: the value, blank where the screen is empty, with its
    # paint as `.bg:` / `.fg:` columns (write_annotated_xlsx()).
    paint <- lane_tile_paint(p, rows)
    lab <- p$label %||% summarize_export_label(p)
    v <- rows[[key]]
    v[!paint$has] <- NA
    cols[[lab]] <- v
    cols[[paste0(".bg:", lab)]] <- paint$bg
    cols[[paste0(".fg:", lab)]] <- paint$fg
    return(cols)
  }
  cols[[summarize_export_label(p)]] <- rows[[key]]
  cols
}

# A column's label: the facet level ("Placebo") joined to the measure name
# from the second header tier.
#' @noRd
summarize_export_label <- function(p) {
  lab <- p$label %||% ""
  sub <- p$sname %||% p$sub_label %||% ""
  if (nzchar(sub) && !identical(sub, lab)) paste0(sub, " \u00b7 ", lab) else lab
}

# A statistic's name in the words of the distribution's header ("Median",
# "Q1-Q3", "P10-P90"), as on screen.
#' @noRd
summarize_export_role <- function(p, role) {
  w <- p$words %||% list()
  switch(
    role,
    bc = w$center %||% "centre",
    n = "n",
    bl = paste0(w$range %||% "range", " (low)"),
    bh = paste0(w$range %||% "range", " (high)"),
    wl = paste0(w$whisk %||% "whiskers", " (low)"),
    wh = paste0(w$whisk %||% "whiskers", " (high)"),
    ol = paste0(w$whisk %||% "outer", " (low)"),
    oh = paste0(w$whisk %||% "outer", " (high)"),
    a = sub(" \u25c7$", "", w$from %||% "from"),
    b = sub(" \u25cf$", "", w$to %||% "to"),
    lo = "range (low)",
    hi = "range (high)",
    role
  )
}

#' Write an Exhibit to a PNG
#'
#' The exhibit as one image: what [pptx_add_exhibit()] places on a slide,
#' without the slide. A summarize table is as tall as it is long (nothing is
#' paged); a ggplot is drawn at the size the plot asks for.
#'
#' The image is a re-render from the cell model the browser gets, not a
#' screenshot.
#'
#' @param x A `summarize_exhibit` from [static_summarize_table()], or a
#'   ggplot.
#' @param file Path to write the `.png` to.
#' @param width_in Image width in inches. `NULL` (charts) keeps the plot's own.
#' @param res Pixels per inch.
#' @param ... Passed to the renderer.
#'
#' @return `file`, invisibly.
#' @seealso [static_summarize_table()], [write_exhibit_pptx()]
#' @export
write_exhibit_png <- function(x, file, width_in = NULL,
                              res = getOption("blockr.viz.paint_res", 300),
                              ...) {
  UseMethod("write_exhibit_png")
}

#' @rdname write_exhibit_png
#' @export
write_exhibit_png.default <- function(x, file, width_in = NULL, res = 300,
                                      ...) {
  stop("write_exhibit_png() has no method for <",
       paste(class(x), collapse = "/"), ">.", call. = FALSE)
}

#' @rdname write_exhibit_png
#' @export
write_exhibit_png.summarize_exhibit <- function(x, file, width_in = NULL,
                                                res = getOption(
                                                  "blockr.viz.paint_res", 300),
                                                ...) {
  summarize_paint_require()
  p <- summarize_paint_grob(x$cells, x$prep, width_in = width_in %||% 12.5,
                            title = x$title, subtitle = x$subtitle,
                            caption = x$caption, ...)
  paint_write_png(p, file, res = res)
  invisible(file)
}

#' @rdname write_exhibit_png
#' @export
write_exhibit_png.gg <- function(x, file, width_in = NULL,
                                 res = getOption("blockr.viz.paint_res", 300),
                                 ...) {
  size <- gg_exhibit_size(x, max_width = width_in)
  gg_write_png(x, file, size$width, size$height, res = res)
  invisible(file)
}
