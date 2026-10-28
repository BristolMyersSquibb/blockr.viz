# Taking a summarize table away ------------------------------------------------
#
# Each format gets what it is good at:
#
#   xlsx  the numbers behind the marks, one column per statistic
#   html  the table itself, script and styles inlined
#   pptx  painted pages (see R/summarize-paint.R)
#   png   one picture, however tall it needs to be
#
# The spreadsheet is not a picture: someone who opens it came to pivot, and
# `openxlsx::insertImage()` images neither sort nor resize with the data.

#' The Numbers Behind the Marks
#'
#' A summarize table's glyphs as a plain data frame: the row stub, then one
#' column per statistic each mark is drawn from (a bar's value, a box's centre,
#' inner range and whiskers, a dot range's fence). This is what the Excel
#' download writes.
#'
#' Marks with no scalar behind them (a swimlane's intervals, a sparkline's
#' series) are left out and listed in `attr(, "dropped")`.
#'
#' @param prep A prepared summarize table (internal).
#' @return A data frame, with `.indent` when the table is nested.
#' @noRd
summarize_export_df <- function(prep) {

  rows <- prep$rows
  n <- nrow(rows)

  out <- list()
  # A table without `by` has a blank stub header; the column still needs a name.
  head <- summarize_label_header(prep)
  out[[if (nzchar(head)) head else " "]] <- as.character(rows$.label)

  dropped <- character()

  # Column names use the words of the distribution's header ("Median",
  # "Q1-Q3", "P10-P90"), as on screen.
  role_label <- function(p, role) {
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

  # A faceted column's label is the facet level ("Placebo"); the measure name
  # is in the second header tier. Joined, they name the cell.
  full_label <- function(p) {
    lab <- p$label %||% ""
    sub <- p$sname %||% p$sub_label %||% ""
    if (nzchar(sub) && !identical(sub, lab)) paste0(sub, " \u00b7 ", lab) else lab
  }

  for (p in prep$plan) {

    kind <- p$kind %||% "num"

    if (kind %in% c("interval", "sparkline")) {
      dropped <- c(dropped, full_label(p))
      next
    }

    if (!is.null(p$cols) && length(p$cols)) {
      # A distribution: every statistic it was drawn from, in reading order.
      for (role in names(p$cols)) {
        key <- p$cols[[role]]
        if (is.null(rows[[key]])) next
        out[[paste0(full_label(p), " \u00b7 ", role_label(p, role))]] <-
          rows[[key]]
      }
      next
    }

    key <- p$key
    if (is.null(key) || is.null(rows[[key]])) next
    if (identical(kind, "tile")) {
      # A heatmap cell: the value, blank where the screen is empty, with its
      # paint as `.bg:` / `.fg:` columns (write_annotated_xlsx()).
      paint <- lane_tile_paint(p, rows)
      lab <- p$label %||% full_label(p)
      v <- rows[[key]]
      v[!paint$has] <- NA
      out[[lab]] <- v
      out[[paste0(".bg:", lab)]] <- paint$bg
      out[[paste0(".fg:", lab)]] <- paint$fg
      next
    }
    out[[full_label(p)]] <- rows[[key]]
  }

  df <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)

  # Nesting travels as `.indent`, which write_annotated_xlsx() reads.
  if (!is.null(rows$.level) && any(rows$.level > 0L)) {
    df$.indent <- as.integer(rows$.level)
  }

  attr(df, "dropped") <- dropped
  df
}

#' Write an Exhibit to a PNG
#'
#' The exhibit as one image: what [pptx_add_exhibit()] places on a slide,
#' without the slide. A summarize table is painted from its cell model and is
#' as tall as the table is long (nothing is paged); a ggplot is drawn at the
#' size the plot asks for.
#'
#' The image is a re-render from the same cell model the browser gets, so it is
#' sharper than a screenshot and not pixel-identical to one.
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
  rp_write_png(p, file, res = res)

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
