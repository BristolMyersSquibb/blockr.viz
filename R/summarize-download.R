# Taking a summarize table away ------------------------------------------------
#
# Four files, one exhibit. Each format gets what it is good at rather than a
# transcription of the screen:
#
#   xlsx  the NUMBERS behind the marks, one column per statistic
#   html  the table itself, script and styles inlined
#   pptx  painted pages (see R/rank-paint.R for why a picture)
#   png   one picture, however tall it needs to be
#
# The spreadsheet is deliberately not a picture: `openxlsx::insertImage()`
# anchors to a cell RANGE rather than a cell, so images neither sort nor resize
# with the data -- and someone who opens the xlsx came to pivot, not to look.
# A box glyph is three or five numbers; the download hands over the numbers.

#' The Numbers Behind the Marks
#'
#' A summarize table's glyphs as a plain data frame: the row stub, then one
#' column per statistic each mark is drawn from -- a bar's value, a box's
#' centre, its inner range and its whiskers, a dot range's fence. This is what
#' the Excel download writes, and what anyone wanting the table's numbers
#' rather than its picture should ask for.
#'
#' Marks with no scalar behind them are left out and say so through
#' `attr(, "dropped")`: a swimlane is a set of intervals per row and a
#' sparkline a series, neither of which is a cell.
#'
#' @param prep A prepared summarize table (internal).
#' @return A data frame, with `.indent` when the table is nested.
#' @noRd
rank_export_df <- function(prep) {

  rows <- prep$rows
  n <- nrow(rows)

  out <- list()
  out[[rank_label_header(prep)]] <- as.character(rows$.label)

  dropped <- character()

  # The words a distribution's own header uses ("Median", "Q1-Q3",
  # "P10-P90"), so the spreadsheet's column names read the same as the screen
  # and nobody has to work out which quantile `bl` was.
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
      role
    )
  }

  # A faceted column's own label is the facet LEVEL ("Placebo"), which says
  # nothing on its own once the table is a grid of columns; the measure name
  # is in the second header tier. Both, joined, is what the cell means.
  full_label <- function(p) {
    lab <- p$label %||% ""
    sub <- p$sname %||% p$sub_label %||% ""
    if (nzchar(sub) && !identical(sub, lab)) paste0(sub, " · ", lab) else lab
  }

  for (p in prep$plan) {

    kind <- p$kind %||% "num"

    if (kind %in% c("interval", "sparkline")) {
      dropped <- c(dropped, full_label(p))
      next
    }

    if (!is.null(p$cols) && length(p$cols)) {
      # A distribution: every statistic it was drawn from, in the order the
      # mark reads (centre, spread, extent).
      for (role in names(p$cols)) {
        key <- p$cols[[role]]
        if (is.null(rows[[key]])) next
        out[[paste0(full_label(p), " · ", role_label(p, role))]] <-
          rows[[key]]
      }
      next
    }

    key <- p$key
    if (is.null(key) || is.null(rows[[key]])) next
    out[[full_label(p)]] <- rows[[key]]
  }

  df <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)

  # Nesting travels as `.indent`, which write_annotated_xlsx() already reads:
  # a preferred term under its system organ class is indented in the sheet the
  # way it is on screen, rather than losing its parent entirely.
  if (!is.null(rows$.level) && any(rows$.level > 0L)) {
    df$.indent <- as.integer(rows$.level)
  }

  attr(df, "dropped") <- dropped
  df
}

#' Write a Painted Exhibit to a PNG
#'
#' The summarize table as one image: the picture [pptx_add_exhibit()] places on
#' a slide, without the slide. No paging -- a file has no fixed box, so the
#' image is as tall as the table is long.
#'
#' It is a re-render, not a screenshot: the marks are drawn from the same cell
#' model the browser gets, so the file is sharper than a capture and not
#' pixel-identical to one.
#'
#' @param x A `summarize_exhibit` from [static_summarize_table()].
#' @param file Path to write the `.png` to.
#' @param width_in Image width in inches.
#' @param res Pixels per inch.
#' @param ... Passed to the painter.
#'
#' @return `file`, invisibly.
#' @seealso [static_summarize_table()], [write_exhibit_pptx()]
#' @export
write_exhibit_png <- function(x, file, width_in = 12.5,
                              res = getOption("blockr.viz.paint_res", 300),
                              ...) {

  if (!inherits(x, "summarize_exhibit")) {
    stop("write_exhibit_png() needs a summarize_exhibit.", call. = FALSE)
  }
  rank_paint_require()

  p <- rank_paint_grob(x$cells, x$prep, width_in = width_in,
                       title = x$title, subtitle = x$subtitle,
                       caption = x$caption, ...)
  rp_write_png(p, file, res = res)

  invisible(file)
}
