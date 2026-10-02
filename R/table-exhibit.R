# ---------------------------------------------------------------------------
# The table block as an exhibit: the frame its downloads write.
#
# The screen draws the table from dt_flat_build(): the rowname first, the
# value columns after it, the colour-source columns dropped, numeric cells
# painted by the shading rules. The downloads used to write the block's raw
# input instead, so they lost the paint and showed the hidden source columns.
# They now write the frame below, which carries the paint as `.bg:<col>` /
# `.fg:<col>` companions (see annotation_cell_paint()) from the same colour
# functions the screen uses.
# ---------------------------------------------------------------------------

#' The paint of one shaded column, as the screen draws it.
#'
#' `sv` is a `kind = "bg"` entry of dd_shading_visuals(). Returns
#' `list(bg =, fg =)`, one hex per row of `data`, NA where the cell is
#' unpainted: an NA cell, or a row whose paint value is missing. With a colour
#' SOURCE (`sv$src`) the paint reads the companion column at the same rows
#' while the cell keeps displaying its own value. dt_flat_build() turns this
#' into the cell styles, the downloads into paint columns, so the two cannot
#' disagree.
#' @noRd
dt_bg_paint <- function(sv, data, col) {
  n <- nrow(data)
  bg <- fg <- rep(NA_character_, n)
  v <- data[[col]]
  pv <- if (is.null(sv$src)) {
    suppressWarnings(as.numeric(v))
  } else {
    suppressWarnings(as.numeric(data[[sv$src]]))
  }
  ok <- !is.na(v) & is.finite(pv)
  if (any(ok)) {
    p <- sv$fun(pv[ok])
    bg[ok] <- p$bg
    fg[ok] <- p$fg
  }
  list(bg = bg, fg = fg)
}

#' The frame the table block displays, before any formatting.
#'
#' Aggregation is a display projection (see dd_table_aggregate()): with a
#' `group` or `summaries` the table draws the summarised frame, keyed on the
#' first group column, or a single "Total" row for grand totals. Without
#' either it draws the input, with the block's own `rowname` / `value` picks.
#' Shared by the screen's payload and the downloads.
#'
#' @return `list(data, label_col, value_cols, agg)`. `label_col` and
#'   `value_cols` may be NULL (the renderer's defaults apply).
#' @noRd
dt_display_frame <- function(data, rowname = NULL, value = NULL,
                             group = character(), summaries = list()) {
  agg <- dd_table_aggregate(data, group, summaries)
  if (!isTRUE(agg$aggregated)) {
    return(list(data = data, label_col = rowname, value_cols = value,
                agg = agg))
  }
  gl <- length(agg$group)
  ad <- agg$data
  # Grand totals (no group): prepend a "Total" stub so there is always a row
  # label plus at least one metric column to render.
  if (!gl) {
    ad <- cbind(
      stats::setNames(data.frame("Total", stringsAsFactors = FALSE,
                                 check.names = FALSE), " "),
      ad
    )
  }
  list(
    data = ad,
    label_col = if (gl) agg$group[1L] else " ",
    value_cols = if (gl) {
      c(setdiff(agg$group, agg$group[1L]), agg$metric_cols)
    } else {
      agg$metric_cols
    },
    agg = agg
  )
}

#' The table block's download frame.
#'
#' The displayed columns in display order (rowname first, colour sources
#' dropped), each shaded column followed by its paint as `.bg:<col>` /
#' `.fg:<col>`. Numbers keep their full precision here; the writers format
#' them. In-cell bars (`mode = "bar"`) are not carried: those columns export
#' plain. A structured ("Table 1") frame is returned unchanged, as the screen
#' draws it unshaded and the writers read its structure themselves.
#' @noRd
dt_exhibit_frame <- function(data, rowname = NULL, value = NULL,
                             group = character(), summaries = list(),
                             shadings = list()) {
  if (!is.data.frame(data) || !ncol(data) || dt_is_structured(data)) {
    return(data)
  }
  disp <- dt_display_frame(data, rowname, value, group, summaries)
  d <- disp$data
  label_col <- disp$label_col
  if (is.null(label_col) || !label_col %in% names(d)) label_col <- names(d)[1L]
  value_cols <- intersect(disp$value_cols %||% character(), names(d))
  # A pick that vanished upstream shows a message on screen; a download has
  # no message to show, so it falls back to every column.
  if (!length(value_cols)) value_cols <- setdiff(names(d), label_col)

  vis <- dd_shading_visuals(dd_parse_shadings(shadings), d, value_cols)
  value_cols <- setdiff(value_cols, attr(vis, "hidden"))

  out <- d[c(label_col, value_cols)]
  for (cn in value_cols) {
    sv <- vis[[cn]]
    if (is.null(sv) || !identical(sv$kind, "bg")) next
    p <- dt_bg_paint(sv, d, cn)
    if (all(is.na(p$bg))) next
    out[[paste0(".bg:", cn)]] <- p$bg
    out[[paste0(".fg:", cn)]] <- p$fg
  }
  out
}
