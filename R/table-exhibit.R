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

#' Format a download frame's numbers the way the table block shows them.
#'
#' Every numeric data column becomes text through dt_format_num(), the
#' screen's own formatter; NA stays NA, so the writers draw their missing
#' mark. The stub (`.label`, else the first column) and the dot-prefixed
#' structure columns are left alone, as on screen. Column attributes (the
#' header label, emphasis) survive. `digits = NULL`, or anything that is not
#' a data frame, comes back unchanged.
#' @noRd
dt_format_digits <- function(data, digits) {
  digits <- suppressWarnings(as.integer(digits))
  if (!is.data.frame(data) || length(digits) != 1L || is.na(digits)) {
    return(data)
  }
  stub <- if (".label" %in% names(data)) ".label" else names(data)[1L]
  cols <- setdiff(names(data), stub)
  cols <- cols[!startsWith(cols, ".")]
  cols <- cols[vapply(data[cols], is.numeric, logical(1L))]
  for (cn in cols) {
    v <- data[[cn]]
    out <- rep(NA_character_, length(v))
    keep <- !is.na(v)
    out[keep] <- dt_format_num(v[keep], digits)
    a <- attributes(v)
    attributes(out) <- a[setdiff(names(a), c("class", "levels", "names",
                                             "dim", "dimnames"))]
    data[[cn]] <- out
  }
  data
}

#' The Table Block as a Printed Table
#'
#' What [new_table_block()] puts into a rendered report or deck: the table as
#' the block shows it, as an [annotated data frame][as_annotated_df()] for
#' [static_exhibit()] to typeset. The rowname comes first, then the value
#' columns (or the aggregate, with `group` / `summaries`); numbers are
#' rounded to `digits` exactly like the screen, trailing zeros dropped; cells
#' shaded by a diverging or sequential rule carry their colours as `.bg:` /
#' `.fg:` columns, which [static_table()] and the HTML table paint. Colour
#' source columns are left out. In-cell bars are not drawn.
#'
#' The block's [report_call()] emits
#' `blockr.viz::static_exhibit(blockr.viz::table_exhibit(<var>, ...))` with
#' its own settings, so blockr.outline needs nothing table-specific.
#'
#' @param data The table block's input: a data frame or an
#'   [as_annotated_df()]-coercible table object.
#' @param rowname,value,group,summaries,shadings,digits As in
#'   [new_table_block()].
#' @param title,subtitle,caption Table text, with the block's three tiers:
#'   `NULL` takes the input's `label` / `subtitle` / `caption` attribute, `""`
#'   shows none, any other string is a title template resolved against
#'   `data`.
#'
#' @return An annotated data frame, with `label`, `subtitle` and `caption`
#'   attributes carrying the resolved text.
#' @seealso [new_table_block()], [static_exhibit()], [report_call()]
#' @examples
#' table_exhibit(
#'   data.frame(term = "A", times_more = 3.81427, ci_low = 2.70123),
#'   digits = 2L, title = "Area effect"
#' )
#' @export
table_exhibit <- function(data, rowname = NULL, value = NULL,
                          group = character(), summaries = list(),
                          shadings = list(), digits = 2L, title = NULL,
                          subtitle = NULL, caption = NULL) {
  data <- as_annotated_df(data)
  auto <- input_display_attrs(data)
  out <- dt_exhibit_frame(data, rowname, value,
                          as.character(group %||% character()),
                          dd_parse_summaries(summaries), shadings)
  # A structured frame shows its cells as they come, on screen too.
  if (!dt_is_structured(data)) out <- dt_format_digits(out, digits)
  attr(out, "label") <- resolve_block_title(title, data, auto = auto$label)
  attr(out, "subtitle") <- resolve_block_title(subtitle, data,
                                               auto = auto$subtitle)
  attr(out, "caption") <- resolve_block_title(caption, data,
                                              auto = auto$caption)
  out
}

#' @rdname report_call
#' @export
report_call.table_block <- function(x, var, ...) {

  # The committed block's state lives in its constructor closure, the same
  # values serialization reads (see report_call.chart_block). The legacy
  # `cell_color` is already folded into `shadings` there.
  env <- environment(x[["expr_server"]])

  state <- function(nm) {
    v <- get0(nm, envir = env, ifnotfound = NULL)
    if (is.function(v)) NULL else v
  }

  # Print-relevant surface only: which columns, how they are shaded and
  # rounded. Drill, search, sort, the identity row colour and the ctrl
  # transports are not part of a printed table.
  spec <- list(rowname = NULL, value = NULL, group = NULL, summaries = NULL,
               shadings = NULL, digits = 2L)

  args <- list()

  for (nm in names(spec)) {
    v <- state(nm)
    if (is.null(v) || !length(v) ||
          (is.character(v) && !any(nzchar(v)))) next
    if (identical_default(v, spec[[nm]])) next
    args[[nm]] <- v
  }

  for (nm in c("title", "subtitle", "caption")) {
    v <- state(nm)
    if (!is.null(v)) args[[nm]] <- as.character(v)[1L]
  }

  exhibit <- as.call(c(
    list(call("::", as.name("blockr.viz"), as.name("table_exhibit")),
         as.name(var)),
    args
  ))

  as.call(list(call("::", as.name("blockr.viz"), as.name("static_exhibit")),
               exhibit))
}
