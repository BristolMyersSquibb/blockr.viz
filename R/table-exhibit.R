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

#' The table block's download frame.
#'
#' The displayed columns in display order (`rowname` first, then `columns`,
#' colour sources dropped), each shaded column followed by its paint as
#' `.bg:<col>` / `.fg:<col>`. Numbers keep their full precision here; the writers format
#' them. In-cell bars (`mode = "bar"`) are not carried: those columns export
#' plain. A structured ("Table 1") frame is returned unchanged, as the screen
#' draws it unshaded and the writers read its structure themselves.
#' @noRd
dt_exhibit_frame <- function(data, rowname = NULL, columns = NULL,
                             shadings = list()) {
  if (!is.data.frame(data) || !ncol(data) || table_is_structured(data)) {
    return(data)
  }
  d <- data
  label_col <- rowname
  if (is.null(label_col) || !label_col %in% names(d)) label_col <- names(d)[1L]
  value_cols <- intersect(columns %||% character(), names(d))
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
#' header label, emphasis) survive. `digits` is the table's number format
#' (table_format_state()); `NULL`, or anything that is not a data frame,
#' comes back unchanged.
#' @noRd
dt_format_digits <- function(data, digits) {
  if (!is.data.frame(data) || is.null(digits) || !length(digits)) {
    return(data)
  }
  digits <- table_format_state(digits)
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
#' [static_exhibit()] to typeset. The rowname comes first, then `columns`;
#' numbers are printed in the block's `format` exactly like the screen, and
#' `unit` heads the numeric columns; cells shaded by a diverging or
#' sequential rule carry their colours as `.bg:` / `.fg:` columns, which
#' [static_table()] and the HTML table paint. Colour source columns are left
#' out. In-cell bars are not drawn.
#'
#' The block's [report_call()] emits
#' `blockr.viz::static_exhibit(blockr.viz::table_exhibit(<var>, ...))` with
#' its own settings, so blockr.outline needs nothing table-specific.
#'
#' @param data The table block's input: a data frame or an
#'   [as_annotated_df()]-coercible table object.
#' @param rowname,columns,shadings,format,unit As in [new_table_block()].
#' @param title,subtitle,caption Table text, with the block's three tiers:
#'   `NULL` takes the input's `label` / `subtitle` / `caption` attribute, `""`
#'   shows none, any other string is a title template resolved against
#'   `data`.
#'
#' @return An annotated data frame, with `label`, `subtitle` and `caption`
#'   attributes carrying the resolved text.
#' @param ... Old argument names, translated with a warning: `value` (now
#'   `columns`), `digits` (now `format`), and `group` with `summaries`, the
#'   aggregation the table block no longer does.
#' @seealso [new_table_block()], [static_exhibit()], [report_call()]
#' @examples
#' table_exhibit(
#'   data.frame(term = "A", times_more = 3.81427, ci_low = 2.70123),
#'   format = 2L, title = "Area effect"
#' )
#' @export
table_exhibit <- function(data, rowname = NULL, columns = NULL,
                          shadings = list(), format = "number", unit = NULL,
                          title = NULL, subtitle = NULL, caption = NULL,
                          ...) {
  data <- as_annotated_df(data)
  auto <- input_display_attrs(data)
  if (...length()) {
    old <- legacy_table_exhibit(data, rowname, columns, format, list(...))
    data <- old$data
    rowname <- old$rowname
    columns <- old$columns
    format <- old$format
  }
  out <- dt_exhibit_frame(data, rowname, columns, shadings)
  # A structured frame shows its cells as they come, on screen too.
  if (!table_is_structured(data)) {
    # The unit first: it heads the columns that are numeric before
    # formatting turns them into text.
    out <- dt_format_digits(dt_unit_labels(out, unit), format)
  }
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

  # The committed block's state, as a saved board holds it. Old argument
  # names are already translated there.
  values <- blockr.core::blockr_ser(x)$payload
  state <- function(nm) values[[nm]]

  # Print-relevant surface only: how the table is built and what it says.
  # Drill, search, the identity row colour and the ctrl transports are not
  # part of a printed table. Settings at the spec's default are left out.
  groups <- identical(state("layout"), "groups")
  keys <- if (groups) {
    c("by", "summaries", "id", "facet_layout", "sort_by", "sort_dir",
      "top_n", "column_axis", "value_labels", "percent_of", "missing",
      "format", "unit", "look", "bar_width")
  } else {
    c("rowname", "columns", "shadings", "format", "unit")
  }
  defaults <- lapply(viz_block_spec("table")$args, spec_default)

  args <- list()

  for (nm in keys) {
    v <- state(nm)
    if (is.null(v) || !length(v) ||
          (is.character(v) && !any(nzchar(v)))) next
    if (identical_default(v, defaults[[nm]])) next
    args[[nm]] <- v
  }

  for (nm in c("title", "subtitle", "caption")) {
    v <- state(nm)
    if (!is.null(v)) args[[nm]] <- as.character(v)[1L]
  }

  if (groups) {
    # The Big N counts the subject column the board's `study_roles` option
    # names, as on screen and in the downloads. Read from the session the
    # document is built in; outside one there is none.
    subject <- shiny::isolate(ctrl_subject())
    if (!is.null(subject)) args$subject <- subject
    return(as.call(c(
      list(call("::", as.name("blockr.viz"),
                as.name("static_summarize_table")),
           as.name(var)),
      args
    )))
  }

  exhibit <- as.call(c(
    list(call("::", as.name("blockr.viz"), as.name("table_exhibit")),
         as.name(var)),
    args
  ))

  as.call(list(call("::", as.name("blockr.viz"), as.name("static_exhibit")),
               exhibit))
}
