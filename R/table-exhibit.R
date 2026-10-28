# The table block as an exhibit: the frame its downloads write ---------------

#' The table block's download frame.
#'
#' The frame the screen draws (dt_flat_build()): the rowname first, the value
#' columns after it, the colour sources dropped. With a `group` or `summaries`
#' it is the aggregate, keyed on the first group column, or a single "Total"
#' row for grand totals (see table_aggregate()). Each shaded column is
#' followed by its paint as `.bg:<col>` / `.fg:<col>` (see
#' annotation_cell_paint()). Numbers keep their full precision; the writers
#' format them. In-cell bars are not carried. A structured ("Table 1") frame
#' is returned unchanged.
#' @noRd
dt_exhibit_frame <- function(data, rowname = NULL, value = NULL,
                             group = character(), summaries = list(),
                             shadings = list()) {
  if (!is.data.frame(data) || !ncol(data) || table_is_structured(data)) {
    return(data)
  }
  agg <- table_aggregate(data, group, summaries)
  d <- data
  label_col <- rowname
  value_cols <- value
  if (isTRUE(agg$aggregated)) {
    d <- agg$data
    # Grand totals: a "Total" stub, so there is a row label and a metric.
    if (!length(agg$group)) {
      d <- cbind(
        stats::setNames(data.frame("Total", stringsAsFactors = FALSE,
                                   check.names = FALSE), " "),
        d
      )
    }
    label_col <- if (length(agg$group)) agg$group[1L] else " "
    value_cols <- if (length(agg$group)) {
      c(setdiff(agg$group, agg$group[1L]), agg$metric_cols)
    } else {
      agg$metric_cols
    }
  }
  if (is.null(label_col) || !label_col %in% names(d)) label_col <- names(d)[1L]
  value_cols <- intersect(value_cols %||% character(), names(d))
  # A pick that vanished upstream shows a message on screen; a download falls
  # back to every column.
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
#' Every numeric data column becomes text through dt_format_num(); NA stays NA.
#' The stub (`.label`, else the first column) and the dot-prefixed structure
#' columns are left alone. Column attributes survive. `digits` is the table's
#' number format (table_format_state()); `NULL`, or anything that is not a
#' data frame, comes back unchanged.
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
#' [static_exhibit()] to typeset. The rowname comes first, then the value
#' columns (or the aggregate, with `group` / `summaries`). Numbers are printed
#' in the block's `format` like the screen, and `unit` heads the numeric
#' columns. Cells shaded by a diverging or sequential rule carry their colours
#' as `.bg:` / `.fg:` columns, which [static_table()] and the HTML table
#' paint. Colour source columns are left out and in-cell bars are not drawn.
#'
#' The block's [report_call()] emits
#' `blockr.viz::static_exhibit(blockr.viz::table_exhibit(<var>, ...))` with
#' its own settings.
#'
#' @param data The table block's input: a data frame or an
#'   [as_annotated_df()]-coercible table object.
#' @param rowname,value,group,summaries,shadings,format,unit As in
#'   [new_table_block()].
#' @param title,subtitle,caption Table text: `NULL` takes the input's
#'   `label` / `subtitle` / `caption` attribute, `""` shows none, any other
#'   string is a title template resolved against `data`.
#'
#' @return An annotated data frame, with `label`, `subtitle` and `caption`
#'   attributes carrying the resolved text.
#' @param digits Old name of `format` (a number of decimals).
#' @seealso [new_table_block()], [static_exhibit()], [report_call()]
#' @examples
#' table_exhibit(
#'   data.frame(term = "A", times_more = 3.81427, ci_low = 2.70123),
#'   format = 2L, title = "Area effect"
#' )
#' @export
table_exhibit <- function(data, rowname = NULL, value = NULL,
                          group = character(), summaries = list(),
                          shadings = list(), format = "number", unit = NULL,
                          title = NULL, subtitle = NULL, caption = NULL,
                          digits = NULL) {
  if (!is.null(digits) && missing(format)) {
    format <- table_digits_format(digits)
  }
  data <- as_annotated_df(data)
  auto <- input_display_attrs(data)
  out <- dt_exhibit_frame(data, rowname, value,
                          as.character(group %||% character()),
                          table_parse_summaries(summaries), shadings)
  # A structured frame shows its cells as they come, on screen too. The unit
  # goes first: it heads the columns that are numeric before formatting.
  if (!table_is_structured(data)) {
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
  # The committed block's state lives in its constructor closure.
  env <- environment(x[["expr_server"]])
  state <- function(nm) {
    v <- get0(nm, envir = env, ifnotfound = NULL)
    if (is.function(v)) NULL else v
  }

  # Print-relevant surface only: how the table is built and what it says.
  # Drill, search, sort and the ctrl transports are not part of a printed table.
  groups <- identical(state("layout"), "groups")
  spec <- if (groups) {
    list(by = NULL, summaries = NULL, id = NULL,
         facet_layout = "by_summary", sort_by = "value", sort_dir = "desc",
         top_n = NULL, axis = TRUE, value_labels = TRUE,
         pct_of = "facet", missing = "drop", format = NULL, unit = NULL)
  } else {
    list(rowname = NULL, columns = NULL, shadings = NULL, format = "number",
         unit = NULL)
  }
  # The static builders keep the engines' names.
  rename <- c(columns = "value", id = "id_var")

  args <- list()
  for (nm in names(spec)) {
    v <- state(nm)
    if (is.null(v) || !length(v) ||
          (is.character(v) && !any(nzchar(v)))) next
    if (identical_default(v, spec[[nm]])) next
    args[[if (nm %in% names(rename)) rename[[nm]] else nm]] <- v
  }
  for (nm in c("title", "subtitle", "caption")) {
    v <- state(nm)
    if (!is.null(v)) args[[nm]] <- as.character(v)[1L]
  }

  blockr_call <- function(fn, ...) {
    as.call(c(list(call("::", as.name("blockr.viz"), as.name(fn))), ...))
  }
  if (groups) {
    # The Big N counts the subject column the board's `study_roles` option
    # names; outside a session there is none.
    subject <- shiny::isolate(dd_ctrl_subject())
    if (!is.null(subject)) args$subject <- subject
    return(blockr_call("static_summarize_table", list(as.name(var)), args))
  }
  blockr_call("static_exhibit",
              list(blockr_call("table_exhibit", list(as.name(var)), args)))
}
