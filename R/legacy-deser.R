#' Legacy deserialization for old block formats
#'
#' Restores boards saved before [new_summary_table_block()] was flattened.
#'
#' The block used to take a single opaque `state = list(...)` constructor
#' argument and serialize it under one `state` payload key. It now takes flat
#' top-level arguments (`vars`, `sections`, `by`, ...) and serializes them as
#' flat sibling payload entries. This deserializer unwraps the interim
#' single-`state` blob back to flat constructor arguments; current (flat)
#' payloads pass straight through.
#'
#' Block attributes (`block_name`, ...) ride along as sibling payload entries
#' next to the state fields, so unwrapping only the `state` key preserves them.
#'
#' Drop this file when backwards compatibility is no longer needed.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @importFrom blockr.core blockr_deser
#' @export
blockr_deser.summary_table_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))

  payload <- data[["payload"]]
  if ("state" %in% names(payload)) {
    # Interim single-blob format: lift the state fields up next to the block
    # attributes (which are stored as siblings of `state`).
    extras <- payload[setdiff(names(payload), "state")]
    payload <- c(payload[["state"]], extras)
  }

  ctor <- blockr.core::blockr_deser(data[["constructor"]])
  args <- c(
    payload,
    list(
      ctor = blockr.core::coal(
        blockr.core::ctor_name(ctor),
        blockr.core::ctor_fun(ctor)
      ),
      ctor_pkg = blockr.core::ctor_pkg(ctor)
    )
  )

  do.call(blockr.core::ctor_fun(ctor), args)
}

#' Legacy deserialization for the tile block
#'
#' The tile block is gone: a tile is a table block in the cards look
#' (`new_table_block(look = "cards")`). A board saved with a tile restores as
#' a table block. A tile that aggregated in place (`summaries`, with or
#' without a `group`) becomes the groups layout in the cards look, one card
#' per summary and one row of cards per group (one row without a group); its
#' drill and click state carry over. Any other tile read a precomputed frame:
#' a wide one (several `value` columns, no `name`) becomes the records layout
#' in the cards look, one card per column; the long tile frame (`name` /
#' `value` / `secondary`) becomes a plain records table. Settings the table
#' has no counterpart for (`format`, `unit`, `secondary`, `style`, ...) are
#' dropped with a warning. The board saves as a table block from then on.
#'
#' Drop this method when backwards compatibility is no longer needed.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @export
blockr_deser.tile_block <- function(x, data, ...) {
  stopifnot("payload" %in% names(data))
  p <- data[["payload"]]
  # The tile's own arguments; whatever else rides on the payload is a block
  # attribute (block_name, ...) and passes through.
  own <- c("value", "group", "by", "name", "summaries", "color", "layout",
           "overline", "caption", "secondary", "style", "good_when",
           "format", "unit", "drill", "filter_column", "filter_values",
           "filter_col", "filter_value", "ctrl_target", "ctrl_table",
           "measures", "showcase", "state")
  attrs <- p[setdiff(names(p), own)]
  value <- chr_vec_state(p[["value"]]) %||% character()
  group <- chr_vec_state(p[["group"]] %||% p[["by"]]) %||% character()
  group <- group[nzchar(group)]
  name <- chr_state(p[["name"]])
  summaries <- p[["summaries"]]
  filter_column <- chr_vec_state(p[["filter_column"]] %||% p[["filter_col"]])
  filter_values <- null_state(p[["filter_values"]] %||% p[["filter_value"]])
  drill_target <- chr_state(p[["ctrl_target"]]) %||% ""
  drill_table <- chr_state(p[["ctrl_table"]]) %||% ""
  label <- chr_state(attrs[["block_name"]]) %||% "a tile"

  dropped <- function(nm) {
    set <- vapply(nm, function(k) {
      v <- p[[k]]
      !is.null(v) && length(v) && !identical(v, "") &&
        !identical(v, "plain") && !identical(v, "number") &&
        !identical(v, "up") && !identical(v, "cards")
    }, logical(1L))
    nm[set]
  }

  if (table_is_agg_summaries(summaries)) {
    args <- list(
      layout = "groups", by = group, look = "cards",
      summaries = table_agg_to_summaries(summaries),
      drill = if (isTRUE(p[["drill"]]) && length(group)) group[[1L]],
      drill_target = drill_target, drill_table = drill_table,
      selection = if (length(filter_column) && length(filter_values)) {
        list(column = filter_column, values = filter_values)
      }
    )
    lost <- dropped(c("format", "unit", "secondary", "caption", "overline"))
  } else if (is.null(name) && length(value)) {
    args <- list(layout = "records", look = "cards", columns = value,
                 rowname = if (length(group)) group[[1L]])
    lost <- dropped(c("format", "unit", "secondary", "style", "caption",
                      "overline", "drill"))
  } else {
    args <- list(
      layout = "records", rowname = name %||% (if (length(group)) group[[1L]]),
      columns = unique(c(group, value, chr_state(p[["secondary"]])))
    )
    lost <- c("the long tile frame", dropped(c("format", "unit", "style",
                                               "caption", "overline",
                                               "drill")))
  }
  if (length(lost)) {
    warning(
      "The tile block is gone: ", label, " restores as a table block. ",
      "Not carried over: ", paste(lost, collapse = ", "), ".",
      call. = FALSE
    )
  }
  do.call(
    new_table_block,
    c(args, attrs, list(ctor = "new_table_block", ctor_pkg = "blockr.viz"))
  )
}

#' Legacy deserialization for the chart block
#'
#' Restores boards saved with blockr.viz < 0.3.0, whose chart state carries
#' the old argument names (`chart_type`, `group`/`value`, `bar_mode`,
#' `tt_fields`, `lo`/`hi`, the `filter_*` fields, ...). The payload is
#' translated by `chart_legacy_args()` (R/chart-args.R), the same table the
#' constructor uses for old names in code, so a saved chart opens with the
#' same picture. Current payloads pass straight through.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @export
blockr_deser.chart_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))

  payload <- chart_legacy_args(data[["payload"]])

  ctor <- blockr.core::blockr_deser(data[["constructor"]])
  args <- c(
    payload,
    list(
      ctor = blockr.core::coal(
        blockr.core::ctor_name(ctor),
        blockr.core::ctor_fun(ctor)
      ),
      ctor_pkg = blockr.core::ctor_pkg(ctor)
    )
  )

  do.call(blockr.core::ctor_fun(ctor), args)
}
