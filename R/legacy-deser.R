# LEGACY: delete with this file.

#' Legacy deserialization for old block formats
#'
#' Restores boards saved before [new_summary_table_block()] took flat
#' arguments. Those boards carry one `state` payload key with the constructor
#' arguments inside; this method lifts them up next to the block attributes
#' (`block_name`, ...). Flat payloads pass straight through.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @importFrom blockr.core blockr_deser
#' @export
blockr_deser.summary_table_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))

  payload <- data[["payload"]]
  if ("state" %in% names(payload)) {
    # Lift the state fields up next to the block attributes.
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
#' A board saved with a tile block restores as a table block. A tile that
#' aggregated in place (`summaries`, with or without a `group`) becomes the
#' groups layout in the cards look, one card per summary and one row of cards
#' per group; its drill and click state carry over. Any other tile read a
#' precomputed frame: a wide one (several `value` columns, no `name`) becomes
#' the records layout in the cards look, one card per column; the long frame
#' (`name`, `value`, `secondary`) becomes a plain records table. Settings the
#' table has no counterpart for (`format`, `unit`, `secondary`, `style`, ...)
#' are dropped with one warning.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @export
blockr_deser.tile_block <- function(x, data, ...) {
  stopifnot("payload" %in% names(data))
  p <- data[["payload"]]
  # Anything else in the payload is a block attribute (block_name, ...).
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

#' Restore a saved chart block
#'
#' Boards saved with any earlier blockr.viz restore through the legacy tables
#' (R/legacy.R): old names and values are translated, bad values are healed
#' with one warning.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @export
blockr_deser.chart_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))
  res <- legacy_deser("chart_block", data)
  do.call(
    new_chart_block,
    c(res$args, res$attrs,
      list(ctor = "new_chart_block", ctor_pkg = "blockr.viz"))
  )
}
