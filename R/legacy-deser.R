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
#' Restores boards saved before the tile gained in-block aggregation, and
#' before the drill-filter state took the shared transport names. The
#' grouping column was `by` (now `group`, the dplyr::group_by column shared with
#' the table), and per-measure display overrides rode on `measures` (dropped --
#' multi-measure tiles are now homogeneous). The click-filter state was
#' `filter_col` / `filter_value` (now `filter_column` / `filter_values`, the
#' names the chart and table share; the old scalar value coerces to the
#' plural list shape). Rename `by` -> `group`, drop `measures`, rename the
#' filter fields; current payloads pass straight through.
#'
#' Drop this method when backwards compatibility is no longer needed.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @export
blockr_deser.tile_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))

  payload <- data[["payload"]]
  if ("by" %in% names(payload) && !("group" %in% names(payload))) {
    payload[["group"]] <- payload[["by"]]
  }
  payload[["by"]] <- NULL
  payload[["measures"]] <- NULL
  # Pre-rename filter transport: filter_col / filter_value -> the shared
  # filter_column / filter_values (the old filter_value was scalar; coerce to
  # the plural list shape). Mapping here keeps restores warning-free -- the
  # ctor's own alias path warns, this one must not.
  if (!is.null(payload[["filter_col"]]) &&
        is.null(payload[["filter_column"]])) {
    payload[["filter_column"]] <- payload[["filter_col"]]
  }
  if (!is.null(payload[["filter_value"]]) &&
        is.null(payload[["filter_values"]])) {
    payload[["filter_values"]] <- as.list(payload[["filter_value"]])
  }
  payload[["filter_col"]] <- NULL
  payload[["filter_value"]] <- NULL

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
