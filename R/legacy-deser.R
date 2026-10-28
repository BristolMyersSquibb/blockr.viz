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
  legacy_new_table(legacy_deser("tile_block", data))
}

#' Legacy deserialization for the heatmap block
#'
#' The heatmap block of blockr.viz < 0.3.0 is gone: a heatmap is a table
#' block in the groups layout and the compact look, one tile summary counting
#' the rows per `col` value and painted by the worst `color`, one row per
#' `row` value grouped by `group`. A board saved with one restores as that
#' table, with a warning, and saves as a table block from then on. A heatmap
#' another package built (blockr.pharma's AE heatmap) restores through that
#' package's constructor.
#'
#' Drop this method when backwards compatibility is no longer needed.
#'
#' @inheritParams blockr_deser.tile_block
#' @keywords internal
#' @export
blockr_deser.heatmap_block <- function(x, data, ...) {
  pkg <- data[["constructor"]][["package"]]
  if (!is.null(pkg) && !identical(pkg, "blockr.viz")) {
    return(NextMethod())
  }
  stopifnot("payload" %in% names(data))
  legacy_new_table(legacy_deser("heatmap_block", data))
}

# The table block a legacy restore becomes, saved as a table from then on.
legacy_new_table <- function(res) {
  do.call(
    new_table_block,
    c(res$args, res$attrs,
      list(ctor = "new_table_block", ctor_pkg = "blockr.viz"))
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
