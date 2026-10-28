#' Restore boards saved by an earlier blockr.viz
#'
#' Old names and values are translated (R/legacy.R), bad values healed with
#' one warning. Blocks that are gone restore as table blocks and save as
#' table blocks from then on: a summarize table in the groups layout, a
#' summary table in the variables layout, a tile in the cards look, and
#' blockr.viz's own heatmap in the groups layout and compact look (another
#' package's heatmap restores through that package's constructor).
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @importFrom blockr.core blockr_deser
#' @export
blockr_deser.chart_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))
  res <- legacy_deser("chart_block", data)
  legacy_build(new_chart_block, "new_chart_block", res$args, res$attrs)
}

#' @rdname blockr_deser.chart_block
#' @export
blockr_deser.summary_table_block <- function(x, data, ...) {
  stopifnot("payload" %in% names(data))
  legacy_new_table(legacy_deser("summary_table_block", data))
}

#' @rdname blockr_deser.chart_block
#' @export
blockr_deser.tile_block <- function(x, data, ...) {
  stopifnot("payload" %in% names(data))
  legacy_new_table(legacy_deser("tile_block", data))
}

#' @rdname blockr_deser.chart_block
#' @export
blockr_deser.heatmap_block <- function(x, data, ...) {
  pkg <- data[["constructor"]][["package"]]
  if (!is.null(pkg) && !identical(pkg, "blockr.viz")) {
    return(NextMethod())
  }
  stopifnot("payload" %in% names(data))
  legacy_new_table(legacy_deser("heatmap_block", data))
}

legacy_new_table <- function(res) {
  legacy_build(new_table_block, "new_table_block", res$args, res$attrs)
}

# A restored block: the constructor with the saved arguments and blockr.core's
# block arguments, then every other saved name (blockr.dock's `visible`) as an
# attribute. In code such a name is an error.
legacy_build <- function(ctor, name, args, attrs) {
  core <- intersect(names(attrs), setdiff(VIZ_CORE_BLOCK_ARGS,
                                          c("ctor", "ctor_pkg")))
  blk <- do.call(ctor, c(args, attrs[core],
                         list(ctor = name, ctor_pkg = "blockr.viz")))
  for (nm in setdiff(names(attrs), VIZ_CORE_BLOCK_ARGS)) {
    attr(blk, nm) <- attrs[[nm]]
  }
  blk
}

#' @rdname blockr_deser.chart_block
#' @export
blockr_deser.table_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))
  legacy_new_table(legacy_deser("table_block", data))
}

#' @rdname blockr_deser.chart_block
#' @export
blockr_deser.summarize_table_block <- function(x, data, ...) {
  stopifnot("payload" %in% names(data))
  saved <- data[["payload"]]
  is_attr <- !names(saved) %in% legacy_arg_names("summarize_table")
  payload <- legacy_table_args(saved[!is_attr], from = "summarize")
  payload$layout <- "groups"
  attrs <- saved[is_attr]
  args <- legacy_restore("table", payload,
                         version = data[["constructor"]][["version"]])
  legacy_build(new_table_block, "new_table_block", args, attrs)
}
