#' Defunct: filter block moved to blockr.dm
#'
#' `new_bi_filter_block()` and `migrate_bi_filter_state()` were moved to
#' `blockr.dm` and renamed [blockr.dm::new_value_filter_block()] /
#' `blockr.dm::migrate_value_filter_state()`. The value filter has a `dm`
#' path (FK-cascading via `dm::dm_filter()`), so it belongs in the package
#' that owns the `dm` dependency. It remains fully usable on any dashboard
#' board by composing the `blockr.dm` block.
#'
#' These stubs intentionally do not forward, to avoid a
#' `blockr.bi -> blockr.dm` back-dependency (which would re-introduce the
#' heavy `dm`/`igraph` dependency into this package).
#'
#' @param ... Ignored.
#' @name bi_filter-defunct
#' @keywords internal
#' @export
new_bi_filter_block <- function(...) {
  .Defunct(
    new = "blockr.dm::new_value_filter_block",
    package = "blockr.bi",
    msg = paste(
      "new_bi_filter_block() moved to blockr.dm and was renamed.",
      "Use blockr.dm::new_value_filter_block() instead."
    )
  )
}

#' @rdname bi_filter-defunct
#' @export
migrate_bi_filter_state <- function(...) {
  .Defunct(
    new = "blockr.dm::migrate_value_filter_state",
    package = "blockr.bi",
    msg = paste(
      "migrate_bi_filter_state() moved to blockr.dm and was renamed.",
      "Use blockr.dm::migrate_value_filter_state() instead."
    )
  )
}
