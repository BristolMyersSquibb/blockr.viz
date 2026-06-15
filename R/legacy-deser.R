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
