# A block's reactive state, one reactiveVal per spec argument, and the
# revision counter that tells a stale settings message from a fresh one.

#' Reactive state from checked values
#'
#' @param values A named list, as viz_check() or viz_heal() returns it.
#' @return A named list of reactiveVals, named like the constructor's
#'   formals, which is what blockr.core saves.
#' @noRd
viz_state <- function(values) {
  lapply(values, shiny::reactiveVal)
}

#' The current values of a state
#'
#' Read reactively: a reactive context that calls it depends on every value.
#' Wrap it in shiny::isolate() to read without depending.
#' @noRd
viz_values <- function(state) {
  lapply(state, function(rv) rv())
}

#' Write values into a state
#'
#' A value identical to the current one is not written, so an echo does not
#' invalidate anything that reads the state.
#'
#' @param state A state from viz_state().
#' @param values A named list of checked values.
#' @param revs A revision counter from new_viz_revs(), or `NULL`. The values
#'   are recorded as this client's own writes, which do not make a later
#'   message from the same client stale.
#' @return The names that changed, invisibly.
#' @noRd
viz_apply <- function(state, values, revs = NULL) {
  changed <- character()
  for (nm in intersect(names(values), names(state))) {
    v <- values[[nm]]
    if (!identical(shiny::isolate(state[[nm]]()), v)) {
      state[[nm]](v)
      changed <- c(changed, nm)
    }
  }
  if (!is.null(revs) && length(changed)) {
    revs$seen(values[changed])
  }
  invisible(changed)
}

#' A revision counter for a block's state
#'
#' The payload sends the current revision; the gear's settings message
#' carries the revision it was built against. A name that changed from
#' another source (the assistant, a ctrl_send, core's revert of a frozen
#' block) after that revision is stale in the message and is not applied.
#' The client's own writes are recorded as known values and bump nothing,
#' so a second quick edit of one setting is not dropped.
#'
#' @param values The state's values when the block starts.
#' @return A list of functions: `now()`, `seen(values)` (record known
#'   values), `track(values)` (bump the names whose value differs from the
#'   known one, return them) and `stale(names, rev)`.
#' @noRd
new_viz_revs <- function(values = list()) {
  env <- new.env(parent = emptyenv())
  env$rev <- 0L
  env$changed <- list()
  env$known <- values
  list(
    now = function() env$rev,
    seen = function(values) {
      env$known[names(values)] <- values
      invisible(NULL)
    },
    track = function(values) {
      diff <- names(values)[!vapply(names(values), function(nm) {
        nm %in% names(env$known) && identical(env$known[[nm]], values[[nm]])
      }, logical(1L))]
      if (length(diff)) {
        env$rev <- env$rev + 1L
        env$changed[diff] <- env$rev
        env$known[diff] <- values[diff]
      }
      diff
    },
    stale = function(names, rev) {
      at <- vapply(names, function(nm) env$changed[[nm]] %||% 0L, integer(1L))
      names[at > rev]
    }
  )
}

#' The arguments a block may hold empty
#'
#' blockr.core holds a block's result back while a state field is empty,
#' unless the block lists it: every argument that takes NULL, or whose
#' default is empty ("", a zero-length vector or list).
#' @noRd
viz_empty_ok <- function(spec) {
  ok <- vapply(spec$args, function(a) {
    d <- a$default
    !isFALSE(a$null) || !length(d) ||
      (is.character(d) && length(d) == 1L && !nzchar(d))
  }, logical(1L))
  names(spec$args)[ok]
}

#' The arguments other blocks and the assistant may set
#' @noRd
viz_ctrl_args <- function(spec) {
  names(spec$args)[vapply(spec$args, function(a) isTRUE(a$ctrl), logical(1L))]
}
