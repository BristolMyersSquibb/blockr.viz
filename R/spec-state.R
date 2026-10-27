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

#' The current values of a state, outside a reactive context
#' @noRd
viz_values <- function(state) {
  shiny::isolate(lapply(state, function(rv) rv()))
}

#' Write values into a state
#'
#' A value identical to the current one is not written, so an echo does not
#' invalidate anything that reads the state.
#'
#' @param state A state from viz_state().
#' @param values A named list of checked values.
#' @param revs A revision counter from new_viz_revs(), or `NULL`.
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
    revs$bump(changed)
  }
  invisible(changed)
}

#' A revision counter for a block's state
#'
#' Every change from any source (the gear, the assistant, a ctrl_send) bumps
#' the revision and records it for the names that changed. The payload sends
#' the current revision; the gear's settings message carries the revision it
#' was built against. A name changed after that revision is stale in the
#' message and is not applied.
#'
#' @return A list of functions: `now()`, `bump(names)` and
#'   `stale(names, rev)`.
#' @noRd
new_viz_revs <- function() {
  env <- new.env(parent = emptyenv())
  env$rev <- 0L
  env$changed <- list()
  list(
    now = function() env$rev,
    bump = function(names) {
      env$rev <- env$rev + 1L
      env$changed[names] <- env$rev
      invisible(env$rev)
    },
    stale = function(names, rev) {
      at <- vapply(names, function(nm) env$changed[[nm]] %||% 0L, integer(1L))
      names[at > rev]
    }
  )
}
