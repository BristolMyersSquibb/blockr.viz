# Helpers of the cross-argument rules (chart_normalize(), table_normalize()),
# pure functions of a block's state and the state before the change.

# Whether `name` differs between two states. Without a previous state
# nothing counts as changed.
changed <- function(state, prev, name) {
  !is.null(prev) && !identical(state[[name]], prev[[name]])
}

# Free-text settings are compared and printed trimmed.
trim_settings <- function(state, names) {
  for (nm in intersect(names, names(state))) {
    v <- state[[nm]]
    if (is.character(v) && length(v) == 1L && !is.na(v)) {
      state[[nm]] <- trimws(v)
    }
  }
  state
}

# A click is held only without a drill target: with one, it is an event.
drop_selection <- function(state, prev, mapped) {
  target <- state[["drill_target"]]
  if (is.character(target) && length(target) && nzchar(target[[1L]])) {
    state["selection"] <- list(NULL)
    return(state)
  }
  if (any(vapply(mapped, changed, logical(1L), state = state, prev = prev))) {
    state["selection"] <- list(NULL)
  }
  state
}
