# What the cross-argument rules (chart_normalize(), table_normalize()) know
# about the data: one kind per column. The rules are pure functions of a
# block's state, the state before the change, and these kinds.

#' The kind of each column of a data frame
#'
#' @param data A data frame, or `NULL`.
#' @return A named character vector, one of "numeric", "date", "factor",
#'   "logical" or "character" per column; `NULL` without data.
#' @noRd
viz_column_kinds <- function(data) {
  if (!is.data.frame(data)) {
    return(NULL)
  }
  vapply(data, function(v) {
    if (inherits(v, c("Date", "POSIXt"))) {
      "date"
    } else if (is.factor(v)) {
      "factor"
    } else if (is.logical(v)) {
      "logical"
    } else if (is.numeric(v)) {
      "numeric"
    } else {
      "character"
    }
  }, character(1L))
}

# TRUE, FALSE, or NA when the column or the data is not known.
kind_is_numeric <- function(kinds, col) {
  if (is.null(kinds) || !length(col) || !col[[1L]] %in% names(kinds)) {
    return(NA)
  }
  identical(unname(kinds[[col[[1L]]]]), "numeric")
}

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
