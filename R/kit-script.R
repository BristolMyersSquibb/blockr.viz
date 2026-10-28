# The prepare script every block runs on its input before it draws: the
# `script` argument, with its declared controls in `script_values`. The parse
# layer is R/prepare-parse.R and R/prepare-expr.R; running it and describing
# its controls is R/prepare-apply.R.

#' The prepare script's reactives
#'
#' One set per block, built by its setup() and handed back to the kit as
#' `script`, which puts `derived()` into the payload. The block decides what
#' the script reads (`raw`): the chart drops a finished table's annotation
#' columns, the table keeps them.
#'
#' Without a script nothing reads the data to declare controls or run, and
#' the prepared frame is `raw()` itself. The payload's `script` entry then
#' never reads the input, so a block without input still sends its empty
#' state.
#'
#' @param raw A reactive: the data frame the script reads, or NULL.
#' @param state The block's state; reads `script` and `script_values`.
#' @return A list:
#'   - `specs()`: the controls the script declares (cb_specs()). They keep
#'     their last value while the data is gone, or a hidden panel would flip
#'     the compiled script and the block's expression;
#'   - `run()`: `list(data, error)` from dd_prepare_run(); both NULL
#'     without a script;
#'   - `prepared()`: the frame the block draws. A failed script gives an
#'     empty frame with the input's columns, so the payload still goes out
#'     and carries the error;
#'   - `compiled()`: the script as code with a `.(data)` slot for the block's
#'     expression (dd_splice_slot()), or NULL;
#'   - `derived()`: the payload's `script` entry, the controls and values in
#'     the gear's vocabulary and the error.
#' @noRd
viz_script_sources <- function(raw, state) {
  has_script <- function() {
    s <- state$script()
    !is.null(s) && nzchar(trimws(paste(s, collapse = "\n")))
  }
  parsed <- shiny::reactive(cb_parse(state$script()))
  last <- new.env(parent = emptyenv())
  last$specs <- list()
  specs <- shiny::reactive({
    p <- parsed()
    d <- if (has_script()) tryCatch(raw(), error = function(e) NULL)
    if (is.null(d) && length(last$specs)) {
      return(last$specs)
    }
    out <- cb_specs(p, d)
    if (!identical(out, last$specs)) last$specs <- out
    last$specs
  })
  run <- shiny::reactive({
    if (!has_script()) {
      return(list(data = NULL, error = NULL))
    }
    dd_prepare_run(raw(), parsed(), specs(), state$script_values())
  })
  prepared <- shiny::reactive({
    if (!has_script()) {
      return(raw())
    }
    r <- run()
    if (is.null(r$error)) {
      return(r$data)
    }
    d <- tryCatch(raw(), error = function(e) NULL)
    if (is.data.frame(d)) d[0, , drop = FALSE]
  })
  list(
    specs = specs,
    run = run,
    prepared = prepared,
    compiled = function() {
      if (!has_script()) {
        return(NULL)
      }
      cb_expr(parsed(), specs(), state$script_values(), slot = TRUE)
    },
    derived = function() {
      list(inputs = dd_script_roles(specs()),
           values = dd_script_cfg(specs(), state$script_values()),
           error = run()$error)
    }
  )
}

#' Take the script controls' values from the browser
#'
#' `script_values` is a map of plain values, so the gear may set it, but each
#' value is checked against the controls the script declares now: a name the
#' script does not declare is dropped, and so is a value its control could
#' not have sent (a number outside its range, a choice it does not offer).
#' A dropped value keeps the current one. The script text itself is code and
#' stays behind the `blockr.viz.browser_script` option (viz_gate_set()).
#'
#' @param incoming The message's `script_values`, a named list.
#' @param specs The script's current controls (cb_specs()).
#' @param current The block's current `script_values`.
#' @return The values to set: `current` with the accepted values replaced.
#' @noRd
viz_script_values <- function(incoming, specs, current = list()) {
  out <- as.list(current %||% list())
  if (!is.list(incoming) || !length(incoming)) {
    return(out)
  }
  for (s in specs) {
    if (!is.null(s$error) || is.na(s$kind) || !s$name %in% names(incoming)) {
      next
    }
    v <- script_value_read(incoming[[s$name]], s)
    if (!is.null(v)) out[s$name] <- list(v)
  }
  out
}

# One control's value as the script reads it, or NULL when its control could
# not have sent it.
script_value_read <- function(x, s) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  switch(
    s$kind,
    number = {
      if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
        return(NULL)
      }
      lo <- s$min %||% -Inf
      hi <- s$max %||% Inf
      if (x < lo || x > hi) NULL else as.numeric(x)
    },
    flag = {
      if (is.character(x) && length(x) == 1L && x %in% c("on", "off")) {
        return(identical(x, "on"))
      }
      if (is.logical(x) && length(x) == 1L && !is.na(x)) x
    },
    select = {
      if (is.null(x)) x <- character()
      if (!is.atomic(x) || (!isTRUE(s$multiple) && length(x) != 1L)) {
        return(NULL)
      }
      if (!all(as.character(x) %in% as.character(s$choices))) {
        return(NULL)
      }
      cb_coerce(as.character(x), s) %||% character()
    },
    date = {
      if (!is.character(x) || length(x) != 1L) {
        return(NULL)
      }
      d <- tryCatch(as.Date(x), error = function(e) NA)
      if (is.na(d)) NULL else d
    },
    text = if (is.character(x) && length(x) == 1L) x,
    NULL
  )
}
