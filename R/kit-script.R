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
