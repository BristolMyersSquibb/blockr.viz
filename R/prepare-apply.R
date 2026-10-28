#' Running a block's prepare script
#'
#' Between the parse layer (R/prepare-parse.R, R/prepare-expr.R) and the
#' block's reactives (viz_script_sources(), R/kit-script.R): run the compiled
#' script on the frame the block draws, and splice it into the block's
#' expression. Its controls as the browser and the titles see them are in
#' R/prepare-controls.R.
#'
#' @name prepare-apply
#' @keywords internal
NULL


#' Run a prepare script on a data frame
#'
#' The script is compiled as for the block's expression (values substituted in
#' as literals, with `slot = FALSE` so the body carries a bare `data` symbol),
#' then evaluated with `data` bound to the frame.
#'
#' A script that throws returns the error message, which the caller ships to
#' the browser. Falling back to the untransformed frame would draw the wrong
#' data without a word.
#'
#' @param data A data frame (or `NULL` before the upstream arrives).
#' @param parsed The result of [cb_parse()].
#' @param specs Input specs from [cb_specs()].
#' @param values Named list of current control values.
#' @return A list with `data` (the prepared frame, or `NULL` on failure) and
#'   `error` (`NULL`, or the message).
#' @noRd
dd_prepare_run <- function(data, parsed, specs, values = list()) {

  if (is.null(data)) {
    return(list(data = NULL, error = NULL))
  }
  # Check the parse first: cb_parse() reports a failure with an empty
  # statement list.
  if (is.null(parsed)) {
    return(list(data = data, error = NULL))
  }
  if (!parsed$ok) {
    return(list(data = NULL, error = parsed$error))
  }
  if (!length(parsed$stmts)) {
    return(list(data = data, error = NULL))
  }

  body <- cb_expr(parsed, specs, values, slot = FALSE)

  # All declarations and no body is a half-written script, not an error.
  if (is.null(body)) {
    return(list(data = data, error = NULL))
  }

  out <- tryCatch(
    eval(body, blockr.core::eval_env(list(data = data))),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    return(list(data = NULL, error = conditionMessage(out)))
  }
  if (!is.data.frame(out)) {
    return(list(
      data = NULL,
      error = paste0("the script returned ", paste(class(out), collapse = "/"),
                     ", not a data frame")
    ))
  }

  # A script that aggregates or regroups returns a frame without the input's
  # filter trail and provenance, which the caption's `{filters}`, the footer's
  # `{dataset}` and the download names read from the prepared frame. They
  # still describe it, so they are carried across unless the script set its
  # own.
  for (a in c("blockr_filters", "blockr_provenance")) {
    if (is.null(attr(out, a, exact = TRUE))) {
      attr(out, a) <- attr(data, a, exact = TRUE)
    }
  }

  # Base `[` on a data.frame strips column attributes, so a script that
  # filters with `data[keep, , drop = FALSE]` loses the labels. A column that
  # kept its name gets its label back unless the script set one.
  for (nm in intersect(names(out), names(data))) {
    lbl <- attr(data[[nm]], "label", exact = TRUE)
    if (!is.null(lbl) && is.null(attr(out[[nm]], "label", exact = TRUE))) {
      attr(out[[nm]], "label") <- lbl
    }
  }

  list(data = out, error = NULL)
}


#' Which click-filter conditions run after the prepare script
#'
#' A click filter wraps into the script's `.(data)` slot, so it filters the
#' source rows. A condition on a column only the script makes cannot run there
#' and goes after the script.
#'
#' @param cols A list, per condition the column names it reads.
#' @param src_cols The upstream's column names; NULL (not seen yet) keeps
#'   every condition before the script.
#' @return A logical vector, TRUE for a condition to filter after the script.
#' @noRd
dd_drill_post <- function(cols, src_cols = NULL) {
  if (is.null(src_cols)) {
    return(rep(FALSE, length(cols)))
  }
  vapply(cols, function(cc) !all(cc %in% src_cols), logical(1L))
}


#' Splice an expression into every `.(data)` slot of another
#'
#' A block's expression is built from pieces that each read `.(data)`: the
#' compiled prepare script, the click filter, the code that builds a table.
#' Nesting one into another replaces the outer piece's marker with the inner
#' piece, which carries the one remaining marker.
#'
#' The chart puts the click filter inside the script, so it filters the
#' source rows: a click names a mark by a column the upstream data has,
#' which the script may not keep. A click on a column the script makes is
#' filtered after the script (dd_drill_post()). The table puts the script
#' inside its filter, and the variables layout its build call.
#'
#' @param e The outer expression.
#' @param repl The expression to splice in.
#' @noRd
dd_splice_slot <- function(e, repl) {

  if (is.call(e) && length(e) == 2L && identical(e[[1L]], quote(`.`)) &&
      identical(e[[2L]], quote(data))) {
    return(repl)
  }
  if (!is.call(e)) {
    return(e)
  }
  cb_map_args(e, function(a) dd_splice_slot(a, repl))
}
