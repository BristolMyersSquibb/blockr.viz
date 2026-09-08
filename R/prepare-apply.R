#' Running the chart block's prepare script
#'
#' Three small things sit between the parse layer (R/prepare-parse.R,
#' R/prepare-expr.R) and the block: run the compiled script on the frame the
#' chart draws, splice it into the block's expression behind the click filter,
#' and describe its controls to the browser.
#'
#' @name chart-prepare-apply
#' @keywords internal
NULL


#' Run a prepare script on a data frame
#'
#' The script is compiled the same way it is for the block's expression (values
#' substituted in as literals), then evaluated with `data` bound to the frame.
#' `slot = FALSE`, so the compiled body carries a bare `data` symbol rather than
#' the `.(data)` bquote marker.
#'
#' Failure is reported, never swallowed. A script that throws returns the error
#' message, and the caller ships it to the browser: silently falling back to the
#' untransformed frame would draw a chart of the wrong data and say nothing,
#' which is the worst of the available behaviours.
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
  # The parse check comes FIRST: cb_parse() reports a failure with an empty
  # statement list, so testing for "no statements" ahead of it swallows every
  # syntax error into "there is no script here" and the block silently draws
  # the untransformed data.
  if (is.null(parsed)) {
    return(list(data = data, error = NULL))
  }
  if (!parsed$ok) {
    return(list(data = NULL, error = parsed$error))
  }
  if (!length(parsed$stmts)) {
    return(list(data = data, error = NULL))
  }

  body <- cb_expr(parsed, specs, values, data_name = "data", slot = FALSE)

  # All declarations and no body is a legitimate half-written script, not an
  # error: the knobs are already on the band and the pipeline comes next.
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

  list(data = out, error = NULL)
}


#' Splice an expression into every `.(data)` slot of another
#'
#' The block's expression is the click filter with the script wrapped around
#' it, so the drill filter still applies to the SOURCE rows: a click identifies
#' a mark by a column the upstream data has, and the script may well not keep
#' that column. Composing them means the script's `.(data)` marker is replaced
#' by the whole filter call, which carries the one remaining marker.
#'
#' @param e The outer expression (the compiled script).
#' @param repl The expression to splice in (the filter call).
#' @noRd
dd_splice_slot <- function(e, repl) {

  if (is.call(e) && length(e) == 2L && identical(e[[1L]], quote(`.`)) &&
      identical(e[[2L]], quote(data))) {
    return(repl)
  }
  if (!is.call(e) || length(e) < 2L) {
    return(e)
  }
  for (i in 2L:length(e)) {
    if (cb_is_empty_sym(e[[i]])) {
      next
    }
    sub <- dd_splice_slot(e[[i]], repl)
    if (is.null(sub)) {
      next
    }
    e[[i]] <- sub
  }
  e
}


#' Describe a script's controls for the browser
#'
#' One record per usable control, in the engine's own role vocabulary, so the
#' band renders them with the same `_buildControl()` every mapping row uses.
#' The kinds map straight across bar one: a flag becomes the engine's boolean
#' `segmented`, which is what it already draws as a checkbox.
#'
#' Specs that failed (a mistyped column name in a factor's levels) are carried
#' through with their message rather than dropped, so a knob that did not
#' appear says why instead of being invisible.
#'
#' @param specs Input specs from [cb_specs()].
#' @return A list of records, JSON-ready.
#' @noRd
dd_script_roles <- function(specs) {

  lapply(specs, function(s) {

    label <- gsub("[._]", " ", s$name)
    label <- paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))

    if (!is.null(s$error) || is.na(s$kind)) {
      return(list(name = s$name, key = dd_script_key(s$name),
                  label = if (is.null(s$label)) label else s$label,
                  kind = "error", error = s$error %||% "no control"))
    }

    rec <- list(
      name = s$name,
      key = dd_script_key(s$name),
      label = if (is.null(s$label)) label else s$label,
      kind = switch(s$kind,
        select = if (isTRUE(s$multiple)) "multi" else "select",
        flag = "segmented",
        s$kind
      )
    )

    if (identical(s$kind, "select")) {
      # BARE STRINGS, not {value, label} pairs. A factor's level is its own
      # label, and the select decorates a labelled option as "value (label)" --
      # which reads "setosa (setosa)" on the chip.
      rec$options <- as.list(as.character(s$choices))
    }
    if (identical(s$kind, "flag")) {
      rec$options <- list(list(value = "on", label = rec$label),
                          list(value = "off", label = rec$label))
    }
    for (k in c("min", "max", "step", "placeholder")) {
      if (!is.null(s[[k]])) rec[[k]] <- s[[k]]
    }

    rec
  })
}


#' The config key a script control writes to
#'
#' Script controls ride the ordinary config channel rather than a second one of
#' their own, so they inherit the gear's whole change path (debounce, restore,
#' the band's renderer) for free. The prefix is what keeps a script variable
#' called `color` from writing the chart's colour mapping.
#'
#' @param name The declared variable name.
#' @noRd
dd_script_key <- function(name) {
  paste0("sv_", name)
}


#' Read the script values back out of a config message
#'
#' @param msg The `config` action message.
#' @param specs Input specs from [cb_specs()].
#' @param current The current values list.
#' @return The updated values list.
#' @noRd
dd_script_values <- function(msg, specs, current = list()) {

  for (s in specs) {

    if (!is.null(s$error) || is.na(s$kind)) {
      next
    }

    key <- dd_script_key(s$name)

    if (is.null(msg[[key]])) {
      next
    }

    v <- msg[[key]]

    # The engine speaks "on"/"off" for a boolean; the declaration is a logical.
    if (identical(s$kind, "flag")) {
      current[[s$name]] <- identical(as.character(v), "on")
      next
    }

    # Emptying a multi-select is a real value, not an absent one, and an empty
    # JS array arrives here as NULL -- indistinguishable from "this key was not
    # in the message", so the write would be skipped and the control would
    # refuse to be cleared. The band sends the literal "" for it, exactly as
    # the expose handler does, and it lands as the declared type's empty value.
    if (isTRUE(s$multiple) && identical(as.character(v), "")) {
      current[[s$name]] <- character()
      next
    }

    current[[s$name]] <- cb_coerce(v, s) %||% character()
  }

  current
}


#' Current script values, in the engine's vocabulary
#'
#' The band's controls are ordinary config rows, so they read their value out of
#' the config object like every other row. A control the user has not touched
#' shows the declaration's own value, which is what the script says it is.
#'
#' @param specs Input specs from [cb_specs()].
#' @param values Named list of current control values.
#' @return A named list of `sv_*` config entries.
#' @noRd
dd_script_cfg <- function(specs, values = list()) {

  out <- list()

  for (s in specs) {

    if (!is.null(s$error) || is.na(s$kind)) {
      next
    }

    v <- values[[s$name]]
    if (is.null(v)) {
      v <- s$default
    }

    out[[dd_script_key(s$name)]] <- switch(
      s$kind,
      flag = if (isTRUE(as.logical(v))) "on" else "off",
      # A multi-select's value is an array on both sides; as.list() keeps a
      # single pick a JSON array rather than auto_unbox'ing it to a string.
      select = if (isTRUE(s$multiple)) as.list(as.character(v)) else
        as.character(v)[[1L]],
      date = format(as.Date(v)),
      number = as.numeric(v)[[1L]],
      as.character(v)[[1L]]
    )
  }

  out
}
