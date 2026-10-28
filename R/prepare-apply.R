#' Running the chart block's prepare script
#'
#' Between the parse layer (R/prepare-parse.R, R/prepare-expr.R) and the block:
#' run the compiled script on the frame the chart draws, splice it into the
#' block's expression behind the click filter, and describe its controls to
#' the browser.
#'
#' @name chart-prepare-apply
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

  body <- cb_expr(parsed, specs, values, data_name = "data", slot = FALSE)

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
#' The block's expression is the click filter with the script wrapped around
#' it, so the drill filter applies to the source rows: a click names a mark by
#' a column the upstream data has, which the script may not keep. The script's
#' `.(data)` marker is replaced by the whole filter call, which carries the one
#' remaining marker. A click on a column the script makes is filtered after
#' the script (dd_drill_post()).
#'
#' @param e The outer expression (the compiled script).
#' @param repl The expression to splice in (the filter call).
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


#' Describe a script's controls for the browser
#'
#' One record per control, in the engine's role vocabulary, so the band renders
#' them with the `_buildControl()` every mapping row uses. A flag becomes the
#' engine's boolean `segmented`; the other kinds map across unchanged.
#'
#' Specs that failed (a mistyped column name in a factor's levels) are carried
#' through with their message, so a missing control says why.
#'
#' @param specs Input specs from [cb_specs()].
#' @return A list of records, JSON-ready.
#' @noRd
dd_script_roles <- function(specs) {

  lapply(specs, function(s) {

    label <- gsub("[._]", " ", s$name)
    label <- paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))

    label <- s$label %||% label

    if (!is.null(s$error) || is.na(s$kind)) {
      return(list(name = s$name, key = dd_script_key(s$name), label = label,
                  kind = "error", error = s$error %||% "no control"))
    }

    rec <- list(
      name = s$name,
      key = dd_script_key(s$name),
      label = label,
      kind = switch(s$kind,
        select = if (isTRUE(s$multiple)) "multi" else "select",
        flag = "segmented",
        s$kind
      )
    )

    if (identical(s$kind, "select")) {
      # Bare strings, not {value, label} pairs: the select would show a
      # labelled option as "setosa (setosa)".
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
#' Script controls use the ordinary config channel, so they share the gear's
#' change path (debounce, restore, the band's renderer). The prefix keeps a
#' script variable called `color` from writing the chart's colour mapping.
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

    # The engine sends "on" or "off" for a boolean.
    if (identical(s$kind, "flag")) {
      current[[s$name]] <- identical(as.character(v), "on")
      next
    }

    # An emptied multi-select is a value, but an empty JS array arrives as
    # NULL, which reads as an absent key. The band sends "" for it instead.
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
#' The band's controls are config rows and read their value from the config
#' object. An untouched control shows the declaration's value.
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
      # as.list() keeps a single pick a JSON array instead of a string.
      select = if (isTRUE(s$multiple)) as.list(as.character(v)) else
        as.character(v)[[1L]],
      date = format(as.Date(v)),
      number = as.numeric(v)[[1L]],
      as.character(v)[[1L]]
    )
  }

  out
}


#' A block's arguments plus its script values, as a title template reads them
#'
#' A template names a declared value by its variable (`{@top_n}`), so the
#' script's values join the block's own roles. A role wins a collision. An
#' untouched value falls back to the declaration's default. A flag prints its
#' control's label when on and nothing when off, so a `[ ... ]` clause can
#' carry it.
#'
#' @param roles Named list of the block's own arguments.
#' @param specs Input specs from [cb_specs()].
#' @param values Named list of current control values.
#' @return A named list.
#' @noRd
script_title_args <- function(roles, specs, values = list()) {

  vals <- values
  for (s in specs) {
    if (!is.null(s$error) || is.na(s$kind)) next
    if (is.null(vals[[s$name]])) vals[[s$name]] <- s$default
  }

  if (!length(vals)) {
    return(roles)
  }

  vals <- vals[setdiff(names(vals), names(roles))]
  specs_ok <- Filter(function(s) is.null(s$error) && !is.na(s$kind), specs)
  vals <- vals[names(vals) %in% vapply(specs_ok, `[[`, "", "name")]

  recs <- dd_script_roles(specs_ok)
  labs <- stats::setNames(
    vapply(recs, function(r) as.character(r$label %||% r$name)[[1L]], ""),
    vapply(recs, function(r) as.character(r$name)[[1L]], "")
  )

  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (is.logical(v) && length(v) == 1L) {
      w <- if (nm %in% names(labs)) unname(labs[[nm]]) else nm
      vals[[nm]] <- if (isTRUE(v)) w else ""
    }
  }

  c(roles, vals)
}
