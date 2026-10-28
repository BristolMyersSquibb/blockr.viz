#' Running the chart block's prepare script
#'
#' Runs the compiled script (R/prepare-parse.R, R/prepare-expr.R) on the frame
#' the chart draws, splices it into the block's expression behind the click
#' filter, and describes its controls to the browser.
#'
#' @name chart-prepare-apply
#' @keywords internal
NULL

# Is this spec a usable control (no error, a known kind)?
dd_spec_ok <- function(s) {
  is.null(s$error) && !is.na(s$kind)
}

# Run a prepare script on a data frame: compile with `slot = FALSE` (values as
# literals, a bare `data` symbol) and evaluate with `data` bound to the frame.
# Returns `data` (the prepared frame, NULL on failure) and `error` (NULL or
# the message). A failure reports to the browser; falling back to the raw
# frame would draw the wrong data silently.
dd_prepare_run <- function(data, parsed, specs, values = list()) {
  if (is.null(data)) return(list(data = NULL, error = NULL))
  if (is.null(parsed) || (parsed$ok && !length(parsed$stmts))) {
    return(list(data = data, error = NULL))
  }
  if (!parsed$ok) return(list(data = NULL, error = parsed$error))

  body <- cb_expr(parsed, specs, values, data_name = "data", slot = FALSE)

  # All declarations and no body is a half-written script, not an error.
  if (is.null(body)) return(list(data = data, error = NULL))

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

  # The filter trail and provenance (read by `{filters}`, `{dataset}` and the
  # download names) carry across a script that aggregates, unless it set its own.
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

# Which click-filter conditions run after the prepare script. A click filter
# wraps into the script's `.(data)` slot, so it filters the source rows; a
# condition on a column only the script makes goes after the script. `cols`
# lists, per condition, the column names it reads; `src_cols` is the
# upstream's column names (NULL, not seen yet, keeps every condition before the
# script). Returns a logical vector, TRUE for a condition to filter after.
dd_drill_post <- function(cols, src_cols = NULL) {
  if (is.null(src_cols)) return(rep(FALSE, length(cols)))
  vapply(cols, function(cc) !all(cc %in% src_cols), logical(1L))
}

# Splice an expression into every `.(data)` slot of another. The block's
# expression is the click filter with the script wrapped around it, so the
# script's `.(data)` marker is replaced by the whole filter call, which carries
# the one remaining marker.
dd_splice_slot <- function(e, repl) {
  if (is.call(e) && length(e) == 2L && identical(e[[1L]], quote(`.`)) &&
      identical(e[[2L]], quote(data))) {
    return(repl)
  }
  if (!is.call(e)) return(e)
  cb_map_args(e, function(a) dd_splice_slot(a, repl))
}

# The label of a script control: its annotation, else the variable name with
# separators as spaces and a capital first letter.
dd_script_label <- function(s) {
  label <- gsub("[._]", " ", s$name)
  s$label %||% paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))
}

# Describe a script's controls for the browser: one JSON-ready record per
# control, in the engine's role vocabulary, so the band renders them with the
# `_buildControl()` every mapping row uses. A flag becomes the engine's boolean
# `segmented`. Failed specs carry their message, so a missing control says why.
dd_script_roles <- function(specs) {
  lapply(specs, function(s) {
    label <- dd_script_label(s)

    if (!dd_spec_ok(s)) {
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
      # Bare strings: a {value, label} option would show as "setosa (setosa)".
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
    if (!dd_spec_ok(s)) next
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

# A block's arguments plus its script values, as a title template reads them:
# `{@top_n}` names a declared value by its variable. A role wins a collision,
# and an untouched value falls back to the declaration's default. A flag prints
# its control's label when on and nothing when off, so a `[ ... ]` clause can
# carry it.
script_title_args <- function(roles, specs, values = list()) {
  specs <- Filter(dd_spec_ok, specs)

  vals <- values
  for (s in specs) {
    if (is.null(vals[[s$name]])) vals[[s$name]] <- s$default
  }
  if (!length(vals)) return(roles)

  vals <- vals[setdiff(names(vals), names(roles))]
  nms <- vapply(specs, `[[`, "", "name")
  vals <- vals[names(vals) %in% nms]
  labs <- stats::setNames(lapply(specs, dd_script_label), nms)

  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (is.logical(v) && length(v) == 1L) {
      vals[[nm]] <- if (isTRUE(v)) as.character(labs[[nm]])[[1L]] else ""
    }
  }

  c(roles, vals)
}
