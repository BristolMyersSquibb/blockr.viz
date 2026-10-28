# The prepare script's controls as others see them: records for the view's
# script band (the payload's `derived$script`, built by viz_script_sources()),
# and the values a title template reads. Taking values back from the browser
# is viz_script_values() (R/kit-script.R).

#' Describe a script's controls for the browser
#'
#' One record per control, in the gear's vocabulary, so the band draws it
#' with the gear's own control builder. A flag becomes the gear's boolean
#' `segmented`; the other kinds map across unchanged.
#'
#' Specs that failed (a mistyped column name in a factor's levels) are carried
#' through with their message, so a missing control says why.
#'
#' @param specs Input specs from [cb_specs()].
#' @return A list of records, JSON-ready.
#' @noRd
script_control_records <- function(specs) {
  lapply(specs, function(s) {
    label <- script_label(s)
    if (!script_spec_ok(s)) {
      return(list(name = s$name, key = script_key(s$name), label = label,
                  kind = "error", error = s$error %||% "no control"))
    }
    rec <- list(
      name = s$name,
      key = script_key(s$name),
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
      rec$options <- list(list(value = "on", label = label),
                          list(value = "off", label = label))
    }
    for (k in c("min", "max", "step", "placeholder")) {
      if (!is.null(s[[k]])) rec[[k]] <- s[[k]]
    }
    rec
  })
}

# Whether a spec became a control. A failed declaration carries its error
# and no kind.
script_spec_ok <- function(s) {
  is.null(s$error) && !is.na(s$kind)
}

# A control's label: the `#| label =` annotation, else the variable name in
# words ("min_age" -> "Min age").
script_label <- function(s) {
  if (!is.null(s$label)) {
    return(as.character(s$label)[[1L]])
  }
  label <- gsub("[._]", " ", s$name)
  paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))
}

#' The config key a script control writes to
#'
#' The band's controls are drawn as config rows keyed `sv_<name>`. The prefix
#' keeps a script variable called `color` apart from the chart's colour
#' mapping.
#'
#' @param name The declared variable name.
#' @noRd
script_key <- function(name) {
  paste0("sv_", name)
}

#' Current script values, in the gear's vocabulary
#'
#' The band's controls read their value from these `sv_*` entries. An
#' untouched control shows the declaration's value.
#'
#' @param specs Input specs from [cb_specs()].
#' @param values Named list of current control values.
#' @return A named list of `sv_*` entries.
#' @noRd
script_control_values <- function(specs, values = list()) {
  out <- list()
  for (s in specs) {
    if (!script_spec_ok(s)) {
      next
    }
    v <- values[[s$name]]
    if (is.null(v)) {
      v <- s$default
    }
    out[[script_key(s$name)]] <- switch(
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
  specs <- Filter(script_spec_ok, specs)
  specs <- specs[!vapply(specs, `[[`, "", "name") %in% names(roles)]
  if (!length(specs)) {
    return(roles)
  }
  vals <- lapply(specs, function(s) {
    v <- values[[s$name]] %||% s$default
    if (is.logical(v) && length(v) == 1L) {
      v <- if (isTRUE(v)) script_label(s) else ""
    }
    v
  })
  names(vals) <- vapply(specs, `[[`, "", "name")
  c(roles, vals)
}
