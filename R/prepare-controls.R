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
  # A control its `when` flag switched off is not drawn.
  specs <- Filter(function(s) !isTRUE(s$off), specs)
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
    if (!script_spec_ok(s) || isTRUE(s$off)) {
      next
    }
    v <- values[[s$name]]
    if (is.null(v)) {
      v <- s$default
    }
    out[[script_key(s$name)]] <- switch(
      s$kind,
      flag = if (isTRUE(as.logical(v))) "on" else "off",
      # as.list() keeps a single pick a JSON array instead of a string. A
      # multi-select shows only picks its pool still offers (the pool can
      # follow another control), so "N selected" counts what is offered.
      select = if (isTRUE(s$multiple)) {
        v <- as.character(v)
        if (length(s$choices)) v <- v[v %in% as.character(s$choices)]
        as.list(v)
      } else {
        as.character(v)[[1L]]
      },
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
#' script's values join the block's own roles. A script value wins a
#' collision: the declaration is written in the block's own script, while a
#' setting of the same name (the table's `top_n`) is easy to clash with
#' unknowingly. That setting stays in the gear; only the word goes to the
#' script. A declaration that failed leaves the setting's word alone. An
#' untouched value falls back to the declaration's default. A flag prints its
#' control's label when on and nothing when off, so a `[ ... ]` clause can
#' carry it; a date without a value prints its placeholder; a multi-select
#' prints its picks.
#'
#' @param roles Named list of the block's own arguments.
#' @param specs Input specs from [cb_specs()].
#' @param values Named list of current control values.
#' @return A named list.
#' @noRd
script_title_args <- function(roles, specs, values = list()) {
  specs <- Filter(script_spec_ok, specs)
  if (!length(specs)) {
    return(roles)
  }
  vals <- lapply(specs, function(s) {
    v <- values[[s$name]] %||% s$default
    # A control its `when` flag switched off prints nothing (and is not
    # offered, see viz_titles()).
    if (isTRUE(s$off)) {
      return("")
    }
    # A flag prints its `on` word while on, else its label; nothing when off.
    if (is.logical(v) && length(v) == 1L) {
      v <- if (isTRUE(v)) as.character(s$on %||% script_label(s)) else ""
    }
    # A date without a value prints its placeholder ("automatic"), or
    # nothing, so a `[ ... ]` clause can carry it.
    if (identical(s$kind, "date") && (!length(v) || is.na(v[[1L]]))) {
      v <- as.character(s$placeholder %||% "")
    }
    # A multi-select prints its picks: "all" when every choice is picked,
    # "none" when none is, the picks in a list otherwise. Picks its pool no
    # longer offers (the pool can follow another control) do not count.
    if (identical(s$kind, "select") && isTRUE(s$multiple)) {
      picks <- as.character(v)
      pool <- as.character(s$choices)
      if (length(pool)) picks <- picks[picks %in% pool]
      v <- script_picks_word(picks, pool)
    }
    v
  })
  names(vals) <- vapply(specs, `[[`, "", "name")
  out <- c(roles[setdiff(names(roles), names(vals))], vals)
  # The switched-off controls, so the title does not offer them.
  attr(out, "script_off") <- names(vals)[vapply(specs, function(s) isTRUE(s$off), NA)]
  out
}

# The word for a multi-select's picks.
script_picks_word <- function(v, choices) {
  if (!length(v)) return("none")
  if (length(choices) > 1L && setequal(v, choices)) return("all")
  if (length(v) > TITLE_MAX_VALUES) {
    v <- c(v[seq_len(TITLE_MAX_VALUES)], "\u2026")
  }
  paste(v, collapse = ", ")
}
