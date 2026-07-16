# Make the drilldown blocks legible to the blockr.ai assistant. Their result is a
# passthrough data.frame (a filter that only narrows on click), so the data
# effect is blind and even reads as a no-op. The meaningful artifact is the CHART
# CONFIG -- the column-to-role bindings. These `config_effect()` methods describe
# that config and flag bindings that reference columns absent from the input, so
# the model gets real feedback instead of "no rows or columns changed".
#
# Registered onto blockr.ai's generic at load (defensive: no hard dependency on
# blockr.ai, no-op when it is absent or too old to export config_effect).

`%||%` <- function(a, b) if (is.null(a)) b else a

# Column-valued roles, in display order, for the chart block. vlines / hlines
# are NOT roles: they hold numeric helper-line positions, not column names
# (reported separately below, never validated against the columns).
dd_chart_roles <- c("group", "x", "y", "xend", "value", "color", "facet",
                    "series", "label", "drill", "lo", "hi")

#' @noRd
config_effect.chart_block <- function(block, args, data = NULL, ...) {
  cols <- if (is.data.frame(data)) names(data) else NULL
  ct <- as.character(args$chart_type %||% "bar")[1]

  parts <- character()
  bad <- character()
  for (r in dd_chart_roles) {
    v <- args[[r]]
    if (is.null(v) || !nzchar(as.character(v)[1])) next
    v <- as.character(v)[1]
    parts <- c(parts, paste0(r, "=", v))
    # `.count`/`auto` are sentinels, not columns; everything else must exist.
    if (!is.null(cols) && !(v %in% c(".count", "auto")) && !(v %in% cols)) {
      bad <- c(bad, paste0(r, " references '", v, "'"))
    }
  }
  # Helper lines: plain numeric values (possibly several per axis), echoed
  # verbatim so the model sees them configured.
  for (r in c("vlines", "hlines")) {
    v <- unlist(args[[r]])
    if (length(v)) parts <- c(parts, paste0(r, "=", paste(v, collapse = ",")))
  }
  agg <- args$func
  agg_txt <- if (!is.null(agg) && nzchar(as.character(agg)[1])) {
    paste0(" agg=", as.character(agg)[1])
  } else {
    ""
  }
  drill_off <- is.null(args$drill) || !nzchar(as.character(args$drill %||% "")[1])

  desc <- paste0(
    ct, " chart configured: ",
    if (length(parts)) paste(parts, collapse = ", ") else "(no column bindings)",
    agg_txt,
    if (drill_off) " -- drill OFF (set `drill` to enable click-to-filter)" else ""
  )
  if (length(bad)) {
    desc <- paste0(
      desc, " -- INVALID column binding(s): ", paste(bad, collapse = "; "),
      ". Available columns: ", paste(cols, collapse = ", ")
    )
  }
  desc
}

#' @noRd
config_effect.table_block <- function(block, args, data = NULL, ...) {
  cols <- if (is.data.frame(data)) names(data) else NULL
  parts <- character()
  bad <- character()
  for (r in c("rowname", "drill")) {
    v <- args[[r]]
    if (is.null(v) || !nzchar(as.character(v)[1])) next
    v <- as.character(v)[1]
    parts <- c(parts, paste0(r, "=", v))
    if (!is.null(cols) && !(v %in% cols)) bad <- c(bad, paste0(r, " references '", v, "'"))
  }
  # `value` (renamed from `values`, see dev/unified-arg-naming.md): the body
  # column(s), a scalar or character vector. unlist() flattens either shape.
  vc <- as.character(unlist(args$value))
  vc <- vc[nzchar(vc)]
  if (length(vc)) {
    parts <- c(parts, paste0("value=", paste(vc, collapse = "/")))
    miss <- setdiff(vc, cols %||% vc)
    if (length(miss)) bad <- c(bad, paste0("value ", paste(miss, collapse = ",")))
  }
  desc <- paste0("drilldown table configured: ",
                 if (length(parts)) paste(parts, collapse = ", ") else "(defaults)")
  if (length(bad)) {
    desc <- paste0(desc, " -- INVALID: ", paste(bad, collapse = "; "),
                   ". Available columns: ", paste(cols, collapse = ", "))
  }
  desc
}

#' Register the drilldown config_effect methods on blockr.ai's generic.
#' @noRd
register_drilldown_ai_effect <- function() {
  if (!requireNamespace("blockr.ai", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  ns <- asNamespace("blockr.ai")
  if (!exists("config_effect", envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  registerS3method("config_effect", "chart_block",
                   config_effect.chart_block, envir = ns)
  registerS3method("config_effect", "table_block",
                   config_effect.table_block, envir = ns)
  invisible(TRUE)
}
