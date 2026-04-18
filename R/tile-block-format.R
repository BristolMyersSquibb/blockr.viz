# Format inference for tile_block (§6 design, §6 requirements).

#' Infer a format from column name + value range.
#'
#' Three-rule cascade: column-name regex → value-range heuristic →
#' fallback number. Returns a list with `kind` and `digits`.
#' @noRd
infer_format <- function(col_name, values) {
  # Rule 1: column-name regex
  nm <- tolower(col_name %||% "")
  kind <- NULL
  if (grepl("%|pct|percent|rate|share|ratio", nm)) {
    kind <- "percent"
  } else if (grepl("usd|dollar|\\$|price|revenue|cost|amount|sales", nm)) {
    kind <- "currency_usd"
  } else if (grepl("eur|euro|\u20ac", nm)) {
    kind <- "currency_eur"
  } else if (grepl("gbp|pound|\u00a3", nm)) {
    kind <- "currency_gbp"
  } else if (grepl("date|time|timestamp", nm)) {
    kind <- "date"
  }

  # Rule 2: value-range heuristic (only if rule 1 didn't fire)
  if (is.null(kind) && is.numeric(values)) {
    v <- values[is.finite(values)]
    if (length(v) > 0) {
      all_pos <- all(v >= 0)
      all_in_unit <- all(v >= 0 & v <= 1)
      if (all_in_unit && all_pos && any(v > 0)) {
        kind <- "percent_unit"  # [0,1] → multiply by 100
      }
    }
  }

  if (is.null(kind)) kind <- "number"

  # Digits default
  digits <- if (is.numeric(values)) {
    v <- abs(values[is.finite(values)])
    if (length(v) == 0) 1L
    else if (max(v) >= 1000) 0L
    else if (max(v) >= 1) 1L
    else 2L
  } else {
    0L
  }

  list(kind = kind, digits = digits)
}

#' Convert old kpi_block prefix/suffix glyphs to a format kind.
#' @noRd
infer_from_glyphs <- function(prefix = "", suffix = "") {
  if (identical(prefix, "$")) return("currency_usd")
  if (identical(prefix, "\u20ac")) return("currency_eur")
  if (identical(prefix, "\u00a3")) return("currency_gbp")
  if (identical(suffix, "%")) return("percent")
  NULL
}

#' Format a numeric value per a format spec.
#' @noRd
format_value <- function(x, kind = "number", digits = 0L) {
  if (is.null(x) || length(x) == 0 || (is.numeric(x) && !is.finite(x))) {
    return("\u2014")
  }
  if (kind %in% c("percent", "percent_unit")) {
    mult <- if (kind == "percent_unit") 100 else 1
    sprintf(paste0("%.", digits, "f%%"), x * mult)
  } else if (startsWith(kind, "currency_")) {
    sym <- switch(kind,
      currency_usd = "$",
      currency_eur = "\u20ac",
      currency_gbp = "\u00a3",
      "$"
    )
    paste0(sym, formatC(x, format = "f", digits = digits, big.mark = ","))
  } else if (kind == "scientific") {
    formatC(x, format = "e", digits = digits)
  } else if (kind == "date") {
    format(as.Date(x, origin = "1970-01-01"))
  } else {
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }
}
