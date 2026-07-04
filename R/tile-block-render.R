# Render layer for new_tile_block(): reshape the input to a normalized long
# "tile frame" (one row per group x measure cell) and emit the tk-* markup.
# A pure presentation of the tidy frame -- the
# renderer does NO arithmetic; secondaries (delta / fill fraction / pill
# status) are precomputed columns. Render-time concerns only: number
# formatting, sign x good_when coloring, and bar / chip / arrow drawing.
#
# The two layouts (cards / table) render the identical cells. Multi-instance
# safety comes from the per-render wrapper id + the ns()-based elem_id (same
# model as table-block.R), so no custom-message namespacing needed.

# ---------------------------------------------------------------------------
# Normalization: input frame + roles -> long cell frame
# ---------------------------------------------------------------------------

#' Is `x` a column name in `data` (vs a literal string)?
#' @noRd
tk_is_col <- function(x, data) {
  is.character(x) && length(x) == 1L && nzchar(x) && x %in% names(data)
}

#' Resolve a literal-or-column role to a per-row character vector.
#' "" -> NULL (unset); a column name -> that column; otherwise the literal,
#' recycled to nrow.
#' @noRd
tk_role_vec <- function(x, data, n) {
  if (is.null(x) || !is.character(x) || length(x) != 1L || !nzchar(x)) {
    return(NULL)
  }
  if (x %in% names(data)) return(as.character(data[[x]]))
  rep(x, n)
}

#' Build the long cell frame from the input data and the mapped roles.
#'
#' Wide input (`value` names multiple numeric columns, no `measure`): each
#' column becomes a measure (measure name = column name), pivoted long.
#' Long input (`value` is one column + `measure` names the label column):
#' one row per (group, measure) cell as-is. Returns a data frame with
#' columns group/measure/value/secondary/overline/caption (character except
#' value=numeric, secondary=list to preserve type), or an empty frame.
#' @noRd
tile_long_frame <- function(data, value = character(), by = "",
                            measure = "", secondary = "",
                            overline = "", caption = "") {
  value <- as.character(value)
  value <- value[nzchar(value)]
  value <- intersect(value, names(data))
  if (length(value) == 0L || nrow(data) == 0L) return(tile_empty_cells())

  has_by      <- tk_is_col(by, data)
  has_measure <- tk_is_col(measure, data)
  has_sec     <- tk_is_col(secondary, data)

  long_mode <- has_measure && length(value) == 1L
  n <- nrow(data)

  if (long_mode) {
    grp <- if (has_by) as.character(data[[by]]) else rep("", n)
    cells <- data.frame(
      group   = grp,
      measure = as.character(data[[measure]]),
      stringsAsFactors = FALSE
    )
    cells$value <- suppressWarnings(as.numeric(data[[value]]))
    cells$secondary <- if (has_sec) data[[secondary]] else rep(NA, n)
    over <- tk_role_vec(overline, data, n)
    cap  <- tk_role_vec(caption, data, n)
    cells$overline <- if (is.null(over)) cells$measure else over
    cells$caption  <- if (is.null(cap)) rep(NA_character_, n) else cap
    cells$.col <- cells$measure
  } else {
    # Wide: each value column is a measure. Group = `by` column, else the
    # row index when there is more than one row (so a multi-row wide frame
    # still separates into per-row cells), else "".
    if (has_by) {
      grp <- as.character(data[[by]])
    } else if (n > 1L) {
      grp <- as.character(seq_len(n))
    } else {
      grp <- rep("", n)
    }
    over <- tk_role_vec(overline, data, n)
    cap  <- tk_role_vec(caption, data, n)
    chunks <- lapply(value, function(col) {
      d <- data.frame(
        group   = grp,
        measure = rep(col, n),
        stringsAsFactors = FALSE
      )
      d$value     <- suppressWarnings(as.numeric(data[[col]]))
      d$secondary <- if (has_sec) data[[secondary]] else rep(NA, n)
      d$overline  <- if (is.null(over)) rep(tk_col_overline(data, col), n) else over
      d$caption   <- if (is.null(cap)) rep(NA_character_, n) else cap
      d$.col      <- rep(col, n)
      d
    })
    cells <- do.call(rbind, chunks)
  }
  cells
}

#' @noRd
tile_empty_cells <- function() {
  data.frame(group = character(), measure = character(), value = numeric(),
             secondary = I(list()), overline = character(),
             caption = character(), .col = character(),
             stringsAsFactors = FALSE)
}

#' Default overline for a wide-input measure column: the column's variable
#' label if it carries one (an aggregated metric column carries a friendly
#' stat-prefixed label, e.g. "Mean: Revenue"; a labelled source column carries
#' its own), else the prettified column name.
#' @noRd
tk_col_overline <- function(data, col) {
  lbl <- attr(data[[col]], "label", exact = TRUE)
  if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl)) lbl else tk_pretty(col)
}

#' Snake / dot case -> Title Case.
#' @noRd
tk_pretty <- function(x) {
  x <- as.character(x)
  tools::toTitleCase(gsub("[_.]", " ", x))
}

# ---------------------------------------------------------------------------
# Value formatting (render-time)
# ---------------------------------------------------------------------------

#' Resolve a format spec for a measure's values.
#'
#' Predictable and NOT name-based -- the renderer never guesses currency from a
#' column name (that produced absurd `$` results). The unit / currency label is
#' the free-text `unit` field's job. `format` is one of:
#'   - "number" (default / "auto") -- thousands separators + smart decimals,
#'   - "compact" -- 1.2K / 38.4M / 1.2B,
#'   - "percent" -- value x100 (when it looks like a fraction) + "%".
#' Returns list(kind, digits, scale).
#' @noRd
tk_resolve_format <- function(format, values) {
  format <- format %||% "number"
  if (identical(format, "compact")) {
    return(list(kind = "compact", digits = 1L, scale = 1))
  }
  if (identical(format, "percent")) {
    v <- values[is.finite(values)]
    scale <- if (length(v) && all(abs(v) <= 1) && any(v != 0)) 100 else 1
    return(list(kind = "percent", digits = 1L, scale = scale))
  }
  list(kind = "number", digits = tk_digits(values), scale = 1)
}

#' @noRd
tk_digits <- function(values) {
  v <- abs(values[is.finite(values)])
  if (length(v) == 0L) return(1L)
  if (max(v) >= 1000) 0L else if (max(v) >= 1) 1L else 2L
}

#' Format one value to its display string per a resolved format spec. The
#' `unit` (a free-text label like "USD" / "CHF" / "apples") is rendered as a
#' SEPARATE `.tk-unit` span by the cell builders, not baked in here -- except
#' percent, where "%" is intrinsic.
#' @noRd
tk_format <- function(x, spec) {
  if (is.null(x) || length(x) == 0L || !is.finite(x)) return("\u2014")
  if (identical(spec$kind, "percent")) {
    return(sprintf(paste0("%.", spec$digits, "f%%"), x * spec$scale))
  }
  if (identical(spec$kind, "compact")) return(tk_compact(x))
  formatC(x, format = "f", digits = spec$digits, big.mark = ",")
}

#' Compact K/M/B formatting (no currency symbol -- the unit field labels it).
#' @noRd
tk_compact <- function(x) {
  neg <- x < 0
  a <- abs(x)
  s <- if (a >= 1e9) {
    paste0(formatC(a / 1e9, format = "f", digits = if (a >= 1e10) 0 else 1),
           "B")
  } else if (a >= 1e6) {
    paste0(formatC(a / 1e6, format = "f", digits = if (a >= 1e7) 0 else 1),
           "M")
  } else if (a >= 1e3) {
    paste0(formatC(a / 1e3, format = "f", digits = 0), "K")
  } else {
    formatC(a, format = "f", digits = 0)
  }
  s <- sub("\\.0([MBK])$", "\\1", s)
  paste0(if (neg) "\u2212" else "", s)
}

# ---------------------------------------------------------------------------
# Cell pieces (value span, secondary by style)
# ---------------------------------------------------------------------------

#' The big value span with is-neg coloring. Rendered at its final value (no
#' count-up animation -- the number should read instantly).
#' @noRd
tk_value_span <- function(x, spec, extra_class = NULL) {
  txt <- tk_format(x, spec)
  cls <- paste(c("tk-value", "num",
                 if (is.finite(x) && x < 0) "is-neg", extra_class),
               collapse = " ")
  htmltools::tags$span(class = cls, txt)
}

#' Up / down caret svg for a delta.
#' @noRd
tk_caret <- function(up) {
  path <- if (up) "M5 1l4 7H1z" else "M5 9L1 2h8z"
  htmltools::HTML(paste0(
    '<svg viewBox="0 0 10 10" fill="currentColor"><path d="', path,
    '"/></svg>'
  ))
}

#' Polarity class for a signed delta: sign x good_when -> good/bad/flat.
#' @noRd
tk_delta_class <- function(value, good_when) {
  if (!is.finite(value) || value == 0) return("flat")
  up <- value > 0
  good <- if (identical(good_when, "down")) !up else up
  if (good) "good" else "bad"
}

#' Draw a delta span: caret + formatted percent, colored by polarity.
#' A delta in `[-1, 1]` is read as a fraction and scaled to a percent.
#' @noRd
tk_delta_node <- function(value, good_when) {
  v <- suppressWarnings(as.numeric(value))
  if (!is.finite(v)) return(NULL)
  cls <- tk_delta_class(v, good_when)
  scale <- if (abs(v) <= 1) 100 else 1
  txt <- paste0(sprintf("%.1f", abs(v) * scale), "%")
  htmltools::tags$span(
    class = paste("tk-delta", cls),
    if (cls != "flat") tk_caret(v > 0),
    txt
  )
}

#' Map a status string to a pill tone.
#' @noRd
tk_pill_tone <- function(status) {
  s <- tolower(trimws(as.character(status)))
  if (s %in% c("ok", "good", "above", "above plan", "pass", "up", "on track",
               "healthy", "green")) return("good")
  if (s %in% c("bad", "fail", "below", "below plan", "down", "critical",
               "red", "off track")) return("bad")
  if (s %in% c("warn", "warning", "at risk", "amber", "neutral")) return("bad")
  "neutral"
}

#' Fill fraction as a 0-100 percent. A value in `[0,1]` is a fraction; a value
#' in (1,100] is already a percent (see "a percent value defaults ref to 100").
#' @noRd
tk_fill_pct <- function(value) {
  v <- suppressWarnings(as.numeric(value))
  if (!is.finite(v)) return(NA_real_)
  pct <- if (v >= 0 && v <= 1) v * 100 else v
  max(0, min(100, pct))
}

#' Draw the secondary in the chosen style. Returns a tag or NULL.
#' `context` is "card" or "cell" (compact, for the matrix).
#' @noRd
tk_secondary_node <- function(style, value, good_when, spec, context = "card") {
  style <- style %||% "plain"
  if (identical(style, "delta")) {
    return(tk_delta_node(value, good_when))
  }
  if (identical(style, "fill")) {
    pct <- tk_fill_pct(value)
    if (!is.finite(pct)) return(NULL)
    good <- !identical(good_when, "down")
    w <- formatC(pct, format = "f", digits = 1)
    if (identical(context, "cell")) {
      return(htmltools::tags$span(
        class = "tk-cellfill",
        htmltools::tags$span(class = "pct num", paste0(round(pct), "%")),
        htmltools::tags$span(class = "tk-fill__track",
          htmltools::tags$span(
            class = paste("tk-fill__bar", if (good) "good"),
            style = sprintf("display:block;width:%s%%", w)))
      ))
    }
    bar <- htmltools::tags$div(
      class = paste("tk-fill__bar", if (good) "good"),
      style = sprintf("width:%s%%", w)
    )
    return(htmltools::tags$div(
      class = "tk-fill",
      htmltools::tags$div(class = "tk-fill__track", bar),
      htmltools::tags$div(class = "tk-fill__meta",
                          htmltools::tags$b(class = "num", paste0(round(pct), "%")))
    ))
  }
  if (identical(style, "pill")) {
    if (is.null(value) || all(is.na(value))) return(NULL)
    tone <- tk_pill_tone(value)
    return(htmltools::tags$span(
      class = paste("tk-pill", tone),
      as.character(value)
    ))
  }
  # plain: a formatted reference line. Numeric -> format with value spec.
  if (is.null(value) || all(is.na(value))) return(NULL)
  num <- suppressWarnings(as.numeric(value))
  ref <- if (is.finite(num)) tk_format(num, spec) else as.character(value)
  htmltools::tags$div(class = "tk-ref num", htmltools::tags$b(ref))
}

# ---------------------------------------------------------------------------
# Empty / message shells
# ---------------------------------------------------------------------------

#' @noRd
tk_empty_card <- function() {
  htmltools::tags$article(
    class = "tk-card is-empty",
    htmltools::tags$div(
      class = "tk-empty__inner",
      htmltools::HTML(paste0(
        '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" ',
        'stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round">',
        '<path d="M3 3v18h18"/>',
        '<path d="M7 14.5l3-2 3 1 4-4.5" stroke-dasharray="2 2.5"/></svg>'
      )),
      htmltools::tags$span("No data")
    )
  )
}
