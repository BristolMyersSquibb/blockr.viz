# Number formats of the table model: the `format` setting, the unit in the
# column headers, and the `.fmt` cell engine (dev/table-and-chart-architecture.md,
# "The `.fmt` convention").
#
# `.fmt` engine: a hidden per-row `.fmt` template column such as
# "{n} ({pct}%)" is interpolated into display strings from the numeric columns
# of the same row. Rounding stays out of the template (`digits` or a per-row
# `.digits` column owns it).

# Fixed decimals with trailing zeros kept (65 at 1 dp is "65.0"); non-numerics
# pass through.
fmt_num <- function(x, digits) {
  if (is.numeric(x)) {
    formatC(round(x, digits), format = "f", digits = digits, drop0trailing = FALSE)
  } else {
    as.character(x)
  }
}

# Interpolates each row's `.fmt` template. `{col}` takes that row's value of
# `col`, rounded to the row's `.digits` or `digits`; `{col:N}` pins N decimals
# for that token. An NA or "" template gives NA; a missing column or an NA value
# renders as `na`.
fmt_cells <- function(df, fmt_col = ".fmt", digits = 2L, na = "NA") {
  stopifnot(is.data.frame(df))
  if (!fmt_col %in% names(df)) {
    return(rep(NA_character_, nrow(df)))
  }
  tmpl <- as.character(df[[fmt_col]])
  row_digits <- if (".digits" %in% names(df)) {
    suppressWarnings(as.integer(df[[".digits"]]))
  } else {
    rep(NA_integer_, nrow(df))
  }
  out <- rep(NA_character_, nrow(df))
  live <- which(!is.na(tmpl) & nzchar(tmpl))
  if (!length(live)) return(out)
  eff_digits <- ifelse(is.na(row_digits[live]), digits, row_digits[live])
  # Templates repeat heavily: parse each distinct (digits, template) once and
  # paste the formatted value vectors between the literal segments.
  for (g in split(seq_along(live), paste0(eff_digits, "\r", tmpl[live]))) {
    rows <- live[g]
    t <- tmpl[rows[1L]]
    d <- eff_digits[g[1L]]
    m <- gregexpr("\\{[^{}]+\\}", t)[[1L]]
    if (m[1L] == -1L) {
      out[rows] <- t                    # no tokens: the template is literal
      next
    }
    starts <- as.integer(m)
    lens   <- attr(m, "match.length")
    toks   <- substring(t, starts, starts + lens - 1L)
    lits   <- substring(t, c(1L, starts + lens), c(starts - 1L, nchar(t)))
    vals   <- list()                    # per-token cache (tokens may repeat)
    pieces <- vector("list", 2L * length(toks) + 1L)
    pieces[[1L]] <- lits[1L]
    for (j in seq_along(toks)) {
      tok <- toks[j]
      if (is.null(vals[[tok]])) {
        spec <- substring(tok, 2L, nchar(tok) - 1L)
        tok_digits <- d
        if (grepl(":", spec, fixed = TRUE)) {
          parts <- strsplit(spec, ":", fixed = TRUE)[[1]]
          spec <- parts[1]
          pd <- suppressWarnings(as.integer(parts[2]))
          if (!is.na(pd)) tok_digits <- pd
        }
        v <- if (spec %in% names(df)) df[[spec]][rows] else rep(NA, length(rows))
        sv <- fmt_num(v, tok_digits)
        sv[is.na(v)] <- na
        vals[[tok]] <- sv
      }
      pieces[[2L * j]]      <- vals[[tok]]
      pieces[[2L * j + 1L]] <- lits[j + 1L]
    }
    out[rows] <- do.call(paste0, pieces)
  }
  out
}

# Formats each row's cell first (n and pct share a row), then spreads
# `group_col` to columns; with `group_col = NULL` it returns `.cell` alone.
# `id_cols` defaults to the dotted row-identity columns present in `df`.
fmt_assemble <- function(df, group_col = NULL, id_cols = NULL,
                         digits = 2L, na = "NA") {
  stopifnot(is.data.frame(df))
  df[[".cell"]] <- fmt_cells(df, digits = digits, na = na)
  if (is.null(id_cols)) {
    # `.variable` is part of row identity (variables share level labels such
    # as "OTHER", which would pivot into list-cols) and stays in the output:
    # drills filter on it.
    id_cols <- grep(
      paste0("^\\.(label|indent|strong|emph",
             "|group\\d+(_level|_label)?",
             "|variable(_level|_label)?)$"),
      names(df), value = TRUE
    )
  }
  if (is.null(group_col)) {
    keep <- c(id_cols, ".cell")
    return(df[, intersect(keep, names(df)), drop = FALSE])
  }
  long <- df[, c(id_cols, group_col, ".cell"), drop = FALSE]
  # pivot_wider() aborts on a "" or NA group level (a blank arm on real
  # data); relabel it.
  gv <- as.character(long[[group_col]])
  blank <- is.na(gv) | !nzchar(trimws(gv))
  if (any(blank)) long[[group_col]][blank] <- "(Missing)"
  wide <- tidyr::pivot_wider(
    long,
    names_from = dplyr::all_of(group_col),
    values_from = ".cell"
  )
  wide
}

#' Convert a tidy `.fmt` summary frame to the wide display grid the
#' table renderers consume.
#'
#' Detects the long tidy form by the presence of a `.fmt` column. When
#' present, formats each row's template, spreads `.group` to columns
#' (`fmt_assemble()`), and reattaches the `"<group>\\nN = <n>"` column
#' labels from `attr(df, "group_n")`. When absent (legacy / already-wide
#' input) the frame is returned unchanged. Idempotent on wide input, so
#' renderers can call it unconditionally at the start of rendering.
#'
#' @param df A data frame: either the tidy `.fmt` form or an
#'   already-wide display frame.
#' @return A wide display frame with dotted id columns + one formatted
#'   column per `.group` level, each carrying a `label` attribute.
#'
#' For exporters in other blockr packages; not part of the user API.
#' @keywords internal
#' @export
fmt_to_wide <- function(df) {
  if (!is.data.frame(df) || !".fmt" %in% names(df) ||
      !".group" %in% names(df)) {
    return(df)
  }
  group_n <- attr(df, "group_n")
  wide <- fmt_assemble(df, group_col = ".group")

  # Reattach per-group N as "<group>\nN = <n>" column labels.
  if (!is.null(group_n)) {
    for (g in names(group_n)) {
      if (!g %in% names(wide)) next
      n_val <- group_n[[g]]
      # For length-2 `by` ("outer|inner"), display only the inner level
      # and let tab_spanner_delim pick up the outer level.
      display_name <- if (grepl("||", g, fixed = TRUE)) {
        parts <- strsplit(g, "||", fixed = TRUE)[[1]]
        parts[length(parts)]
      } else {
        g
      }
      if (is.null(n_val) || is.na(n_val)) {
        attr(wide[[g]], "label") <- display_name
      } else {
        attr(wide[[g]], "label") <- sprintf("%s\nN = %d",
                                             display_name, as.integer(n_val))
      }
    }
  }

  # Display attributes describe the table, not its layout, so they survive the
  # pivot; input_display_attrs() reads them for the title bands.
  for (a in c("label", "subtitle", "caption")) {
    v <- attr(df, a, exact = TRUE)
    if (!is.null(v)) attr(wide, a) <- v
  }
  wide
}

# `digits = d` read as a format: 2 is "number", any other number is that many
# decimals.
table_digits_format <- function(digits) {
  f <- table_format_state(digits)
  if (identical(f, 2L)) "number" else f
}

# The number format, as the chart's: "number" (up to two decimals), "compact",
# "percent", or 0 to 6 decimals as an integer. A number outside 0 to 6 is
# clamped; anything else reads as "number".
table_format_state <- function(format) {
  f <- unlist(format, use.names = FALSE)
  if (!length(f) || is.na(f[[1L]])) return("number")
  f <- f[[1L]]
  if (is.character(f) && f %in% c("number", "compact", "percent")) return(f)
  n <- suppressWarnings(as.numeric(f))
  if (length(n) == 1L && is.finite(n)) {
    return(as.integer(max(0, min(6, round(n)))))
  }
  "number"
}

# A numeric cell in the table's number format (table_format_state()), by the
# rules of inst/js/chart/format.js: a whole number `d` is exactly d decimals;
# "number" is up to two decimals, and a value that would round to 0 keeps two
# significant digits; "compact" is 1.2K / 3.4M / 5.6B; "percent" is a fraction
# x100 with one decimal. Cards, downloads and the report format through here
# too. Expects non-NA values.
dt_format_num <- function(v, format = "number", big.mark = "") {
  v <- as.numeric(v)
  dec <- function(x, d, drop0 = TRUE) {
    x <- round(x, d)
    # No "-0": a value that rounds to zero prints as 0.
    x[x == 0] <- 0
    formatC(x, format = "f", digits = d, drop0trailing = drop0,
            big.mark = big.mark)
  }
  num <- function(x) {
    out <- dec(x, 2L)
    tiny <- !is.na(x) & x != 0 & abs(x) < 0.005
    if (any(tiny)) {
      out[tiny] <- formatC(signif(x[tiny], 2L), format = "fg", digits = 2L)
    }
    out
  }
  format <- table_format_state(format)
  if (is.numeric(format)) return(dec(v, format, drop0 = FALSE))
  if (identical(format, "percent")) return(paste0(dec(v * 100, 1L), "%"))
  if (!identical(format, "compact")) return(num(v))
  out <- num(v)
  a <- abs(v)
  for (s in list(c(1e3, "K"), c(1e6, "M"), c(1e9, "B"))) {
    at <- !is.na(a) & a >= as.numeric(s[[1L]])
    out[at] <- paste0(dec(v[at] / as.numeric(s[[1L]]), 1L), s[[2L]])
  }
  out
}

# Appends " (unit)" to the headers of the numeric columns, leaving the stub and
# dot-prefixed columns alone. Returns `data` unchanged for an empty unit or a
# structured or non-data-frame input.
dt_unit_labels <- function(data, unit, stub = NULL) {
  unit <- chr_state(trimws(as.character(unlist(unit))))
  if (is.null(unit) || !is.data.frame(data) || dt_is_structured(data)) {
    return(data)
  }
  if (is.null(stub) || !stub %in% names(data)) {
    stub <- if (".label" %in% names(data)) ".label" else names(data)[1L]
  }
  cols <- setdiff(names(data), stub)
  cols <- cols[!startsWith(cols, ".")]
  cols <- cols[vapply(data[cols], is.numeric, logical(1L))]
  for (cn in cols) {
    lbl <- dt_col_label(data[[cn]], cn) %||% cn
    attr(data[[cn]], "label") <- paste0(lbl, " (", unit, ")")
  }
  data
}
