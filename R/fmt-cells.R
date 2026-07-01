# The `.fmt` cell-formatting engine.
#
# Tidy shapers (summary_table/describe, broom, frequencies, ...) emit plain
# NUMERIC columns plus a hidden per-row `.fmt` template column (a sibling of
# `.section_*` / `.var` / `.label` / `.indent`). The renderer turns those into
# display strings by pure interpolation: each `{col}` token in the template is
# replaced with that row's (rounded) value of `col`.
#
#   .fmt = "{n} ({pct}%)"               -> "38 (54%)"
#   .fmt = "{estimate} ({std.error})"   -> "0.05 (0.01)"
#   .fmt = "{median} ({q1}, {q3})"      -> "12 (8, 19)"
#
# No codebook of named formats, no domain knowledge: the template names the
# columns and carries the literal punctuation. Rounding stays OUT of the
# template (a `digits` arg / per-row `.digits` owns it). See
# dev/table-and-chart-architecture.md ("The `.fmt` convention").

#' Format one numeric value for display
#'
#' Fixed-decimal display (trailing zeros KEPT — `65` at 1 dp renders
#' `65.0`), matching the pharma fixed-precision convention the shapers
#' produced via `sprintf("%.1f", ...)`. Non-numerics pass through.
#' @noRd
fmt_num <- function(x, digits) {
  if (is.numeric(x)) {
    formatC(round(x, digits), format = "f", digits = digits, drop0trailing = FALSE)
  } else {
    as.character(x)
  }
}

#' Interpolate per-row `.fmt` templates into display cells
#'
#' For each row, replaces `{col}` tokens in the `.fmt` template with that row's
#' value of `col`, rounded to `digits` (or to a per-row `.digits` column when
#' present). A token MAY carry its own precision as `{col:N}` (e.g.
#' `"{mean:1} ({sd:2})"`), which overrides both `.digits` and `digits` for that
#' one token — needed when a single cell mixes precisions (mean at 1 dp, SD at
#' 2 dp). Rows whose template is `NA`/`""` yield `NA` (the caller decides the
#' fallback). Tokens naming a missing column, or rows whose referenced value is
#' `NA`, render as `na`.
#'
#' @param df A data frame carrying a `.fmt` template column + the numeric
#'   columns it references.
#' @param fmt_col Name of the template column (default `".fmt"`).
#' @param digits Default decimal places when no `.digits` column is present.
#' @param na String used for `NA` cell values.
#' @return A character vector, one formatted cell per row.
#' @noRd
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
  for (i in seq_len(nrow(df))) {
    t <- tmpl[i]
    if (is.na(t) || !nzchar(t)) next
    d <- if (is.na(row_digits[i])) digits else row_digits[i]
    toks <- regmatches(t, gregexpr("\\{[^{}]+\\}", t))[[1]]
    cell <- t
    for (tok in unique(toks)) {
      spec <- sub("^\\{(.*)\\}$", "\\1", tok)
      # Optional per-token precision: "{col:N}" pins this token's digits.
      tok_digits <- d
      if (grepl(":", spec, fixed = TRUE)) {
        parts <- strsplit(spec, ":", fixed = TRUE)[[1]]
        spec <- parts[1]
        pd <- suppressWarnings(as.integer(parts[2]))
        if (!is.na(pd)) tok_digits <- pd
      }
      col <- spec
      val <- if (col %in% names(df)) df[[col]][i] else NA
      sval <- if (length(val) != 1L || is.na(val)) na else fmt_num(val, tok_digits)
      cell <- gsub(tok, sval, cell, fixed = TRUE)
    }
    out[i] <- cell
  }
  out
}

#' Format a tidy frame's cells, then optionally spread a group dimension to
#' columns (the "format-then-spread" display assembly).
#'
#' Order matters: combine the composite cell **per row** first (n & pct live on
#' the same row), then a plain spread on the single formatted-string column.
#' Spreading the raw numeric columns first would force messy column-pairing.
#'
#' @param df Tidy frame with a `.fmt` column.
#' @param group_col Optional column whose levels become the value columns. When
#'   `NULL`, no spread — the formatted cell is returned in `.cell` and the
#'   referenced numeric columns are dropped.
#' @param id_cols Row-identity columns to keep (default: the dotted columns
#'   `.section_*`/`.label`/`.indent`/`.strong`/`.emph` present in `df`).
#' @param digits,na Passed to `fmt_cells()`.
#' @return A data frame: row-identity columns + either a `.cell` column (no
#'   spread) or one formatted column per `group_col` level (spread).
#' @noRd
fmt_assemble <- function(df, group_col = NULL, id_cols = NULL,
                         digits = 2L, na = "NA") {
  stopifnot(is.data.frame(df))
  df[[".cell"]] <- fmt_cells(df, digits = digits, na = na)
  if (is.null(id_cols)) {
    dotted <- grep("^\\.(section|label|indent|strong|emph)", names(df), value = TRUE)
    id_cols <- setdiff(dotted, ".fmt")
  }
  if (is.null(group_col)) {
    keep <- c(id_cols, ".cell")
    return(df[, intersect(keep, names(df)), drop = FALSE])
  }
  # Plain spread on the single formatted-string column.
  long <- df[, c(id_cols, group_col, ".cell"), drop = FALSE]
  # pivot_wider() aborts ("spec$.name can't contain the empty string") when a
  # group level is "" or NA — which happens on real data (e.g. an untreated /
  # screen-fail subject with a blank TRT01A arm). Relabel those degenerate
  # levels so the spread can never throw at render time.
  gv <- as.character(long[[group_col]])
  blank <- is.na(gv) | !nzchar(trimws(gv))
  if (any(blank)) long[[group_col]][blank] <- "(Missing)"
  tidyr::pivot_wider(
    long,
    names_from = dplyr::all_of(group_col),
    values_from = ".cell"
  )
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
#' @noRd
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
  wide
}
