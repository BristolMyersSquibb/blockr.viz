#' Summary Table — Wide, Display-Shaped Multi-Variable Summary
#'
#' Aggregate a flat data.frame into a display-shaped **plain tibble**
#' with section structure encoded via well-known dotted columns
#' (`.section_1, ..., .section_k, .var, .label`). Designed as a
#' wide-first sibling to [pivot_table()], covering the
#' "list of variables by Y" pattern rather than the
#' "one measurement across two dimensions" pattern.
#'
#' Unlike a pivot table, each `vars` entry contributes rows shaped
#' by the variable's type and the chosen `stats` preset:
#'
#' - **numeric** under `stats = "compact"` → one row per variable,
#'   cell value `"Mean (SD)"`.
#' - **numeric** under `stats = "expanded"` → six rows per variable,
#'   one per stat (N, Mean, SD, Median, Q1 Q3, Min Max).
#' - **categorical** → one row per level, cell value `"n (p%)"`.
#' - **logical** → one row per variable, cell value `"n (p%)"` for
#'   the TRUE count. The FALSE row is suppressed — this matches
#'   pharma flag-variable conventions (TEAE, SAE, etc.).
#'
#' Output columns:
#'
#' - `.section_1, ..., .section_k` — section columns (dotted,
#'   block-internal) holding values from user-provided `sections`.
#'   Present iff `length(sections) > 0`. Each carries a `label`
#'   attribute pulled from the original column (ADaM label if
#'   present, else the original column name) for renderer display.
#' - `.var` — synthetic variable-name column. Present iff
#'   `length(vars) > 1`. Holds strings from `vars`.
#' - `.label` — innermost row label. Stat name for numeric vars
#'   ("N", "Mean", "SD", ...), level name for categoricals,
#'   variable name for logicals.
#' - Data columns named from `by` cell-keys (pipe-delimited for
#'   length-2 `by`). Plus an overall column (`"Total"` by default)
#'   when `add_overall = TRUE`.
#'
#' @param data A flat data.frame.
#' @param vars Character vector of variables to summarise. Each
#'   becomes a section in the output.
#' @param sections Optional character vector of outer section
#'   columns from `data`. Used for nested row-side hierarchy (e.g.
#'   `sections = "AESOC", vars = "AEDECOD"`).
#' @param by Character vector of length 0–2. Values become column
#'   headers. Length-2 produces pipe-delimited nested headers
#'   consumable by `gt::tab_spanner_delim("|")`.
#' @param stats One of `"compact"` (default) or `"expanded"`.
#'   See Details.
#' @param add_overall Logical. If TRUE, append a final column
#'   computed on the unsplit data (ignoring `by`). Matches
#'   `gtsummary::add_overall()`.
#' @param overall_label Character. Column label for the overall
#'   column when `add_overall = TRUE`. Default `"Total"` — matches
#'   all in-scope CSR numeric-summary workflows. gtsummary users
#'   can set `overall_label = "Overall"` to match that convention.
#' @param subject_var Optional subject-identifier column for
#'   distinct-subject counts (for categorical `n_pct`). If NULL,
#'   rows are counted instead.
#'
#' @return A plain `tibble` (NOT a `grouped_df`) with the dotted
#'   column convention described above. Renderer-agnostic: consumed
#'   by `gt_table_block` via regex on column names.
#'
#' @examples
#' if (FALSE) {
#'   # Simple demographics
#'   summary_table(
#'     iris,
#'     vars = c("Sepal.Length", "Species"),
#'     by = character(),
#'     stats = "compact"
#'   )
#'
#'   # Expanded numeric layout
#'   summary_table(
#'     mtcars,
#'     vars = c("mpg", "hp"),
#'     by = "cyl",
#'     stats = "expanded",
#'     add_overall = TRUE
#'   )
#' }
#'
#' @export
summary_table <- function(data,
                          vars = character(),
                          sections = character(),
                          by = character(),
                          stats = "compact",
                          add_overall = FALSE,
                          overall_label = "Total",
                          subject_var = NULL) {
  stopifnot(is.data.frame(data))
  vars <- as.character(vars)
  sections <- as.character(sections)
  by <- as.character(by)

  if (length(vars) == 0L) {
    stop("summary_table() requires at least one variable in `vars`.")
  }
  if (length(by) > 2L) {
    stop("summary_table() supports at most 2 `by` dimensions. ",
         "Got ", length(by), ".")
  }
  if (!stats %in% c("compact", "expanded")) {
    stop("`stats` must be one of \"compact\" or \"expanded\". ",
         "Got \"", stats, "\".")
  }

  missing_cols <- setdiff(c(vars, sections, by, subject_var), names(data))
  if (length(missing_cols)) {
    stop("Columns not found in data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check for reuse of columns across the three slots
  overlap_vs <- intersect(vars, sections)
  overlap_vb <- intersect(vars, by)
  overlap_sb <- intersect(sections, by)
  if (length(c(overlap_vs, overlap_vb, overlap_sb))) {
    stop("Columns used in multiple slots: ",
         paste(unique(c(overlap_vs, overlap_vb, overlap_sb)), collapse = ", "))
  }

  # Check block-internal namespace collision in the input data
  input_cols <- names(data)
  bad <- c(
    intersect(c(".var", ".label"), input_cols),
    grep("^\\.section_\\d+$", input_cols, value = TRUE)
  )
  if (length(bad)) {
    stop("Input data contains columns in the block-internal namespace: ",
         paste(bad, collapse = ", "),
         ". Rename or drop these columns upstream.")
  }

  # Capture labels from sections columns for later round-trip
  section_labels <- vapply(sections, function(col) {
    lbl <- attr(data[[col]], "label")
    if (is.null(lbl) || !nzchar(lbl)) col else as.character(lbl)
  }, character(1))

  # Compute per-var frame
  per_var <- lapply(vars, function(v) {
    compute_one_var(
      data = data,
      var = v,
      stats = stats,
      sections = sections,
      by = by,
      add_overall = add_overall,
      overall_label = overall_label,
      subject_var = subject_var
    )
  })

  # Bind with a .var marker if length(vars) > 1
  if (length(vars) > 1L) {
    for (i in seq_along(per_var)) {
      per_var[[i]]$.var <- vars[i]
    }
  }

  # Align columns: first .section_*, then .var (if present), then .label,
  # then data cells in consistent order
  out <- bind_per_var_frames(per_var, sections = sections,
                             has_var = length(vars) > 1L)

  # Rename user `sections` columns to .section_1, .section_2, ...
  if (length(sections) > 0L) {
    for (i in seq_along(sections)) {
      src <- sections[i]
      dst <- paste0(".section_", i)
      names(out)[names(out) == src] <- dst
      attr(out[[dst]], "label") <- section_labels[i]
    }
  }

  # Reorder: .section_*, .var, .label, then data cells
  front_order <- c(
    if (length(sections) > 0L) paste0(".section_", seq_along(sections)) else character(),
    if (length(vars) > 1L) ".var" else character(),
    ".label"
  )
  front_order <- intersect(front_order, names(out))
  back <- setdiff(names(out), front_order)
  out <- out[, c(front_order, back), drop = FALSE]

  # Attach column label attributes with per-column subject count.
  # `gt::gt()` reads `attr(col, "label")` natively, so this is the
  # cleanest way to surface "KarXT\nN = 97" style headers without
  # coupling the renderer to the data shape. The shaper knows the
  # denominators because it computed the percentages against them.
  out <- attach_column_labels(
    out,
    data = data,
    sections = sections,
    by = by,
    subject_var = subject_var,
    overall_label = overall_label,
    add_overall = add_overall
  )

  tibble::as_tibble(out)
}

# =============================================================================
# Internal: compute per-column subject counts and attach as column labels
# =============================================================================

#' @noRd
attach_column_labels <- function(out, data, sections, by, subject_var,
                                 overall_label, add_overall) {
  # Identify the data cell columns (everything that isn't a dotted
  # block-internal column like .section_N / .var / .label)
  dotted <- grepl("^\\.(section_\\d+|var|label)$", names(out))
  data_cols <- names(out)[!dotted]

  if (length(data_cols) == 0L) return(out)

  # Helper: count distinct subjects (or rows) in the input data
  # matching a given column-key value. Must match how
  # pivot_cells() builds column names.
  compute_col_n <- function(col_key) {
    if (length(by) == 0L) {
      # No by, single column named "Overall"
      n_distinct_in(data, subject_var)
    } else if (length(by) == 1L) {
      matching <- !is.na(data[[by]]) &
                  as.character(data[[by]]) == col_key
      n_distinct_in(data[matching, , drop = FALSE], subject_var)
    } else {
      # length(by) == 2 — col_key is "a|b"
      parts <- strsplit(col_key, "|", fixed = TRUE)[[1]]
      if (length(parts) != 2L) return(NA_integer_)
      matching <- !is.na(data[[by[1]]]) & !is.na(data[[by[2]]]) &
                  as.character(data[[by[1]]]) == parts[1] &
                  as.character(data[[by[2]]]) == parts[2]
      n_distinct_in(data[matching, , drop = FALSE], subject_var)
    }
  }

  for (cn in data_cols) {
    n_val <- if (isTRUE(add_overall) && identical(cn, overall_label)) {
      n_distinct_in(data, subject_var)
    } else {
      compute_col_n(cn)
    }
    if (is.na(n_val) || is.null(n_val)) {
      attr(out[[cn]], "label") <- cn
    } else {
      attr(out[[cn]], "label") <- sprintf("<strong>%s</strong><br>N = %d",
                                           cn, as.integer(n_val))
    }
  }

  out
}

#' @noRd
n_distinct_in <- function(data, subject_var) {
  if (!is.null(subject_var) && subject_var %in% names(data)) {
    dplyr::n_distinct(data[[subject_var]])
  } else {
    nrow(data)
  }
}

# =============================================================================
# Internal: compute per-var frame
# =============================================================================

#' @noRd
compute_one_var <- function(data, var, stats, sections, by,
                            add_overall, overall_label, subject_var) {
  col <- data[[var]]

  if (is.logical(col)) {
    compute_logical_var(data, var, sections, by,
                        add_overall, overall_label, subject_var)
  } else if (is.numeric(col)) {
    if (stats == "compact") {
      compute_numeric_compact(data, var, sections, by,
                              add_overall, overall_label)
    } else {
      compute_numeric_expanded(data, var, sections, by,
                               add_overall, overall_label)
    }
  } else {
    compute_categorical_var(data, var, sections, by,
                            add_overall, overall_label, subject_var)
  }
}

# ---- Numeric compact ----

#' @noRd
compute_numeric_compact <- function(data, var, sections, by,
                                    add_overall, overall_label) {
  stats_df <- compute_numeric_stats(data, var, group_vars = c(sections, by))
  stats_df$value <- ifelse(
    stats_df$n > 0,
    sprintf("%.1f (%.2f)", stats_df$mean, stats_df$sd),
    "-"
  )

  wide <- pivot_cells(stats_df, id_cols = sections, by = by, value_col = "value")
  wide$.label <- "Mean (SD)"

  if (isTRUE(add_overall)) {
    ov <- compute_numeric_stats(data, var, group_vars = sections)
    ov$value <- ifelse(
      ov$n > 0,
      sprintf("%.1f (%.2f)", ov$mean, ov$sd),
      "-"
    )
    wide[[overall_label]] <- overall_join(wide, ov, sections = sections)
  }

  reorder_cols(wide, sections, has_label = TRUE)
}

# ---- Numeric expanded — 6 rows per var matching pharma SAP ----

#' @noRd
compute_numeric_expanded <- function(data, var, sections, by,
                                     add_overall, overall_label) {
  stats_df <- compute_numeric_stats(data, var, group_vars = c(sections, by))

  stat_rows <- list(
    list(lbl = "N",        fmt = function(r) sprintf("%d", as.integer(r$n))),
    list(lbl = "Mean",     fmt = function(r) sprintf("%.1f", r$mean)),
    list(lbl = "SD",       fmt = function(r) sprintf("%.2f", r$sd)),
    list(lbl = "Median",   fmt = function(r) sprintf("%.1f", r$median)),
    list(lbl = "Q1, Q3",   fmt = function(r) sprintf("%.1f, %.1f", r$q1, r$q3)),
    list(lbl = "Min, Max", fmt = function(r) sprintf("%.1f, %.1f", r$min, r$max))
  )

  # For each stat, format the column, then pivot wide on by
  wide_list <- lapply(stat_rows, function(s) {
    tmp <- stats_df
    tmp$value <- vapply(seq_len(nrow(tmp)), function(i) {
      if (tmp$n[i] == 0) return("-")
      s$fmt(lapply(tmp[i, ], identity))
    }, character(1))

    w <- pivot_cells(tmp, id_cols = sections, by = by, value_col = "value")
    w$.label <- s$lbl

    if (isTRUE(add_overall)) {
      ov <- compute_numeric_stats(data, var, group_vars = sections)
      ov$value <- vapply(seq_len(nrow(ov)), function(i) {
        if (ov$n[i] == 0) return("-")
        s$fmt(lapply(ov[i, ], identity))
      }, character(1))
      w[[overall_label]] <- overall_join(w, ov, sections = sections)
    }

    reorder_cols(w, sections, has_label = TRUE)
  })

  do.call(rbind, wide_list)
}

# ---- Categorical ----

#' @noRd
compute_categorical_var <- function(data, var, sections, by,
                                    add_overall, overall_label, subject_var) {
  group_vars <- c(sections, by)

  # Denominator per (sections × by)
  denom <- compute_denom(data, group_vars, subject_var)

  # Levels
  levels_v <- sort(unique(as.character(data[[var]][!is.na(data[[var]])])))
  if (length(levels_v) == 0L) {
    # Empty — return a placeholder row
    out <- data.frame(.label = NA_character_, stringsAsFactors = FALSE)
    if (length(sections) > 0L) {
      for (s in sections) out[[s]] <- NA
    }
    return(out)
  }

  per_level <- lapply(levels_v, function(lv) {
    sub <- data[!is.na(data[[var]]) & as.character(data[[var]]) == lv, , drop = FALSE]
    num <- compute_denom(sub, group_vars, subject_var)
    names(num)[names(num) == "N"] <- "n"
    num$.level <- lv
    num
  })
  num_all <- do.call(rbind, per_level)

  if (length(group_vars) == 0L) {
    stats_df <- cbind(num_all, denom)
  } else {
    stats_df <- dplyr::left_join(num_all, denom, by = group_vars)
  }

  stats_df$pct <- ifelse(stats_df$N > 0, stats_df$n / stats_df$N * 100, 0)
  stats_df$value <- sprintf("%d (%.1f%%)",
                            as.integer(stats_df$n),
                            stats_df$pct)

  wide <- pivot_cells(
    stats_df,
    id_cols = c(sections, ".level"),
    by = by,
    value_col = "value"
  )

  if (isTRUE(add_overall)) {
    # Overall denominator
    ov_denom <- compute_denom(data, sections, subject_var)
    ov_per_level <- lapply(levels_v, function(lv) {
      sub <- data[!is.na(data[[var]]) & as.character(data[[var]]) == lv, , drop = FALSE]
      n <- compute_denom(sub, sections, subject_var)
      names(n)[names(n) == "N"] <- "n"
      n$.level <- lv
      n
    })
    ov_num <- do.call(rbind, ov_per_level)
    ov_stats <- if (length(sections) == 0L) {
      cbind(ov_num, ov_denom)
    } else {
      dplyr::left_join(ov_num, ov_denom, by = sections)
    }
    ov_stats$value <- sprintf("%d (%.1f%%)",
                              as.integer(ov_stats$n),
                              ifelse(ov_stats$N > 0,
                                     ov_stats$n / ov_stats$N * 100, 0))
    # Key on .level while it's still present on wide
    wide[[overall_label]] <- overall_join_by_level(
      wide, ov_stats, sections = sections
    )
  }

  # Rename .level → .label after the overall join (which needed .level)
  wide$.label <- wide$.level
  wide$.level <- NULL

  reorder_cols(wide, sections, has_label = TRUE)
}

# ---- Logical (one-row TRUE only, for pharma flag variables) ----

#' @noRd
compute_logical_var <- function(data, var, sections, by,
                                add_overall, overall_label, subject_var) {
  group_vars <- c(sections, by)

  denom <- compute_denom(data, group_vars, subject_var)

  # Numerator: subjects where flag == TRUE
  sub <- data[!is.na(data[[var]]) & data[[var]], , drop = FALSE]
  num <- compute_denom(sub, group_vars, subject_var)
  names(num)[names(num) == "N"] <- "n"

  if (length(group_vars) == 0L) {
    stats_df <- cbind(num, denom)
  } else {
    # left_join from denom (to keep all cells even if num is empty for some)
    stats_df <- dplyr::left_join(denom, num, by = group_vars)
    stats_df$n[is.na(stats_df$n)] <- 0
  }

  stats_df$pct <- ifelse(stats_df$N > 0, stats_df$n / stats_df$N * 100, 0)
  stats_df$value <- sprintf("%d (%.1f%%)",
                            as.integer(stats_df$n),
                            stats_df$pct)

  wide <- pivot_cells(stats_df, id_cols = sections, by = by, value_col = "value")
  wide$.label <- var  # variable name as the row label

  if (isTRUE(add_overall)) {
    ov_denom <- compute_denom(data, sections, subject_var)
    ov_num <- compute_denom(sub, sections, subject_var)
    names(ov_num)[names(ov_num) == "N"] <- "n"
    ov_stats <- if (length(sections) == 0L) {
      cbind(ov_num, ov_denom)
    } else {
      dplyr::left_join(ov_denom, ov_num, by = sections)
    }
    ov_stats$n[is.na(ov_stats$n)] <- 0
    ov_stats$value <- sprintf("%d (%.1f%%)",
                              as.integer(ov_stats$n),
                              ifelse(ov_stats$N > 0,
                                     ov_stats$n / ov_stats$N * 100, 0))
    wide[[overall_label]] <- overall_join(wide, ov_stats, sections = sections)
  }

  reorder_cols(wide, sections, has_label = TRUE)
}

# =============================================================================
# Internal helpers
# =============================================================================

#' @noRd
compute_numeric_stats <- function(data, var, group_vars) {
  if (length(group_vars) == 0L) {
    v <- data[[var]]
    tibble::tibble(
      n      = sum(!is.na(v)),
      mean   = mean(v, na.rm = TRUE),
      sd     = stats::sd(v, na.rm = TRUE),
      median = stats::median(v, na.rm = TRUE),
      q1     = suppressWarnings(stats::quantile(v, 0.25, na.rm = TRUE, names = FALSE)),
      q3     = suppressWarnings(stats::quantile(v, 0.75, na.rm = TRUE, names = FALSE)),
      min    = suppressWarnings(min(v, na.rm = TRUE)),
      max    = suppressWarnings(max(v, na.rm = TRUE))
    )
  } else {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        n      = sum(!is.na(.data[[var]])),
        mean   = mean(.data[[var]], na.rm = TRUE),
        sd     = stats::sd(.data[[var]], na.rm = TRUE),
        median = stats::median(.data[[var]], na.rm = TRUE),
        q1     = suppressWarnings(stats::quantile(.data[[var]], 0.25, na.rm = TRUE, names = FALSE)),
        q3     = suppressWarnings(stats::quantile(.data[[var]], 0.75, na.rm = TRUE, names = FALSE)),
        min    = suppressWarnings(min(.data[[var]], na.rm = TRUE)),
        max    = suppressWarnings(max(.data[[var]], na.rm = TRUE)),
        .groups = "drop"
      )
  }
}

#' @noRd
compute_denom <- function(data, group_vars, subject_var) {
  if (length(group_vars) == 0L) {
    N <- if (!is.null(subject_var)) {
      dplyr::n_distinct(data[[subject_var]])
    } else {
      nrow(data)
    }
    tibble::tibble(N = N)
  } else {
    if (!is.null(subject_var)) {
      data |>
        dplyr::distinct(dplyr::across(dplyr::all_of(c(subject_var, group_vars)))) |>
        dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "N")
    } else {
      data |>
        dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "N")
    }
  }
}

#' @noRd
pivot_cells <- function(stats_df, id_cols, by, value_col) {
  id_cols <- intersect(id_cols, names(stats_df))

  if (length(by) == 0L) {
    # No pivot — just id_cols + value_col
    cols <- c(id_cols, value_col)
    out <- stats_df[, cols, drop = FALSE]
    names(out)[names(out) == value_col] <- "Overall"
    return(out)
  }

  if (length(by) == 1L) {
    stats_df[[by]] <- as.character(stats_df[[by]])
    stats_df |>
      dplyr::select(dplyr::all_of(c(id_cols, by, value_col))) |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(id_cols),
        names_from = dplyr::all_of(by),
        values_from = dplyr::all_of(value_col),
        values_fill = NA_character_
      )
  } else {
    # length(by) == 2 — pipe-delimited composite column keys
    stats_df$.colkey <- paste(stats_df[[by[1]]], stats_df[[by[2]]], sep = "|")
    stats_df |>
      dplyr::select(dplyr::all_of(c(id_cols, ".colkey", value_col))) |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(id_cols),
        names_from = ".colkey",
        values_from = dplyr::all_of(value_col),
        values_fill = NA_character_
      )
  }
}

#' @noRd
overall_join <- function(wide, ov, sections) {
  # ov has one row per (sections) combination with a `value` column
  if (length(sections) == 0L) {
    rep(ov$value[1], nrow(wide))
  } else {
    m <- match(
      do.call(paste, c(wide[sections], sep = "|")),
      do.call(paste, c(ov[sections], sep = "|"))
    )
    ov$value[m]
  }
}

#' @noRd
overall_join_by_level <- function(wide, ov_stats, sections) {
  # ov_stats has one row per (sections, .level) with a `value` column
  key_cols <- c(sections, ".level")
  key_wide <- do.call(paste, c(wide[intersect(key_cols, names(wide))], sep = "|"))
  key_ov   <- do.call(paste, c(ov_stats[intersect(key_cols, names(ov_stats))], sep = "|"))
  m <- match(key_wide, key_ov)
  ov_stats$value[m]
}

#' @noRd
reorder_cols <- function(df, sections, has_label = TRUE) {
  # Put sections first, .label next, then data cells
  front <- intersect(c(sections, ".label"), names(df))
  back <- setdiff(names(df), front)
  df[, c(front, back), drop = FALSE]
}

#' @noRd
bind_per_var_frames <- function(per_var, sections, has_var) {
  # Align columns across frames (different vars may produce different
  # column sets when categorical levels differ)
  all_names <- unique(unlist(lapply(per_var, names)))

  # Canonical front order: sections, .var, .label; back stays in first-seen order
  front <- intersect(c(sections, ".var", ".label"), all_names)
  back <- setdiff(all_names, front)
  all_names <- c(front, back)

  per_var <- lapply(per_var, function(df) {
    missing <- setdiff(all_names, names(df))
    for (mc in missing) df[[mc]] <- NA_character_
    df[, all_names, drop = FALSE]
  })

  do.call(rbind, per_var)
}

# dplyr uses `.data` pronoun; silence R CMD check warnings
utils::globalVariables(c(".data", ".colkey", ".level", ".var", ".label"))
