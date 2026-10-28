# The statistics of the Table 1 engine (summary_table_long(),
# R/summary-table.R): per variable, its rows of numbers by group, the
# denominators, the statistic catalog, and the nesting of two categoricals
# into one hierarchy.

#' @noRd
compute_group_n <- function(data, by, subject_var, groups,
                            overall_label, add_overall) {
  compute_col_n <- function(col_key) {
    if (length(by) == 0L) {
      n_distinct_in(data, subject_var)
    } else if (length(by) == 1L) {
      matching <- !is.na(data[[by]]) & as.character(data[[by]]) == col_key
      n_distinct_in(data[matching, , drop = FALSE], subject_var)
    } else {
      parts <- strsplit(col_key, "||", fixed = TRUE)[[1]]
      if (length(parts) != 2L) return(NA_integer_)
      matching <- !is.na(data[[by[1]]]) & !is.na(data[[by[2]]]) &
        as.character(data[[by[1]]]) == parts[1] &
        as.character(data[[by[2]]]) == parts[2]
      n_distinct_in(data[matching, , drop = FALSE], subject_var)
    }
  }

  groups <- groups[!is.na(groups)]
  out <- vapply(groups, function(g) {
    if (isTRUE(add_overall) && identical(g, overall_label)) {
      as.numeric(n_distinct_in(data, subject_var))
    } else {
      as.numeric(compute_col_n(g))
    }
  }, numeric(1))
  stats::setNames(out, groups)
}

#' Pick the display label for a variable section header.
#' @noRd
var_header_label <- function(col, fallback) {
  lbl <- attr(col, "label")
  if (is.null(lbl) || !is.character(lbl) || !nzchar(lbl)) {
    return(fallback)
  }
  as.character(lbl)
}

#' @noRd
n_distinct_in <- function(data, subject_var) {
  if (!is.null(subject_var) && subject_var %in% names(data)) {
    dplyr::n_distinct(data[[subject_var]], na.rm = TRUE)
  } else {
    nrow(data)
  }
}

# =============================================================================
# Internal: long group keys
# =============================================================================

#' Build the `.group` key vector for a stats frame and append the
#' overall rows (a constant group keyed by `overall_label`) when asked.
#' `stats_df` must carry the `by` columns; the returned frame gains a
#' single `.group` chr column and drops the raw `by` columns.
#' @noRd
add_group_col <- function(stats_df, by) {
  if (length(by) == 0L) {
    stats_df$.group <- "Overall"
  } else if (length(by) == 1L) {
    stats_df$.group <- as.character(stats_df[[by]])
    stats_df[[by]] <- NULL
  } else {
    stats_df$.group <- paste(as.character(stats_df[[by[1]]]),
                             as.character(stats_df[[by[2]]]), sep = "||")
    stats_df[[by[1]]] <- NULL
    stats_df[[by[2]]] <- NULL
  }
  stats_df
}

# =============================================================================
# Internal: numeric stat catalog (`stats` vocabulary)
# =============================================================================

# Each entry maps a stat key to the row label and `.fmt` template it emits
# for numeric variables. Catalog order is the order of the gear's option list;
# the rows follow the order the stats are given in.
#
# **`composer` is the standard for clinical table numbers, and this catalog does
# not exceed it.** composer is BMS's own CSR-output package and computes through
# `cards`/`cardx`; its stat registry (`composer/R/stat-registry.R`) and row
# labels are the vocabulary. So: `Q1`/`Q3` rather than tern's
# "25% and 75%-ile", R's default `quantile()` type rather than tern's type 2,
# `SD` at one decimal to match composer's `{mean:xx.x} ({sd:xx.x})`, and **no
# confidence intervals** -- composer has none, and a statistic the standard does
# not carry is one we do not offer. If in doubt, check composer.
#
# Rationale and the full convention:
# `blockr.sandbox/dev/summary-statistics-conventions.md`.
SUMMARY_STATS_CATALOG <- list(
  n            = list(label = "N",               fmt = "{n:0}"),
  n_pct        = list(label = "n (%)",           fmt = "{n:0} ({pct:1}%)"),
  mean         = list(label = "Mean",            fmt = "{mean:1}"),
  sd           = list(label = "SD",              fmt = "{sd:1}"),
  mean_sd      = list(label = "Mean (SD)",       fmt = "{mean:1} ({sd:1})"),
  median       = list(label = "Median",          fmt = "{median:1}"),
  median_q1_q3 = list(label = "Median (Q1, Q3)", fmt = "{median:1} ({q1:1}, {q3:1})"),
  q1_q3        = list(label = "Q1, Q3",          fmt = "{q1:1}, {q3:1}"),
  min_max      = list(label = "Min, Max",        fmt = "{min:1}, {max:1}")
)

# Legacy preset values accepted for back-compat with boards serialized
# before `stats` became a key vector.
SUMMARY_STATS_LEGACY <- list(
  compact  = "mean_sd",
  expanded = c("n", "mean", "sd", "median", "q1_q3", "min_max")
)

#' Normalize a `stats` value to a deduplicated vector of catalog keys, in
#' the order given. Accepts the legacy `"compact"` / `"expanded"` scalars.
#' @noRd
normalize_summary_stats <- function(stats) {
  stats <- as.character(stats)
  if (length(stats) == 1L && stats %in% names(SUMMARY_STATS_LEGACY)) {
    return(SUMMARY_STATS_LEGACY[[stats]])
  }
  bad <- setdiff(stats, names(SUMMARY_STATS_CATALOG))
  if (length(bad)) {
    stop("Unknown `stats` value(s): ",
         paste0("\"", bad, "\"", collapse = ", "),
         ". Valid keys: ",
         paste0("\"", names(SUMMARY_STATS_CATALOG), "\"", collapse = ", "),
         " (or legacy \"compact\" / \"expanded\").")
  }
  if (length(stats) == 0L) {
    stop("summary_table() requires at least one `stats` key.")
  }
  unique(stats)
}

# =============================================================================
# Internal: compute per-var long frame
# =============================================================================

#' @noRd
compute_one_var <- function(data, var, stats, sections, by,
                            add_overall, overall_label, subject_var) {
  col <- data[[var]]

  if (is.logical(col)) {
    compute_logical_var(data, var, sections, by,
                        add_overall, overall_label, subject_var)
  } else if (is.numeric(col)) {
    compute_numeric_rows(data, var, stats, sections, by,
                         add_overall, overall_label)
  } else {
    compute_categorical_var(data, var, sections, by,
                            add_overall, overall_label, subject_var)
  }
}

# ---- Numeric -- one row per selected stat key ----

#' @noRd
compute_numeric_rows <- function(data, var, stats, sections, by,
                                 add_overall, overall_label) {
  stats_df <- compute_numeric_stats(data, var, group_vars = c(sections, by))
  base <- add_group_col(stats_df, by)

  if (isTRUE(add_overall)) {
    ov <- compute_numeric_stats(data, var, group_vars = sections)
    ov$.group <- overall_label
    base <- rbind_long(base, ov)
  }

  # A single stat renders as one un-indented row per variable (the old
  # "compact" shape); several stats render as one indented row each (the
  # old "expanded" shape, headed by the variable when there are 2+ vars).
  indent <- if (length(stats) == 1L) 0L else 1L

  frames <- lapply(stats, function(key) {
    entry <- SUMMARY_STATS_CATALOG[[key]]
    f <- base
    f$.label <- entry$label
    f$.indent <- indent
    f$.fmt <- entry$fmt
    # Empty cells (n == 0): blank template -> NA cell at render time.
    f$.fmt[!is.na(f$n) & f$n == 0] <- NA_character_
    f
  })

  long <- do.call(rbind_long, frames)
  # Stat rows are not data values: .variable_level stays NA (the row makes no
  # "source rows where var == level" claim -- see the annotated-df contract).
  long$.variable <- var
  long$.variable_level <- NA_character_
  reorder_long(long, sections)
}

# ---- Categorical ----

#' @noRd
compute_categorical_var <- function(data, var, sections, by,
                                    add_overall, overall_label, subject_var) {
  group_vars <- c(sections, by)
  denom <- compute_denom(data, group_vars, subject_var)

  levels_v <- sort(unique(as.character(data[[var]][!is.na(data[[var]])])))
  if (length(levels_v) == 0L) {
    out <- data.frame(.label = NA_character_, .indent = 1L,
                      .variable = var, .variable_level = NA_character_,
                      .group = NA_character_, .fmt = NA_character_,
                      n = NA_real_, pct = NA_real_,
                      stringsAsFactors = FALSE)
    if (length(sections) > 0L) {
      for (s in sections) out[[s]] <- NA
    }
    return(out)
  }

  # Count per level in one grouped operation (not one dplyr pipeline per
  # level, which is O(levels) and dominates on high-cardinality vars).
  # Exclude NA subjects to match n_distinct(na.rm = TRUE) semantics.
  data_nona <- data[!is.na(data[[var]]), , drop = FALSE]
  if (!is.null(subject_var)) {
    data_nona <- data_nona[!is.na(data_nona[[subject_var]]), , drop = FALSE]
  }
  num_all <- compute_denom(data_nona, c(group_vars, var), subject_var)
  names(num_all)[names(num_all) == "N"] <- "n"
  names(num_all)[names(num_all) == var] <- ".level"
  num_all$.level <- as.character(num_all$.level)

  if (length(group_vars) == 0L) {
    stats_df <- cbind(num_all, denom)
  } else {
    stats_df <- dplyr::left_join(num_all, denom, by = group_vars)
  }
  stats_df$pct <- ifelse(stats_df$N > 0, stats_df$n / stats_df$N * 100, 0)
  stats_df$N <- NULL

  long <- add_group_col(stats_df, by)
  long$.label <- long$.level
  long$.level <- NULL
  long$.indent <- 1L
  long$.fmt <- "{n:0} ({pct:1}%)"

  if (isTRUE(add_overall)) {
    ov_denom <- compute_denom(data, sections, subject_var)
    ov_num <- compute_denom(data_nona, c(sections, var), subject_var)
    names(ov_num)[names(ov_num) == "N"] <- "n"
    names(ov_num)[names(ov_num) == var] <- ".level"
    ov_num$.level <- as.character(ov_num$.level)
    ov_stats <- if (length(sections) == 0L) {
      cbind(ov_num, ov_denom)
    } else {
      dplyr::left_join(ov_num, ov_denom, by = sections)
    }
    ov_stats$pct <- ifelse(ov_stats$N > 0, ov_stats$n / ov_stats$N * 100, 0)
    ov_stats$N <- NULL
    ov_stats$.group <- overall_label
    ov_stats$.label <- ov_stats$.level
    ov_stats$.level <- NULL
    ov_stats$.indent <- 1L
    ov_stats$.fmt <- "{n:0} ({pct:1}%)"
    long <- rbind_long(long, ov_stats)
  }

  # Every categorical row is a level row: the label IS the (character-coerced)
  # data value, so it doubles as .variable_level.
  long$.variable <- var
  long$.variable_level <- long$.label
  reorder_long(long, sections)
}

# ---- Logical (one-row TRUE only, for pharma flag variables) ----

#' @noRd
compute_logical_var <- function(data, var, sections, by,
                                add_overall, overall_label, subject_var) {
  group_vars <- c(sections, by)
  denom <- compute_denom(data, group_vars, subject_var)

  sub <- data[!is.na(data[[var]]) & data[[var]], , drop = FALSE]
  num <- compute_denom(sub, group_vars, subject_var)
  names(num)[names(num) == "N"] <- "n"

  if (length(group_vars) == 0L) {
    stats_df <- cbind(num, denom)
  } else {
    stats_df <- dplyr::left_join(denom, num, by = group_vars)
    stats_df$n[is.na(stats_df$n)] <- 0
  }
  stats_df$pct <- ifelse(stats_df$N > 0, stats_df$n / stats_df$N * 100, 0)
  stats_df$N <- NULL

  long <- add_group_col(stats_df, by)
  long$.label <- var
  long$.indent <- 0L
  long$.fmt <- "{n:0} ({pct:1}%)"

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
    ov_stats$pct <- ifelse(ov_stats$N > 0, ov_stats$n / ov_stats$N * 100, 0)
    ov_stats$N <- NULL
    ov_stats$.group <- overall_label
    ov_stats$.label <- var
    ov_stats$.indent <- 0L
    ov_stats$.fmt <- "{n:0} ({pct:1}%)"
    long <- rbind_long(long, ov_stats)
  }

  # The flag row counts TRUEs, so its level is the value "TRUE" (a drill on
  # it filters source rows where the flag is TRUE, via %in% coercion).
  long$.variable <- var
  long$.variable_level <- "TRUE"
  reorder_long(long, sections)
}

# =============================================================================
# Internal helpers
# =============================================================================

# Canonical numeric stat columns the `.fmt` templates reference.
SUMMARY_STAT_COLS <- c("n", "pct", "mean", "sd", "median", "q1", "q3",
                       "min", "max")

#' rbind two long frames, aligning the union of columns (filling missing
#' stat columns with NA). Used to append overall rows / stack stat rows.
#' @noRd
rbind_long <- function(...) {
  frames <- list(...)
  frames <- Filter(function(x) !is.null(x) && nrow(x) > 0L, frames)
  if (length(frames) == 0L) return(NULL)
  all_names <- unique(unlist(lapply(frames, names)))
  frames <- lapply(frames, function(df) {
    missing <- setdiff(all_names, names(df))
    for (mc in missing) {
      df[[mc]] <- if (mc %in% SUMMARY_STAT_COLS) NA_real_ else NA
    }
    df[, all_names, drop = FALSE]
  })
  do.call(rbind, frames)
}

#' Per-group numeric stats. `pct` is the share of non-missing rows in the
#' group (the `n_pct` denominator is always a row count -- a distinct-subject
#' denominator would not match the non-missing-value numerator).
#' @noRd
compute_numeric_stats <- function(data, var, group_vars) {
  if (length(group_vars) == 0L) {
    v <- data[[var]]
    tibble::tibble(
      n      = sum(!is.na(v)),
      pct    = if (length(v) > 0) sum(!is.na(v)) / length(v) * 100 else 0,
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
        pct    = sum(!is.na(.data[[var]])) / dplyr::n() * 100,
        mean   = mean(.data[[var]], na.rm = TRUE),
        sd     = stats::sd(.data[[var]], na.rm = TRUE),
        median = stats::median(.data[[var]], na.rm = TRUE),
        q1     = suppressWarnings(stats::quantile(.data[[var]], 0.25, na.rm = TRUE, names = FALSE)),
        q3     = suppressWarnings(stats::quantile(.data[[var]], 0.75, na.rm = TRUE, names = FALSE)),
        min    = suppressWarnings(min(.data[[var]], na.rm = TRUE)),
        max    = suppressWarnings(max(.data[[var]], na.rm = TRUE)),
        .groups = "drop"
      ) |>
      as.data.frame()
  }
}

#' @noRd
compute_denom <- function(data, group_vars, subject_var) {
  if (length(group_vars) == 0L) {
    N <- if (!is.null(subject_var)) {
      dplyr::n_distinct(data[[subject_var]], na.rm = TRUE)
    } else {
      nrow(data)
    }
    tibble::tibble(N = N)
  } else {
    res <- if (!is.null(subject_var)) {
      data |>
        dplyr::distinct(dplyr::across(dplyr::all_of(c(subject_var, group_vars)))) |>
        dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "N")
    } else {
      data |>
        dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "N")
    }
    as.data.frame(res)
  }
}

#' Reorder a long per-var frame: sections, .variable*, .label, .indent,
#' .group, .fmt, then numeric stat columns.
#' @noRd
reorder_long <- function(df, sections) {
  front <- intersect(c(sections, ".variable", ".variable_label",
                       ".variable_level", ".label", ".indent", ".strong",
                       ".group", ".fmt"), names(df))
  stats_present <- intersect(SUMMARY_STAT_COLS, names(df))
  rest <- setdiff(names(df), c(front, stats_present))
  df[, c(front, stats_present, rest), drop = FALSE]
}

#' @noRd
bind_per_var_frames <- function(per_var, sections) {
  all_names <- unique(unlist(lapply(per_var, names)))

  # Canonical front order: sections, .variable*, .label, .indent, .strong,
  # .group, .fmt
  front <- intersect(c(sections, ".variable", ".variable_label",
                       ".variable_level", ".label", ".indent", ".strong",
                       ".group", ".fmt"), all_names)
  stats_present <- intersect(SUMMARY_STAT_COLS, all_names)
  back <- setdiff(all_names, c(front, stats_present))
  all_names <- c(front, stats_present, back)

  per_var <- lapply(per_var, function(df) {
    missing <- setdiff(all_names, names(df))
    for (mc in missing) {
      df[[mc]] <- if (identical(mc, ".indent")) {
        0L
      } else if (identical(mc, ".strong")) {
        NA
      } else if (mc %in% SUMMARY_STAT_COLS) {
        NA_real_
      } else {
        NA_character_
      }
    }
    df[, all_names, drop = FALSE]
  })

  bound <- do.call(rbind, per_var)

  # Section-major order. The frames arrive one per VARIABLE, each carrying
  # every section value, so a plain rbind interleaves them: with sections =
  # "species" and three variables the outer level reads Adelie, Chinstrap,
  # Gentoo, Adelie, ... and the renderer -- which synthesizes headers from
  # runs of equal values -- emits the same section header three times.
  # `sections` is documented as the OUTER row hierarchy above the summarized
  # variables, so it has to be the outermost sort key; the row index breaks
  # ties, which keeps variable order within a section and parent/child order
  # within a variable.
  if (length(sections) > 0L && nrow(bound) > 0L) {
    keys <- lapply(sections, function(s) {
      v <- as.character(bound[[s]])
      factor(v, levels = unique(v), exclude = NULL)
    })
    ord <- do.call(order, c(keys, list(seq_len(nrow(bound)))))
    bound <- bound[ord, , drop = FALSE]
    rownames(bound) <- NULL
  }

  bound
}

# =============================================================================
# Hierarchy auto-nest (nest_hierarchies = TRUE)
# =============================================================================

#' Check the functional dependency inner -> outer in `data`.
#' @noRd
hierarchy_fd_holds <- function(data, outer, inner) {
  uniq <- unique(data[, c(outer, inner), drop = FALSE])
  uniq <- uniq[!is.na(uniq[[outer]]) & !is.na(uniq[[inner]]), , drop = FALSE]
  if (nrow(uniq) == 0L) return(FALSE)
  counts <- table(as.character(uniq[[inner]]))
  all(counts == 1L)
}

#' Greedy adjacent walk over `vars`, grouping FD-linked runs.
#' @noRd
group_vars_into_runs <- function(vars, data) {
  is_cat <- vapply(vars, function(v) {
    col <- data[[v]]
    !is.numeric(col) && !is.logical(col)
  }, logical(1))

  runs <- list()
  i <- 1L
  n <- length(vars)
  while (i <= n) {
    current <- vars[i]
    j <- i + 1L
    while (j <= n && is_cat[i] && is_cat[j] &&
           hierarchy_fd_holds(data, vars[j - 1L], vars[j])) {
      current <- c(current, vars[j])
      j <- j + 1L
    }
    runs[[length(runs) + 1L]] <- current
    i <- j
  }
  runs
}

#' Compute a 2-level hierarchy run as a long frame (parent rows at
#' `.indent = 0`, child rows at `.indent = 1`), each with `.group`,
#' `.fmt`, `n`, `pct`.
#' @noRd
compute_hierarchy_run <- function(data, run, sections, by,
                                  add_overall, overall_label, subject_var) {
  stopifnot(length(run) == 2L)
  outer <- run[1]
  inner <- run[2]
  group_vars <- c(sections, by)

  denom <- compute_denom(data, group_vars, subject_var)

  build_long <- function(gvars) {
    outer_long <- compute_denom(
      data[!is.na(data[[outer]]), , drop = FALSE],
      c(gvars, outer), subject_var
    )
    names(outer_long)[names(outer_long) == "N"] <- "n"
    outer_long$.label <- as.character(outer_long[[outer]])
    outer_long$.indent <- 0L
    outer_long$.variable <- outer
    outer_long$.variable_level <- outer_long$.label
    outer_long$.sort_outer <- as.character(outer_long[[outer]])
    outer_long$.sort_inner <- ""
    outer_long[[outer]] <- NULL

    inner_long <- compute_denom(
      data[!is.na(data[[outer]]) & !is.na(data[[inner]]), , drop = FALSE],
      c(gvars, outer, inner), subject_var
    )
    names(inner_long)[names(inner_long) == "N"] <- "n"
    inner_long$.label <- as.character(inner_long[[inner]])
    inner_long$.indent <- 1L
    inner_long$.variable <- inner
    inner_long$.variable_level <- inner_long$.label
    inner_long$.sort_outer <- as.character(inner_long[[outer]])
    inner_long$.sort_inner <- as.character(inner_long[[inner]])
    inner_long[[outer]] <- NULL
    inner_long[[inner]] <- NULL

    rbind(outer_long, inner_long)
  }

  long <- build_long(group_vars)
  if (length(group_vars) == 0L) {
    long$N <- denom$N[1]
  } else {
    long <- dplyr::left_join(long, denom, by = group_vars)
  }
  long$N[is.na(long$N)] <- 0L
  long$pct <- ifelse(long$N > 0, long$n / long$N * 100, 0)
  long$N <- NULL
  long <- add_group_col(long, by)
  long$.fmt <- "{n:0} ({pct:1}%)"

  if (isTRUE(add_overall)) {
    ov_denom <- compute_denom(data, sections, subject_var)
    ov_long <- build_long(sections)
    if (length(sections) == 0L) {
      ov_long$N <- ov_denom$N[1]
    } else {
      ov_long <- dplyr::left_join(ov_long, ov_denom, by = sections)
    }
    ov_long$N[is.na(ov_long$N)] <- 0L
    ov_long$pct <- ifelse(ov_long$N > 0, ov_long$n / ov_long$N * 100, 0)
    ov_long$N <- NULL
    ov_long$.group <- overall_label
    ov_long$.fmt <- "{n:0} ({pct:1}%)"
    long <- rbind_long(long, ov_long)
  }

  # Sort: parents immediately above their children.
  ord <- order(long$.sort_outer, long$.sort_inner)
  long <- long[ord, , drop = FALSE]
  long$.sort_outer <- NULL
  long$.sort_inner <- NULL

  reorder_long(long, sections)
}

# dplyr uses `.data` pronoun; silence R CMD check warnings
utils::globalVariables(c(".data", ".colkey", ".level", ".label",
                         ".group", ".fmt"))
