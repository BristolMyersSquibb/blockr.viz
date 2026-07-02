#' Summary Table -- Tidy, Long-Format Multi-Variable Summary
#'
#' Aggregate a flat data.frame into a **tidy, long** summary frame: raw
#' numeric statistic columns plus a hidden per-row `.fmt` template that
#' names how to combine them into a display cell. Section structure is
#' encoded via well-known dotted columns (`.section_1, ..., .section_k,
#' .label, .indent, .strong`), and the by-group dimension lives in a single
#' `.group` column. Designed for the "list of variables by Y" pattern;
#' the complementary "one measurement across two dimensions" pivot is
#' produced upstream by composing `summarize` with `tidyr::pivot_wider`.
#'
#' The output stays `dplyr`-able (every number is a plain numeric column)
#' and re-renderable: a renderer turns it into the familiar wide display
#' grid by formatting each row's `.fmt` template, then spreading `.group`
#' to columns (see `fmt_assemble()`). The shaper no longer bakes display
#' strings -- it emits numbers + templates and leaves formatting to the
#' renderer.
#'
#' Each `vars` entry contributes rows shaped by the variable's type and
#' the chosen `stats` selection:
#'
#' - **numeric** -> one row per selected stat key (see `stats`). A single
#'   key emits one un-indented row per variable (e.g. `"mean_sd"` ->
#'   `.fmt = "{mean:1} ({sd:2})"`); several keys emit one indented row
#'   each, in catalog order.
#' - **categorical** -> one row per level, `.fmt = "{n:0} ({pct:1}%)"`.
#' - **logical** -> one row per variable, `.fmt = "{n:0} ({pct:1}%)"` for
#'   the TRUE count. The FALSE row is suppressed -- this matches pharma
#'   flag-variable conventions (TEAE, SAE, etc.).
#'
#' Output columns:
#'
#' - `.section_1, ..., .section_k` -- section columns (dotted,
#'   block-internal) holding values from user-provided `sections`.
#'   Present iff `length(sections) > 0`. Each carries a `label`
#'   attribute pulled from the original column for renderer display.
#' - `.label` -- innermost row label. Stat name for numeric vars
#'   ("N", "Mean", "SD", ...), level name for categoricals, variable
#'   name for logicals.
#' - `.indent` -- detail-row indentation level (see `indent_details`).
#' - `.strong` -- logical, `TRUE` on variable-header rows (bold,
#'   blank data cells). Present only when `length(vars) > 1`.
#' - `.group` -- by-group value (one row per var/level x group).
#'   Pipe-delimited (`"outer|inner"`) for length-2 `by`; the constant
#'   `"Overall"` when `by` is empty; the `overall_label` value for the
#'   appended overall column when `add_overall = TRUE`.
#' - `.fmt` -- per-row display template (the `.fmt` convention).
#' - Raw numeric stat columns: `n, pct` (categorical/logical, and
#'   numeric where `pct` is the share of non-missing rows), `mean,
#'   sd, median, q1, q3, min, max` (numeric). `NA` where not applicable.
#'
#' The per-group denominators (for `"<group>\\nN = <n>"` column headers)
#' ride along as a **named numeric vector** in `attr(out, "group_n")`
#' (group key -> N). A renderer assembling the wide display reattaches
#' these as column labels.
#'
#' @param data A flat data.frame.
#' @param vars Character vector of variables to summarise. Each
#'   becomes a section in the output.
#' @param sections Optional character vector of outer section
#'   columns from `data`. Used for nested row-side hierarchy (e.g.
#'   `sections = "AESOC", vars = "AEDECOD"`).
#' @param by Character vector of length 0-2. Values become the `.group`
#'   dimension. Length-2 produces pipe-delimited nested group keys
#'   consumable by `gt::tab_spanner_delim("|")` after the renderer
#'   spreads `.group` to columns.
#' @param stats Character vector of stat keys controlling which rows /
#'   templates are emitted for **numeric** variables (the underlying
#'   numbers are always all computed). Any combination of `"n"`,
#'   `"n_pct"` (non-missing n and % of group rows), `"mean"`, `"sd"`,
#'   `"mean_sd"`, `"median"`, `"median_q1_q3"`, `"q1_q3"`, `"min_max"`;
#'   rows follow this canonical order regardless of input order. A single
#'   key gives one row per variable, several give one row per stat. The
#'   legacy presets `"compact"` (= `"mean_sd"`) and `"expanded"`
#'   (= `c("n", "mean", "sd", "median", "q1_q3", "min_max")`) are still
#'   accepted. Default `"mean_sd"`.
#' @param add_overall Logical. If TRUE, append an extra `.group` whose
#'   rows are computed on the unsplit data (ignoring `by`). Matches
#'   `gtsummary::add_overall()`.
#' @param overall_label Character. `.group` value for the overall rows
#'   when `add_overall = TRUE`. Default `"Total"`.
#' @param subject_var Optional subject-identifier column for
#'   distinct-subject counts (for categorical `n_pct`). If NULL,
#'   rows are counted instead.
#' @param indent_details Logical, default `TRUE`. When `TRUE`, detail
#'   rows (stat rows for expanded numerics, category-level rows for
#'   categoricals) are tagged with `.indent = 1`. When `FALSE`, no
#'   `.indent` column is emitted.
#' @param nest_hierarchies Logical, default `FALSE`. Advanced option.
#'   When `TRUE`, adjacent categorical entries in `vars` that form a
#'   functional dependency in `data` are rendered as a row-side
#'   drill-down. v1 supports 2-level hierarchies only.
#'
#' @return A tidy, long-format tibble: raw numeric statistic columns plus
#'   dotted structure columns (`.section_*`, `.label`, `.indent`,
#'   `.strong`), a `.group` dimension column, and a per-row `.fmt`
#'   template column that a renderer interpolates into display cells.
#' @examples
#' # Simple demographics
#' summary_table(
#'   iris,
#'   vars = c("Sepal.Length", "Species"),
#'   by = character(),
#'   stats = "mean_sd"
#' )
#'
#' # Any stat combination, split by a grouping column
#' summary_table(
#'   mtcars,
#'   vars = c("mpg", "hp"),
#'   by = "cyl",
#'   stats = c("n_pct", "median_q1_q3", "min_max"),
#'   add_overall = TRUE
#' )
#'
#' @export
summary_table <- function(data,
                          vars = character(),
                          sections = character(),
                          by = character(),
                          stats = "mean_sd",
                          add_overall = FALSE,
                          overall_label = "Total",
                          subject_var = NULL,
                          indent_details = TRUE,
                          nest_hierarchies = FALSE) {
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
  stats <- normalize_summary_stats(stats)

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
    intersect(c(".strong", ".label", ".group", ".fmt"), input_cols),
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

  # Group vars into hierarchy runs if nest_hierarchies is enabled.
  runs <- if (isTRUE(nest_hierarchies)) {
    group_vars_into_runs(vars, data)
  } else {
    lapply(vars, function(v) v)
  }

  # Compute per-run long frames.
  per_run <- lapply(runs, function(run) {
    if (length(run) == 2L) {
      compute_hierarchy_run(
        data = data, run = run,
        sections = sections, by = by,
        add_overall = add_overall,
        overall_label = overall_label,
        subject_var = subject_var
      )
    } else if (length(run) == 1L) {
      compute_one_var(
        data = data, var = run,
        stats = stats, sections = sections, by = by,
        add_overall = add_overall,
        overall_label = overall_label,
        subject_var = subject_var
      )
    } else {
      parts <- lapply(run, function(v) {
        compute_one_var(
          data = data, var = v,
          stats = stats, sections = sections, by = by,
          add_overall = add_overall,
          overall_label = overall_label,
          subject_var = subject_var
        )
      })
      bind_per_var_frames(parts, sections = sections)
    }
  })

  per_var <- per_run

  # When multiple independent variable blocks are present, emit
  # annotated-df-style header rows (.strong=TRUE, blank data cells,
  # .indent=0) and bump every detail row's .indent by 1 -- instead of the
  # old .var section column.  Hierarchy runs (length(run)==2) already
  # carry their own parent/child structure so they count as one block.
  all_logical <- all(vapply(vars, function(v) is.logical(data[[v]]),
                            logical(1)))
  if (length(per_var) > 1L && !all_logical) {
    for (i in seq_along(per_var)) {
      head_var <- if (length(runs[[i]]) == 1L) runs[[i]] else runs[[i]][1]
      hdr_label <- var_header_label(data[[head_var]], head_var)

      df <- per_var[[i]]
      # Hierarchy frames (parent at 0, child at 1) need a full +1 so the
      # nesting is preserved under the header.  Non-hierarchy frames
      # (categorical at 1, compact numeric at 0) just need every row at
      # indent >= 1 (one level below the header).
      has_inner_nesting <- any(df$.indent == 0L) && any(df$.indent > 0L)
      if (has_inner_nesting) {
        df$.indent <- df$.indent + 1L
      } else {
        df$.indent <- pmax(df$.indent, 1L)
      }

      # One header row per unique (sections... x .group) combination.
      key_cols <- c(intersect(sections, names(df)), ".group")
      keys <- unique(df[, key_cols, drop = FALSE])
      hdr <- keys
      hdr$.label <- hdr_label
      hdr$.indent <- 0L
      hdr$.strong <- TRUE
      hdr$.fmt <- NA_character_
      for (sc in intersect(SUMMARY_STAT_COLS, names(df))) {
        hdr[[sc]] <- NA_real_
      }

      per_var[[i]] <- rbind_long(hdr, df)
    }
  }

  out <- bind_per_var_frames(per_var, sections = sections)

  # If indent_details is off, strip the .indent column.
  if (!isTRUE(indent_details) && ".indent" %in% names(out) &&
      !isTRUE(nest_hierarchies)) {
    out$.indent <- NULL
  }

  # Rename user `sections` columns to .section_1, .section_2, ...
  if (length(sections) > 0L) {
    for (i in seq_along(sections)) {
      src <- sections[i]
      dst <- paste0(".section_", i)
      names(out)[names(out) == src] <- dst
      attr(out[[dst]], "label") <- section_labels[i]
    }
  }

  # Reorder: .section_*, .label, .indent, .strong, .group, .fmt, then numbers
  stat_cols <- c("n", "pct", "mean", "sd", "median", "q1", "q3", "min", "max")
  front_order <- c(
    if (length(sections) > 0L) paste0(".section_", seq_along(sections)) else character(),
    ".label",
    ".indent",
    ".strong",
    ".group",
    ".fmt"
  )
  front_order <- intersect(front_order, names(out))
  back <- setdiff(names(out), front_order)
  # Keep stat columns in canonical order; drop all-NA stat columns.
  back <- c(intersect(stat_cols, back), setdiff(back, stat_cols))
  out <- out[, c(front_order, back), drop = FALSE]

  # Per-group denominators for "<group>\nN = <n>" headers, as an attr.
  group_n <- compute_group_n(
    data = data, by = by, subject_var = subject_var,
    groups = unique(out$.group),
    overall_label = overall_label, add_overall = add_overall
  )

  out <- tibble::as_tibble(out)
  attr(out, "group_n") <- group_n
  out
}

# =============================================================================
# Internal: per-group denominators (named vector, group key -> N)
# =============================================================================

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
# for numeric variables. Catalog order is the canonical row order -- the
# selection is reordered to it, so serialized boards are order-insensitive.
SUMMARY_STATS_CATALOG <- list(
  n            = list(label = "N",               fmt = "{n:0}"),
  n_pct        = list(label = "n (%)",           fmt = "{n:0} ({pct:1}%)"),
  mean         = list(label = "Mean",            fmt = "{mean:1}"),
  sd           = list(label = "SD",              fmt = "{sd:2}"),
  mean_sd      = list(label = "Mean (SD)",       fmt = "{mean:1} ({sd:2})"),
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

#' Normalize a `stats` value to a deduplicated vector of catalog keys in
#' canonical (catalog) order. Accepts the legacy `"compact"` / `"expanded"`
#' scalars.
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
  intersect(names(SUMMARY_STATS_CATALOG), stats)
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

#' Reorder a long per-var frame: sections, .label, .indent, .strong,
#' .group, .fmt, then numeric stat columns.
#' @noRd
reorder_long <- function(df, sections) {
  front <- intersect(c(sections, ".label", ".indent", ".strong", ".group",
                       ".fmt"), names(df))
  stats_present <- intersect(SUMMARY_STAT_COLS, names(df))
  rest <- setdiff(names(df), c(front, stats_present))
  df[, c(front, stats_present, rest), drop = FALSE]
}

#' @noRd
bind_per_var_frames <- function(per_var, sections) {
  all_names <- unique(unlist(lapply(per_var, names)))

  # Canonical front order: sections, .label, .indent, .strong, .group, .fmt
  front <- intersect(c(sections, ".label", ".indent", ".strong", ".group",
                       ".fmt"), all_names)
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

  do.call(rbind, per_var)
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
