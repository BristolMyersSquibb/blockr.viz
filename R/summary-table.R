#' Summary table in its internal long form
#'
#' One row per statistic or level and by-group: raw numeric columns (`n`,
#' `pct`, `mean`, `sd`, `median`, `q1`, `q3`, `min`, `max`, `NA` where they do
#' not apply) and a per-row `.fmt` template naming how to print them.
#' `fmt_to_wide()` spreads it into the wide annotated df, the only public
#' shape. Numeric variables give one row per selected stat (one un-indented
#' row for a single stat), categoricals one row per level, logicals one row
#' for the TRUE count.
#'
#' Dotted columns carry the structure: `.group<k>` / `.group<k>_level` (the
#' `sections`), `.variable` / `.variable_level` (the source of a row; `NA`
#' level on stat rows), `.variable_label` (one per block when there are
#' several variables), `.label`, `.indent`, and `.group`, the by-group key
#' (`"outer|inner"` for two `by` columns, `"Overall"` without `by`).
#' `attr(out, "group_n")` holds the denominator per group key.
#'
#' Arguments as in [summary_table()]; `indent_details = FALSE` emits no
#' `.indent`, `nest_hierarchies = TRUE` nests two adjacent categoricals that
#' form a functional dependency.
#' @noRd
summary_table_long <- function(data,
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
  summary_table_check(data, vars, sections, by, subject_var)
  stats <- normalize_summary_stats(stats)

  # Group vars into hierarchy runs if nest_hierarchies is enabled.
  runs <- if (isTRUE(nest_hierarchies)) {
    group_vars_into_runs(vars, data)
  } else {
    lapply(vars, function(v) v)
  }

  # One long frame per run. Each row carries its source identity as the
  # `.variable` / `.variable_level` pair (the annotated-df v2 contract):
  # distinct variables routinely share level labels ("OTHER", "UNKNOWN",
  # "MISSING"), and `.variable` keeps their rows apart in the display pivot
  # while also naming the column a drill filters on.
  per_var <- lapply(runs, summary_table_run, data = data, stats = stats,
                    sections = sections, by = by, add_overall = add_overall,
                    overall_label = overall_label, subject_var = subject_var)
  blocks <- summary_table_blocks(per_var, runs, data, vars)

  out <- bind_per_var_frames(blocks$frames, sections = sections)
  if (!isTRUE(indent_details) && ".indent" %in% names(out) &&
      !isTRUE(nest_hierarchies)) {
    out$.indent <- NULL
  }
  out <- summary_table_sections(out, sections, data)
  out <- summary_table_order(out, sections)

  # Per-group denominators for "<group>\nN = <n>" headers, as an attr.
  group_n <- compute_group_n(
    data = data, by = by, subject_var = subject_var,
    groups = unique(out$.group),
    overall_label = overall_label, add_overall = add_overall
  )
  out <- tibble::as_tibble(out)
  attr(out, "group_n") <- group_n

  # The statistic is constant down a compact table, so it is named once here
  # rather than repeated in every stub. This is the auto tier the table block
  # already reads (`input_display_attrs()`): a subtitle the user types wins,
  # and being an attribute on the frame it follows the table into the Excel
  # export and a report exhibit.
  if (blocks$compact && length(stats) == 1L) {
    stat_label <- SUMMARY_STATS_CATALOG[[stats[[1L]]]]$label
    if (is.character(stat_label) && nzchar(stat_label)) {
      attr(out, "subtitle") <- stat_label
    }
  }
  out
}

# The arguments summary_table() refuses: no variable, more than two `by`,
# a column the data lacks, a column in two slots, or input columns in the
# block's own dotted namespace.
summary_table_check <- function(data, vars, sections, by, subject_var) {
  if (length(vars) == 0L) {
    stop("summary_table() requires at least one variable in `vars`.")
  }
  if (length(by) > 2L) {
    stop("summary_table() supports at most 2 `by` dimensions. ",
         "Got ", length(by), ".")
  }
  missing_cols <- setdiff(c(vars, sections, by, subject_var), names(data))
  if (length(missing_cols)) {
    stop("Columns not found in data: ",
         paste(missing_cols, collapse = ", "))
  }
  overlap <- unique(c(intersect(vars, sections), intersect(vars, by),
                      intersect(sections, by)))
  if (length(overlap)) {
    stop("Columns used in multiple slots: ", paste(overlap, collapse = ", "))
  }
  input_cols <- names(data)
  bad <- c(
    intersect(c(".strong", ".label", ".indent", ".group", ".fmt",
                ".variable", ".variable_level", ".variable_label"),
              input_cols),
    grep("^\\.group\\d+(_level|_label)?$", input_cols, value = TRUE)
  )
  if (length(bad)) {
    stop("Input data contains columns in the block-internal namespace: ",
         paste(bad, collapse = ", "),
         ". Rename or drop these columns upstream.")
  }
  invisible(NULL)
}

# The long frame of one run: a hierarchy (two nested categoricals), one
# variable, or several bound together.
summary_table_run <- function(run, data, stats, sections, by, add_overall,
                              overall_label, subject_var) {
  if (length(run) == 2L) {
    return(compute_hierarchy_run(
      data = data, run = run, sections = sections, by = by,
      add_overall = add_overall, overall_label = overall_label,
      subject_var = subject_var
    ))
  }
  one <- function(v) {
    compute_one_var(
      data = data, var = v, stats = stats, sections = sections, by = by,
      add_overall = add_overall, overall_label = overall_label,
      subject_var = subject_var
    )
  }
  if (length(run) == 1L) {
    return(one(run))
  }
  bind_per_var_frames(lapply(run, one), sections = sections)
}

# The runs' frames shaped into the table's blocks, and whether the table is
# compact: `list(frames, compact)`.
#
# When multiple independent variable blocks are present, every row of a run
# gets a constant `.variable_label` -- the renderer synthesizes one
# collapsible header per label run (headers are rendering, never data rows;
# annotated-df v2). Detail rows keep their absolute indents. Hierarchy runs
# (length(run)==2) count as one block: their rows share the outer variable's
# label even though `.variable` alternates outer/inner. Flag lists
# (all-logical vars) render flat -- no `.variable_label`.
#
# Compact shape: every variable resolves to exactly ONE row, which is what
# compute_one_var() already aims at for a single statistic (indent 0).
# Heading each of those single rows with its own variable name doubles the
# table's height to say nothing -- with twenty variables it is forty rows
# carrying twenty numbers -- so keep the variable in the row's own stub and
# let the statistic be named once, in the subtitle.
#
# Restricted to numeric variables on purpose: a categorical that happens to
# have one level also yields one row, but there the label IS the level, and
# replacing it with the variable name would lose which level was counted.
summary_table_blocks <- function(per_var, runs, data, vars) {
  all_logical <- all(vapply(vars, function(v) is.logical(data[[v]]),
                            logical(1)))
  all_numeric <- all(vapply(vars, function(v) is.numeric(data[[v]]),
                            logical(1)))
  # Count DISPLAY rows per variable, and count them WITHIN a section: a frame
  # still holds one long row per by-group (the group dimension only becomes
  # columns in the wide pivot further down) and one per section value. Neither
  # multiplies the rows a variable contributes to its own block -- with
  # sections = "species" each species still shows one row per variable, which
  # is exactly the shape worth collapsing.
  display_rows <- function(df) {
    id <- intersect(c(".variable", ".variable_level", ".label"), names(df))
    nrow(unique(df[, id, drop = FALSE]))
  }
  compact <- all_numeric &&
    all(vapply(per_var, display_rows, integer(1)) == 1L)

  if (compact) {
    for (i in seq_along(per_var)) {
      head_var <- runs[[i]][1]
      per_var[[i]]$.label <- var_header_label(data[[head_var]], head_var)
      per_var[[i]]$.indent <- 0L
    }
  } else if (length(per_var) > 1L && !all_logical) {
    for (i in seq_along(per_var)) {
      head_var <- runs[[i]][1]
      df <- per_var[[i]]
      # Hierarchy frames (parent at 0, child at 1) need a full +1 so the
      # nesting is preserved under the header. Non-hierarchy frames
      # (categorical at 1, compact numeric at 0) just need every row at
      # indent >= 1 (one level below the header).
      has_inner_nesting <- any(df$.indent == 0L) && any(df$.indent > 0L)
      df$.indent <- if (has_inner_nesting) df$.indent + 1L else pmax(df$.indent, 1L)
      df$.variable_label <- var_header_label(data[[head_var]], head_var)
      per_var[[i]] <- df
    }
  }
  list(frames = per_var, compact = compact)
}

# Encode user `sections` columns as ARD-named pairs: `.group<i>` holds the
# source variable NAME (constant), `.group<i>_level` its value on the row
# (character-coerced per the contract). The section display label rides as a
# `label` attribute on the `_level` column (chrome, renderer prefix).
summary_table_sections <- function(out, sections, data) {
  for (i in seq_along(sections)) {
    src <- sections[i]
    lbl <- attr(data[[src]], "label")
    dst <- paste0(".group", i, "_level")
    names(out)[names(out) == src] <- dst
    out[[dst]] <- as.character(out[[dst]])
    # Named by the source column, as the recorded frames carry it.
    attr(out[[dst]], "label") <- stats::setNames(
      if (is.null(lbl) || !nzchar(lbl)) src else as.character(lbl), src
    )
    out[[paste0(".group", i)]] <- src
  }
  out
}

# Columns in the contract's order: the `.group<i>` / `.group<i>_level`
# pairs, `.variable*`, `.label`, `.indent`, `.group`, `.fmt`, then the
# statistics in catalog order, then anything else.
summary_table_order <- function(out, sections) {
  stat_cols <- c("n", "pct", "mean", "sd", "median", "q1", "q3", "min", "max")
  front <- c(
    if (length(sections)) {
      as.vector(rbind(paste0(".group", seq_along(sections)),
                      paste0(".group", seq_along(sections), "_level")))
    },
    ".variable", ".variable_label", ".variable_level", ".label", ".indent",
    ".group", ".fmt"
  )
  front <- intersect(front, names(out))
  back <- setdiff(names(out), front)
  back <- c(intersect(stat_cols, back), setdiff(back, stat_cols))
  out[, c(front, back), drop = FALSE]
}

#' Summary Table -- Multi-Variable Descriptive Summary (Table 1)
#'
#' Aggregate a flat data.frame into the **wide annotated data frame** the
#' blockr table renderers consume directly: one formatted column per by-group
#' level (each cell baked from its statistic at produce time, so the renderer
#' does no arithmetic), dotted identity / structure columns for the row-side
#' hierarchy (the ARD-named `.group<k>` / `.group<k>_level` pairs for
#' `sections`, `.variable` / `.variable_level` / `.variable_label` for the
#' leaf rows -- section and variable headers are synthesized by the renderer
#' from value runs, never materialized as rows), `Top||Leaf` spanner column
#' names for length-2 `by`, and per-group `"<group>\nN = <n>"` column labels.
#' This is the same annotated-df contract that `composer` emits via
#' [as_annotated_df()], so the table block treats both identically. The
#' identity pairs are what a drill-down filters on; a level row asserts
#' "the source rows where `.variable == .variable_level`".
#'
#' Numeric variables emit one row per selected `stats` key
#' (mean/sd/median/...), categoricals one row per level as `n (%)`, and
#' logicals a single TRUE-count row (the pharma flag convention). Digit
#' precision is fixed per statistic and applied here, at aggregation time.
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
#'   rows follow the order given. A single
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
#' @param missing Missing values of a categorical variable, of `by` and of
#'   `sections`: `"drop"` (default) gives them no row or column, `"level"`
#'   shows them as "(Missing)". Numeric variables are summarised without
#'   their missing values either way. A [drill_source()] claim on the
#'   "(Missing)" level selects the rows whose value is missing.
#' @return A wide annotated data frame: dotted identity / structure columns
#'   plus one formatted character column per by-group level, each carrying a
#'   `label` attribute holding its `"<group>\nN = <n>"` header. Consumed by
#'   [new_table_block()], [html_exhibit()] and any renderer that understands the
#'   annotated-data-frame convention.
#' @seealso [as_annotated_df()]
#' @examples
#' # Simple demographics
#' summary_table(
#'   iris,
#'   vars = c("Sepal.Length", "Species"),
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
                          nest_hierarchies = FALSE,
                          missing = "drop") {
  # The table is computed from `input`; `data`, with its missing values,
  # stays the source a drill resolves against.
  input <- data
  if (identical(chr_state(missing), "level")) {
    cats <- Filter(function(v) !is.numeric(data[[v]]),
                   intersect(as.character(vars), names(data)))
    input <- table_missing_levels(data, c(by, sections, cats))
  }
  out <- fmt_to_wide(
    summary_table_long(
      input,
      vars             = vars,
      sections         = sections,
      by               = by,
      stats            = stats,
      add_overall      = add_overall,
      overall_label    = overall_label,
      subject_var      = subject_var,
      indent_details   = indent_details,
      nest_hierarchies = nest_hierarchies
    )
  )

  # The frame the table was computed from, so a click on a display row can be
  # resolved back to the RECORDS behind it: `new_table_block(drill =
  # "source")` hands the clicked row to [drill_source()], which reads the
  # row's `.variable` / `.variable_level` claim ("SEX == F") and applies it
  # here. Without this attribute that drill errors at eval time, which is how
  # the contract is written: a producer either stamps the frame or the click
  # is not resolvable, never a silent fallback to display rows.
  #
  # Stamping the INPUT is correct for this producer and would not be for
  # every one. A composer table applies a population filter and restricts to
  # denominator members before summarising, so its input over-selects and it
  # must stamp what it actually summarised. `summary_table()` summarises
  # `data` as given, so the two frames are the same frame. With `missing =
  # "level"` the stamped frame keeps its missing values, and drill_source()
  # reads a "(Missing)" claim as those.
  attr(out, "source_data") <- data

  # Which arm each column is, so a click on a cell claims its row and its
  # arm (annotated_column_keys()). The overall column is every row and
  # claims nothing.
  by1 <- as.character(unlist(by))
  if (length(by1) == 1L && by1 %in% names(input)) {
    lv <- intersect(names(out), unique(as.character(input[[by1]])))
    lv <- setdiff(lv, if (isTRUE(add_overall)) overall_label)
    if (length(lv)) {
      attr(out, "column_keys") <- stats::setNames(
        lapply(lv, function(l) list(list(column = by1, values = l))), lv
      )
    }
  }

  out
}

# =============================================================================
# Internal: per-group denominators (named vector, group key -> N)
# =============================================================================
