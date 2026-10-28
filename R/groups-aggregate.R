# The groups engine's aggregation helpers: counts, denominators, levels and
# their colours, keyed lookups and column names.

# Level colours, shared with the chart (dd_palette()). `column` is the data
# column behind `col`, resolved through dd_resolve_scales so a copied column
# keeps its source's binding. A caller holding only the levels passes none and
# the levels stand in for it.
#' @noRd
groups_level_colors <- function(map, col, levels, column = NULL) {
  levels <- as.character(levels)
  if (!length(levels)) {
    return(character())
  }
  if (!is.null(map) && !is.null(col) &&
        has_blockr_theme()) {
    res <- dd_resolve_scales(map, col, column %||% levels)
    pal <- res$color
    if (!is.null(pal) && all(levels %in% names(pal))) {
      out <- stats::setNames(unname(pal[levels]), levels)
      # `pal[""]` is NA (the empty string matches no name): fill from the pool
      # so no mark renders with `background:NA`.
      gap <- is.na(out)
      if (any(gap)) out[gap] <- rep_len(dd_palette(), length(levels))[gap]
      return(out)
    }
  }
  # A factor's colours follow its FULL level set, so dropping an unused level
  # does not shift the rest. Only with a `col`: a custom summary's split has
  # none and its callers pass `data[[NULL]]`, which errors once forced.
  pos <- if (!is.null(col) && is.factor(column)) {
    match(levels, levels(column))
  }
  pos <- if (is.null(pos) || anyNA(pos)) seq_along(levels) else pos
  pool <- dd_palette()
  stats::setNames(pool[(pos - 1L) %% length(pool) + 1L], levels)
}

# The aggregation vocabulary as a summarise expression over one group. Mirrors
# the JS AGG_FNS plus "identity"; `count` needs no value column, count_distinct
# reduces `id_var`.
#' @noRd
groups_agg_expr <- function(func, value, id_var) {
  switch(
    func %||% "count",
    count = quote(dplyr::n()),
    count_distinct = bquote(dplyr::n_distinct(.data[[.(id_var)]])),
    sum = bquote(sum(.data[[.(value)]], na.rm = TRUE)),
    mean = bquote(mean(.data[[.(value)]], na.rm = TRUE)),
    median = bquote(stats::median(.data[[.(value)]], na.rm = TRUE)),
    min = bquote(table_agg_min(.data[[.(value)]])),
    max = bquote(table_agg_max(.data[[.(value)]])),
    identity = bquote(groups_agg_first(.data[[.(value)]])),
    quote(dplyr::n())
  )
}

# identity ("None (as is)"): the group's value untouched, for data already one
# row per group. Duplicates collapse to the first non-missing value; an
# all-missing group stays NA (as in inst/js/chart/agg.js).
#' @noRd
groups_agg_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) as.numeric(x[[1L]]) else NA_real_
}

# One row per key combination (`keys` may be empty for a grand total): the keys
# plus `.v`.
#' @noRd
groups_aggregate <- function(data, keys, func, value, id_var) {
  func <- func %||% "count"
  if (length(keys) && func %in% c("count", "count_distinct") &&
        (identical(func, "count") || isTRUE(id_var %in% names(data)))) {
    return(groups_count_by(data, keys,
                              if (identical(func, "count_distinct")) id_var))
  }
  ex <- groups_agg_expr(func, value, id_var)
  if (!length(keys)) {
    out <- dplyr::summarise(data, .v = !!ex)
  } else {
    g <- dplyr::group_by(data, dplyr::across(dplyr::all_of(keys)))
    out <- dplyr::summarise(g, .v = !!ex, .groups = "drop")
  }
  out <- as.data.frame(out, check.names = FALSE)
  out$.v <- as.numeric(out$.v)
  out
}

# groups_aggregate()'s counts without dplyr: rows, or distinct `id_var`
# values (a missing id counts as one, as in dplyr::n_distinct()), per key
# combination in first-appearance order. A faceted table aggregates once per
# facet and row level and group_by() + summarise() costs ~1 ms each. Keys are
# joined as groups_match_col() joins them.
#' @noRd
groups_count_by <- function(data, keys, id_var = NULL) {
  k <- do.call(paste, c(lapply(keys, function(x) as.character(data[[x]])),
                        list(sep = "\r")))
  first <- which(!duplicated(k))
  ki <- match(k, k[first])
  n <- if (is.null(id_var)) {
    tabulate(ki, nbins = length(first))
  } else {
    id <- data[[id_var]]
    ii <- match(id, unique(id))
    pair <- !duplicated(cbind(ki, ii))
    tabulate(ki[pair], nbins = length(first))
  }
  out <- data[first, keys, drop = FALSE]
  rownames(out) <- NULL
  out <- as.data.frame(out, check.names = FALSE)
  out$.v <- as.numeric(n)
  out
}

# The denominator a percentage is taken over: distinct subjects when the
# measure counts subjects, rows otherwise. Other aggregations have no percentage.
#' @noRd
groups_denom <- function(data, func, id_var) {
  if (identical(func, "count_distinct") && !is.null(id_var) &&
        id_var %in% names(data)) {
    return(dplyr::n_distinct(data[[id_var]]))
  }
  groups_n_rows(data)
}

# The input rows a slice holds. Pooled groups (expand_groups()) copy a row into
# every group it belongs to, so groups_prepare() tags each input row
# (GROUPS_ROW_ID) and a percent base counts the tags.
GROUPS_ROW_ID <- ".blockr_row"

#' @noRd
groups_n_rows <- function(data) {
  id <- data[[GROUPS_ROW_ID]]
  if (is.null(id)) nrow(data) else length(unique(id))
}

# A facet level's Big N: subjects in its rows, else rows. Counts every row of
# the level, including the population join's rows without a group.
#' @noRd
groups_pop_n <- function(data, subject = NULL) {
  subject <- chr_state(subject)
  if (!is.null(subject) && subject %in% names(data)) {
    return(dplyr::n_distinct(data[[subject]]))
  }
  groups_n_rows(data)
}

#' @noRd
groups_has_pct <- function(func) {
  isTRUE(func %in% c("count", "count_distinct"))
}

# Measures whose parts SUM to the whole (counts, sum). Only these may be
# STACKED into a split bar; the rest split side by side.
#' @noRd
groups_additive <- function(func) {
  isTRUE(func %in% c("count", "count_distinct", "sum"))
}

# Level order, matching the chart's _orderLevels: factor levels, else sorted,
# NUMERICALLY for a numeric or date column (as text, day 10 sorts before 2).
#' @noRd
groups_levels <- function(x) {
  lv <- if (is.factor(x)) {
    levels(x)
  } else if (is.numeric(x) || inherits(x, c("Date", "POSIXct"))) {
    as.character(sort(unique(x)))
  } else {
    sort(unique(as.character(x)))
  }
  lv[!is.na(lv)]
}

# Colour levels: groups_levels() over the drawn rows, minus factor levels
# none of them carries (an upstream filter keeps a factor's full set; a
# population join adds rows with no group that carry their arm). `drawn` marks
# the rows with a complete group.
#' @noRd
groups_color_levels <- function(x, drawn = NULL) {
  if (!is.null(drawn)) x <- x[drawn]
  groups_levels(if (is.factor(x)) droplevels(x) else x)
}

# The DATA's own order for a grouping column (chart dataOrder()): factor levels,
# else first appearance, so a character AVISIT reads chronologically.
# Alphabetical would put "Week 10" before "Week 2".
#' @noRd
groups_data_levels <- function(x) {
  lv <- if (is.factor(x)) {
    levels(x)
  } else if (is.numeric(x) || inherits(x, c("Date", "POSIXct"))) {
    # A number is its own order; first appearance would scatter study days.
    as.character(sort(unique(x)))
  } else {
    unique(as.character(x))
  }
  lv[!is.na(lv)]
}

# Rank of each label in the data's own order; labels the column no longer
# carries (an upstream filter dropped them) sort last.
#' @noRd
groups_data_ord <- function(x, labels) {
  lv <- groups_data_levels(x)
  i <- match(as.character(labels), lv)
  i[is.na(i)] <- length(lv) + 1L
  as.numeric(i)
}

# The first underlying row's value of `col` per key combination, any type;
# absent keys are NA. Only the as-is measure calls this, where one row per
# group is the data's contract.
#' @noRd
groups_match_field <- function(target, src, keys, col) {
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  src[[col]][match(tk, sk)]
}

# A column as the hover card names it: its variable label, else its name.
#' @noRd
groups_col_name <- function(data, col) {
  if (is.null(col)) return(NULL)
  dt_col_label(data[[col]], col) %||% col
}

# The label column's sub-line: the group column's variable label, or the pair
# of labels when nested; NULL when there is none (dt_col_label()'s rule).
#' @noRd
groups_group_label <- function(data, group, parent) {
  g <- dt_col_label(data[[group]], group)
  if (is.null(parent)) return(g)
  p <- dt_col_label(data[[parent]], parent)
  if (is.null(p) && is.null(g)) return(NULL)
  paste0(p %||% parent, " / ", g %||% group)
}
