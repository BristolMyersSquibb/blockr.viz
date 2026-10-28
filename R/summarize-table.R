# Level colours, shared with the chart (dd_palette()). `column` is the data
# column behind `col`, resolved through dd_resolve_scales so a copied column
# keeps its source's binding. A caller holding only the levels passes none and
# the levels stand in for it.
#' @noRd
summarize_level_colors <- function(map, col, levels, column = NULL) {
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
summarize_agg_expr <- function(func, value, id_var) {
  switch(
    func %||% "count",
    count = quote(dplyr::n()),
    count_distinct = bquote(dplyr::n_distinct(.data[[.(id_var)]])),
    sum = bquote(sum(.data[[.(value)]], na.rm = TRUE)),
    mean = bquote(mean(.data[[.(value)]], na.rm = TRUE)),
    median = bquote(stats::median(.data[[.(value)]], na.rm = TRUE)),
    min = bquote(dd_agg_min(.data[[.(value)]])),
    max = bquote(dd_agg_max(.data[[.(value)]])),
    identity = bquote(summarize_agg_first(.data[[.(value)]])),
    quote(dplyr::n())
  )
}

# identity ("None (as is)"): the group's value untouched, for data already one
# row per group. Duplicates collapse to the first non-missing value; an
# all-missing group stays NA (as in inst/js/chart/agg.js).
#' @noRd
summarize_agg_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) as.numeric(x[[1L]]) else NA_real_
}

# One row per key combination (`keys` may be empty for a grand total): the keys
# plus `.v`.
#' @noRd
summarize_aggregate <- function(data, keys, func, value, id_var) {
  func <- func %||% "count"
  if (length(keys) && func %in% c("count", "count_distinct") &&
        (identical(func, "count") || isTRUE(id_var %in% names(data)))) {
    return(summarize_count_by(data, keys,
                              if (identical(func, "count_distinct")) id_var))
  }
  ex <- summarize_agg_expr(func, value, id_var)
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

# summarize_aggregate()'s counts without dplyr: rows, or distinct `id_var`
# values (a missing id counts as one, as in dplyr::n_distinct()), per key
# combination in first-appearance order. A faceted table aggregates once per
# facet and row level and group_by() + summarise() costs ~1 ms each. Keys are
# joined as summarize_match_col() joins them.
#' @noRd
summarize_count_by <- function(data, keys, id_var = NULL) {
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
summarize_denom <- function(data, func, id_var) {
  if (identical(func, "count_distinct") && !is.null(id_var) &&
        id_var %in% names(data)) {
    return(dplyr::n_distinct(data[[id_var]]))
  }
  summarize_n_rows(data)
}

# The input rows a slice holds. Pooled groups (expand_groups()) copy a row into
# every group it belongs to, so summarize_prepare() tags each input row
# (SUMMARIZE_ROW_ID) and a percent base counts the tags.
SUMMARIZE_ROW_ID <- ".blockr_row"

#' @noRd
summarize_n_rows <- function(data) {
  id <- data[[SUMMARIZE_ROW_ID]]
  if (is.null(id)) nrow(data) else length(unique(id))
}

# A facet level's Big N: subjects in its rows, else rows. Counts every row of
# the level, including the population join's rows without a group.
#' @noRd
summarize_pop_n <- function(data, subject = NULL) {
  subject <- chr_state(subject)
  if (!is.null(subject) && subject %in% names(data)) {
    return(dplyr::n_distinct(data[[subject]]))
  }
  summarize_n_rows(data)
}

#' @noRd
summarize_has_pct <- function(func) {
  isTRUE(func %in% c("count", "count_distinct"))
}

# Measures whose parts SUM to the whole (counts, sum). Only these may be
# STACKED into a split bar; the rest split side by side.
#' @noRd
summarize_additive <- function(func) {
  isTRUE(func %in% c("count", "count_distinct", "sum"))
}

# Level order, matching the chart's _orderLevels: factor levels, else sorted,
# NUMERICALLY for a numeric or date column (as text, day 10 sorts before 2).
#' @noRd
summarize_levels <- function(x) {
  lv <- if (is.factor(x)) {
    levels(x)
  } else if (is.numeric(x) || inherits(x, c("Date", "POSIXct"))) {
    as.character(sort(unique(x)))
  } else {
    sort(unique(as.character(x)))
  }
  lv[!is.na(lv)]
}

# Colour levels: summarize_levels() over the drawn rows, minus factor levels
# none of them carries (an upstream filter keeps a factor's full set; a
# population join adds rows with no group that carry their arm). `drawn` marks
# the rows with a complete group.
#' @noRd
summarize_color_levels <- function(x, drawn = NULL) {
  if (!is.null(drawn)) x <- x[drawn]
  summarize_levels(if (is.factor(x)) droplevels(x) else x)
}

# The DATA's own order for a grouping column (chart dataOrder()): factor levels,
# else first appearance, so a character AVISIT reads chronologically.
# Alphabetical would put "Week 10" before "Week 2".
#' @noRd
summarize_data_levels <- function(x) {
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
summarize_data_ord <- function(x, labels) {
  lv <- summarize_data_levels(x)
  i <- match(as.character(labels), lv)
  i[is.na(i)] <- length(lv) + 1L
  as.numeric(i)
}

#' Prepare the table and its column plan, computed once server-side and split
#' from the HTML so it is testable and the block can validate before rendering.
#' One row per level of `by` (outer first, at most two), one column per entry of
#' `summaries`; empty `summaries` counts rows (table_groups_seed()).
#' @return `list(err =)` when the config cannot be honored, else
#'   `list(rows =, plan =, ...)`.
#' @noRd
summarize_prepare <- function(data, summaries = list(), by = NULL,
                              facet_layout = "by_summary", sort_by = "value",
                              sort_dir = "desc", top_n = NULL,
                              scale_map = NULL, subject = NULL,
                              pct_of = "facet", missing = "drop",
                              format = NULL, unit = NULL) {
  bad <- function(msg) list(err = msg)

  if (!is.data.frame(data)) return(bad("No data"))
  if (!nrow(data)) return(bad("No rows to display"))

  by <- as.character(unlist(by %||% character()))
  by <- by[!is.na(by) & nzchar(by)]
  if (!is.list(summaries) || !length(summaries)) {
    summaries <- table_groups_seed()
  }
  split_cols <- unlist(lapply(summaries, function(s) {
    if (is.list(s)) c(chr_state(s[["color"]]), chr_state(s[["facet"]]))
  }))

  # Missing values of a grouping, colour or facet column: no group of their
  # own ("drop"; counted in N, not drawn) or a "(Missing)" level ("level").
  if (identical(chr_state(missing), "level")) {
    data <- table_missing_levels(data, c(by, split_cols))
  }

  # One copy of each group's rows for every split column that carries a group
  # definition (expand_groups()). Each input row is tagged first so a count's
  # percent base counts it once.
  pooled <- intersect(c(by, split_cols), names(data))
  if (any(vapply(pooled, function(col) !is.null(group_def(data[[col]])),
                 logical(1L)))) {
    data[[SUMMARIZE_ROW_ID]] <- seq_len(nrow(data))
  }
  data <- expand_role_groups(data, c(by, split_cols))

  prep <- lane_prepare_summaries(
    data, by, summaries,
    facet_layout = chr_state(facet_layout) %||% "by_summary",
    sort_by = sort_by, sort_dir = sort_dir, top_n = top_n,
    scale_map = scale_map, subject = subject, pct_of = pct_of
  )
  summarize_number_format(prep, format, unit)
}

# The table's `format` and `unit` on a prepared groups table. NULL `format`
# keeps the engine's number formatting; else number cells, bar value labels and
# cards print through dt_format_num(). `unit` heads the columns with `unit_ok`
# after the summary's name.
#' @noRd
summarize_number_format <- function(prep, format = NULL, unit = NULL) {
  if (!is.null(prep$err)) return(prep)
  if (!is.null(format)) prep$format <- table_format_state(format)
  unit <- chr_state(trimws(as.character(unlist(unit))))
  if (!is.null(unit)) {
    prep$unit <- unit
    with_unit <- function(x) paste0(x, " (", unit, ")")
    prep$plan <- lapply(prep$plan, function(p) {
      if (!isTRUE(p$unit_ok)) return(p)
      if (is.null(p$flevel)) {
        p$label <- with_unit(p$label)
      } else if (!is.null(p$sub_label) && !is.null(p$sname)) {
        p$sub_label <- sub(p$sname, with_unit(p$sname), p$sub_label,
                           fixed = TRUE)
      }
      p
    })
  }
  prep
}

# Drops facet columns with nothing to draw (the chart draws no panel for a level
# without data). Empty = every cell missing, or for a count (`zero_empty`) every
# cell zero. Checked over ALL rows, never the Top N cut. A table left with no
# column keeps them all.
#' @noRd
summarize_drop_empty_facets <- function(plan, leaf, par_rows = NULL) {
  frames <- Filter(Negate(is.null), list(leaf, par_rows))
  empty <- vapply(plan, function(p) {
    if (is.null(p$flevel) || is.null(p$key)) return(FALSE)
    v <- unlist(lapply(frames, function(f) f[[p$key]]), use.names = FALSE)
    if (is.null(v)) return(FALSE)
    v <- suppressWarnings(as.numeric(v))
    all(is.na(v) | (isTRUE(p$zero_empty) & !is.na(v) & v == 0))
  }, logical(1L))
  if (all(empty)) return(plan)
  plan[!empty]
}

# "label" is an alias of "alpha".
#' @noRd
summarize_sort_by <- function(sort_by) {
  sb <- chr_state(sort_by) %||% "value"
  if (identical(sb, "label")) "alpha" else sb
}

# `sort_by` is either a plan-independent keyword or a facet level name.
#' @noRd
summarize_sort_key <- function(sort_by, plan) {
  sb <- summarize_sort_by(sort_by)
  if (identical(sb, "alpha")) return(".label")
  if (identical(sb, "data")) return(".ord")
  if (identical(sb, "value")) return(".v")
  hit <- vapply(plan, function(p) identical(p$label, sb), logical(1L))
  if (any(hit)) {
    key <- plan[[which(hit)[[1L]]]]$key
    if (!is.null(key)) return(key)
  }
  ".v"
}

# A raw data column as the ordering key: the group's MINIMUM of it (chart `mins`
# in orderGroups). AVISITN orders visits a character AVISIT cannot. Groups with
# no value keep NA and sort last in both directions.
#' @noRd
summarize_min_ord <- function(data, keycol, sortcol, labels) {
  v <- suppressWarnings(as.numeric(data[[sortcol]]))
  m <- tapply(v, as.character(data[[keycol]]), function(z) {
    z <- z[is.finite(z)]
    if (length(z)) min(z) else NA_real_
  })
  as.numeric(m[as.character(labels)])
}

# Resolve `sort_by` to a column on the assembled frames; a raw data column is
# folded into `.ord` so the assembler sees a single key.
#' @noRd
summarize_resolve_sort <- function(sort_by, plan, data, leaf, par_rows, group,
                                   parent) {
  key <- summarize_sort_key(sort_by, plan)
  sb <- summarize_sort_by(sort_by)
  # A numeric row NAME sorts numerically; `.label` is text, where "10" sorts
  # before "2".
  if (identical(key, ".label")) {
    num <- function(df, col) {
      if (is.null(df) || is.null(col) || !is.numeric(data[[col]])) return(df)
      df$.labn <- suppressWarnings(as.numeric(df$.label))
      df
    }
    if (is.numeric(data[[group]])) {
      leaf <- num(leaf, group)
      par_rows <- if (is.null(parent)) NULL else num(par_rows, parent)
      if (!is.null(leaf$.labn)) key <- ".labn"
    }
  }
  if (identical(key, ".v") && !sb %in% c("value", "alpha", "data") &&
        sb %in% names(data)) {
    leaf$.ord <- summarize_min_ord(data, group, sb, leaf$.label)
    if (!is.null(par_rows)) {
      par_label <- if (!is.null(par_rows$.parent_key)) {
        par_rows$.parent_key
      } else {
        par_rows$.label
      }
      par_rows$.ord <- summarize_min_ord(data, parent, sb, par_label)
    }
    key <- ".ord"
  }
  list(key = key, leaf = leaf, par_rows = par_rows)
}

# The first underlying row's value of `col` per key combination, any type;
# absent keys are NA. Only the as-is measure calls this, where one row per
# group is the data's contract.
#' @noRd
summarize_match_field <- function(target, src, keys, col) {
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  src[[col]][match(tk, sk)]
}

# A column as the hover card names it: its variable label, else its name.
#' @noRd
summarize_col_name <- function(data, col) {
  if (is.null(col)) return(NULL)
  dt_col_label(data[[col]], col) %||% col
}

# The label column's sub-line: the group column's variable label, or the pair
# of labels when nested; NULL when there is none (dt_col_label()'s rule).
#' @noRd
summarize_group_label <- function(data, group, parent) {
  g <- dt_col_label(data[[group]], group)
  if (is.null(parent)) return(g)
  p <- dt_col_label(data[[parent]], parent)
  if (is.null(p) && is.null(g)) return(NULL)
  paste0(p %||% parent, " / ", g %||% group)
}
