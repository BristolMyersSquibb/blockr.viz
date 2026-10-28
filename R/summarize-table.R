# Summarize table: the data half ---------------------------------------------
#
# One row per group, one column per summary, drawn as an HTML table instead of
# an echarts chart: a mark is a div in a cell, so search, click-to-sort, exact
# values, a sticky header and an arbitrary row count come from the table form
# for free. Began as a single ranked bar; the column list
# (R/table-model-groups*.R) replaced it, and old boards map onto the list in
# table_legacy_args().
#
# Large tables follow the table block exactly: every row rendered, scrolling
# at `max_height` with a sticky header, search and sort client-side. `top_n`
# is opt-in (report exhibits: a pptx slide wants ten bars, not a hundred) and
# always draws a visible fold row -- never a silent truncation.

# One-hue-per-level pool, shared with the chart (dd_palette()) so a summarize
# table and a chart of the same split agree on colors.
#
# `column` is the actual data column behind `col`, and resolution always goes
# through the package's one seam (dd_resolve_scales -> blockr.theme's
# provenance-aware resolver), so a column the picker block copied (SEX picked
# into "color") inherits the source column's binding and the board's fixed
# colors survive the rename. A caller holding only the levels passes none;
# the levels then stand in for the column, which resolves them by name (they
# carry no provenance to follow).
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
      # `pal[""]` is NA even when the theme carries a blank level -- the empty
      # string matches no name. Fill any such gap from the pool so no mark
      # ever renders with `background:NA`.
      gap <- is.na(out)
      if (any(gap)) out[gap] <- rep_len(dd_palette(), length(levels))[gap]
      return(out)
    }
  }
  # A factor's colours follow its FULL level set, so dropping an unused level
  # (summarize_color_levels) does not shift the colours of the ones after it.
  # Read only with a `col`: a custom summary's split has none, and its callers
  # pass `data[[NULL]]`, which errors once forced.
  pos <- if (!is.null(col) && is.factor(column)) {
    match(levels, levels(column))
  }
  pos <- if (is.null(pos) || anyNA(pos)) seq_along(levels) else pos
  pool <- dd_palette()
  stats::setNames(pool[(pos - 1L) %% length(pool) + 1L], levels)
}

# The aggregation vocabulary, as a summarise expression over one group.
# Mirrors the JS AGG_FNS plus the chart-only "identity" ("None (as is)"):
# `count` needs no value column, everything else reduces `value`
# (count_distinct reduces `id_var`).
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

# identity ("None (as is)"): the group's value untouched, for data whose bar
# lengths are already computed upstream (one row per group -- e.g. one value
# per subject). Duplicate rows collapse to the first non-missing value and an
# all-missing group stays NA, matching the chart engine's identity branch
# (inst/js/chart/agg.js aggregate()).
#' @noRd
summarize_agg_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) as.numeric(x[[1L]]) else NA_real_
}

# Aggregate to one row per key combination. `keys` may be empty (a grand
# total). Returns a data frame of the keys plus `.v`.
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

# summarize_aggregate()'s counts without dplyr: rows per key combination, or
# distinct values of `id_var` per combination (a missing id counts as one
# value, as in dplyr::n_distinct()). A faceted table aggregates once per
# facet level and row level, so the heatmap's 25 terms are 50 calls, and
# group_by() + summarise() costs about 1 ms each before any work. Keys are
# joined as summarize_match_col() joins them, which is how every caller
# reads the result; rows come in first-appearance order.
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
# measure counts subjects, rows otherwise. Any other aggregation (a mean, a
# sum) has no meaningful percentage -- callers drop the pct column instead of
# inventing a denominator.
#' @noRd
summarize_denom <- function(data, func, id_var) {
  if (identical(func, "count_distinct") && !is.null(id_var) &&
        id_var %in% names(data)) {
    return(dplyr::n_distinct(data[[id_var]]))
  }
  summarize_n_rows(data)
}

# The input rows a slice holds. Pooled groups (expand_groups()) copy a row
# into every group it belongs to, so after the expansion the same input row
# can sit in a slice twice; summarize_prepare() tags each input row first
# (SUMMARIZE_ROW_ID), and a percent base counts the tags. Without the tag a
# slice's rows are input rows.
SUMMARIZE_ROW_ID <- ".blockr_row"

#' @noRd
summarize_n_rows <- function(data) {
  id <- data[[SUMMARIZE_ROW_ID]]
  if (is.null(id)) nrow(data) else length(unique(id))
}

# A facet level's Big N: the subjects in its rows, or the rows when there is
# no subject column. Over every row of the level, so the population join's
# rows without a group count.
#' @noRd
summarize_pop_n <- function(data, subject = NULL) {
  subject <- summarize_chr1(subject)
  if (!is.null(subject) && subject %in% names(data)) {
    return(dplyr::n_distinct(data[[subject]]))
  }
  summarize_n_rows(data)
}

#' @noRd
summarize_has_pct <- function(func) {
  isTRUE(func %in% c("count", "count_distinct"))
}

# The measures whose parts SUM to the whole: counting the mild rows plus
# counting the severe rows IS the group's count, while the mean of each is not
# its mean. Only these may be STACKED into a split bar -- everything else
# splits side by side. Both split-bar producers (summarize_table_prep() and the
# summaries plan in table-model-groups-plan.R) ask here.
#' @noRd
summarize_additive <- function(func) {
  isTRUE(func %in% c("count", "count_distinct", "sum"))
}

# Level order, matching the chart's _orderLevels: factor levels, else
# sorted -- NUMERICALLY for a numeric or date column, which is the order
# such a column carries. Sorting its labels as text puts day 10 before
# day 2.
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

# Colour levels: summarize_levels() over the rows the table draws, minus the
# factor levels none of them carries. A filter upstream keeps a factor's full
# level set, and a population join adds rows with no group that still carry
# their arm. Either way the level would draw an empty slot in each row and a
# legend key for nothing. `drawn` marks the rows with a complete group.
#' @noRd
summarize_color_levels <- function(x, drawn = NULL) {
  if (!is.null(drawn)) x <- x[drawn]
  summarize_levels(if (is.factor(x)) droplevels(x) else x)
}

# The DATA's own order for a grouping column -- the chart's dataOrder():
# factor levels when the column carries them (order lives in factors), else
# first-appearance in the rows. ADaM arrives visit-sorted, so a character
# AVISIT still reads chronologically without an upstream factor mutate.
# Alphabetical would put "Week 10" before "Week 2".
#' @noRd
summarize_data_levels <- function(x) {
  lv <- if (is.factor(x)) {
    levels(x)
  } else if (is.numeric(x) || inherits(x, c("Date", "POSIXct"))) {
    # A number IS its own order. First appearance is the fallback for a
    # column that carries no order of its own -- a character one -- and
    # applying it to study days scatters them (2, 10, 1, 100).
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

#' Prepare the table and its column plan.
#'
#' Everything the renderer needs, computed once server-side: the row list
#' (parents and children), the per-column values, the scales, the
#' denominators, and the palette. Split from the HTML so it is testable
#' without htmltools and so the block can validate before rendering.
#'
#' One row per level of `by` (outer first, at most two), one column per
#' entry of `summaries` (lane_prepare_summaries()). An empty `summaries`
#' counts rows, as a new groups table does (table_groups_seed()).
#'
#' Returns `list(err =)` when the config cannot be honored (missing column),
#' else `list(rows =, plan =, ...)`.
#'
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
    if (is.list(s)) c(summarize_chr1(s[["color"]]), summarize_chr1(s[["facet"]]))
  }))

  # Missing values of a grouping, colour or facet column: no group of their
  # own ("drop", the rows count toward the N but draw nothing) or a level
  # called "(Missing)" ("level").
  if (identical(summarize_chr1(missing), "level")) {
    data <- table_missing_levels(data, c(by, split_cols))
  }

  # One copy of each group's rows for every column the table splits by that
  # carries a group definition (expand_groups()): the rows and the colour
  # and facet splits of the summaries. Each input row is tagged first, so a
  # count's percent base counts it once (summarize_n_rows()).
  pooled <- intersect(c(by, split_cols), names(data))
  if (any(vapply(pooled, function(col) !is.null(group_def(data[[col]])),
                 logical(1L)))) {
    data[[SUMMARIZE_ROW_ID]] <- seq_len(nrow(data))
  }
  data <- expand_role_groups(data, c(by, split_cols))

  prep <- lane_prepare_summaries(
    data, by, summaries,
    facet_layout = summarize_chr1(facet_layout) %||% "by_summary",
    sort_by = sort_by, sort_dir = sort_dir, top_n = top_n,
    scale_map = scale_map, subject = subject, pct_of = pct_of
  )
  summarize_number_format(prep, format, unit)
}

# The table's `format` and `unit` on a prepared groups table. `format` NULL
# keeps the engine's own number formatting (summarize_num_parts()); else the
# number cells, the value labels beside bars and the cards print through
# dt_format_num(). `unit` heads the columns whose values are in the data's
# unit (`unit_ok`: a simple summary that is not a count), after the summary's
# name, as the records layout heads its numeric columns.
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

# Facet columns with nothing to draw, left out: the chart draws no panel for a
# level without data, and a column of empty tracks reads as a broken table.
# Empty = every cell missing, or for a count (`zero_empty`) every cell zero --
# a group filtered away but kept as a factor level, or one whose rows carry no
# record (the population join's). Checked over ALL rows, never the Top N cut,
# so a cut cannot drop a column. A table left with no column keeps them all.
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

# `sort_by` as one string. "label" is the old name of "alpha" (alphabetical),
# renamed to the chart's word.
#' @noRd
summarize_sort_by <- function(sort_by) {
  sb <- summarize_chr1(sort_by) %||% "value"
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

# A raw data column as the ordering key: the group's MINIMUM of that column
# (chart parity: `mins` in orderGroups, chart/model-aggregated.js). AVISITN
# orders the visits that a character AVISIT cannot -- first appearance breaks
# down as soon as one subject discontinues early. Groups the column has nothing
# for keep NA and sort last in both directions (na.last in
# summarize_assemble_rows).
#' @noRd
summarize_min_ord <- function(data, keycol, sortcol, labels) {
  v <- suppressWarnings(as.numeric(data[[sortcol]]))
  m <- tapply(v, as.character(data[[keycol]]), function(z) {
    z <- z[is.finite(z)]
    if (length(z)) min(z) else NA_real_
  })
  as.numeric(m[as.character(labels)])
}

# Resolve `sort_by` to a column that exists on the assembled frames. Keywords,
# summary names and facet levels already name one; a raw data column is folded
# into `.ord` here, so the assembler still sees a single key.
#' @noRd
summarize_resolve_sort <- function(sort_by, plan, data, leaf, par_rows, group,
                                   parent) {
  key <- summarize_sort_key(sort_by, plan)
  sb <- summarize_sort_by(sort_by)
  # Ordering by the row NAME when that name is a number: compare the
  # numbers. `.label` is the display string, and as text "10" sorts before
  # "2".
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



# The first underlying row's value of `col` per key combination, ANY type
# (a field column may be text). Absent keys are NA -- there is no row to
# read. Only the as-is measure calls this, where one row per group is the
# data's own contract.
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

#' @noRd
summarize_chr1 <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NULL else x[[1L]]
}

# The label column's sub-line: the group column's variable label, or the pair
# of labels when the table is nested. NULL when the columns carry none (or the
# label just repeats the name), which is dt_col_label()'s own rule.
#' @noRd
summarize_group_label <- function(data, group, parent) {
  g <- dt_col_label(data[[group]], group)
  if (is.null(parent)) return(g)
  p <- dt_col_label(data[[parent]], parent)
  if (is.null(p) && is.null(g)) return(NULL)
  paste0(p %||% parent, " / ", g %||% group)
}
