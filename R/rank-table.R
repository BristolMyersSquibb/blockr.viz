# Ranked bar table -----------------------------------------------------------
#
# Horizontal ranked bars rendered as an HTML table instead of an echarts
# chart: the bar is a div in a cell, so search, click-to-sort, exact values,
# a sticky header and an arbitrary row count come from the table form for
# free. Chart-block VOCABULARY (group / color / facet / bar_mode / sort_by /
# drill), table-block RENDERING (the shared html-table chrome and CSS).
#
# Deliberately horizontal bars only -- that constraint is what keeps the arg
# surface small. Design + mockups: dev/rank-table-design.md.
#
# Large tables follow the table block exactly: every row rendered, scrolling
# at `max_height` with a sticky header, search and sort client-side. `top_n`
# is opt-in (report exhibits: a pptx slide wants ten bars, not a hundred) and
# always draws a visible fold row -- never a silent truncation.

# One-hue-per-level pool, shared with the chart (dd_palette()) so a rank
# table and a chart of the same split agree on colors.
#' @noRd
rank_level_colors <- function(map, col, levels) {
  levels <- as.character(levels)
  if (!length(levels)) {
    return(character())
  }
  if (!is.null(map) && !is.null(col) &&
        requireNamespace("blockr.theme", quietly = TRUE)) {
    res <- tryCatch(
      blockr.theme::resolve_scales(
        map, col, levels = levels, palette = dd_palette()
      ),
      error = function(e) NULL
    )
    pal <- res$color
    if (!is.null(pal) && all(levels %in% names(pal))) {
      return(stats::setNames(unname(pal[levels]), levels))
    }
  }
  stats::setNames(rep_len(dd_palette(), length(levels)), levels)
}

# The aggregation vocabulary, as a summarise expression over one group.
# Mirrors the chart's AGG_FNS: `count` needs no value column, everything else
# reduces `value` (count_distinct reduces `id_var`).
#' @noRd
rank_agg_expr <- function(func, value, id_var) {
  switch(
    func %||% "count",
    count = quote(dplyr::n()),
    count_distinct = bquote(dplyr::n_distinct(.data[[.(id_var)]])),
    sum = bquote(sum(.data[[.(value)]], na.rm = TRUE)),
    mean = bquote(mean(.data[[.(value)]], na.rm = TRUE)),
    median = bquote(stats::median(.data[[.(value)]], na.rm = TRUE)),
    min = bquote(dd_agg_min(.data[[.(value)]])),
    max = bquote(dd_agg_max(.data[[.(value)]])),
    quote(dplyr::n())
  )
}

# Aggregate to one row per key combination. `keys` may be empty (a grand
# total). Returns a data frame of the keys plus `.v`.
#' @noRd
rank_aggregate <- function(data, keys, func, value, id_var) {
  ex <- rank_agg_expr(func, value, id_var)
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

# The denominator a percentage is taken over: distinct subjects when the
# measure counts subjects, rows otherwise. Any other aggregation (a mean, a
# sum) has no meaningful percentage -- callers drop the pct column instead of
# inventing a denominator.
#' @noRd
rank_denom <- function(data, func, id_var) {
  if (identical(func, "count_distinct") && !is.null(id_var) &&
        id_var %in% names(data)) {
    return(dplyr::n_distinct(data[[id_var]]))
  }
  nrow(data)
}

#' @noRd
rank_has_pct <- function(func) {
  isTRUE(func %in% c("count", "count_distinct"))
}

# Level order, matching the chart's _orderLevels: factor levels, else sorted.
#' @noRd
rank_levels <- function(x) {
  lv <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  lv[!is.na(lv)]
}

#' Prepare the ranked frame and its column plan.
#'
#' Everything the renderer needs, computed once server-side: the row list
#' (parents and children), the per-column values, the shared bar scale, the
#' denominators, and the palette. Split from the HTML so it is testable
#' without htmltools and so the block can validate before rendering.
#'
#' Returns `list(err =)` when the config cannot be honored (missing column,
#' no group picked), else `list(rows =, cols =, ...)`.
#'
#' @noRd
rank_prepare <- function(data, group, value = ".count", func = "count",
                         id_var = NULL, parent = NULL, color = NULL,
                         bar_mode = "stacked", facet = NULL, compare = NULL,
                         cols = c("n", "pct"), sort_by = "value",
                         sort_dir = "desc", top_n = NULL, scale_map = NULL) {
  bad <- function(msg) list(err = msg)

  if (!is.data.frame(data)) return(bad("No data"))
  if (!nrow(data)) return(bad("No rows to display"))

  group <- rank_chr1(group)
  if (is.null(group)) return(bad("Pick a Rank by column in the gear"))

  # Every mapped column must exist. A rename or pivot upstream is the usual
  # cause, and naming the column beats a stack trace.
  # `value` is only a mapped column for the aggregations that reduce it --
  # count and count_distinct never touch it, so a stale ".count" default must
  # not be reported as missing.
  needs_value <- func %in% c("sum", "mean", "median", "min", "max")
  # ".count" is the unset sentinel for the value slot (the constructor default,
  # meaningful only for count): asking for a mean without picking a column is a
  # "pick one" prompt, not a missing-column report.
  if (needs_value && (is.null(rank_chr1(value)) ||
                        identical(rank_chr1(value), ".count"))) {
    return(bad(paste0("Pick a Value column to ", func)))
  }
  mapped <- c(
    `Rank by` = group, Parent = rank_chr1(parent), Color = rank_chr1(color),
    Facet = rank_chr1(facet), `Subject id` = rank_chr1(id_var),
    Value = if (needs_value) rank_chr1(value) else NULL
  )
  miss <- mapped[!mapped %in% names(data)]
  if (length(miss)) {
    return(bad(paste0(
      "Mapped column not in data: ",
      paste0(names(miss), " = \"", unname(miss), "\"", collapse = ", "),
      ". Re-pick it in the gear."
    )))
  }
  if (identical(func, "count_distinct") && is.null(rank_chr1(id_var))) {
    return(bad("Pick a Subject id column to count distinct subjects"))
  }

  parent <- rank_chr1(parent)
  color <- rank_chr1(color)
  facet <- rank_chr1(facet)
  id_var <- rank_chr1(id_var)

  # color and facet both claim the bar column. Rather than invent a two-way
  # layout, facet wins and color is ignored -- reported, never silent.
  note <- NULL
  if (!is.null(color) && !is.null(facet)) {
    note <- paste0(
      "Both Color and Facet are set; faceting by \"", facet,
      "\" and ignoring the color split."
    )
    color <- NULL
  }
  layout <- if (!is.null(facet)) {
    if (!is.null(compare) && nzchar(compare)) "compare" else "facet"
  } else if (!is.null(color)) {
    "split"
  } else {
    "simple"
  }

  keys <- c(parent, group)
  pct_ok <- rank_has_pct(func)
  cols <- intersect(as.character(cols %||% character()), c("n", "pct"))
  if (!pct_ok) cols <- setdiff(cols, "pct")
  if (!length(cols)) cols <- "n"

  # --- leaf rows -----------------------------------------------------------
  leaf <- rank_aggregate(data, keys, func, value, id_var)
  if (!nrow(leaf)) return(bad("No rows to display"))
  leaf$.label <- as.character(leaf[[group]])
  leaf$.parent <- if (is.null(parent)) NA_character_ else as.character(leaf[[parent]])

  denom <- rank_denom(data, func, id_var)

  # --- the column plan -----------------------------------------------------
  # Each entry: kind (bar / barsplit / bardiv / num), the per-row numeric
  # vector(s) it draws, and its header. The renderer walks this and knows
  # nothing about group / facet / compare.
  plan <- list()
  series <- NULL
  pal <- character()
  facet_levels <- character()
  denoms <- c(all = denom)

  # A single-series bar takes the chart's FIRST palette colour, not a CSS token:
  # a rank table and a bar chart of the same data are then the same blue (and
  # follow a themed board's palette together).
  solo_fill <- dd_palette(1L)
  measure_sub <- if (needs_value) {
    lbl <- dt_col_label(data[[value]], value) %||% value
    paste0(AGG_WORDS[[func]] %||% func, ": ", lbl)
  } else if (identical(func, "count_distinct")) {
    paste0("distinct ", id_var)
  } else {
    NULL
  }
  if (identical(layout, "simple")) {
    plan <- list(list(kind = "bar", label = rank_measure_label(func, value),
                      key = ".v", sub_label = measure_sub, fill = solo_fill))
  } else if (identical(layout, "split")) {
    series <- rank_levels(data[[color]])
    pal <- rank_level_colors(scale_map, color, series)
    seg <- rank_aggregate(data, c(keys, color), func, value, id_var)
    # One column per level, joined onto the leaf rows in level order.
    for (lv in series) {
      s <- seg[as.character(seg[[color]]) == lv, , drop = FALSE]
      leaf[[paste0(".s_", lv)]] <- rank_match(leaf, s, keys)
    }
    plan <- list(list(kind = "barsplit", label = rank_measure_label(func, value),
                      series = series, mode = bar_mode,
                      sub_label = measure_sub))
  } else {
    facet_levels <- rank_levels(data[[facet]])
    if (length(facet_levels) < 2L) {
      return(bad(paste0(
        "Facet column \"", facet, "\" has fewer than two levels; ",
        "nothing to compare across columns."
      )))
    }
    pal <- rank_level_colors(scale_map, facet, facet_levels)
    fac <- rank_aggregate(data, c(keys, facet), func, value, id_var)
    for (lv in facet_levels) {
      s <- fac[as.character(fac[[facet]]) == lv, , drop = FALSE]
      leaf[[paste0(".f_", lv)]] <- rank_match(leaf, s, keys)
      # Per-facet denominator: a percentage within an arm is over that arm's
      # own N, never the pooled total.
      sub <- data[as.character(data[[facet]]) == lv, , drop = FALSE]
      denoms[[lv]] <- rank_denom(sub, func, id_var)
    }
    if (identical(layout, "compare")) {
      if (!compare %in% facet_levels) {
        return(bad(paste0(
          "Compare to \"", compare, "\" is not a level of \"", facet,
          "\". Levels: ", paste(facet_levels, collapse = ", "), "."
        )))
      }
      if (!pct_ok) {
        return(bad(paste0(
          "A difference column needs a counting measure (Count or Count ",
          "distinct), so the two arms are comparable as percentages."
        )))
      }
      others <- setdiff(facet_levels, compare)
      plan <- c(plan, list(list(kind = "num", label = compare,
                                key = paste0(".f_", compare),
                                denom = denoms[[compare]], combined = TRUE,
                                sub_label = paste0("N = ", denoms[[compare]]))))
      for (lv in others) {
        # Risk difference in percentage points, comparator first.
        leaf[[paste0(".d_", lv)]] <-
          leaf[[paste0(".f_", lv)]] / denoms[[lv]] * 100 -
          leaf[[paste0(".f_", compare)]] / denoms[[compare]] * 100
        plan <- c(plan, list(
          list(kind = "num", label = lv, key = paste0(".f_", lv),
               denom = denoms[[lv]], combined = TRUE,
               sub_label = paste0("N = ", denoms[[lv]])),
          list(kind = "bardiv", label = "Difference (pp)",
               key = paste0(".d_", lv), compare = compare, level = lv,
               sub_label = paste0("vs ", compare)),
          list(kind = "num", label = "Δ", key = paste0(".d_", lv),
               signed = TRUE)
        ))
      }
    } else {
      for (lv in facet_levels) {
        plan <- c(plan, list(
          list(kind = "bar", label = lv, key = paste0(".f_", lv),
               fill = pal[[lv]], denom = denoms[[lv]],
               sub_label = paste0("N = ", denoms[[lv]])),
          list(kind = "num", label = "n (%)", key = paste0(".f_", lv),
               denom = denoms[[lv]], combined = TRUE)
        ))
      }
    }
  }

  # The plain n / % columns ride after the bar on the non-faceted layouts.
  if (layout %in% c("simple", "split")) {
    if ("n" %in% cols) {
      plan <- c(plan, list(list(kind = "num", label = "n", key = ".v")))
    }
    if ("pct" %in% cols) {
      plan <- c(plan, list(list(kind = "num", label = "%", key = ".v",
                                denom = denom, pct_only = TRUE,
                                sub_label = paste0("of ", denom))))
    }
  }

  # --- parent rows ---------------------------------------------------------
  # A parent is NOT the sum of its children (the same subject appears under
  # several preferred terms), so it is aggregated in its own pass.
  par_rows <- NULL
  if (!is.null(parent)) {
    par_rows <- rank_aggregate(data, parent, func, value, id_var)
    par_rows$.label <- as.character(par_rows[[parent]])
    par_rows$.parent <- NA_character_
    if (identical(layout, "split")) {
      seg <- rank_aggregate(data, c(parent, color), func, value, id_var)
      for (lv in series) {
        s <- seg[as.character(seg[[color]]) == lv, , drop = FALSE]
        par_rows[[paste0(".s_", lv)]] <- rank_match(par_rows, s, parent)
      }
    }
    if (layout %in% c("facet", "compare")) {
      fac <- rank_aggregate(data, c(parent, facet), func, value, id_var)
      for (lv in facet_levels) {
        s <- fac[as.character(fac[[facet]]) == lv, , drop = FALSE]
        par_rows[[paste0(".f_", lv)]] <- rank_match(par_rows, s, parent)
      }
      if (identical(layout, "compare")) {
        for (lv in setdiff(facet_levels, compare)) {
          par_rows[[paste0(".d_", lv)]] <-
            par_rows[[paste0(".f_", lv)]] / denoms[[lv]] * 100 -
            par_rows[[paste0(".f_", compare)]] / denoms[[compare]] * 100
        }
      }
    }
  }

  # --- ordering ------------------------------------------------------------
  sort_key <- rank_sort_key(sort_by, plan)
  ord <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    v <- if (identical(sort_key, ".label")) df$.label else df[[sort_key]]
    if (is.null(v)) v <- df$.v
    if (is.character(v)) {
      df[order(v, decreasing = identical(sort_dir, "desc")), , drop = FALSE]
    } else {
      df[order(v, decreasing = identical(sort_dir, "desc"), na.last = TRUE), ,
         drop = FALSE]
    }
  }

  # --- row list ------------------------------------------------------------
  # Flat: leaves in rank order, optionally capped. Nested: parents in rank
  # order, each followed by its own children in rank order (a cap applies to
  # parents, since capping inside a class would hide a class's own drivers).
  folded <- 0L
  fold_max <- NA_real_
  if (is.null(parent)) {
    rows <- ord(leaf)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 && nrow(rows) > top_n) {
      rest <- rows[seq.int(top_n + 1L, nrow(rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      rows <- rows[seq_len(top_n), , drop = FALSE]
    }
    rows$.level <- 0L
    rows$.is_parent <- FALSE
  } else {
    par_rows <- ord(par_rows)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 &&
          nrow(par_rows) > top_n) {
      rest <- par_rows[seq.int(top_n + 1L, nrow(par_rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      par_rows <- par_rows[seq_len(top_n), , drop = FALSE]
    }
    par_rows$.level <- 0L
    par_rows$.is_parent <- TRUE
    leaf$.level <- 1L
    leaf$.is_parent <- FALSE
    keep <- c(".label", ".parent", ".level", ".is_parent", ".v",
              grep("^\\.(s|f|d)_", names(leaf), value = TRUE))
    pieces <- list()
    for (i in seq_len(nrow(par_rows))) {
      p <- par_rows[i, , drop = FALSE]
      kids <- ord(leaf[!is.na(leaf$.parent) & leaf$.parent == p$.label, ,
                       drop = FALSE])
      pieces[[length(pieces) + 1L]] <- p[, intersect(keep, names(p)), drop = FALSE]
      if (nrow(kids)) {
        pieces[[length(pieces) + 1L]] <- kids[, intersect(keep, names(kids)),
                                              drop = FALSE]
      }
    }
    rows <- do.call(rbind, pieces)
  }

  # --- bar scale -----------------------------------------------------------
  # ONE scale over the whole column (parents included), computed here and
  # never from the visible or filtered rows -- otherwise scrolling or
  # searching silently rescales a bar.
  bar_max <- rank_bar_max(rows, plan, denoms)

  list(
    rows = rows, plan = plan, layout = layout, bar_max = bar_max,
    group_label = rank_group_label(data, group, parent),
    series = series, palette = pal, facet_levels = facet_levels,
    denoms = denoms, group = group, parent = parent, color = color,
    facet = facet, compare = compare, folded = folded, fold_max = fold_max,
    n_total = if (is.null(parent)) nrow(leaf) else nrow(par_rows) + folded,
    note = note, pct_ok = pct_ok, func = func
  )
}

# `sort_by` is either a plan-independent keyword or a facet level name.
#' @noRd
rank_sort_key <- function(sort_by, plan) {
  sb <- rank_chr1(sort_by) %||% "value"
  if (identical(sb, "label")) return(".label")
  if (identical(sb, "value")) return(".v")
  hit <- vapply(plan, function(p) identical(p$label, sb), logical(1L))
  if (any(hit)) {
    key <- plan[[which(hit)[[1L]]]]$key
    if (!is.null(key)) return(key)
  }
  ".v"
}

#' @noRd
rank_bar_max <- function(rows, plan, denoms) {
  vals <- numeric()
  for (p in plan) {
    if (identical(p$kind, "bar")) {
      v <- rows[[p$key]]
      if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
        v <- v / p$denom * 100
      }
      vals <- c(vals, v)
    } else if (identical(p$kind, "barsplit")) {
      seg <- vapply(p$series, function(lv) {
        v <- rows[[paste0(".s_", lv)]]
        if (is.null(v)) rep(0, nrow(rows)) else v
      }, numeric(nrow(rows)))
      seg <- matrix(seg, nrow = nrow(rows))
      vals <- c(vals, rowSums(seg, na.rm = TRUE))
    } else if (identical(p$kind, "bardiv")) {
      vals <- c(vals, abs(rows[[p$key]]))
    }
  }
  vals <- vals[is.finite(vals)]
  if (!length(vals)) 0 else max(vals)
}

# Percentages are shared across a faceted bar column set, so a bar's length
# means the same thing in every column. Absolute counts share the raw scale.
#' @noRd
rank_match <- function(target, src, keys) {
  if (!nrow(src)) return(rep(NA_real_, nrow(target)))
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  out <- src$.v[match(tk, sk)]
  out[is.na(out)] <- 0
  as.numeric(out)
}

#' @noRd
rank_measure_label <- function(func, value) {
  switch(
    func %||% "count",
    count = "Rows",
    count_distinct = "Subjects",
    sum = paste0("Sum of ", value),
    mean = paste0("Mean ", value),
    median = paste0("Median ", value),
    min = paste0("Min ", value),
    max = paste0("Max ", value),
    "Value"
  )
}

#' @noRd
rank_chr1 <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NULL else x[[1L]]
}

# The label column's sub-line: the group column's variable label, or the pair
# of labels when the table is nested. NULL when the columns carry none (or the
# label just repeats the name), which is dt_col_label()'s own rule.
#' @noRd
rank_group_label <- function(data, group, parent) {
  g <- dt_col_label(data[[group]], group)
  if (is.null(parent)) return(g)
  p <- dt_col_label(data[[parent]], parent)
  if (is.null(p) && is.null(g)) return(NULL)
  paste0(p %||% parent, " / ", g %||% group)
}

# The `n` column's sub-line: what one unit of n IS. A count of rows says
# "events"; a distinct count says which entity it counted.
#' @noRd
rank_n_sub <- function(func, id_var, data) {
  if (identical(func, "count")) return("rows")
  if (identical(func, "count_distinct") && !is.null(id_var)) {
    return(paste0("distinct ", id_var))
  }
  NULL
}
