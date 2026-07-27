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
#
# `column` is the actual data column behind `col`, and resolution always goes
# through the package's one seam (dd_resolve_scales -> blockr.theme's
# provenance-aware resolver), so a column the picker block copied (SEX picked
# into "color") inherits the source column's binding and the board's fixed
# colors survive the rename. A caller holding only the levels passes none;
# the levels then stand in for the column, which resolves them by name (they
# carry no provenance to follow).
#' @noRd
rank_level_colors <- function(map, col, levels, column = NULL) {
  levels <- as.character(levels)
  if (!length(levels)) {
    return(character())
  }
  if (!is.null(map) && !is.null(col) &&
        requireNamespace("blockr.theme", quietly = TRUE)) {
    res <- dd_resolve_scales(map, col, column %||% levels)
    pal <- res$color
    if (!is.null(pal) && all(levels %in% names(pal))) {
      return(stats::setNames(unname(pal[levels]), levels))
    }
  }
  stats::setNames(rep_len(dd_palette(), length(levels)), levels)
}

# The aggregation vocabulary, as a summarise expression over one group.
# Mirrors the chart's AGG_FNS plus its chart-only "identity" ("None (as is)"):
# `count` needs no value column, everything else reduces `value`
# (count_distinct reduces `id_var`).
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
    identity = bquote(rank_agg_first(.data[[.(value)]])),
    quote(dplyr::n())
  )
}

# identity ("None (as is)"): the group's value untouched, for data whose bar
# lengths are already computed upstream (one row per group -- e.g. one value
# per subject). Duplicate rows collapse to the first non-missing value and an
# all-missing group stays NA, matching the chart engine's identity branch
# (drilldown-agg.js aggregate()).
#' @noRd
rank_agg_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) as.numeric(x[[1L]]) else NA_real_
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

# The DATA's own order for a grouping column -- the chart's dataOrder():
# factor levels when the column carries them (order lives in factors), else
# first-appearance in the rows. ADaM arrives visit-sorted, so a character
# AVISIT still reads chronologically without an upstream factor mutate.
# Alphabetical would put "Week 10" before "Week 2".
#' @noRd
rank_data_levels <- function(x) {
  lv <- if (is.factor(x)) levels(x) else unique(as.character(x))
  lv[!is.na(lv)]
}

# Rank of each label in the data's own order; labels the column no longer
# carries (an upstream filter dropped them) sort last.
#' @noRd
rank_data_ord <- function(x, labels) {
  lv <- rank_data_levels(x)
  i <- match(as.character(labels), lv)
  i[is.na(i)] <- length(lv) + 1L
  as.numeric(i)
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
                         cols = NULL, fields = NULL, sort_by = "value",
                         sort_dir = "desc", top_n = NULL, scale_map = NULL,
                         summaries = list(), by = NULL,
                         facet_layout = "by_summary") {
  bad <- function(msg) list(err = msg)

  if (!is.data.frame(data)) return(bad("No data"))
  if (!nrow(data)) return(bad("No rows to display"))

  # The summarize-table path (_blockr.design/open/summarize-table/): the
  # column list is THE config model. Only the ranked bar keeps its own
  # path (its colour split / comparison / percent machinery predates the
  # list and ships in real boards). `by` (outer -> inner) wins over
  # group/parent; unset, they fill in.
  if (is.list(summaries) && length(summaries)) {
    eby <- as.character(by %||% character())
    eby <- eby[nzchar(eby)]
    if (!length(eby)) eby <- c(rank_chr1(parent), rank_chr1(group))
    return(lane_prepare_summaries(
      data, eby, summaries, facet = rank_chr1(facet),
      facet_layout = rank_chr1(facet_layout) %||% "by_summary",
      color = rank_chr1(color),
      sort_by = sort_by,
      sort_dir = sort_dir, top_n = top_n, scale_map = scale_map
    ))
  }

  group <- rank_chr1(group)
  if (is.null(group)) return(bad("Pick a Group column in the gear"))
  if (!group %in% names(data)) {
    return(bad(paste0("Mapped column not in data: Group = \"", group,
                      "\". Re-pick it in the gear.")))
  }


  # Every mapped column must exist. A rename or pivot upstream is the usual
  # cause, and naming the column beats a stack trace.
  # `value` is only a mapped column for the aggregations that reduce it --
  # count and count_distinct never touch it, so a stale ".count" default must
  # not be reported as missing.
  needs_value <- func %in% c("identity", "sum", "mean", "median", "min", "max")
  # ".count" is the unset sentinel for the value slot (the constructor default,
  # meaningful only for count): asking for a mean without picking a column is a
  # "pick one" prompt, not a missing-column report.
  if (needs_value && (is.null(rank_chr1(value)) ||
                        identical(rank_chr1(value), ".count"))) {
    return(bad(if (identical(func, "identity")) {
      "Pick a Value column to show as is"
    } else {
      paste0("Pick a Value column to ", func)
    }))
  }
  # A REQUIRED column that vanished upstream is a broken table, reported by
  # name (a rename or pivot is the usual cause, and naming the column beats a
  # stack trace).
  mapped <- c(
    Group = group,
    Value = if (needs_value) rank_chr1(value) else NULL,
    `Subject id` = if (identical(func, "count_distinct")) {
      rank_chr1(id_var)
    }
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

  # An OPTIONAL dimension mapped to a column not present in the current data
  # reads as unmapped -- the table renders without that dim (chart parity:
  # present_role in chart-block.R). This is what makes an upstream picker's
  # "(none)" (which emits no such column) actually turn the dim off: the
  # picker changes DATA, never this block's config, so the config must
  # self-heal against the data. The saved pick survives untouched, and the
  # dim comes back the moment the column does.
  present <- function(col) {
    col <- rank_chr1(col)
    if (is.null(col) || !col %in% names(data)) NULL else col
  }
  parent <- present(parent)
  color <- present(color)
  facet <- present(facet)
  id_var <- rank_chr1(id_var)

  # color and facet TOGETHER mirror the chart: one bar column per facet
  # level, each bar split into colour segments. Only a comparison still owns
  # the colour slot (its bars are coloured by direction) -- reported, never
  # silent.
  note <- NULL
  compare <- if (is.null(facet)) NULL else rank_chr1(compare)
  if (!is.null(color) && !is.null(compare)) {
    note <- paste0(
      "A comparison colours its bars by direction; ignoring the colour ",
      "split by \"", color, "\"."
    )
    color <- NULL
  }
  layout <- if (!is.null(facet)) {
    if (!is.null(compare)) "compare" else "facet"
  } else if (!is.null(color)) {
    "split"
  } else {
    "simple"
  }

  keys <- c(parent, group)
  pct_ok <- rank_has_pct(func)
  # Separate numeric columns beside the bar are OPT-IN: the bar cell carries
  # its own value label (see `show_val` below), so the columns exist for
  # boards that ask for them -- and asking for them mutes the in-bar label,
  # never duplicates it.
  cols <- intersect(as.character(cols %||% character()), c("n", "pct"))
  if (!pct_ok) cols <- setdiff(cols, "pct")

  # --- leaf rows -----------------------------------------------------------
  leaf <- rank_aggregate(data, keys, func, value, id_var)
  if (!nrow(leaf)) return(bad("No rows to display"))
  leaf$.label <- as.character(leaf[[group]])
  leaf$.parent <- if (is.null(parent)) NA_character_ else as.character(leaf[[parent]])
  # The data's own order, kept alongside the measure so `sort_by = "data"`
  # costs nothing when unused.
  leaf$.ord <- rank_data_ord(data[[group]], leaf$.label)

  denom <- rank_denom(data, func, id_var)

  # What an ABSENT (group, level) cell is: zero for the additive measures
  # (no rows = nothing to count or sum), no value at all for the rest -- a
  # subject has no mean or as-is value in an arm they are not in. NA renders
  # as a blank cell and a zero-width bar, the chart's null gap.
  absent <- if (func %in% c("count", "count_distinct", "sum")) 0 else NA_real_

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
  measure_sub <- if (identical(func, "identity")) {
    # The bar column header is the value column itself (rank_measure_label);
    # the sub-line carries its variable label when it adds one.
    dt_col_label(data[[value]], value)
  } else if (needs_value) {
    lbl <- dt_col_label(data[[value]], value) %||% value
    paste0(AGG_WORDS[[func]] %||% func, ": ", lbl)
  } else if (identical(func, "count_distinct")) {
    paste0("distinct ", id_var)
  } else {
    NULL
  }
  # The bar cell carries its own value label ("26 (43%)" for a counting
  # measure, the plain value otherwise) unless separate columns were asked
  # for. `val_denom` is the LABEL's percentage base; `denom` (facet layouts)
  # scales the bar WIDTHS.
  show_val <- !length(cols)
  if (identical(layout, "simple")) {
    plan <- list(list(kind = "bar", label = rank_measure_label(func, value),
                      key = ".v", sub_label = measure_sub, fill = solo_fill,
                      show_val = show_val,
                      val_denom = if (pct_ok) denom))
  } else if (identical(layout, "split")) {
    series <- rank_levels(data[[color]])
    pal <- rank_level_colors(scale_map, color, series, data[[color]])
    seg <- rank_aggregate(data, c(keys, color), func, value, id_var)
    # One column per level, joined onto the leaf rows in level order.
    for (lv in series) {
      s <- seg[as.character(seg[[color]]) == lv, , drop = FALSE]
      leaf[[paste0(".s_", lv)]] <- rank_match(leaf, s, keys, absent)
    }
    plan <- list(list(kind = "barsplit", label = rank_measure_label(func, value),
                      key = ".v", series = series, mode = bar_mode,
                      sub_label = measure_sub, show_val = show_val,
                      val_denom = if (pct_ok) denom))
  } else {
    facet_levels <- rank_levels(data[[facet]])
    if (length(facet_levels) < 2L) {
      return(bad(paste0(
        "Facet column \"", facet, "\" has fewer than two levels; ",
        "nothing to compare across columns."
      )))
    }
    fac <- rank_aggregate(data, c(keys, facet), func, value, id_var)
    for (lv in facet_levels) {
      s <- fac[as.character(fac[[facet]]) == lv, , drop = FALSE]
      leaf[[paste0(".f_", lv)]] <- rank_match(leaf, s, keys, absent)
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
          # The signed delta rides IN the difference bar's cell; a separate
          # column would say the same number twice.
          list(kind = "bardiv", label = "Difference (pp)",
               key = paste0(".d_", lv), compare = compare, level = lv,
               sub_label = paste0("vs ", compare), show_val = TRUE)
        ))
      }
    } else if (!is.null(color)) {
      # Facet AND colour, the chart's two independent mappings: one bar
      # column per facet level, each bar split into colour segments. Facet
      # columns are keyed by INDEX (.f<i>s_<level>) so a facet level name can
      # never collide with a colour level name.
      series <- rank_levels(data[[color]])
      pal <- rank_level_colors(scale_map, color, series, data[[color]])
      seg <- rank_aggregate(data, c(keys, facet, color), func, value, id_var)
      for (fi in seq_along(facet_levels)) {
        fv <- facet_levels[[fi]]
        sf <- seg[as.character(seg[[facet]]) == fv, , drop = FALSE]
        for (cv in series) {
          s <- sf[as.character(sf[[color]]) == cv, , drop = FALSE]
          leaf[[paste0(".f", fi, "s_", cv)]] <-
            rank_match(leaf, s, keys, absent)
        }
        plan <- c(plan, list(list(
          kind = "barsplit", label = fv, key = paste0(".f_", fv),
          prefix = paste0(".f", fi, "s_"), series = series, mode = bar_mode,
          denom = if (pct_ok) denoms[[fv]],
          sub_label = paste0("N = ", denoms[[fv]]),
          show_val = TRUE, val_denom = if (pct_ok) denoms[[fv]]
        )))
      }
    } else {
      # A denominator only exists for the counting measures: their faceted
      # bars share a percentage scale (each arm over its own N). A mean or an
      # as-is value has no percentage -- those bars share the raw scale.
      # Facet bars are NOT hue-coded by level: the column header already
      # names the level, and the colour slot stays free for a real `color`
      # mapping (chart parity -- a facet never recolours the marks).
      for (lv in facet_levels) {
        plan <- c(plan, list(list(
          kind = "bar", label = lv, key = paste0(".f_", lv),
          fill = solo_fill,
          denom = if (pct_ok) denoms[[lv]],
          sub_label = paste0("N = ", denoms[[lv]]),
          show_val = TRUE, val_denom = if (pct_ok) denoms[[lv]]
        )))
      }
    }
  }

  # The plain n / % columns ride after the bar on the non-faceted layouts,
  # only when explicitly asked for (the in-bar label is the default).
  if (layout %in% c("simple", "split") && length(cols)) {
    if ("n" %in% cols) {
      # "n" only reads as n for a COUNT: a max or a mean in a column headed "n"
      # is a lie, so a non-counting measure gets a neutral "Value" (the bar
      # column's header already names the statistic).
      plan <- c(plan, list(list(
        kind = "num", label = if (pct_ok) "n" else "Value", key = ".v"
      )))
    }
    if ("pct" %in% cols) {
      plan <- c(plan, list(list(kind = "num", label = "%", key = ".v",
                                denom = denom, pct_only = TRUE,
                                sub_label = paste0("of ", denom))))
    }
  }

  # Extra columns from the underlying row -- the chart's tooltip fields,
  # shown as real (sortable, searchable) columns. Only the as-is measure has
  # ONE underlying row per group; under any aggregation a row column would be
  # an arbitrary representative, which a table must not present as data.
  fields <- as.character(fields %||% character())
  fields <- setdiff(fields[nzchar(fields)], group)
  if (length(fields) && !identical(func, "identity")) {
    note <- paste(c(note, paste0(
      "Extra columns need the as-is measure; ignoring ",
      paste0("\"", fields, "\"", collapse = ", "), "."
    )), collapse = " ")
    fields <- character()
  }
  fields <- intersect(fields, names(data))
  if (length(fields)) {
    fr <- data[!duplicated(data[keys]), , drop = FALSE]
    for (fld in fields) {
      leaf[[paste0(".x_", fld)]] <- rank_match_field(leaf, fr, keys, fld)
      plan <- c(plan, list(list(
        kind = "num", label = fld, key = paste0(".x_", fld),
        raw = TRUE, text = !is.numeric(data[[fld]]),
        sub_label = dt_col_label(data[[fld]], fld)
      )))
    }
  }

  # --- parent rows ---------------------------------------------------------
  # A parent is NOT the sum of its children (the same subject appears under
  # several preferred terms), so it is aggregated in its own pass.
  par_rows <- NULL
  if (!is.null(parent)) {
    par_rows <- rank_aggregate(data, parent, func, value, id_var)
    par_rows$.label <- as.character(par_rows[[parent]])
    par_rows$.ord <- rank_data_ord(data[[parent]], par_rows$.label)
    par_rows$.parent <- NA_character_
    if (identical(layout, "split")) {
      seg <- rank_aggregate(data, c(parent, color), func, value, id_var)
      for (lv in series) {
        s <- seg[as.character(seg[[color]]) == lv, , drop = FALSE]
        par_rows[[paste0(".s_", lv)]] <- rank_match(par_rows, s, parent, absent)
      }
    }
    if (layout %in% c("facet", "compare")) {
      fac <- rank_aggregate(data, c(parent, facet), func, value, id_var)
      for (lv in facet_levels) {
        s <- fac[as.character(fac[[facet]]) == lv, , drop = FALSE]
        par_rows[[paste0(".f_", lv)]] <- rank_match(par_rows, s, parent, absent)
      }
      if (identical(layout, "facet") && !is.null(color)) {
        seg <- rank_aggregate(data, c(parent, facet, color), func, value,
                              id_var)
        for (fi in seq_along(facet_levels)) {
          fv <- facet_levels[[fi]]
          sf <- seg[as.character(seg[[facet]]) == fv, , drop = FALSE]
          for (cv in series) {
            s <- sf[as.character(sf[[color]]) == cv, , drop = FALSE]
            par_rows[[paste0(".f", fi, "s_", cv)]] <-
              rank_match(par_rows, s, parent, absent)
          }
        }
      }
      if (identical(layout, "compare")) {
        for (lv in setdiff(facet_levels, compare)) {
          par_rows[[paste0(".d_", lv)]] <-
            par_rows[[paste0(".f_", lv)]] / denoms[[lv]] * 100 -
            par_rows[[paste0(".f_", compare)]] / denoms[[compare]] * 100
        }
      }
    }
    # A parent aggregates many rows, so it has no single underlying row: its
    # field cells stay blank.
    for (fld in fields) {
      par_rows[[paste0(".x_", fld)]] <- rep(NA, nrow(par_rows))
    }
  }

  # --- ordering + row list ---------------------------------------------------
  # Flat: leaves in rank order, optionally capped. Nested: parents in rank
  # order, each followed by its own children in rank order (a cap applies to
  # parents, since capping inside a class would hide a class's own drivers).
  # One shared implementation for every mark: rank_assemble_rows().
  srt <- rank_resolve_sort(sort_by, plan, data, leaf, par_rows, group, parent)
  asm <- rank_assemble_rows(srt$leaf, srt$par_rows, parent, srt$key, sort_dir,
                            top_n)
  rows <- asm$rows
  folded <- asm$folded
  fold_max <- asm$fold_max

  # --- bar scale -----------------------------------------------------------
  # ONE scale over the whole column (parents included), computed here and
  # never from the visible or filtered rows -- otherwise scrolling or
  # searching silently rescales a bar.
  bar_max <- rank_bar_max(rows, plan, denoms)

  list(
    rows = rows, plan = plan, layout = layout, mark = "bar",
    bar_max = bar_max, bar_min = 0,
    group_label = rank_group_label(data, group, parent),
    series = series, palette = pal, facet_levels = facet_levels,
    denoms = denoms, group = group, parent = parent, color = color,
    facet = facet, compare = compare, folded = folded, fold_max = fold_max,
    # par_rows is the UNCAPPED frame here (rank_assemble_rows caps a copy),
    # so its row count already is the group total.
    n_total = if (is.null(parent)) nrow(leaf) else nrow(par_rows),
    note = note, pct_ok = pct_ok, func = func
  )
}

# `sort_by` is either a plan-independent keyword or a facet level name.
#' @noRd
rank_sort_key <- function(sort_by, plan) {
  sb <- rank_chr1(sort_by) %||% "value"
  if (identical(sb, "label")) return(".label")
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
# (chart parity: `mins` in chart.js orderGroups). AVISITN orders the visits
# that a character AVISIT cannot -- first appearance breaks down as soon as
# one subject discontinues early. Groups the column has nothing for keep NA
# and sort last in both directions (na.last in rank_assemble_rows).
#' @noRd
rank_min_ord <- function(data, keycol, sortcol, labels) {
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
rank_resolve_sort <- function(sort_by, plan, data, leaf, par_rows, group,
                              parent) {
  key <- rank_sort_key(sort_by, plan)
  sb <- rank_chr1(sort_by) %||% "value"
  if (identical(key, ".v") && !sb %in% c("value", "label", "data") &&
        sb %in% names(data)) {
    leaf$.ord <- rank_min_ord(data, group, sb, leaf$.label)
    if (!is.null(par_rows)) {
      par_rows$.ord <- rank_min_ord(data, parent, sb, par_rows$.label)
    }
    key <- ".ord"
  }
  list(key = key, leaf = leaf, par_rows = par_rows)
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
      prefix <- p$prefix %||% ".s_"
      seg <- vapply(p$series, function(lv) {
        v <- rows[[paste0(prefix, lv)]]
        if (is.null(v)) rep(0, nrow(rows)) else v
      }, numeric(nrow(rows)))
      seg <- matrix(seg, nrow = nrow(rows))
      tot <- rowSums(seg, na.rm = TRUE)
      if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
        tot <- tot / p$denom * 100
      }
      vals <- c(vals, tot)
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
rank_match <- function(target, src, keys, fill = 0) {
  if (!nrow(src)) return(rep(fill, nrow(target)))
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  out <- src$.v[match(tk, sk)]
  out[is.na(out)] <- fill
  as.numeric(out)
}

# The first underlying row's value of `col` per key combination, ANY type
# (a field column may be text). Absent keys are NA -- there is no row to
# read. Only the as-is measure calls this, where one row per group is the
# data's own contract.
#' @noRd
rank_match_field <- function(target, src, keys, col) {
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  src[[col]][match(tk, sk)]
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
    # As-is: the column IS the measure, so it heads the bar column itself.
    identity = value,
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
