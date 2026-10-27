# Table model, groups layout: plan entries and domains -------------------------
#
# The plan of the groups table: one entry per (summary x facet copy) naming
# the leaf columns, the display kind and the header words, then the domains
# computed over the assembled rows and shared across a summary's copies. The
# emitters and the JS packer read the entries; nothing here draws.

#' The colour dimension of a distribution column: the per-level statistic
#' columns, the level names and their colours. Empty (no extra plan fields)
#' when the summary has no colour, so an uncoloured column is untouched.
#'
#' Colours come from the same resolver as every other mark
#' (`summarize_level_colors`: board scale map -> theme -> palette), so a box
#' split by SEX matches the chart's boxplot split by SEX.
#' @noRd
lane_color_split <- function(s, sid, stats, data, scale_map) {
  lv <- s$.levels
  if (is.null(s$.color) || !length(lv)) return(list())
  list(
    levels = lv,
    lcols = lapply(seq_along(lv), function(j) {
      stats::setNames(paste0(sid, "_L", j, "_", stats), stats)
    }),
    fills = unname(summarize_level_colors(scale_map, s$.color, lv,
                                          data[[s$.color]])[lv]),
    cvar = summarize_col_name(data, s$.color)
  )
}

# A facet copy is headed by its level, the summary name moving to the
# sub-label. Where the table faceted by SEVERAL columns the level alone is
# ambiguous next to another summary's groups, so it names its column too.
lane_plan_label <- function(s, cp) {
  if (is.null(cp$level)) {
    s$name
  } else if (isTRUE(cp$qualify)) {
    paste0(cp$fcol, ": ", cp$level)
  } else {
    cp$level
  }
}

lane_plan_sub <- function(s, cp) {
  if (is.null(cp$level)) {
    NULL
  } else if (is.null(cp$pop)) {
    s$name
  } else {
    paste0(s$name, " · N = ", cp$pop)
  }
}

# A count with an N (see lane_prepare_summaries): the denominator when the
# copy carries a usable one, else NULL.
lane_plan_den <- function(cp) {
  den <- cp$den
  if (is.numeric(den) && length(den) == 1L && is.finite(den) && den > 0) {
    den
  }
}

# The fields every entry shares.
lane_plan_base <- function(s, cp, sid, label, sub, den) {
  base <- list(label = label, sub_label = sub, sid = sid, stype = s$type,
               flevel = cp$level, fcol = cp$fcol, sname = s$name,
               meas = s$name,
               # a count with nothing in it is an empty column
               # (summarize_drop_empty_facets)
               zero_empty = identical(s$type, "simple") &&
                 summarize_has_pct(summarize_chr1(s$func) %||% "count"),
               # Its values are in the data's unit (a sum, mean, ...), not a
               # count: the table's `unit` heads it.
               unit_ok = identical(s$type, "simple") &&
                 !summarize_has_pct(summarize_chr1(s$func) %||% "count"))
  # A count with an N (see lane_prepare_summaries): the header's second line
  # says N, the value reads "8 (12%)", and a bar's length is the percentage,
  # so arms of different size compare on one scale.
  if (!is.null(den)) {
    base$sub_label <- if (isTRUE(cp$named)) {
      paste0(s$name, " · N = ", den)
    } else {
      paste0("N = ", den)
    }
    base$val_denom <- den
  }
  base
}

#' One plan entry for one (summary x facet-copy). The entry carries the
#' leaf column names, the display kind, and (after lane_summary_domains)
#' its own scale.
#' @noRd
lane_summary_plan <- function(s, cp, data, scale_map = NULL) {
  sid <- cp$suffix
  sub <- lane_plan_sub(s, cp)
  den <- lane_plan_den(cp)
  base <- lane_plan_base(s, cp, sid, lane_plan_label(s, cp), sub, den)
  if (identical(s$type, "simple")) {
    lane_plan_simple(s, sid, base, den, data, scale_map)
  } else if (identical(s$type, "dist")) {
    lane_plan_dist(s, sid, base, sub, data, scale_map)
  } else if (!is.null(s$.failed)) {
    c(base, list(kind = "num", key = paste0(sid, "_t"), raw = TRUE,
                 text = TRUE, failed = s$.failed))
  } else if (identical(s$type, "field")) {
    c(base, list(kind = "num", key = paste0(sid, "_t"), raw = TRUE,
                 text = TRUE,
                 sub_label = sub %||% dt_col_label(data[[s$col]], s$col)))
  } else if (identical(s$type, "expr")) {
    # Numeric vs text is decided by the eval; the plan names the numeric
    # key and summarize_cells falls back to the text column when it is absent.
    c(base, list(kind = "num", key = paste0(sid, "_v"),
                 alt_text = paste0(sid, "_t")))
  } else if (identical(s$type, "pair")) {
    lane_plan_pair(s, sid, base, sub, data, scale_map)
  } else if (identical(s$type, "series")) {
    c(base, list(kind = "sparkline", key = paste0(sid, "_last"),
                 pts = paste0(sid, "_pts"), x = summarize_chr1(s$x),
                 dom_date = isTRUE(s$.date), ref = cp$ref,
                 show_val = TRUE))
  } else {
    lane_plan_spans(s, sid, base, data, scale_map)
  }
}

# The simple row: a dot, a split bar, a tile, or a plain bar / number.
lane_plan_simple <- function(s, sid, base, den, data, scale_map) {
  if (identical(s$show, "dot")) {
    # A single value as a positioned point on the zero-based lane: the
    # pointrange emitter with no interval. Less ink than a bar.
    c(base, list(kind = "pointrange", key = paste0(sid, "_v"),
                 cols = c(bc = paste0(sid, "_v")),
                 # Length-from-zero semantics, like the bar it replaces:
                 # the domain keeps its zero (lane_summary_domains).
                 zero = TRUE,
                 # No range of any kind, so nothing spans the cell: this
                 # is the mark that keeps its hairline.
                 bare = TRUE,
                 words = list(center = s$name), show_val = TRUE),
      lane_color_split(s, sid, "bc", data, scale_map))
  } else if (identical(s$show, "bar") && !is.null(s$.color) &&
               length(s$.levels)) {
    lane_plan_barsplit(s, sid, base, den, data, scale_map)
  } else if (identical(s$show, "tile")) {
    lane_plan_tile(s, sid, base, data, scale_map)
  } else {
    kind <- if (identical(s$show, "bar")) "bar" else "num"
    c(base, list(kind = kind, key = paste0(sid, "_v"),
                 fill = if (identical(kind, "bar")) dd_palette(1L),
                 show_val = identical(kind, "bar"),
                 # A bar's length is the percentage; a number column
                 # prints "54 (18%)", the table's own "n (%)" cell.
                 denom = den,
                 combined = !is.null(den) && identical(kind, "num")))
  }
}

# Colour on a magnitude is its composition: the bar keeps meaning the
# group's total and the segments say what it is made of. A group whose
# rows are all one level draws ONE segment, in that level's colour.
# Only for an ADDITIVE measure, though: the parts of a mean do not add
# up to it, so those segments sit side by side (summarize_additive())
# rather than stacking into a length nothing computes. A custom row's
# `value` may be a mean as easily as a count, and the block cannot tell,
# so its levels always sit side by side.
#
# `mode` is provisional: nothing here has seen a value yet, so a
# non-additive measure asks for lanes even when every row turns out to
# hold ONE level and there is nothing to lay beside anything.
# lane_summary_domains() sees the assembled rows and stacks those back
# (lane_split_degenerate()).
lane_plan_barsplit <- function(s, sid, base, den, data, scale_map) {
  c(base, list(kind = "barsplit", key = paste0(sid, "_v"),
               prefix = paste0(sid, "_S_"), series = s$.levels,
               cvar = summarize_col_name(data, s$.color),
               ccol = s$.color,
               mode = local({
                 additive <- is.null(s$.custom) && summarize_additive(
                   summarize_chr1(s$func) %||% "count"
                 )
                 # Parts of a mean do not add up to it: such a split
                 # sits side by side whatever is asked for.
                 if (!additive) {
                   "grouped"
                 } else {
                   s$.mode %||% "stacked"
                 }
               }),
               show_val = TRUE,
               denom = den,
               fills = unname(summarize_level_colors(
                 scale_map, s$.color, s$.levels, data[[s$.color]]
               )[s$.levels])))
}

# The matrix cell: the value in a square, painted by the worst level of
# `fill`, or by the value on the column's ramp. A copy's header is its
# level alone; the N line would not fit a rotated title.
lane_plan_tile <- function(s, sid, base, data, scale_map) {
  base$sub_label <- NULL
  fills <- if (!is.null(s$.fill)) {
    lane_tile_colors(scale_map, s$.fill, s$.fill_levels, data[[s$.fill]])
  }
  c(base, list(kind = "tile", key = paste0(sid, "_v"),
               wkey = if (!is.null(s$.fill)) paste0(sid, "_w"),
               fills = if (!is.null(fills)) unname(fills),
               flevels_fill = s$.fill_levels,
               fvar = if (!is.null(s$.fill)) {
                 summarize_col_name(data, s$.fill)
               },
               show_val = TRUE))
}

# The distribution row: a text column or one glyph in two styles.
lane_plan_dist <- function(s, sid, base, sub, data, scale_map) {
  stat <- summarize_chr1(s$stat) %||% "median_q1_q3"
  meta <- LANE_STAT_META[[stat]] %||% LANE_STAT_META$median_q1_q3
  if (identical(s$show, "text")) {
    return(c(base, list(kind = "num", key = paste0(sid, "_bc"),
                        dispkey = "dist_text",
                        lo = paste0(sid, "_bl"), hi = paste0(sid, "_bh"),
                        sub_label = sub %||% meta$label)))
  }
  # ONE glyph, two styles. The leaf columns follow the pieces that are
  # switched on, so an absent range ships no columns at all and the
  # emitter draws what it is given (never re-derived arithmetic).
  inner <- summarize_chr1(s$inner) %||% "median_q1_q3"
  outer <- summarize_chr1(s$outer) %||% "none"
  cmeta <- LANE_STAT_META[[lane_dist_centre_stat(s)]] %||%
    LANE_STAT_META$median_q1_q3
  cols <- c(bc = paste0(sid, "_bc"), n = paste0(sid, "_n"))
  if (!identical(inner, "none")) {
    cols <- c(cols, bl = paste0(sid, "_bl"), bh = paste0(sid, "_bh"))
  }
  if (!identical(outer, "none")) {
    cols <- c(cols, wl = paste0(sid, "_wl"), wh = paste0(sid, "_wh"))
  }
  wmeta <- LANE_STAT_META[[outer]] %||% LANE_STAT_META$tukey
  c(base, list(kind = if (identical(s$style, "box")) "box" else
                 "pointrange",
               key = paste0(sid, "_bc"), cols = cols,
               # The ground rule, decided here rather than in CSS: a mark
               # that draws an outer range brings its own rail, so its
               # lane is bare; one without gets the hairline back.
               bare = identical(outer, "none"),
               words = list(
                 center = cmeta$center,
                 range = if (!identical(inner, "none")) meta$range,
                 whisk = if (!identical(outer, "none")) wmeta$range
               ),
               sub_label = sub %||% lane_dist_sub_label(s, meta, wmeta),
               show_val = TRUE),
    lane_color_split(s, sid, names(cols), data, scale_map))
}

# The columns of one dumbbell under `prefix`.
lane_pair_cols <- function(prefix) {
  c(a = paste0(prefix, "_from"), b = paste0(prefix, "_to"),
    lo = paste0(prefix, "_lo"), hi = paste0(prefix, "_hi"))
}

# The pair row: a dumbbell, one per colour level when split.
lane_plan_pair <- function(s, sid, base, sub, data, scale_map) {
  lv <- s$.levels
  dlv <- s$.dlevels
  words <- c(
    trimws(paste(LANE_PAIR_WORDS[[s$from_func]],
                 summarize_col_name(data, s$from), "◇")),
    trimws(paste(LANE_PAIR_WORDS[[s$to_func]],
                 summarize_col_name(data, s$to), "●"))
  )
  # Set on `base` itself: c() would append a second `sub_label` that `$`
  # never reads past the first (NULL) one.
  base$sub_label <- sub %||% paste0(
    words[[1L]], " → ", words[[2L]],
    if (length(dlv) > 1L) {
      paste0(" · dashed: ", summarize_col_name(data, s$dash), " ≠ ",
             dlv[[1L]])
    } else {
      ""
    }
  )
  c(base, list(kind = "pair", key = paste0(sid, "_to"),
               cols = lane_pair_cols(sid), ncol = paste0(sid, "_n"),
               fidx = paste0(sid, "_f"), didx = paste0(sid, "_d"),
               ref = s$ref, levels = lv,
               words = list(from = words[[1L]], to = words[[2L]]),
               fills = if (!is.null(lv)) {
                 unname(summarize_level_colors(
                   scale_map, summarize_chr1(s$color), lv,
                   data[[summarize_chr1(s$color)]]
                 )[lv])
               },
               show_val = TRUE),
    # The colour split: one dumbbell per level, each with its own ends,
    # band and dash index (lane_pair_fill() writes `<sid>_L<j>_*`).
    if (!is.null(s$.color) && length(lv)) {
      list(lcols = lapply(seq_along(lv), function(j) {
             lane_pair_cols(paste0(sid, "_L", j))
           }),
           ldidx = paste0(sid, "_L", seq_along(lv), "_d"),
           lncol = paste0(sid, "_L", seq_along(lv), "_n"),
           cvar = summarize_col_name(data, s$.color))
    })
}

# The spans row: the swimlane of segments.
lane_plan_spans <- function(s, sid, base, data, scale_map) {
  lv <- s$.levels
  x <- summarize_chr1(s$x)
  xend <- summarize_chr1(s$xend)
  c(base, list(kind = "interval", key = paste0(sid, "_start"),
               segs = paste0(sid, "_segs"), x = x,
               xend = xend, levels = lv,
               dom = lane_axis_domain_attr(data, c(x, xend)),
               dom_date = isTRUE(s$.date),
               tfields = intersect(as.character(s$fields %||% character()),
                                   names(data)),
               # What the segment tooltip calls the colour and the extra
               # fields: their variable labels, else their names.
               cvar = summarize_col_name(data, summarize_chr1(s$color)),
               tlabels = vapply(
                 intersect(as.character(s$fields %||% character()), names(data)),
                 function(f) summarize_col_name(data, f), character(1L),
                 USE.NAMES = FALSE
               ),
               section = identical(s$parent, FALSE),
               size = summarize_chr1(s$size) %||% "md",
               fills = if (!is.null(lv)) {
                 unname(summarize_level_colors(
                   scale_map, summarize_chr1(s$color), lv,
                   data[[summarize_chr1(s$color)]]
                 )[lv])
               } else {
                 dd_palette(1L)
               }))
}

#' The column's sub-label: what the glyph is made of, in the header, so the
#' cell never has to explain itself. "Median . Q1-Q3 . 1.5xIQR" reads as the
#' three pieces in the order they are drawn; a piece that is off is absent
#' rather than named as "none".
#' @noRd
lane_dist_sub_label <- function(s, meta, wmeta) {
  parts <- c(
    if (identical(summarize_chr1(s$inner) %||% "none", "none")) {
      LANE_STAT_META[[lane_dist_centre_stat(s)]]$center
    } else {
      meta$label
    },
    if (!identical(summarize_chr1(s$outer) %||% "none", "none")) wmeta$range
  )
  paste(parts, collapse = " \u00b7 ")
}

#' @noRd
lane_axis_domain_attr <- function(data, cols) {
  vals <- unlist(lapply(cols, function(col) {
    if (!col %in% names(data)) return(NULL)
    attr(data[[col]], "blockr_domain", exact = TRUE)
  }), use.names = FALSE)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) return(NULL)
  rng <- range(vals)
  if (rng[[2L]] <= rng[[1L]]) return(NULL)
  rng
}

#' The facet layout. by_summary (default): each summary's level copies sit
#' adjacent, in authored order (adjacency is the comparison affordance).
#' by_level: the Table-1 reading -- unfaceted columns and fields lead, then
#' one column group per facet level spanning the summaries; the header grows
#' a spanning row (summarize_thead) and each copy is re-labelled by its
#' SUMMARY (the level moves up into the group header). That reading needs ONE
#' facet column for the whole table (`facet_levels` is empty otherwise),
#' because a group header spanning "F | M" and "Placebo | Active" is not a
#' span.
#' @return `list(plan, facet_spans)`.
#' @noRd
lane_facet_layout <- function(plan, facet_levels, facet_layout, data,
                              shared_facet, subject) {
  facet_spans <- NULL
  has_cell <- any(vapply(plan, function(p) !is.null(p$flevel), logical(1L)))
  if (identical(summarize_chr1(facet_layout), "by_level") &&
        length(facet_levels) && has_cell) {
    lead <- Filter(function(p) is.null(p$flevel), plan)
    groups <- lapply(facet_levels, function(lv) {
      Filter(function(p) identical(p$flevel, lv), plan)
    })
    # One summary per level: a spanner over a single column would only push
    # the summary name ("Patients") into every leaf header. The column keeps
    # its by_summary header, the level over its N.
    one <- all(lengths(groups) == 1L)
    if (!one) groups <- lapply(groups, lane_level_group)
    plan <- c(lead, do.call(c, groups))
    if (!one) {
      facet_spans <- lane_facet_spans(lead, groups, facet_levels, data,
                                      shared_facet, subject)
    }
  }
  list(plan = plan, facet_spans = facet_spans)
}

# A level group's entries, each headed by its SUMMARY.
lane_level_group <- function(g) {
  lapply(g, function(p) {
    p$label <- p$sname %||% p$label
    # Every copy: a plan entry can carry `sub_label` twice (c() appends).
    p[names(p) == "sub_label"] <- NULL
    p
  })
}

# The spanning header row of the by_level layout.
lane_facet_spans <- function(lead, groups, facet_levels, data, shared_facet,
                             subject) {
  list(
    lead = length(lead),
    groups = lapply(seq_along(facet_levels), function(j) {
      lv <- facet_levels[[j]]
      list(label = lv, n = length(groups[[j]]),
           pop = summarize_pop_n(
             data[as.character(data[[shared_facet]]) == lv, , drop = FALSE],
             subject
           ))
    })
  )
}

#' The legend: ONE group per colour column in use, in column order. Colour
#' identity must not ride on colour alone, and with colour on the summary
#' a table can carry more than one colour dimension -- which is exactly why
#' each group is titled by its column rather than merged into one strip.
#' @noRd
lane_color_groups <- function(summaries, data, drawn, scale_map) {
  legend_cols <- unique(unlist(lapply(summaries, function(s) {
    if (lane_color_capable(s)) lane_present(s$color, data)
  })))
  color_groups <- Filter(Negate(is.null), lapply(legend_cols, function(cc) {
    lv <- summarize_color_levels(data[[cc]], drawn)
    if (length(lv) < 2L) return(NULL)
    list(column = cc, title = summarize_col_name(data, cc), levels = lv,
         palette = summarize_level_colors(scale_map, cc, lv, data[[cc]]))
  }))
  # A tile column painted by a worst level gets its own legend group, titled
  # by what the paint says.
  for (s in summaries) {
    if (!identical(s$show, "tile") || is.null(s$.fill)) next
    lv <- s$.fill_levels
    if (!length(lv)) next
    color_groups[[length(color_groups) + 1L]] <- list(
      column = s$.fill,
      title = paste("Worst", summarize_col_name(data, s$.fill)),
      levels = lv,
      palette = lane_tile_colors(scale_map, s$.fill, lv, data[[s$.fill]])
    )
  }
  color_groups
}
