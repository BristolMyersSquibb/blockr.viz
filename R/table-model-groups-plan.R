#' Colour dimension of a distribution column: per-level stat columns, levels
#' and colours (`summarize_level_colors`). `list()` when there is no colour.
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
# sub-label; `qualify` (several facet columns) prefixes the column.
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
    paste0(s$name, " \u00b7 N = ", cp$pop)
  }
}

# The denominator when the copy carries a usable N, else NULL.
lane_plan_den <- function(cp) {
  den <- cp$den
  if (is.numeric(den) && length(den) == 1L && is.finite(den) && den > 0) {
    den
  }
}

lane_plan_base <- function(s, cp, sid, label, sub, den) {
  base <- list(label = label, sub_label = sub, sid = sid, stype = s$type,
               flevel = cp$level, fcol = cp$fcol, sname = s$name,
               meas = s$name,
               # a zero count is an empty column (summarize_drop_empty_facets)
               zero_empty = identical(s$type, "simple") &&
                 summarize_has_pct(chr_state(s$func) %||% "count"),
               # Values in the data's unit, not a count: `unit` heads it.
               unit_ok = identical(s$type, "simple") &&
                 !summarize_has_pct(chr_state(s$func) %||% "count"))
  # A count with an N: the header's second line says N and a bar's length is
  # the percentage, so arms of different size share a scale.
  if (!is.null(den)) {
    base$sub_label <- if (isTRUE(cp$named)) {
      paste0(s$name, " \u00b7 N = ", den)
    } else {
      paste0("N = ", den)
    }
    base$val_denom <- den
  }
  base
}

#' One plan entry per (summary x facet copy): leaf columns, display kind and
#' header words. lane_summary_domains() adds its scale.
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
  } else if (identical(s$type, "sparkline")) {
    c(base, list(kind = "sparkline", key = paste0(sid, "_last"),
                 pts = paste0(sid, "_pts"), x = chr_state(s$x),
                 dom_date = isTRUE(s$.date), ref = cp$ref,
                 show_val = TRUE))
  } else {
    lane_plan_spans(s, sid, base, data, scale_map)
  }
}

lane_plan_simple <- function(s, sid, base, den, data, scale_map) {
  if (identical(s$show, "dot")) {
    c(base, list(kind = "pointrange", key = paste0(sid, "_v"),
                 cols = c(bc = paste0(sid, "_v")),
                 # Length from zero, like the bar: the domain keeps its zero.
                 zero = TRUE,
                 # No range, so nothing spans the cell: it keeps its hairline.
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
                 # Bar length is the percentage; a number prints "54 (18%)".
                 denom = den,
                 combined = !is.null(den) && identical(kind, "num")))
  }
}

# Colour on a magnitude is its composition: segments say what the bar's total
# is made of. Only for an ADDITIVE measure (summarize_additive()); the parts of
# a mean do not add up, and a custom `value` may be a mean, so those levels sit
# side by side.
# `mode` is provisional: if every row holds one level, lane_summary_domains()
# stacks it back (lane_split_degenerate()).
lane_plan_barsplit <- function(s, sid, base, den, data, scale_map) {
  c(base, list(kind = "barsplit", key = paste0(sid, "_v"),
               prefix = paste0(sid, "_S_"), series = s$.levels,
               cvar = summarize_col_name(data, s$.color),
               ccol = s$.color,
               mode = local({
                 additive <- is.null(s$.custom) && summarize_additive(
                   chr_state(s$func) %||% "count"
                 )
                 # Parts of a mean do not add up: side by side.
                 if (!additive) {
                   "grouped"
                 } else {
                   LANE_POSITIONS[[s$position %||% "stack"]]
                 }
               }),
               show_val = TRUE,
               denom = den,
               fills = unname(summarize_level_colors(
                 scale_map, s$.color, s$.levels, data[[s$.color]]
               )[s$.levels])))
}

# Matrix cell: the value in a square, painted by the worst level of `fill`,
# else by the value on a ramp. The header is the level alone (the N line would
# not fit a rotated title).
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

lane_plan_dist <- function(s, sid, base, sub, data, scale_map) {
  stat <- chr_state(s$summary) %||% "median_q1_q3"
  meta <- LANE_STAT_META[[stat]] %||% LANE_STAT_META$median_q1_q3
  if (identical(s$show, "text")) {
    return(c(base, list(kind = "num", key = paste0(sid, "_bc"),
                        dispkey = "dist_text",
                        lo = paste0(sid, "_bl"), hi = paste0(sid, "_bh"),
                        sub_label = sub %||% meta$label)))
  }
  # ONE glyph, two styles. Leaf columns follow the pieces switched on, so an
  # absent range ships no columns.
  inner <- chr_state(s$summary) %||% "median_q1_q3"
  outer <- chr_state(s$whiskers) %||% "none"
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
               # A mark with an outer range brings its own rail (bare lane); one
               # without gets the hairline.
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

lane_pair_cols <- function(prefix) {
  c(a = paste0(prefix, "_from"), b = paste0(prefix, "_to"),
    lo = paste0(prefix, "_lo"), hi = paste0(prefix, "_hi"))
}

lane_plan_pair <- function(s, sid, base, sub, data, scale_map) {
  lv <- s$.levels
  dlv <- s$.dlevels
  words <- c(
    trimws(paste(LANE_PAIR_WORDS[[s$x_func]],
                 summarize_col_name(data, s$x), "\u25c7")),
    trimws(paste(LANE_PAIR_WORDS[[s$xend_func]],
                 summarize_col_name(data, s$xend), "\u25cf"))
  )
  # Set on `base`: c() would append a second `sub_label` that `$` never reads.
  base$sub_label <- sub %||% paste0(
    words[[1L]], " \u2192 ", words[[2L]],
    if (length(dlv) > 1L) {
      paste0(" \u00b7 dashed: ", summarize_col_name(data, s$linetype), " \u2260 ",
             dlv[[1L]])
    } else {
      ""
    }
  )
  c(base, list(kind = "pair", key = paste0(sid, "_to"),
               cols = lane_pair_cols(sid), ncol = paste0(sid, "_n"),
               fidx = paste0(sid, "_f"), didx = paste0(sid, "_d"),
               ref = s$x_lines, levels = lv,
               words = list(from = words[[1L]], to = words[[2L]]),
               fills = if (!is.null(lv)) {
                 unname(summarize_level_colors(
                   scale_map, chr_state(s$color), lv,
                   data[[chr_state(s$color)]]
                 )[lv])
               },
               show_val = TRUE),
    # Colour split: one dumbbell per level (lane_pair_fill() writes
    # `<sid>_L<j>_*`).
    if (!is.null(s$.color) && length(lv)) {
      list(lcols = lapply(seq_along(lv), function(j) {
             lane_pair_cols(paste0(sid, "_L", j))
           }),
           ldidx = paste0(sid, "_L", seq_along(lv), "_d"),
           lncol = paste0(sid, "_L", seq_along(lv), "_n"),
           cvar = summarize_col_name(data, s$.color))
    })
}

lane_plan_spans <- function(s, sid, base, data, scale_map) {
  lv <- s$.levels
  x <- chr_state(s$x)
  xend <- chr_state(s$xend)
  c(base, list(kind = "interval", key = paste0(sid, "_start"),
               segs = paste0(sid, "_segs"), x = x,
               xend = xend, levels = lv,
               dom = lane_axis_domain_attr(data, c(x, xend)),
               dom_date = isTRUE(s$.date),
               tfields = intersect(as.character(s$tooltip %||% character()),
                                   names(data)),
               # Tooltip names for the colour and extra fields: labels, else
               # names.
               cvar = summarize_col_name(data, chr_state(s$color)),
               tlabels = vapply(
                 intersect(as.character(s$tooltip %||% character()), names(data)),
                 function(f) summarize_col_name(data, f), character(1L),
                 USE.NAMES = FALSE
               ),
               section = identical(s$parent, FALSE),
               size = chr_state(s$size) %||% "md",
               fills = if (!is.null(lv)) {
                 unname(summarize_level_colors(
                   scale_map, chr_state(s$color), lv,
                   data[[chr_state(s$color)]]
                 )[lv])
               } else {
                 dd_palette(1L)
               }))
}

#' The column's sub-label: the pieces of the glyph ("Median . Q1-Q3 . 1.5xIQR"),
#' a piece that is off is absent.
#' @noRd
lane_dist_sub_label <- function(s, meta, wmeta) {
  parts <- c(
    if (identical(chr_state(s$summary) %||% "none", "none")) {
      LANE_STAT_META[[lane_dist_centre_stat(s)]]$center
    } else {
      meta$label
    },
    if (!identical(chr_state(s$whiskers) %||% "none", "none")) wmeta$range
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

#' The facet layout. `by_summary` (default): each summary's level copies sit
#' adjacent. `by_level`: unfaceted columns lead, then one column group per
#' facet level spanning the summaries, copies labelled by summary. Needs ONE
#' facet column (`facet_levels` is empty otherwise): a header spanning "F | M"
#' and "Placebo | Active" is not a span.
#' @return `list(plan, facet_spans)`.
#' @noRd
lane_facet_layout <- function(plan, facet_levels, facet_layout, data,
                              shared_facet, subject) {
  facet_spans <- NULL
  has_cell <- any(vapply(plan, function(p) !is.null(p$flevel), logical(1L)))
  if (identical(chr_state(facet_layout), "by_level") &&
        length(facet_levels) && has_cell) {
    lead <- Filter(function(p) is.null(p$flevel), plan)
    groups <- lapply(facet_levels, function(lv) {
      Filter(function(p) identical(p$flevel, lv), plan)
    })
    # One summary per level: a spanner would push its name into every leaf
    # header, so the column keeps its by_summary header.
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

lane_level_group <- function(g) {
  lapply(g, function(p) {
    p$label <- p$sname %||% p$label
    # Every copy: a plan entry can carry `sub_label` twice (c() appends).
    p[names(p) == "sub_label"] <- NULL
    p
  })
}

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

#' The legend: one group per colour column in use, each titled by its column
#' since a table can carry more than one colour dimension.
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
  # A tile painted by a worst level gets its own legend group.
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
