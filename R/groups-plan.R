# One plan entry per summary column: the mark it draws, its stat columns,
# colour split, facet and labels, read by the cell model. An entry's `kind`
# and a split bar's `mode` are provisional: the plan is built before any value
# exists, and groups_summary_domains() settles both over the assembled rows
# (a bar with a negative value is `bardiv`, a grouped split whose rows each
# hold one level is stacked).

#' Colour dimension of a distribution column: per-level stat columns, levels
#' and colours (`groups_level_colors`). `list()` when there is no colour.
#' @noRd
groups_color_split <- function(s, sid, stats, data, scale_map) {
  lv <- s$.levels
  if (is.null(s$.color) || !length(lv)) return(list())
  list(
    levels = lv,
    lcols = lapply(seq_along(lv), function(j) {
      stats::setNames(groups_col(groups_level_sid(sid, j), stats), stats)
    }),
    fills = unname(groups_level_colors(scale_map, s$.color, lv,
                                          data[[s$.color]])[lv]),
    cvar = groups_col_name(data, s$.color)
  )
}

# A facet copy is headed by its level, the summary name moving to the
# sub-label; `qualify` (several facet columns) prefixes the column.
groups_plan_label <- function(s, cp) {
  if (is.null(cp$level)) {
    s$name
  } else if (isTRUE(cp$qualify)) {
    paste0(cp$fcol, ": ", cp$level)
  } else {
    cp$level
  }
}

groups_plan_sub <- function(s, cp) {
  if (is.null(cp$level)) {
    NULL
  } else if (is.null(cp$pop)) {
    s$name
  } else {
    paste0(s$name, " \u00b7 N = ", cp$pop)
  }
}

# The denominator when the copy carries a usable N, else NULL.
groups_plan_den <- function(cp) {
  den <- cp$den
  if (is.numeric(den) && length(den) == 1L && is.finite(den) && den > 0) {
    den
  }
}

# A plan entry: `base` with the kind's fields set over it; a field the kind
# sets again replaces the base's. A NULL field stays in the list, so `p$dom`
# finds it rather than partially matching `p$dom_date`.
groups_plan_entry <- function(base, ...) {
  fields <- c(...)
  base[names(fields)] <- fields
  base
}

groups_plan_base <- function(s, cp, sid, label, sub_label, den) {
  base <- list(label = label, sub_label = sub_label, sid = sid, stype = s$type,
               flevel = cp$level, fcol = cp$fcol, sname = s$name,
               meas = s$name,
               # a zero count is an empty column (groups_drop_empty_facets)
               zero_empty = identical(s$type, "simple") &&
                 groups_has_pct(chr_state(s$func) %||% "count"),
               # Values in the data's unit, not a count: `unit` heads it.
               unit_ok = identical(s$type, "simple") &&
                 !groups_has_pct(chr_state(s$func) %||% "count"))
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
#' header words. groups_summary_domains() adds its scale.
#' @noRd
groups_summary_plan <- function(s, cp, data, scale_map = NULL) {
  sid <- cp$suffix
  sub_label <- groups_plan_sub(s, cp)
  den <- groups_plan_den(cp)
  base <- groups_plan_base(s, cp, sid, groups_plan_label(s, cp), sub_label, den)
  if (identical(s$type, "simple")) {
    groups_plan_simple(s, sid, base, den, data, scale_map)
  } else if (identical(s$type, "dist")) {
    groups_plan_dist(s, sid, base, data, scale_map)
  } else if (!is.null(s$.failed)) {
    groups_plan_entry(base, list(
      kind = "text", key = groups_col(sid, "t"), failed = s$.failed
    ))
  } else if (identical(s$type, "field")) {
    groups_plan_entry(base, list(kind = "text", key = groups_col(sid, "t")))
  } else if (identical(s$type, "expr")) {
    # Numeric vs text is decided by the eval: `key` is the number, `alt_text`
    # the text column the cell model reads when there is no number.
    groups_plan_entry(base, list(
      kind = "expr", key = groups_col(sid, "v"),
      alt_text = groups_col(sid, "t")
    ))
  } else if (identical(s$type, "pair")) {
    groups_plan_pair(s, sid, base, sub_label, data, scale_map)
  } else if (identical(s$type, "sparkline")) {
    groups_plan_entry(base, list(
      kind = "sparkline", key = groups_col(sid, "last"),
      pts = groups_col(sid, "pts"), x = chr_state(s$x),
      dom_date = isTRUE(s$.date), ref = cp$ref
    ))
  } else {
    groups_plan_spans(s, sid, base, data, scale_map)
  }
}

groups_plan_simple <- function(s, sid, base, den, data, scale_map) {
  if (identical(s$show, "dot")) {
    groups_plan_entry(
      base,
      list(
        kind = "pointrange", key = groups_col(sid, "v"),
        cols = c(bc = groups_col(sid, "v")),
        # Length from zero, like the bar: the domain keeps its zero.
        zero = TRUE,
        # No range, so nothing spans the cell: it keeps its hairline.
        bare = TRUE,
        words = list(center = s$name)
      ),
      groups_color_split(s, sid, "bc", data, scale_map)
    )
  } else if (identical(s$show, "bar") && !is.null(s$.color) &&
               length(s$.levels)) {
    groups_plan_barsplit(s, sid, base, den, data, scale_map)
  } else if (identical(s$show, "tile")) {
    groups_plan_tile(s, sid, base, data, scale_map)
  } else {
    kind <- if (identical(s$show, "bar")) "bar" else "num"
    groups_plan_entry(base, list(
      kind = kind, key = groups_col(sid, "v"),
      fill = if (identical(kind, "bar")) dd_palette(1L),
      # Bar length is the percentage; a number prints "54 (18%)".
      denom = den,
      combined = !is.null(den) && identical(kind, "num")
    ))
  }
}

# Colour on a magnitude is its composition: segments say what the bar's total
# is made of. Only for an ADDITIVE measure (groups_additive()); the parts of
# a mean do not add up, and a custom `value` may be a mean, so those levels sit
# side by side.
# `mode` is provisional: if every row holds one level, groups_summary_domains()
# stacks it back (groups_split_degenerate()).
groups_plan_barsplit <- function(s, sid, base, den, data, scale_map) {
  additive <- is.null(s$.custom) &&
    groups_additive(chr_state(s$func) %||% "count")
  groups_plan_entry(base, list(
    kind = "barsplit", key = groups_col(sid, "v"),
    series = s$.levels, scols = groups_split_col(sid, s$.levels),
    cvar = groups_col_name(data, s$.color),
    ccol = s$.color,
    # Parts of a mean do not add up: side by side.
    mode = if (additive) {
      GROUPS_POSITIONS[[s$position %||% "stack"]]
    } else {
      "grouped"
    },
    denom = den,
    fills = unname(groups_level_colors(
      scale_map, s$.color, s$.levels, data[[s$.color]]
    )[s$.levels])
  ))
}

# Matrix cell: the value in a square, painted by the worst level of `fill`,
# else by the value on a ramp. The header is the level alone (the N line would
# not fit a rotated title).
groups_plan_tile <- function(s, sid, base, data, scale_map) {
  base$sub_label <- NULL
  fill <- s$.fill
  groups_plan_entry(base, list(
    kind = "tile", key = groups_col(sid, "v"),
    wkey = if (!is.null(fill)) groups_col(sid, "w"),
    fills = if (!is.null(fill)) {
      unname(groups_tile_colors(scale_map, fill, s$.fill_levels,
                                data[[fill]]))
    },
    flevels_fill = s$.fill_levels,
    fvar = if (!is.null(fill)) groups_col_name(data, fill)
  ))
}

groups_plan_dist <- function(s, sid, base, data, scale_map) {
  if (identical(s$show, "text")) {
    return(groups_plan_entry(base, list(
      kind = "dist_text", key = groups_col(sid, "bc"),
      lo = groups_col(sid, "bl"), hi = groups_col(sid, "bh")
    )))
  }
  # ONE glyph, two styles. Leaf columns follow the pieces switched on, so an
  # absent range ships no columns.
  inner <- chr_state(s$summary) %||% "median_q1_q3"
  outer <- chr_state(s$whiskers) %||% "none"
  meta <- GROUPS_STAT_META[[inner]] %||% GROUPS_STAT_META$median_q1_q3
  cmeta <- GROUPS_STAT_META[[groups_dist_centre_stat(s)]] %||%
    GROUPS_STAT_META$median_q1_q3
  wmeta <- GROUPS_STAT_META[[outer]] %||% GROUPS_STAT_META$tukey
  cols <- c(bc = groups_col(sid, "bc"), n = groups_col(sid, "n"))
  if (!identical(inner, "none")) {
    cols <- c(cols, bl = groups_col(sid, "bl"), bh = groups_col(sid, "bh"))
  }
  if (!identical(outer, "none")) {
    cols <- c(cols, wl = groups_col(sid, "wl"), wh = groups_col(sid, "wh"))
  }
  groups_plan_entry(
    base,
    list(
      kind = if (identical(s$style, "box")) "box" else "pointrange",
      key = groups_col(sid, "bc"), cols = cols,
      # A mark with an outer range brings its own rail (bare lane); one
      # without gets the hairline.
      bare = identical(outer, "none"),
      words = list(
        center = cmeta$center,
        range = if (!identical(inner, "none")) meta$range,
        whisk = if (!identical(outer, "none")) wmeta$range
      )
    ),
    groups_color_split(s, sid, names(cols), data, scale_map)
  )
}

groups_pair_cols <- function(prefix) {
  c(a = groups_col(prefix, "from"), b = groups_col(prefix, "to"),
    lo = groups_col(prefix, "lo"), hi = groups_col(prefix, "hi"))
}

groups_plan_pair <- function(s, sid, base, sub_label, data, scale_map) {
  lv <- s$.levels
  dlv <- s$.dlevels
  words <- c(
    trimws(paste(GROUPS_PAIR_WORDS[[s$x_func]],
                 groups_col_name(data, s$x), "\u25c7")),
    trimws(paste(GROUPS_PAIR_WORDS[[s$xend_func]],
                 groups_col_name(data, s$xend), "\u25cf"))
  )
  dashed <- if (length(dlv) > 1L) {
    paste0(" \u00b7 dashed: ", groups_col_name(data, s$linetype), " \u2260 ",
           dlv[[1L]])
  } else {
    ""
  }
  groups_plan_entry(
    base,
    list(
      kind = "pair", key = groups_col(sid, "to"),
      sub_label = sub_label %||%
        paste0(words[[1L]], " \u2192 ", words[[2L]], dashed),
      cols = groups_pair_cols(sid), ncol = groups_col(sid, "n"),
      fidx = groups_col(sid, "f"), didx = groups_col(sid, "d"),
      ref = s$x_lines, levels = lv,
      words = list(from = words[[1L]], to = words[[2L]]),
      fills = if (!is.null(lv)) {
        unname(groups_level_colors(
          scale_map, chr_state(s$color), lv, data[[chr_state(s$color)]]
        )[lv])
      }
    ),
    # Colour split: one dumbbell per level (groups_pair_fill() writes
    # `<sid>_L<j>_*`).
    if (!is.null(s$.color) && length(lv)) {
      lsid <- groups_level_sid(sid, seq_along(lv))
      list(lcols = lapply(lsid, groups_pair_cols),
           ldidx = groups_col(lsid, "d"),
           lncol = groups_col(lsid, "n"),
           cvar = groups_col_name(data, s$.color))
    }
  )
}

groups_plan_spans <- function(s, sid, base, data, scale_map) {
  lv <- s$.levels
  x <- chr_state(s$x)
  xend <- chr_state(s$xend)
  color <- chr_state(s$color)
  tfields <- intersect(as.character(s$tooltip %||% character()), names(data))
  groups_plan_entry(base, list(
    kind = "interval", key = groups_col(sid, "start"),
    segs = groups_col(sid, "segs"), x = x, xend = xend, levels = lv,
    dom = groups_axis_domain_attr(data, c(x, xend)),
    dom_date = isTRUE(s$.date),
    tfields = tfields,
    # Tooltip names for the colour and extra fields: labels, else names.
    cvar = groups_col_name(data, color),
    tlabels = vapply(tfields, function(f) groups_col_name(data, f),
                     character(1L), USE.NAMES = FALSE),
    section = identical(s$parent, FALSE),
    size = chr_state(s$size) %||% "md",
    ovl = if (groups_spans_has_overlay(s)) groups_col(sid, "ovl"),
    shapes = s$.shape_levels,
    ename = if (!is.null(s$event_x)) groups_col_name(data, s$event_x),
    fills = if (!is.null(lv)) {
      unname(groups_level_colors(scale_map, color, lv, data[[color]])[lv])
    } else {
      dd_palette(1L)
    }
  ))
}

#' @noRd
groups_axis_domain_attr <- function(data, cols) {
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
groups_facet_layout <- function(plan, facet_levels, facet_layout, data,
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
    if (!one) groups <- lapply(groups, groups_level_group)
    plan <- c(lead, do.call(c, groups))
    if (!one) {
      facet_spans <- groups_facet_spans(lead, groups, facet_levels, data,
                                      shared_facet, subject)
    }
  }
  list(plan = plan, facet_spans = facet_spans)
}

groups_level_group <- function(g) {
  lapply(g, function(p) {
    p$label <- p$sname %||% p$label
    p$sub_label <- NULL
    p
  })
}

groups_facet_spans <- function(lead, groups, facet_levels, data, shared_facet,
                             subject) {
  list(
    lead = length(lead),
    groups = lapply(seq_along(facet_levels), function(j) {
      lv <- facet_levels[[j]]
      list(label = lv, n = length(groups[[j]]),
           pop = groups_pop_n(
             data[as.character(data[[shared_facet]]) == lv, , drop = FALSE],
             subject
           ))
    })
  )
}
