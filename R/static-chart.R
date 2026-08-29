#' Static ggplot Renderer for Chart-Block State
#'
#' Renders the chart block's configuration as a ggplot -- the static chart
#' output for report / deck rendering, the plot-side sibling of
#' [static_table()]. The interactive chart draws client-side (canvas); a
#' rendered document has no browser, so blockr.outline emits
#' `static_chart(<result>, <state...>)` for chart blocks and this function
#' rebuilds the chart server-side from the same vocabulary.
#'
#' Numbers match the app by construction: aggregation goes through the same
#' engine the table/tile renderers use (`dd_table_aggregate`, golden-tested
#' against the JS engine), colors cycle the same `dd_palette()` with
#' the same level ordering (factor levels, else sorted unique), and a board
#' scale map (arm colors) resolves through the same blockr.theme resolver.
#'
#' The look mirrors the canvas chart, constant for constant (chart.js is the
#' source of truth): 11px `#666` tick labels, dashed `#f3f4f6` gridlines on
#' the value axis only, `#ccc` axis lines, the category-first-at-the-top
#' horizontal layout, 60%-band bars with no rounding and no value labels,
#' boxes filled at the series color over a full-strength border, monotone
#' interpolation on lines, and the bottom-centered legend band.
#'
#' Argument names mirror [new_chart_block()] one to one, so the emitted call
#' is literally the block's state. Interaction-only state (tooltips, drill,
#' zoom, runtime filters) has no meaning in print and is not part of this
#' surface.
#'
#' Supported types: `"bar"` (stacked / grouped / percent, `func =
#' "identity"` for precomputed heights), `"boxplot"`, `"scatter"` and
#' `"line"`. Any other type degrades to the aggregated data (a table in the
#' rendered document) with a warning, so a report never breaks on an
#' uncovered chart type.
#'
#' For pptx decks the returned plot carries `pptx_width` / `pptx_height`
#' attributes sized from the chart's fixed row geometry (28px per category
#' row, 14px per series + 12px gap in grouped mode -- the canvas
#' constants), so officer places each chart at its natural height instead
#' of a one-size box.
#'
#' @param data A data frame (the block's result: its filtered input).
#' @param chart_type,group,color,facet,value,func,x,y,series Chart state,
#'   as in [new_chart_block()].
#' @param bar_mode,orientation,sort_by,sort_dir Bar layout and category
#'   ordering, as in [new_chart_block()]. `orientation = NULL` resolves per
#'   type like the canvas: horizontal for bars, vertical for boxplots.
#'   `sort_by = NULL` means `"alpha"` (level order for factors).
#' @param count_on,count_col Observation-count labels, as in
#'   [new_chart_block()].
#' @param facet_scales Panel scales, passed straight to
#'   [ggplot2::facet_wrap()]'s `scales`: `"fixed"` (default), `"free_y"`,
#'   `"free_x"` or `"free"`. Same argument as in [new_chart_block()].
#' @param box_points,smoother,identity_line,lo,hi Family-specific options,
#'   as in [new_chart_block()].
#' @param summary,whiskers Boxplot statistics, as in [new_chart_block()]:
#'   `summary` is the box body (`NULL` = `"median_q1_q3"`), `whiskers` the
#'   outer rule (`NULL` = `"tukey"`; also `"min_max"`, `"p10_p90"`).
#' @param connect Line interpolation, as in [new_chart_block()]:
#'   `"monotone"` (default -- a monotone spline through the points),
#'   `"straight"`, or `"step-start"` / `"step-middle"` / `"step-end"`.
#' @param step Deprecated spelling of the step modes (`"start"` /
#'   `"middle"` / `"end"`); folded into `connect` when that is unset.
#' @param vlines,hlines Numeric helper-line positions.
#' @param line_width_mult,dot_size_mult Size multipliers.
#' @param title,subtitle,caption Title band text; `{token}` templates
#'   resolve against `data` (same resolver as the app), `NULL` falls back
#'   to the data's display attributes (`label` / `subtitle` / `caption`,
#'   the canvas auto tier), `""` suppresses.
#' @param pct_of Which mapped role a `func = "pct_distinct"` denominator is
#'   taken within: `"facet"` (default), `"group"` or `"color"`. Must match the
#'   canvas, or an exported chart divides by something else than the one on
#'   screen.
#' @param na_group What to do with rows whose `group` value is missing:
#'   `"level"` (default) gives them their own category, `"drop"` removes them
#'   from the categories but NOT from the panel, so they draw nothing and still
#'   count toward `func = "pct_distinct"`'s denominator. Must match the canvas,
#'   or an exported chart says something different from the one on screen.
#' @param scale_map A blockr.theme scale map binding columns to colors.
#'   Default `NULL` reads the board's `scale_map` option when called inside
#'   a live session (the officer deck path) and falls back to palette
#'   cycling headless (the quarto path).
#' @param ... Ignored; accepted so emitted calls stay forward-compatible.
#'
#' @return A ggplot object, or (for unsupported types) the aggregated data
#'   frame the chart would have drawn.
#'
#' @importFrom rlang .data
#' @export
static_chart <- function(data,
                     chart_type = "bar",
                     group = NULL,
                     color = NULL,
                     facet = NULL,
                     value = ".count",
                     func = "count",
                     x = NULL,
                     y = NULL,
                     series = NULL,
                     bar_mode = "stacked",
                     orientation = NULL,
                     sort_by = NULL,
                     sort_dir = NULL,
                     count_on = "off",
                     count_col = NULL,
                     na_group = "level",
                     pct_of = "facet",
                     facet_scales = "fixed",
                     box_points = "none",
                     summary = NULL,
                     whiskers = NULL,
                     smoother = "none",
                     identity_line = FALSE,
                     lo = NULL,
                     hi = NULL,
                     connect = "monotone",
                     step = NULL,
                     vlines = NULL,
                     hlines = NULL,
                     line_width_mult = 1,
                     dot_size_mult = 1,
                     title = NULL,
                     subtitle = NULL,
                     caption = NULL,
                     scale_map = NULL,
                     ...) {

  stopifnot(is.data.frame(data))

  chart_type <- (chart_type %||% "bar")[1L]

  group <- gg_col(group, data)
  color <- gg_col(color, data)
  facet <- gg_col(facet, data)
  x <- gg_col(x, data)
  y <- gg_col(y, data)
  series <- gg_col(series, data)
  count_col <- gg_col(count_col, data)
  value_col <- if (!identical(value %||% ".count", ".count")) {
    gg_col(value, data)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("static_chart() needs ggplot2; returning the data instead.")
    return(gg_fallback(data, chart_type, group, color, facet,
                             value_col, func, na_group, pct_of))
  }

  if (is.null(scale_map)) {
    scale_map <- tryCatch(
      blockr.core::get_board_option_or_null(
        "scale_map", blockr.core::get_session()
      ),
      error = function(e) NULL
    )
  }

  # The canvas normalizes unset state per family (_ensureFamilyDefaults):
  # bars sort by value DESCENDING and lie horizontal; distributions keep
  # the data's own order ascending and stand vertical.
  if (identical(chart_type, "boxplot")) {
    sort_by <- sort_by %||% "data"
    sort_dir <- sort_dir %||% "asc"
    horiz <- identical(orientation, "horizontal")
  } else {
    sort_by <- sort_by %||% "value"
    sort_dir <- sort_dir %||% "desc"
    horiz <- !identical(orientation %||% "horizontal", "vertical")
  }

  # `step` was this renderer's early spelling of the step modes; the block
  # vocabulary is `connect`.
  if (!is.null(step) && identical(connect, "monotone")) {
    connect <- paste0("step-", step)
  }

  # A facet with fewer than two levels draws as a single panel with no
  # strip in the canvas; dropping it here renders the same.
  if (!is.null(facet) && length(dd_levels(data[[facet]])) < 2L) {
    facet <- NULL
  }

  p <- switch(
    chart_type,
    bar = gg_bar(
      data, group, color, facet, value_col, func, bar_mode, horiz,
      sort_by, sort_dir, count_on, count_col, scale_map, na_group, pct_of
    ),
    boxplot = gg_boxplot(
      data, group, color, facet, value_col, box_points, summary, whiskers,
      horiz, sort_by, sort_dir, count_on, count_col, scale_map
    ),
    scatter = gg_scatter(
      data, x, y, color, facet, smoother, identity_line, vlines, hlines,
      line_width_mult, dot_size_mult, scale_map
    ),
    line = gg_line(
      data, x, y, series, color, facet, lo, hi, connect, vlines, hlines,
      line_width_mult, dot_size_mult, scale_map
    ),
    NULL
  )

  if (is.null(p)) {
    # Either an uncovered chart type, or covered but undrawable from the
    # current state + data (a mapped column dropped upstream, say).
    warning(
      "static_chart() cannot draw chart_type = \"", chart_type,
      "\" from this state; returning the chart's data instead."
    )
    return(gg_fallback(data, chart_type, group, color, facet,
                             value_col, func, na_group, pct_of))
  }

  if (!is.null(facet)) {
    labs <- if (count_on %in% c("facet", "both")) {
      gg_count_labels(data, facet, count_col, func)
    }
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(.data[[facet]]),
      # The block's `facet_scales` IS facet_wrap()'s `scales` (that is where
      # the vocabulary comes from). The canvas' "free_y" only exists for the
      # families whose value axis is y, and ggplot2 spells that the same way.
      scales = match.arg(
        as.character(facet_scales %||% "fixed")[1L],
        c("fixed", "free_y", "free_x", "free")
      ),
      # Every canvas panel is its own ECharts instance with its own axes.
      axes = "all",
      # The canvas strip is uppercase (CSS text-transform on .dd-facet-label).
      labeller = ggplot2::as_labeller(function(v) {
        toupper(if (!is.null(labs)) labs[v] else v)
      })
    )
  }

  p <- gg_apply_titles(p, title, subtitle, caption, data)

  p <- p + gg_theme() + gg_grid_theme(chart_type, horiz)

  if (!is.null(facet)) {
    # The canvas boxes each panel in a hairline (.dd-facet border); added
    # after the theme because theme_minimal blanks panel.border.
    p <- p + ggplot2::theme(panel.border = ggplot2::element_rect(
      fill = NA, color = GG_SPLIT_LINE_COLOR, linewidth = gg_px_lw(1)
    ))
  }

  gg_attach_pptx_size(p, data, chart_type, horiz, group, color,
                      facet, bar_mode)
}

# -- canvas geometry constants -----------------------------------------------

# The canvas draws at CSS-pixel sizes; ggplot draws in pt (text) and mm
# (lines, point diameters). 1 px = 0.75 pt; ggplot linewidth / size are mm,
# where 1 mm = 72.27 / 25.4 pt (ggplot2::.pt).
gg_px_pt <- function(px) px * 0.75

gg_px_lw <- function(px) px * 0.75 / 2.845276

gg_px_size <- function(px) px * 0.75 / 2.845276

# The category-band width in device pixels for a VERTICAL layout: the
# canvas caps marks in px (barMaxWidth 48, boxWidth clamp [7, 50]), which
# only translates to a ggplot width fraction through the target device
# width. Defaults to the deck slide (ft_fit_width); harnesses rendering at
# other sizes can set blockr.viz.gg_device_width (inches).
gg_band_px <- function(n_groups, n_panels = 1L) {
  w_in <- getOption(
    "blockr.viz.gg_device_width",
    getOption("blockr.viz.ft_fit_width", 11.9)
  )
  ncol <- ceiling(sqrt(max(1L, n_panels))) # facet_wrap's default grid
  plot_px <- (w_in * 96) / ncol - 130      # axis gutter + margins
  max(20, plot_px / max(1L, n_groups))
}

# Structural colors, verbatim from chart.js.
GG_AXIS_LABEL_COLOR <- "#666666"
GG_AXIS_LINE_COLOR <- "#cccccc"
GG_SPLIT_LINE_COLOR <- "#f3f4f6"
GG_REF_LINE_COLOR <- "#dc2626"
GG_IDENTITY_LINE_COLOR <- "#64748b"

# -- column handling ---------------------------------------------------------

# A state column reference: "" and absent columns become NULL, so every
# downstream branch tests presence once. A column the state names but the
# data lost (renamed upstream) degrades silently, same as the app.
gg_col <- function(col, data) {
  col <- as.character(col %||% character())[1L]
  if (is.na(col) || !nzchar(col) || !col %in% names(data)) NULL else col
}

# The axis title for a column: its `label` attribute when present, else the
# name -- chart.js `_axisTitle`.
gg_axis_title <- function(col, data) {
  lab <- attr(data[[col]], "label", exact = TRUE)
  if (is.character(lab) && length(lab) && nzchar(lab[[1L]])) {
    lab[[1L]]
  } else {
    col
  }
}

# Reorder a column onto the canvas' level order without losing its label
# attribute (factor() strips attributes; the label feeds legend titles).
gg_as_level_factor <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  out <- factor(as.character(x), levels = gg_js_levels(x))
  attr(out, "label") <- lab
  out
}

# -- aggregation -------------------------------------------------------------

# The chart's aggregation: one row per (facet, group, color) cell with the
# metric in `.value`. Same engine as the table renderer
# (dd_table_aggregate -> golden-tested against the JS chart), plus the
# chart-only `identity` mode: no aggregation, the cell's FIRST row wins
# (precomputed heights; duplicate categories collapse, as in the app).
gg_agg <- function(data, group, color, facet, value_col, func,
                   na_group = "level", pct_of = "facet") {

  cells <- c(facet, group, color)

  # The population, BEFORE any dropping. Order matters and is the whole
  # invariant: a row with no category still belongs to the panel, so the
  # denominator has to be counted while it is still here. Counting after the
  # drop gives 2/3 where the answer is 2/4 -- plausible, wrong, and silent.
  # (drilldown-agg.js builds facetPop first for the same reason.)
  full <- data

  # Missing cell coordinates: their own category, or dropped. ANY of the
  # mapped roles, matching both other engines -- a row with no facet is no
  # more a panel than a row with no group is a category. Same fold as the JS
  # engine (NA or ""), applied before the cells so `identity` inherits it too.
  if (identical(na_group, "drop") && length(cells)) {
    miss <- lapply(data[intersect(cells, names(data))], function(x) {
      x <- as.character(x)
      is.na(x) | !nzchar(x)
    })
    if (length(miss)) data <- data[!Reduce(`|`, miss), , drop = FALSE]
  }

  # pct_distinct: the share of the PANEL's distinct values. Not in
  # dd_table_aggregate because it is not a per-cell summary -- the denominator
  # spans the facet -- and not in the shared AGG_FNS for the same reason the
  # canvas keeps it chart-only. Mirrors drilldown-agg.js aggregate().
  if (identical(func, "pct_distinct")) {
    if (is.null(value_col)) {
      return(NULL)
    }
    # The denominator is taken within `pct_of`'s roles. Columns, not roles,
    # once resolved -- and an unmapped role contributes a constant, so it
    # collapses to "everything" exactly as the JS twin does.
    role_col <- list(facet = facet, group = group, color = color)
    dcols <- unlist(role_col[intersect(as.character(pct_of), names(role_col))])
    dcols <- dcols[!vapply(dcols, is.null, TRUE)]
    dkey <- function(d) {
      if (!length(dcols)) return(rep("__all__", nrow(d)))
      do.call(paste, c(lapply(dcols, function(cc) as.character(d[[cc]])),
                       list(sep = "|||")))
    }
    den <- vapply(split(full[[value_col]], dkey(full)),
                  function(v) length(unique(v[!is.na(v)])), 0L)
    agg <- dd_table_aggregate(
      data, group = cells,
      summaries = list(list(func = "count_distinct", cols = value_col))
    )
    if (!length(agg$metric_cols)) {
      return(NULL)
    }
    out <- agg$data
    d <- den[dkey(out)]
    out$.value <- ifelse(is.na(d) | d == 0L, NA_real_,
                         out[[agg$metric_cols[[1L]]]] / d)
    return(out[c(cells, ".value")])
  }

  if (identical(func, "identity")) {

    if (is.null(value_col)) {
      return(NULL)
    }

    keep <- if (length(cells)) {
      !duplicated(data[cells])
    } else {
      seq_len(nrow(data)) == 1L # no cells at all: first row only
    }

    out <- data[keep, , drop = FALSE]
    out$.value <- out[[value_col]]
    return(out[c(cells, ".value")])
  }

  summaries <- if (identical(func, "count") || is.null(func)) {
    list(list(func = "count"))
  } else {
    if (is.null(value_col)) {
      return(NULL)
    }
    list(list(func = func, cols = value_col))
  }

  agg <- dd_table_aggregate(data, group = cells, summaries = summaries,
                            na_group = na_group)

  if (!length(agg$metric_cols)) {
    return(NULL)
  }

  out <- agg$data
  out$.value <- out[[agg$metric_cols[[1L]]]]
  out[c(cells, ".value")]
}

# The fallback exhibit for uncovered types: the aggregated frame when the
# chart aggregates, the raw frame otherwise. In the officer deck this
# becomes an static_table slide; in the quarto document it prints as a table.
gg_fallback <- function(data, chart_type, group, color, facet,
                              value_col, func, na_group = "level",
                              pct_of = "facet") {

  if (!is.null(group)) {
    agg <- gg_agg(data, group, color, facet, value_col, func, na_group, pct_of)
    if (!is.null(agg)) {
      return(agg)
    }
  }

  data
}

# -- ordering and labels -----------------------------------------------------

# Category-axis order as a level vector, mirroring chart.js orderGroups:
# "alpha" (the default) = factor level order when the column is a factor
# (the data-level contract), else locale sort; "data" = factor levels, else
# first-seen order in the raw rows; "value" = ASCENDING by the level's
# total metric; a column name = ascending by that column's minimum per
# level (levels with no value last). `sort_dir = "desc"` flips the
# comparator, exactly like the canvas' -1 multiplier.
#
# The canvas draws horizontal category axes with `inverse: true` -- the
# FIRST level at the TOP. ggplot draws the first level of a discrete y axis
# at the BOTTOM, so horizontal layouts reverse the final order (`flip`).
gg_sorted_levels <- function(agg, data, group, sort_by, sort_dir,
                             flip = FALSE) {

  lv <- dd_levels(data[[group]])
  desc <- identical(sort_dir, "desc")

  first_seen <- function() {
    seen <- unique(as.character(data[[group]]))
    c(seen[seen %in% lv], setdiff(lv, seen))
  }

  ord <- if (identical(sort_by, "data")) {
    dl <- if (is.factor(data[[group]])) lv else first_seen()
    if (desc) rev(dl) else dl
  } else if (identical(sort_by, "alpha")) {
    # dd_levels already IS the alpha order: factor levels when a factor,
    # sorted unique otherwise.
    al <- if (is.factor(data[[group]])) lv else sort(lv)
    if (desc) rev(al) else al
  } else if (identical(sort_by, "value")) {
    tot <- tapply(agg$.value, as.character(agg[[group]]), sum, na.rm = TRUE)
    key <- unname(tot[lv])
    key[is.na(key)] <- 0
    if (desc) lv[order(-key)] else lv[order(key)]
  } else if (is.character(sort_by) && sort_by %in% names(data)) {
    mins <- tapply(
      suppressWarnings(as.numeric(data[[sort_by]])),
      as.character(data[[group]]), min, na.rm = TRUE
    )
    key <- unname(mins[lv])
    key[!is.finite(key)] <- NA
    if (desc) {
      lv[order(-key, na.last = TRUE)]
    } else {
      lv[order(key, na.last = TRUE)]
    }
  } else {
    lv # unknown key: keep the data-level order
  }

  if (flip) rev(ord) else ord
}

# Per-level "Label (n)" axis text: n = distinct `count_col` values (subject
# counts), raw rows when no column is set. For identity bars a count_col is
# shown AS-IS (the level's first row value) -- precomputed Ns.
gg_count_labels <- function(data, col, count_col, func) {

  lv <- dd_levels(data[[col]])
  key <- as.character(data[[col]])

  n <- if (is.null(count_col)) {
    tapply(seq_len(nrow(data)), key, length)
  } else if (identical(func, "identity")) {
    tapply(as.character(data[[count_col]]), key, function(v) v[[1L]])
  } else {
    tapply(data[[count_col]], key, function(v) length(unique(v[!is.na(v)])))
  }

  stats::setNames(paste0(lv, " (", unname(n[lv]), ")"), lv)
}

# -- colors ------------------------------------------------------------------

# Level order exactly as the canvas' _orderLevels: factor levels when the
# column is a factor (column meta), else a PLAIN JS Array.sort() -- UTF-16
# code units, which is R's byte-wise "radix" sort, NOT the locale collation
# sort() applies ("<65" sorts after "65-80" here, before it in a locale).
gg_js_levels <- function(x) {
  lv <- if (is.factor(x)) {
    levels(x)
  } else {
    sort(unique(as.character(x)), method = "radix")
  }
  lv[!is.na(lv)]
}

# Level -> hex, exactly as the canvas assigns colors: the board scale map
# through blockr.theme's resolver when the column is bound, else the shared
# palette cycled over the levels in _orderLevels order.
gg_level_colors <- function(map, col, data) {

  x <- data[[col]]
  lv <- gg_js_levels(x)

  if (!is.null(map) && has_blockr_theme()) {
    res <- dd_resolve_scales(map, col, x)
    if (!is.null(res$color)) {
      hex <- res$color[lv]
      hex[is.na(hex)] <- dd_level_palette(length(lv))[is.na(hex)]
      return(stats::setNames(unname(hex), lv))
    }
  }

  stats::setNames(dd_level_palette(length(lv)), lv)
}

gg_scale_fill <- function(map, col, data) {
  ggplot2::scale_fill_manual(
    values = gg_level_colors(map, col, data),
    name = gg_axis_title(col, data)
  )
}

gg_scale_color <- function(map, col, data) {
  ggplot2::scale_color_manual(
    values = gg_level_colors(map, col, data),
    name = gg_axis_title(col, data)
  )
}

# -- value axis --------------------------------------------------------------

# ECharts numeric tick labels go through addCommas: thousands separators.
gg_comma <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

gg_percent_labels <- function(x) {
  paste0(round(100 * x), "%")
}

# The ECharts value-axis extent: the interval is span / 5 rounded onto the
# 1/2/3/5/10 ladder (echarts nice(round = true)), the extent floor/ceils
# onto interval multiples. `zero` = the bar family's cross-zero rule (no
# `scale: true`); the other families fit the data.
gg_nice_extent <- function(vals, zero = FALSE) {

  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    return(NULL)
  }

  rng <- range(vals)
  if (zero) {
    rng <- range(0, rng)
  }
  span <- diff(rng)
  if (span <= 0) {
    return(NULL)
  }

  raw <- span / 5
  exp10 <- 10^floor(log10(raw))
  f <- raw / exp10
  nf <- if (f < 1.5) 1 else if (f < 2.5) 2 else if (f < 4) 3 else
    if (f < 7) 5 else 10
  int <- nf * exp10

  lo <- floor(rng[1L] / int) * int
  hi <- ceiling(rng[2L] / int) * int
  list(limits = c(lo, hi), breaks = seq(lo, hi, by = int))
}

# A continuous position scale over the nice extent: ticks at the interval,
# no expansion past the extent (ECharts value axes have no boundaryGap),
# comma tick labels. Values fall inside the extent by construction; fitted
# overlays (smoothers) may poke out, so out-of-bounds keeps rather than
# clips.
gg_nice_scale <- function(vals, axis = c("x", "y"), name = NULL,
                          zero = FALSE) {

  scale <- if (identical(axis[1L], "x")) {
    ggplot2::scale_x_continuous
  } else {
    ggplot2::scale_y_continuous
  }

  ext <- gg_nice_extent(vals, zero = zero)
  if (is.null(ext)) {
    return(scale(name = name %||% ggplot2::waiver(), labels = gg_comma))
  }

  scale(
    name = name %||% ggplot2::waiver(),
    limits = ext$limits, breaks = ext$breaks, labels = gg_comma,
    expand = ggplot2::expansion(0), oob = scales::oob_keep
  )
}

# The bar value axis: starts at zero, nice extent, comma ticks; percent
# mode is a fixed 0..100% axis.
gg_bar_value_scale <- function(vals, horiz, name, percent = FALSE) {

  axis <- if (horiz) "x" else "y"

  if (percent) {
    # No explicit limits: position "fill" spans [0, 1] by construction, and
    # limits touching the filled max trips a spurious guide warning.
    scale <- if (horiz) {
      ggplot2::scale_x_continuous
    } else {
      ggplot2::scale_y_continuous
    }
    return(scale(
      name = name, breaks = seq(0, 1, 0.2),
      labels = gg_percent_labels, expand = ggplot2::expansion(0)
    ))
  }

  gg_nice_scale(vals, axis, name, zero = TRUE)
}

# -- families ----------------------------------------------------------------

gg_bar <- function(data, group, color, facet, value_col, func,
                         bar_mode, horiz, sort_by, sort_dir,
                         count_on, count_col, scale_map,
                         na_group = "level", pct_of = "facet") {

  if (is.null(group)) {
    return(NULL)
  }

  agg <- gg_agg(data, group, color, facet, value_col, func, na_group, pct_of)

  if (is.null(agg)) {
    return(NULL)
  }

  lv <- gg_sorted_levels(agg, data, group, sort_by, sort_dir, flip = horiz)
  agg[[group]] <- factor(as.character(agg[[group]]), levels = lv)

  axis_labs <- if (count_on %in% c("axis", "both")) {
    gg_count_labels(data, group, count_col, func)
  }

  grouped <- identical(bar_mode, "grouped") && !is.null(color)
  pct <- identical(bar_mode, "percent") && !is.null(color)

  # Canvas bar geometry. Horizontal (fixed row px): stacked / percent /
  # single bars take 60% of the 28px band; grouped bars touch (barGap 0)
  # at 14px each with a 12px gap between groups. Vertical: same fractions,
  # but capped at barMaxWidth 48px of the device band.
  n_colors <- if (is.null(color)) 1L else length(dd_levels(data[[color]]))
  width <- if (grouped) {
    (n_colors * 14) / (n_colors * 14 + 12)
  } else {
    0.6
  }
  if (!horiz) {
    band <- gg_band_px(length(lv), if (is.null(facet)) 1L else
      length(dd_levels(data[[facet]])))
    width <- if (grouped) {
      min(0.7, n_colors * 48 / band)
    } else {
      min(0.6, 48 / band)
    }
  }

  # Segment order: the canvas stacks the FIRST color level at the axis;
  # ggplot stacks it furthest away, so stacking reverses. Level order
  # itself is _orderLevels (code-unit sort).
  if (!is.null(color)) {
    agg[[color]] <- factor(
      as.character(agg[[color]]), levels = gg_js_levels(data[[color]])
    )
  }

  position <- if (grouped) {
    ggplot2::position_dodge2(padding = 0, preserve = "single", reverse = horiz)
  } else if (pct) {
    ggplot2::position_fill(reverse = TRUE)
  } else {
    ggplot2::position_stack(reverse = TRUE)
  }

  mapping <- if (horiz) {
    ggplot2::aes(x = .data$.value, y = .data[[group]])
  } else {
    ggplot2::aes(x = .data[[group]], y = .data$.value)
  }

  p <- ggplot2::ggplot(agg, mapping)

  p <- if (!is.null(color)) {
    p + ggplot2::geom_col(
      ggplot2::aes(fill = .data[[color]]),
      position = position, width = width
    ) + gg_scale_fill(scale_map, color, data)
  } else {
    p + ggplot2::geom_col(
      fill = dd_palette(1L), width = width
    )
  }

  val_lab <- if (pct) {
    "% of group total"
  } else {
    gg_value_label(value_col, func, data)
  }

  # Axis extent: stack totals for stacked bars, cell values otherwise.
  vals <- if (identical(bar_mode %||% "stacked", "stacked") &&
                !is.null(color)) {
    tapply(agg$.value, agg[c(facet, group)], sum, na.rm = TRUE)
  } else {
    agg$.value
  }

  p <- p + gg_bar_value_scale(as.numeric(vals), horiz, val_lab, percent = pct)

  if (horiz) {
    p + ggplot2::scale_y_discrete(
      labels = axis_labs %||% ggplot2::waiver()
    ) + ggplot2::labs(y = NULL)
  } else {
    p + ggplot2::scale_x_discrete(
      labels = axis_labs %||% ggplot2::waiver()
    ) + ggplot2::labs(x = NULL)
  }
}

# chart.js summarizeStat, verbatim: {center, lo, hi} of a summary statistic
# over the values. The JS quantile (linear interpolation on p * (n - 1)) IS
# R's type 7, sd is the sample sd (n - 1, 0 for one observation), and the
# Tukey fences clip to the data -- so box body, whiskers and outliers agree
# with the canvas to the digit.
gg_summarize_stat <- function(vals, stat) {

  n <- length(vals)
  q <- function(p) stats::quantile(vals, p, names = FALSE, type = 7)

  switch(
    stat %||% "median_q1_q3",
    mean_sd = {
      m <- mean(vals)
      s <- if (n > 1L) stats::sd(vals) else 0
      list(center = m, lo = m - s, hi = m + s)
    },
    mean_2sd = {
      m <- mean(vals)
      s <- if (n > 1L) stats::sd(vals) else 0
      list(center = m, lo = m - 2 * s, hi = m + 2 * s)
    },
    mean_se = {
      m <- mean(vals)
      s <- if (n > 1L) stats::sd(vals) else 0
      list(center = m, lo = m - s / sqrt(n), hi = m + s / sqrt(n))
    },
    p5_p95 = list(center = q(0.5), lo = q(0.05), hi = q(0.95)),
    p10_p90 = list(center = q(0.5), lo = q(0.1), hi = q(0.9)),
    min_max = list(center = q(0.5), lo = min(vals), hi = max(vals)),
    tukey = {
      q1 <- q(0.25)
      q3 <- q(0.75)
      iqr <- q3 - q1
      list(
        center = q(0.5),
        lo = max(min(vals), q1 - 1.5 * iqr),
        hi = min(max(vals), q3 + 1.5 * iqr)
      )
    },
    list(center = q(0.5), lo = q(0.25), hi = q(0.75)) # median_q1_q3
  )
}

# One box's numbers: body from `summary` (default median over Q1-Q3),
# whiskers from `whiskers` (default Tukey), outliers = the points past the
# DRAWN whiskers, so the whisker rule and the outlier set can never
# disagree -- all exactly as the canvas computes them.
gg_box_stats <- function(vals, summary, whiskers) {

  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    return(NULL)
  }

  body <- gg_summarize_stat(vals, summary %||% "median_q1_q3")
  whisk <- gg_summarize_stat(vals, whiskers %||% "tukey")

  list(
    ymin = whisk$lo, lower = body$lo, middle = body$center,
    upper = body$hi, ymax = whisk$hi,
    outliers = vals[vals < whisk$lo | vals > whisk$hi]
  )
}

gg_boxplot <- function(data, group, color, facet, value_col,
                             box_points, summary, whiskers, horiz,
                             sort_by, sort_dir, count_on,
                             count_col, scale_map) {

  if (is.null(group) || is.null(value_col) ||
        !is.numeric(data[[value_col]])) {
    return(NULL)
  }

  # Order via the shared rule; the "value" sort totals the metric like the
  # canvas' orderGroups over the raw values.
  agg <- data.frame(
    g = as.character(data[[group]]),
    .value = data[[value_col]]
  )
  names(agg)[1L] <- group
  lv <- gg_sorted_levels(agg, data, group, sort_by, sort_dir, flip = horiz)

  # Stats per (facet, group, color) slot, precomputed so the drawn box is
  # numerically the canvas box (same quantile type, same whisker rule --
  # and outliers are the points past the DRAWN whiskers by construction).
  cells <- c(facet, group, color)
  idx <- split(seq_len(nrow(data)), lapply(cells, function(cc) {
    as.character(data[[cc]])
  }), drop = TRUE, sep = "\r")

  rows <- list()
  pts <- list()
  for (key in names(idx)) {
    i <- idx[[key]]
    st <- gg_box_stats(data[[value_col]][i], summary, whiskers)
    if (is.null(st)) {
      next
    }
    parts <- strsplit(key, "\r", fixed = TRUE)[[1L]]
    # strsplit drops trailing empties; pad so the cell always fills.
    parts <- c(parts, rep("", length(cells) - length(parts)))
    cell <- stats::setNames(as.list(parts), cells)
    rows[[key]] <- data.frame(
      cell, ymin = st$ymin, lower = st$lower, middle = st$middle,
      upper = st$upper, ymax = st$ymax,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    if (identical(box_points %||% "none", "outliers") &&
          length(st$outliers)) {
      pts[[key]] <- data.frame(
        cell, .value = st$outliers,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) {
    return(NULL)
  }

  box <- do.call(rbind, unname(rows))
  box[[group]] <- factor(box[[group]], levels = lv)
  out <- if (length(pts)) {
    o <- do.call(rbind, unname(pts))
    o[[group]] <- factor(o[[group]], levels = lv)
    o
  }

  axis_labs <- if (count_on %in% c("axis", "both")) {
    gg_count_labels(data, group, count_col, NULL)
  }

  if (!is.null(color)) {
    box[[color]] <- factor(box[[color]], levels = gg_js_levels(data[[color]]))
    if (!is.null(out)) {
      out[[color]] <- factor(out[[color]],
                             levels = gg_js_levels(data[[color]]))
    }
  }

  lwd <- gg_px_lw(1)
  n_colors <- if (is.null(color)) 1L else length(dd_levels(data[[color]]))
  # Box widths follow the ECharts layout: available = 80% of the band - 2,
  # a 30% gap between split boxes, each box clamped to [7, 50] px of the
  # device band. Horizontal keeps the fixed 28px-slot fraction. Plain
  # position_dodge keeps the outlier points on the box centre lines.
  if (horiz) {
    bw <- 0.73 / max(1L, n_colors)
    dodge <- ggplot2::position_dodge(width = 0.75)
  } else {
    band <- gg_band_px(length(lv), if (is.null(facet)) 1L else
      length(dd_levels(data[[facet]])))
    avail <- band * 0.8 - 2
    gap <- avail / n_colors * 0.3
    bx <- min(50, max(7, (avail - gap * (n_colors - 1)) / n_colors))
    bw <- bx / band
    dodge <- ggplot2::position_dodge(
      width = (n_colors * bx + (n_colors - 1) * gap) / band
    )
  }

  # geom_boxplot draws x -> box over y stats; horizontal flips via the
  # orientation-aware aes below.
  stat_aes <- if (horiz) {
    ggplot2::aes(
      y = .data[[group]], xmin = .data$ymin, xlower = .data$lower,
      xmiddle = .data$middle, xupper = .data$upper, xmax = .data$ymax
    )
  } else {
    ggplot2::aes(
      x = .data[[group]], ymin = .data$ymin, lower = .data$lower,
      middle = .data$middle, upper = .data$upper, ymax = .data$ymax
    )
  }

  p <- ggplot2::ggplot(box)

  # Fill = the series color at the canvas' 0x22 alpha over a full-strength
  # border; median line at the border width (fatten = 1). Whisker caps and
  # the median run the full box width in the canvas -- geom_boxplot's stat
  # identity draws exactly that (staples off matches stems; the canvas caps
  # ARE full width, which geom_boxplot does not draw, so keep the default
  # capless stems: the closest ggplot form without a custom grob).
  if (!is.null(color)) {
    p <- p + ggplot2::geom_boxplot(
      gg_aes(stat_aes, ggplot2::aes(fill = .data[[color]],
                                    color = .data[[color]],
                                    group = interaction(
                                      .data[[group]], .data[[color]]
                                    ))),
      data = box, stat = "identity", position = dodge,
      width = bw, fatten = 1, linewidth = lwd, alpha = 0.133
    ) +
      gg_scale_fill(scale_map, color, data) +
      gg_scale_color(scale_map, color, data)
  } else {
    hex <- dd_palette(1L)
    p <- p + ggplot2::geom_boxplot(
      stat_aes,
      data = box, stat = "identity",
      width = bw, fatten = 1, linewidth = lwd,
      fill = ggplot2::alpha(hex, 0.133), color = hex
    )
  }

  if (!is.null(out)) {
    pt_aes <- if (horiz) {
      ggplot2::aes(x = .data$.value, y = .data[[group]])
    } else {
      ggplot2::aes(x = .data[[group]], y = .data$.value)
    }
    # Outliers sit ON the box's centre line -- no jitter, 5px circles at
    # 0.85 opacity in the series color.
    if (!is.null(color)) {
      p <- p + ggplot2::geom_point(
        gg_aes(pt_aes, ggplot2::aes(color = .data[[color]],
                                    group = interaction(
                                      .data[[group]], .data[[color]]
                                    ))),
        data = out, position = dodge,
        size = gg_px_size(5), alpha = 0.85
      )
    } else {
      p <- p + ggplot2::geom_point(
        pt_aes, data = out,
        color = dd_palette(1L), size = gg_px_size(5), alpha = 0.85
      )
    }
  }

  val_lab <- gg_axis_title(value_col, data)
  vals <- c(box$ymin, box$ymax, if (!is.null(out)) out$.value)

  if (horiz) {
    p + ggplot2::scale_y_discrete(
      labels = axis_labs %||% ggplot2::waiver()
    ) +
      gg_nice_scale(vals, "x", val_lab) +
      ggplot2::labs(y = NULL)
  } else {
    p + ggplot2::scale_x_discrete(
      labels = axis_labs %||% ggplot2::waiver()
    ) +
      gg_nice_scale(vals, "y", val_lab) +
      ggplot2::labs(x = NULL)
  }
}

gg_scatter <- function(data, x, y, color, facet, smoother,
                             identity_line, vlines, hlines,
                             line_width_mult, dot_size_mult, scale_map) {

  if (is.null(x) || is.null(y)) {
    return(NULL)
  }

  if (!is.null(color)) {
    data[[color]] <- gg_as_level_factor(data[[color]])
  }

  p <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data[[x]], y = .data[[y]])
  )

  # Canvas scatter: 6px circles, fully opaque.
  size <- gg_px_size(6 * (dot_size_mult %||% 1))

  p <- if (!is.null(color)) {
    # key_glyph rect: the legend band swatch is a rounded rect, not a dot.
    p + ggplot2::geom_point(
      ggplot2::aes(color = .data[[color]]), size = size, key_glyph = "rect"
    ) + gg_scale_color(scale_map, color, data)
  } else {
    p + ggplot2::geom_point(
      color = dd_palette(1L), size = size
    )
  }

  identity_on <- isTRUE(identity_line) || identical(identity_line, "on")

  if (identity_on) {
    # Slate dashed diagonal; the guide only reads on matched axes (the
    # canvas pins both axes to the shared union extent when the guide is
    # on).
    p <- p + ggplot2::geom_abline(
      slope = 1, intercept = 0, linetype = "dashed",
      color = GG_IDENTITY_LINE_COLOR,
      linewidth = gg_px_lw(1.5 * (line_width_mult %||% 1))
    )
  }

  if ((smoother %||% "none") %in% c("lm", "loess")) {
    # The canvas smoother is one fit PER COLOR GROUP in the series color:
    # 2px solid at 0.9 opacity, no confidence ribbon.
    sm_lw <- gg_px_lw(2)
    p <- if (!is.null(color)) {
      p + ggplot2::geom_smooth(
        ggplot2::aes(color = .data[[color]]),
        method = smoother, se = FALSE, formula = y ~ x,
        linewidth = sm_lw, alpha = 0.9, show.legend = FALSE
      )
    } else {
      p + ggplot2::geom_smooth(
        method = smoother, se = FALSE, formula = y ~ x,
        linewidth = sm_lw, color = dd_palette(1L), alpha = 0.9
      )
    }
  }

  p <- gg_helper_lines(p, vlines, hlines, line_width_mult)

  xv <- data[[x]]
  yv <- data[[y]]
  if (identity_on) {
    xv <- yv <- c(data[[x]], data[[y]])
  }

  p + gg_nice_scale(xv, "x", gg_axis_title(x, data)) +
    gg_nice_scale(yv, "y", gg_axis_title(y, data))
}

# Merge aes() fragments (later wins); NULL fragments drop out. aes objects
# are "uneval"-classed lists, so modifyList composes them.
gg_aes <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  out <- Reduce(function(a, b) utils::modifyList(a, b), parts)
  structure(out, class = "uneval")
}

# Monotone interpolation through a series' points -- the canvas' default
# `connect = "monotone"` (ECharts smooth + smoothMonotone: 'x'). Resampled
# through monoH.FC, the same shape-preserving Hermite family: no overshoot,
# no invented extrema. Falls back to the raw points when a series is too
# short or x is not numeric-like.
gg_monotone_paths <- function(data, x, y, grp) {

  xv_all <- data[[x]]
  proto <- xv_all[0L]
  numeric_x <- is.numeric(xv_all) || inherits(xv_all, c("Date", "POSIXct"))

  keys <- if (length(grp)) {
    interaction(lapply(grp, function(g) data[[g]]), drop = TRUE)
  } else {
    rep.int(1L, nrow(data))
  }

  out <- lapply(split(seq_len(nrow(data)), keys), function(i) {
    d <- data[i, , drop = FALSE]
    d <- d[order(as.numeric(d[[x]])), , drop = FALSE]
    d <- d[is.finite(as.numeric(d[[x]])) & is.finite(d[[y]]), , drop = FALSE]
    d <- d[!duplicated(d[[x]]), , drop = FALSE]
    if (!numeric_x || nrow(d) < 3L) {
      return(d[c(x, y, grp)])
    }
    xs <- as.numeric(d[[x]])
    fn <- stats::splinefun(xs, d[[y]], method = "monoH.FC")
    xi <- seq(min(xs), max(xs), length.out = min(400L, nrow(d) * 20L))
    smooth <- data.frame(xi, fn(xi))
    names(smooth) <- c(x, y)
    for (g in grp) {
      smooth[[g]] <- d[[g]][1L]
    }
    # Restore the x class (Date / POSIXct) so scales line up.
    attributes(smooth[[x]]) <- attributes(proto)
    smooth[c(x, y, grp)]
  })

  do.call(rbind, unname(out))
}

# Series color assignment for series != color: all series sharing a color
# value share one hue, assigned in FIRST-SEEN order over the series levels
# (the canvas' _colorLookup), not the sorted level order.
gg_series_color_values <- function(map, color, series, data) {

  if (!is.null(map) && has_blockr_theme()) {
    res <- dd_resolve_scales(map, color, data[[color]])
    if (!is.null(res$color)) {
      return(gg_level_colors(map, color, data))
    }
  }

  first_color <- vapply(
    split(as.character(data[[color]]), as.character(data[[series]])),
    `[[`, character(1L), 1L
  )
  seen <- unique(unname(first_color[gg_js_levels(data[[series]])]))
  seen <- seen[!is.na(seen)]
  pal <- stats::setNames(rep_len(dd_palette(), length(seen)), seen)

  lv <- gg_js_levels(data[[color]])
  hex <- pal[lv]
  # rep_len, NOT dd_level_palette: this is the individual family, whose JS twin
  # (colorForLevel / _colorLookup in inst/js/chart.js) still cycles on purpose.
  # Those colours do stability under filtering, not legend decodability, and a
  # per-patient chart greyed past level 7 would be no chart at all.
  hex[is.na(hex)] <- rep_len(dd_palette(), length(lv))[is.na(hex)]
  stats::setNames(unname(hex), lv)
}

gg_line <- function(data, x, y, series, color, facet, lo, hi, connect,
                          vlines, hlines, line_width_mult, dot_size_mult,
                          scale_map) {

  if (is.null(x) || is.null(y)) {
    return(NULL)
  }

  grp <- c(series, color)

  aes_grp <- if (length(grp) == 2L) {
    ggplot2::aes(group = interaction(.data[[grp[1L]]], .data[[grp[2L]]]))
  } else if (length(grp) == 1L) {
    ggplot2::aes(group = .data[[grp[1L]]])
  }

  aes_col <- if (!is.null(color)) {
    ggplot2::aes(color = .data[[color]])
  }

  mark_aes <- if (!is.null(aes_col) || !is.null(aes_grp)) {
    gg_aes(aes_col, aes_grp)
  }

  # Points draw in axis order, not row order (the canvas sorts them).
  data <- data[order(as.numeric(data[[x]])), , drop = FALSE]

  if (!is.null(color)) {
    data[[color]] <- gg_as_level_factor(data[[color]])
  }

  p <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data[[x]], y = .data[[y]])
  )

  lo <- gg_col(lo, data)
  hi <- gg_col(hi, data)

  lm <- line_width_mult %||% 1
  lw <- gg_px_lw(1.4 * lm)

  # Marker + opacity budget by series count, the canvas' degradation
  # ladder: <= 50 series full-opacity 4px markers; 51-500 no markers at
  # 0.35; past 500 no markers at 0.15.
  n_series <- if (length(grp)) {
    nrow(unique(data[grp]))
  } else {
    1L
  }
  line_alpha <- if (n_series > 500L) 0.15 else if (n_series > 50L) 0.35 else 1

  # The default line color (no color mapping) is the palette's first hue,
  # like every canvas mark; a color mapping routes through aes instead.
  fixed_color <- if (is.null(color)) list(color = dd_palette(1L))

  if (!is.null(lo) && !is.null(hi)) {
    # The canvas draws lo/hi on a line as per-x whiskers (error bars), not
    # a ribbon: 1px stems with small caps in the series color.
    p <- p + do.call(ggplot2::geom_errorbar, c(
      list(
        gg_aes(ggplot2::aes(ymin = .data[[lo]], ymax = .data[[hi]]),
               mark_aes),
        linewidth = gg_px_lw(1), width = 0
      ),
      fixed_color
    ))
  }

  connect <- (connect %||% "monotone")[1L]

  step_dir <- switch(
    connect,
    `step-start` = "vh",
    `step-middle` = "mid",
    `step-end` = "hv",
    NULL
  )

  p <- p + if (!is.null(step_dir)) {
    do.call(ggplot2::geom_step, c(
      list(mapping = mark_aes, linewidth = lw, direction = step_dir,
           alpha = line_alpha, key_glyph = "rect"),
      fixed_color
    ))
  } else if (identical(connect, "straight")) {
    do.call(ggplot2::geom_line, c(
      list(mapping = mark_aes, linewidth = lw, alpha = line_alpha,
           key_glyph = "rect"),
      fixed_color
    ))
  } else {
    # monotone (the default): resampled per series AND facet, so panels
    # never share an interpolation.
    do.call(ggplot2::geom_path, c(
      list(mapping = mark_aes,
           data = gg_monotone_paths(data, x, y, c(grp, facet)),
           linewidth = lw, alpha = line_alpha, key_glyph = "rect"),
      fixed_color
    ))
  }

  if (!is.null(color)) {
    p <- p + if (!is.null(series) && !identical(series, color)) {
      ggplot2::scale_color_manual(
        values = gg_series_color_values(scale_map, color, series, data),
        name = gg_axis_title(color, data)
      )
    } else {
      gg_scale_color(scale_map, color, data)
    }
  }

  if (n_series <= 50L) {
    p <- p + do.call(ggplot2::geom_point, c(
      list(mapping = mark_aes,
           size = gg_px_size(4 * (dot_size_mult %||% 1)),
           key_glyph = "rect"),
      fixed_color
    ))
  }

  p <- gg_helper_lines(p, vlines, hlines, line_width_mult)

  p <- p + gg_nice_scale(
    c(data[[y]], if (!is.null(lo)) data[[lo]],
      if (!is.null(hi)) data[[hi]]),
    "y", gg_axis_title(y, data)
  )

  # A numeric x takes the same nice extent; dates keep ggplot's date scale
  # (the canvas' adaptive time axis has no fixed-interval equivalent).
  p <- p + if (is.numeric(data[[x]])) {
    gg_nice_scale(data[[x]], "x", gg_axis_title(x, data))
  } else {
    ggplot2::labs(x = gg_axis_title(x, data))
  }

  p
}

gg_helper_lines <- function(p, vlines, hlines, line_width_mult = 1) {

  vlines <- suppressWarnings(as.numeric(unlist(vlines %||% list())))
  hlines <- suppressWarnings(as.numeric(unlist(hlines %||% list())))
  vlines <- vlines[is.finite(vlines)]
  hlines <- hlines[is.finite(hlines)]

  lw <- gg_px_lw(1.5 * (line_width_mult %||% 1))

  if (length(vlines)) {
    p <- p + ggplot2::geom_vline(
      xintercept = vlines, linetype = "dashed",
      color = GG_REF_LINE_COLOR, linewidth = lw
    )
  }
  if (length(hlines)) {
    p <- p + ggplot2::geom_hline(
      yintercept = hlines, linetype = "dashed",
      color = GG_REF_LINE_COLOR, linewidth = lw
    )
  }

  p
}

# -- titles, labels, theme ---------------------------------------------------

# The value-axis title: the value column's label attribute (else its name),
# "Count" for a row count -- chart.js valueTitle. The aggregation function
# is NOT part of the axis title on the canvas. Callers without a data
# frame at hand (chart_expr headless) fall back to the column name.
gg_value_label <- function(value_col, func, data = NULL) {
  if (is.null(value_col)) {
    return("Count")
  }
  if (is.null(data)) {
    return(value_col)
  }
  gg_axis_title(value_col, data)
}

# `{token}` templates resolve against the data through the block's own
# resolver; NULL falls to the auto tier (the data's display attribute,
# verbatim); "" suppresses the band. resolve_block_title is the same
# three-tier contract the canvas receives pre-resolved strings from.
gg_apply_titles <- function(p, title, subtitle, caption, data) {
  auto <- input_display_attrs(data)
  p + ggplot2::labs(
    title = resolve_block_title(title, data, auto = auto$label),
    subtitle = resolve_block_title(subtitle, data, auto = auto$subtitle),
    caption = resolve_block_title(caption, data, auto = auto$caption)
  )
}

# The canvas chrome, constant for constant. Text sizes are the CSS pixel
# values converted to pt (x 0.75): title 15px/600/#1f2937, subtitle
# 13px/#6b7280, caption 12px italic/#6b7280 (left-aligned, like the HTML
# band), ticks and axis names 11px/#666. Gridlines dashed #f3f4f6, axis
# lines #ccc. Legend: a bottom-centered band, 11px labels over 25x14px
# rounded swatches, semibold #6b7280 title. Facet strips: uppercase
# semibold #6b7280 on #f9fafb.
gg_theme <- function() {
  ggplot2::theme_minimal(base_size = gg_px_pt(11)) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      text = ggplot2::element_text(color = "#333333"),
      plot.title = ggplot2::element_text(
        face = "bold", size = gg_px_pt(15), color = "#1f2937"
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        color = "#6b7280", size = gg_px_pt(13)
      ),
      plot.caption = ggplot2::element_text(
        color = "#6b7280", size = gg_px_pt(12), hjust = 0,
        face = "italic"
      ),
      plot.caption.position = "plot",
      axis.text = ggplot2::element_text(
        size = gg_px_pt(11), color = GG_AXIS_LABEL_COLOR
      ),
      axis.title = ggplot2::element_text(
        size = gg_px_pt(11), color = GG_AXIS_LABEL_COLOR
      ),
      panel.grid.major = ggplot2::element_line(
        color = GG_SPLIT_LINE_COLOR, linewidth = gg_px_lw(1),
        linetype = "dashed"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = ggplot2::element_text(
        size = gg_px_pt(11), face = "bold", color = "#6b7280"
      ),
      legend.text = ggplot2::element_text(
        size = gg_px_pt(11), color = "#333333"
      ),
      legend.key.width = grid::unit(25 / 96, "in"),
      legend.key.height = grid::unit(14 / 96, "in"),
      strip.background = ggplot2::element_rect(
        fill = "#f9fafb", color = GG_SPLIT_LINE_COLOR,
        linewidth = gg_px_lw(1)
      ),
      strip.text = ggplot2::element_text(
        face = "bold", color = "#6b7280", size = gg_px_pt(12)
      ),
      panel.spacing = grid::unit(8 / 96, "in")
    )
}

# Per-family grid and axis-line pruning: the canvas draws gridlines on the
# VALUE axis only (dashed #f3f4f6) -- never on a category axis, and not at
# all on a horizontal boxplot. The individual charts (scatter / line) grid
# both axes. Axis lines are #ccc; horizontal layouts hide the category
# axis line, like the canvas.
gg_grid_theme <- function(chart_type, horiz) {

  none <- ggplot2::element_blank()
  axis_line <- ggplot2::element_line(
    color = GG_AXIS_LINE_COLOR, linewidth = gg_px_lw(1)
  )

  if (chart_type %in% c("scatter", "line")) {
    return(ggplot2::theme(
      axis.line = axis_line
    ))
  }

  # Horizontal category labels sit LEFT-ALIGNED in the canvas' fixed
  # gutter, not right-aligned against the axis.
  cat_labels_y <- ggplot2::element_text(hjust = 0)

  if (identical(chart_type, "boxplot") && horiz) {
    return(ggplot2::theme(
      panel.grid.major = none,
      axis.line.x = axis_line,
      axis.text.y = cat_labels_y
    ))
  }

  if (horiz) {
    # Horizontal bar / boxplot: value axis is x.
    ggplot2::theme(
      panel.grid.major.y = none,
      axis.line.x = axis_line,
      axis.text.y = cat_labels_y
    )
  } else {
    ggplot2::theme(
      panel.grid.major.x = none,
      axis.line.y = axis_line,
      axis.line.x = axis_line
    )
  }
}

# -- pptx sizing -------------------------------------------------------------

# Natural deck size from the canvas row geometry: 28px per category row
# (stacked / percent bars, boxplot slots), n * 14px + 12px per group for
# dodged bars, 30px top + 46px bottom chrome, 96px/in. Vertical bars and
# the individual charts take the fixed 350px content box. Capped to the
# usable slide body; the officer placement (blockr.outline::place_exhibit)
# reads these attributes.
gg_attach_pptx_size <- function(p, data, chart_type, horiz, group,
                                color, facet, bar_mode) {

  fit_w <- getOption("blockr.viz.ft_fit_width", 11.9)
  max_h <- 5.6

  n_of <- function(col) {
    if (is.null(col)) 1L else length(dd_levels(data[[col]]))
  }

  rows_px <- if (identical(chart_type, "bar") && horiz) {
    if (identical(bar_mode, "grouped") && !is.null(color)) {
      n_of(group) * (n_of(color) * 14 + 12)
    } else {
      n_of(group) * 28
    }
  } else if (identical(chart_type, "boxplot") && horiz) {
    n_of(group) * max(1L, n_of(color)) * 28
  }

  h <- if (is.null(rows_px)) {
    # Vertical bars / boxplots and the individual charts: the canvas'
    # fixed 350px panel plus title / legend chrome.
    (350 + 130) / 96
  } else {
    n_panels <- n_of(facet)
    panel_rows <- ceiling(n_panels / min(2L, max(1L, n_panels)))
    # 30px top + rows + 46px bottom per panel row, plus the title band.
    min(max_h, panel_rows * (30 + rows_px + 46) / 96 + 0.4)
  }

  attr(p, "pptx_width") <- fit_w
  attr(p, "pptx_height") <- max(2.2, h)
  p
}
