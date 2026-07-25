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
#' against the JS engine), colors cycle the same `DD_BLOCKR_PALETTE` with
#' the same level ordering (factor levels, else sorted unique), and a board
#' scale map (arm colors) resolves through the same blockr.theme resolver.
#' The look is print typography, not a pixel copy of the canvas.
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
#' row, 14px per series in grouped mode -- the canvas constants), so
#' officer places each chart at its natural height instead of a one-size
#' box.
#'
#' @param data A data frame (the block's result: its filtered input).
#' @param chart_type,group,color,facet,value,func,x,y,series Chart state,
#'   as in [new_chart_block()].
#' @param bar_mode,orientation,sort_by,sort_dir Bar layout and category
#'   ordering, as in [new_chart_block()].
#' @param count_on,count_col Observation-count labels, as in
#'   [new_chart_block()].
#' @param box_points,smoother,identity_line,lo,hi,step Family-specific
#'   options, as in [new_chart_block()].
#' @param vlines,hlines Numeric helper-line positions.
#' @param line_width_mult,dot_size_mult Size multipliers.
#' @param title,subtitle,caption Title band text; `{token}` templates
#'   resolve against `data` (same resolver as the app), `NULL` composes an
#'   automatic title, `""` suppresses.
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
                     orientation = "horizontal",
                     sort_by = "value",
                     sort_dir = NULL,
                     count_on = "off",
                     count_col = NULL,
                     box_points = "none",
                     smoother = "none",
                     identity_line = FALSE,
                     lo = NULL,
                     hi = NULL,
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
                             value_col, func))
  }

  if (is.null(scale_map)) {
    scale_map <- tryCatch(
      blockr.core::get_board_option_or_null(
        "scale_map", blockr.core::get_session()
      ),
      error = function(e) NULL
    )
  }

  p <- switch(
    chart_type,
    bar = gg_bar(
      data, group, color, facet, value_col, func, bar_mode, orientation,
      sort_by, sort_dir, count_on, count_col, scale_map
    ),
    boxplot = gg_boxplot(
      data, group, color, facet, value_col, box_points, sort_by, sort_dir,
      count_on, count_col, scale_map
    ),
    scatter = gg_scatter(
      data, x, y, color, facet, smoother, identity_line, vlines, hlines,
      dot_size_mult, scale_map
    ),
    line = gg_line(
      data, x, y, series, color, facet, lo, hi, step, vlines, hlines,
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
                             value_col, func))
  }

  if (!is.null(facet)) {
    labs <- if (count_on %in% c("facet", "both")) {
      gg_count_labels(data, facet, count_col, func)
    }
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(.data[[facet]]),
      labeller = if (!is.null(labs)) {
        ggplot2::as_labeller(labs)
      } else {
        "label_value"
      }
    )
  }

  p <- gg_apply_titles(
    p, title, subtitle, caption, data,
    auto = gg_auto_title(chart_type, group, color, value_col, func, x, y)
  )

  p <- p + gg_theme()

  gg_attach_pptx_size(p, data, chart_type, orientation, group, color,
                      facet, bar_mode)
}

# -- column handling ---------------------------------------------------------

# A state column reference: "" and absent columns become NULL, so every
# downstream branch tests presence once. A column the state names but the
# data lost (renamed upstream) degrades silently, same as the app.
gg_col <- function(col, data) {
  col <- as.character(col %||% character())[1L]
  if (is.na(col) || !nzchar(col) || !col %in% names(data)) NULL else col
}

# -- aggregation -------------------------------------------------------------

# The chart's aggregation: one row per (facet, group, color) cell with the
# metric in `.value`. Same engine as the table renderer
# (dd_table_aggregate -> golden-tested against the JS chart), plus the
# chart-only `identity` mode: no aggregation, the cell's FIRST row wins
# (precomputed heights; duplicate categories collapse, as in the app).
gg_agg <- function(data, group, color, facet, value_col, func) {

  cells <- c(facet, group, color)

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

  agg <- dd_table_aggregate(data, group = cells, summaries = summaries)

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
                              value_col, func) {

  if (!is.null(group)) {
    agg <- gg_agg(data, group, color, facet, value_col, func)
    if (!is.null(agg)) {
      return(agg)
    }
  }

  data
}

# -- ordering and labels -----------------------------------------------------

# Category-axis order as a factor level vector. "value": by the level's
# total metric; "alpha": alphabetical; a column name: by that column's
# first value per level. Direction defaults to descending for "value"
# (largest first, i.e. on top of a horizontal chart) and ascending
# otherwise; `sort_dir` ("asc" / "desc") overrides.
#
# ggplot draws the FIRST level of a discrete y axis at the BOTTOM, so
# "largest first" means the level order is reversed before it becomes the
# factor -- handled by the caller via `flip`.
gg_sorted_levels <- function(agg, data, group, sort_by, sort_dir,
                             flip = FALSE) {

  lv <- dd_levels(data[[group]])

  key <- if (identical(sort_by %||% "value", "value")) {
    tot <- tapply(agg$.value, as.character(agg[[group]]), sum, na.rm = TRUE)
    -unname(tot[lv])
  } else if (identical(sort_by, "alpha")) {
    NULL # alphabetical = sorted levels
  } else if (is.character(sort_by) && sort_by %in% names(data)) {
    first <- tapply(
      seq_len(nrow(data)), as.character(data[[group]]), min
    )
    rank(data[[sort_by]][unname(first[lv])], na.last = TRUE)
  } else {
    return(lv) # unknown key: keep the data-level order
  }

  ord <- if (is.null(key)) sort(lv) else lv[order(key)]

  if (identical(sort_dir, "asc")) {
    # "asc" = smallest first.
    ord <- if (is.null(key)) ord else rev(ord)
  } else if (identical(sort_dir, "desc") && is.null(key)) {
    ord <- rev(ord)
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

# Level -> hex, exactly as the canvas assigns colors: the board scale map
# through blockr.theme's resolver when the column is bound, else the shared
# palette cycled over the levels (factor levels, else sorted unique --
# mirroring dd_ident_hex / the JS _orderLevels).
gg_level_colors <- function(map, col, data) {

  x <- data[[col]]
  lv <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  lv <- lv[!is.na(lv)]

  if (!is.null(map) && requireNamespace("blockr.theme", quietly = TRUE)) {
    res <- blockr.theme::resolve_scales(
      map, col,
      levels = dd_levels(x),
      palette = DD_BLOCKR_PALETTE
    )
    if (!is.null(res$color)) {
      hex <- res$color[lv]
      hex[is.na(hex)] <- rep_len(DD_BLOCKR_PALETTE, length(lv))[is.na(hex)]
      return(stats::setNames(unname(hex), lv))
    }
  }

  stats::setNames(rep_len(DD_BLOCKR_PALETTE, length(lv)), lv)
}

gg_scale_fill <- function(map, col, data) {
  ggplot2::scale_fill_manual(values = gg_level_colors(map, col, data))
}

gg_scale_color <- function(map, col, data) {
  ggplot2::scale_color_manual(values = gg_level_colors(map, col, data))
}

# -- families ----------------------------------------------------------------

gg_bar <- function(data, group, color, facet, value_col, func,
                         bar_mode, orientation, sort_by, sort_dir,
                         count_on, count_col, scale_map) {

  if (is.null(group)) {
    return(NULL)
  }

  agg <- gg_agg(data, group, color, facet, value_col, func)

  if (is.null(agg)) {
    return(NULL)
  }

  horiz <- !identical(orientation, "vertical")

  lv <- gg_sorted_levels(agg, data, group, sort_by, sort_dir, flip = horiz)
  agg[[group]] <- factor(as.character(agg[[group]]), levels = lv)

  axis_labs <- if (count_on %in% c("axis", "both")) {
    gg_count_labels(data, group, count_col, func)
  }

  position <- switch(
    bar_mode %||% "stacked",
    grouped = ggplot2::position_dodge2(preserve = "single"),
    percent = "fill",
    "stack"
  )

  mapping <- if (horiz) {
    ggplot2::aes(x = .data$.value, y = .data[[group]])
  } else {
    ggplot2::aes(x = .data[[group]], y = .data$.value)
  }

  p <- ggplot2::ggplot(agg, mapping)

  p <- if (!is.null(color)) {
    p + ggplot2::geom_col(
      ggplot2::aes(fill = .data[[color]]),
      position = position, width = 0.6
    ) + gg_scale_fill(scale_map, color, data)
  } else {
    p + ggplot2::geom_col(
      fill = DD_BLOCKR_PALETTE[[1L]], width = 0.6
    )
  }

  val_lab <- gg_value_label(value_col, func)
  pct <- identical(bar_mode, "percent") && !is.null(color)

  if (horiz) {
    p <- p + ggplot2::scale_y_discrete(
      labels = axis_labs %||% ggplot2::waiver()
    )
    if (pct) {
      p <- p + ggplot2::scale_x_continuous(labels = gg_percent_labels)
    }
    p <- p + ggplot2::labs(
      x = if (pct) "Share" else val_lab, y = NULL
    )
  } else {
    p <- p + ggplot2::scale_x_discrete(
      labels = axis_labs %||% ggplot2::waiver()
    )
    if (pct) {
      p <- p + ggplot2::scale_y_continuous(labels = gg_percent_labels)
    }
    p <- p + ggplot2::labs(
      x = NULL, y = if (pct) "Share" else val_lab
    )
  }

  p
}

gg_boxplot <- function(data, group, color, facet, value_col,
                             box_points, sort_by, sort_dir, count_on,
                             count_col, scale_map) {

  if (is.null(group) || is.null(value_col) ||
        !is.numeric(data[[value_col]])) {
    return(NULL)
  }

  # Order by the median of the value per level ("value" sort), matching the
  # bar's total-based order in spirit.
  med <- tapply(data[[value_col]], as.character(data[[group]]), stats::median,
                na.rm = TRUE)
  lv <- dd_levels(data[[group]])
  lv <- switch(
    sort_by %||% "value",
    alpha = sort(lv),
    lv[order(-unname(med[lv]))]
  )
  if (identical(sort_dir, "asc")) {
    lv <- rev(lv)
  }
  # Horizontal: one row per category, largest at the top.
  lv <- rev(lv)

  d <- data
  d[[group]] <- factor(as.character(d[[group]]), levels = lv)

  axis_labs <- if (count_on %in% c("axis", "both")) {
    gg_count_labels(data, group, count_col, NULL)
  }

  mapping <- ggplot2::aes(x = .data[[value_col]], y = .data[[group]])

  p <- ggplot2::ggplot(d, mapping)

  outlier_shape <- if (identical(box_points %||% "none", "outliers")) {
    19
  } else {
    NA
  }

  p <- if (!is.null(color)) {
    p + ggplot2::geom_boxplot(
      ggplot2::aes(fill = .data[[color]]),
      outlier.shape = outlier_shape, outlier.size = 1, alpha = 0.85
    ) + gg_scale_fill(scale_map, color, data)
  } else {
    p + ggplot2::geom_boxplot(
      fill = DD_BLOCKR_PALETTE[[1L]],
      outlier.shape = outlier_shape, outlier.size = 1, alpha = 0.85
    )
  }

  if (identical(box_points, "all")) {
    pos <- if (!is.null(color)) {
      ggplot2::position_jitterdodge(jitter.height = 0.15)
    } else {
      ggplot2::position_jitter(height = 0.15)
    }
    p <- p + ggplot2::geom_point(
      position = pos, size = 0.7, alpha = 0.45, color = "#4b5563"
    )
  }

  p + ggplot2::scale_y_discrete(labels = axis_labs %||% ggplot2::waiver()) +
    ggplot2::labs(x = value_col, y = NULL)
}

gg_scatter <- function(data, x, y, color, facet, smoother,
                             identity_line, vlines, hlines, dot_size_mult,
                             scale_map) {

  if (is.null(x) || is.null(y)) {
    return(NULL)
  }

  p <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data[[x]], y = .data[[y]])
  )

  size <- 1.8 * (dot_size_mult %||% 1)

  p <- if (!is.null(color)) {
    p + ggplot2::geom_point(
      ggplot2::aes(color = .data[[color]]), size = size, alpha = 0.8
    ) + gg_scale_color(scale_map, color, data)
  } else {
    p + ggplot2::geom_point(
      color = DD_BLOCKR_PALETTE[[1L]], size = size, alpha = 0.8
    )
  }

  if (isTRUE(identity_line)) {
    # The identity guide only reads on matched axes (the app pins both axes
    # to the shared range when the guide is on).
    rng <- range(c(data[[x]], data[[y]]), na.rm = TRUE)
    p <- p +
      ggplot2::geom_abline(
        slope = 1, intercept = 0, linetype = "dashed", color = "#6b7280"
      ) +
      ggplot2::expand_limits(x = rng, y = rng)
  }

  if ((smoother %||% "none") %in% c("lm", "loess")) {
    p <- p + ggplot2::geom_smooth(
      method = smoother, se = FALSE, formula = y ~ x,
      linewidth = 0.7, color = "#374151"
    )
  }

  p <- gg_helper_lines(p, vlines, hlines)

  p + ggplot2::labs(x = x, y = y)
}

# Merge aes() fragments (later wins); NULL fragments drop out. aes objects
# are "uneval"-classed lists, so modifyList composes them.
gg_aes <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  out <- Reduce(function(a, b) utils::modifyList(a, b), parts)
  structure(out, class = "uneval")
}

gg_line <- function(data, x, y, series, color, facet, lo, hi, step,
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

  p <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data[[x]], y = .data[[y]])
  )

  lo <- gg_col(lo, data)
  hi <- gg_col(hi, data)

  if (!is.null(lo) && !is.null(hi)) {
    p <- p + ggplot2::geom_ribbon(
      gg_aes(ggplot2::aes(ymin = .data[[lo]], ymax = .data[[hi]]), aes_grp),
      alpha = 0.15, fill = DD_BLOCKR_PALETTE[[1L]]
    )
  }

  lw <- 0.7 * (line_width_mult %||% 1)

  direction <- switch(
    step %||% "",
    start = "vh",
    middle = "mid",
    end = "hv",
    NULL
  )

  p <- p + if (is.null(direction)) {
    ggplot2::geom_line(mapping = mark_aes, linewidth = lw)
  } else {
    ggplot2::geom_step(mapping = mark_aes, linewidth = lw,
                       direction = direction)
  }

  if (!is.null(color)) {
    p <- p + gg_scale_color(scale_map, color, data)
  }

  # Point markers ride the line up to the same series budget the canvas
  # uses for its marker cutoff.
  n_series <- if (length(grp)) {
    nrow(unique(data[grp]))
  } else {
    1L
  }

  if (n_series <= 50L) {
    p <- p + ggplot2::geom_point(
      mapping = mark_aes, size = 1.4 * (dot_size_mult %||% 1)
    )
  }

  p <- gg_helper_lines(p, vlines, hlines)

  p + ggplot2::labs(x = x, y = y)
}

gg_helper_lines <- function(p, vlines, hlines) {

  vlines <- suppressWarnings(as.numeric(unlist(vlines %||% list())))
  hlines <- suppressWarnings(as.numeric(unlist(hlines %||% list())))
  vlines <- vlines[is.finite(vlines)]
  hlines <- hlines[is.finite(hlines)]

  if (length(vlines)) {
    p <- p + ggplot2::geom_vline(
      xintercept = vlines, linetype = "dashed", color = "#9ca3af"
    )
  }
  if (length(hlines)) {
    p <- p + ggplot2::geom_hline(
      yintercept = hlines, linetype = "dashed", color = "#9ca3af"
    )
  }

  p
}

# -- titles, labels, theme ---------------------------------------------------

gg_value_label <- function(value_col, func) {
  if (identical(func, "identity")) {
    return(value_col %||% "Value")
  }
  word <- AGG_WORDS[[func %||% "count"]] %||% "Count"
  if (is.null(value_col)) word else paste(word, value_col)
}

gg_percent_labels <- function(x) {
  paste0(round(100 * x), "%")
}

gg_auto_title <- function(chart_type, group, color, value_col, func, x, y) {

  if (chart_type %in% c("scatter", "line")) {
    if (is.null(x) || is.null(y)) {
      return("")
    }
    joiner <- if (identical(chart_type, "line")) " over " else " vs "
    return(paste0(y, joiner, x))
  }

  if (is.null(group)) {
    return("")
  }

  paste0(
    gg_value_label(value_col, func), " by ", group,
    if (!is.null(color) && !identical(color, group)) paste0(" and ", color)
  )
}

# `{token}` templates resolve against the data through the block's own
# resolver; NULL falls to the automatic tier; "" suppresses the band.
gg_resolve_title <- function(txt, data, auto = "") {

  if (is.null(txt)) {
    txt <- auto
  }

  txt <- as.character(txt)[1L]

  if (is.na(txt) || !nzchar(txt)) {
    return(NULL)
  }

  if (grepl("{", txt, fixed = TRUE)) {
    txt <- resolve_title_template(txt, data)
  }

  if (is.character(txt) && nzchar(txt)) txt else NULL
}

gg_apply_titles <- function(p, title, subtitle, caption, data, auto) {
  p + ggplot2::labs(
    title = gg_resolve_title(title, data, auto),
    subtitle = gg_resolve_title(subtitle, data),
    caption = gg_resolve_title(caption, data)
  )
}

# Print typography over the app's text palette: dark #333 body text, grey
# #6b7280 subtitle / caption (the canvas band colors), light grid.
gg_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#333333"),
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(color = "#6b7280", size = 10),
      plot.caption = ggplot2::element_text(
        color = "#6b7280", size = 8, hjust = 0
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", color = "#333333")
    )
}

# -- pptx sizing -------------------------------------------------------------

# Natural deck size from the canvas row geometry: 28px per category row
# (stacked / percent bars, boxplot categories), 14px per series within a
# group for dodged bars, 96px/in. Vertical bars and the individual charts
# take a fixed content box. Capped to the usable slide body; the officer
# placement (blockr.outline::place_exhibit) reads these attributes.
gg_attach_pptx_size <- function(p, data, chart_type, orientation, group,
                                color, facet, bar_mode) {

  fit_w <- getOption("blockr.viz.ft_fit_width", 11.9)
  max_h <- 5.6

  n_of <- function(col) {
    if (is.null(col)) 1L else length(dd_levels(data[[col]]))
  }

  horiz_rows <- if (identical(chart_type, "bar") &&
                      !identical(orientation, "vertical")) {
    if (identical(bar_mode, "grouped") && !is.null(color)) {
      n_of(group) * max(1L, n_of(color)) / 2 # 14px per series row
    } else {
      n_of(group)
    }
  } else if (identical(chart_type, "boxplot")) {
    n_of(group)
  }

  h <- if (is.null(horiz_rows)) {
    5.2
  } else {
    n_panels <- n_of(facet)
    panel_rows <- ceiling(n_panels / min(2L, max(1L, n_panels)))
    # 28px rows + ~1in of title band, axis and legend per panel row.
    min(max_h, panel_rows * (horiz_rows * 28 / 96 + 1.0) + 0.2)
  }

  attr(p, "pptx_width") <- fit_w
  attr(p, "pptx_height") <- max(2.2, h)
  p
}
