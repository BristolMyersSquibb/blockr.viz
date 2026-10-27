#' Chart block
#'
#' An interactive chart (ECharts) that can also act as a filter. The
#' arguments are grammar-of-graphics aesthetics: each one names a **data
#' column**, never a literal value. Roles are orthogonal: `color` only
#' colours, `series` only splits into series, `label` only writes on-mark
#' text.
#'
#' `x` and `y` are the screen axes for every type, as in ggplot2. For the
#' aggregated types (bar, waterfall, pie, treemap, boxplot, pointrange) one of
#' them is the category and the other the column `func` aggregates, and which
#' way the chart lies follows from that: `x = "class"` draws vertical bars of
#' counts, `y = "class"` horizontal ones, and `x = "sales", y = "region",
#' func = "sum"` horizontal bars of summed sales. With one column and a row
#' count that column is the category; with two, a numeric column beside a
#' non-numeric one is the value. For pie and treemap `x` is the slice and `y`
#' the value.
#'
#' Interactivity is opt-in via `drill`. A click identifies a mark, the mark
#' maps to source rows, and the downstream filter is `drill %in%` the values
#' of `drill` in those rows. With `drill` unset a click does nothing.
#'
#' Arguments of blockr.viz < 0.3.0 (`chart_type`, `group`, `value`,
#' `bar_mode`, `y_scale`, ...) are still accepted, with a warning, and boards
#' saved with them load unchanged. Any other unknown argument, or a value an
#' argument does not take, is an error.
#'
#' @param type Chart type: `"bar"`, `"histogram"`, `"waterfall"`, `"pie"`,
#'   `"treemap"`, `"boxplot"`, `"pointrange"` (aggregated), `"scatter"`,
#'   `"line"`, `"area"` (a line with its area filled, stacked with a colour
#'   split unless `position = "dodge"`),
#'   `"band"` (one mark per row), or `"gantt"` (intervals on lanes). A
#'   waterfall is a bar whose bars float from the running total. Boxplot and
#'   pointrange summarize the raw values within each category in the browser
#'   (see `summary`).
#' @param x,y Columns on the horizontal and vertical axis. See Details for
#'   the aggregated types. For gantt, `x` is the interval start and `y` the
#'   lane.
#' @param func Aggregation of the value column per category (aggregated types
#'   only): `"count"` (default; set only the category column),
#'   `"count_distinct"`, `"mean"`, `"median"`, `"sum"`, `"min"`, `"max"`,
#'   `"identity"` or `"pct_distinct"`. `"identity"` does not aggregate: it
#'   draws the value as is, one bar per row when the category is unique.
#'   `"pct_distinct"` is the share of the panel's distinct values (see
#'   `percent_of`). With a `color` split, `"count_distinct"` counts an entity
#'   once per colour level it appears under.
#' @param color Column mapped to colour.
#' @param facet Column to facet by, one small panel per level.
#' @param series Column whose values split rows into separate series (one
#'   line or point group per value; per-bar label on a gantt). Independent of
#'   `color`.
#' @param xend Interval end column (gantt only). Rows without an end are drawn
#'   as a dot at `x`.
#' @param label Column written on the marks: beside each point (scatter,
#'   line, area), on each bar, slice or tile (bar, pie, treemap) and on each
#'   gantt bar. A bar, slice or tile shows the value only when its rows agree
#'   on one. Not drawn on waterfall, histogram, boxplot, pointrange, radar or
#'   band.
#' @param orientation `NULL` (default) infers which way a bar, boxplot or
#'   pointrange lies (see Details). Set `"horizontal"` (categories on y) or
#'   `"vertical"` (categories on x) only when both columns are non-numeric,
#'   such as a distinct count of a character id by a category.
#' @param position Layout of a colour-split bar: `"stack"` (default),
#'   `"dodge"` (side by side) or `"fill"` (stacked to 100%).
#' @param sort_by Category order: `"value"`, `"data"` (factor levels; a
#'   date or numeric category in time or numeric order; else first
#'   appearance), `"alpha"`, or a column name (the per-category minimum of
#'   that column). The default for bar, pie and treemap follows the category
#'   column: `"data"` for a date, date-time, numeric or factor column,
#'   `"value"` for any other. Boxplot and pointrange default to `"data"`.
#'   For gantt: `"onset"` (default), `"alpha"` or a column name. Ignored for
#'   scatter and line.
#' @param sort_dir `"asc"` or `"desc"`. The default is `"desc"` for
#'   `sort_by = "value"`, `"asc"` otherwise.
#' @param value_labels `TRUE` writes each bar's value at its end. A stacked bar
#'   labels the total, a dodged bar every bar, a filled bar each segment's
#'   share. Labels that would overlap are left out. Bar and waterfall only.
#' @param format,unit How the values are printed on the value axis, in
#'   tooltips and in value labels, as on the tile block: `format` is
#'   `"number"` (default; separators and up to two decimals, trailing zeros
#'   dropped, and a value that would round to 0 keeps two significant
#'   digits: 0.0034), `"compact"` (1.2K, 3.4M), `"percent"` (a fraction
#'   times 100, with "%") or a whole number of decimals from 0 to 6, printed
#'   exactly (`2` prints 4.30). Each prints as on the table block.
#'   `unit` is free text after the number (`"CHF"`); the axis title carries
#'   it once.
#'   On scatter, line and area charts it applies to y.
#' @param y_transform,x_transform `"identity"` (default) or `"log10"`, a
#'   base-10 axis. `y_transform` applies to scatter, line and band charts,
#'   `x_transform` to a
#'   scatter; other types ignore them, and so does a date or category axis.
#'   On a log axis values at or below zero are not drawn, and a note under
#'   the chart counts them. The low end of an interval (`ymin`, a band)
#'   that falls at or below zero is cut at the bottom of the axis. A
#'   smoother is fitted on the scale that is drawn.
#' @param tooltip Extra columns shown in each mark's tooltip.
#' @param drill Column a click filters downstream on, `"auto"` for the type's
#'   natural target, or `NULL` (default) for no filtering.
#' @param line_width,point_size Line width and marker size relative to the
#'   default (1). Range 0.5 to 3.
#' @param connect How a line connects its points: `"straight"` (default),
#'   `"monotone"` (smoothed without overshoot), `"step_start"`,
#'   `"step_middle"` or `"step_end"`. A saved chart keeps the value it was
#'   saved with.
#' @param y_lines,x_lines Positions at which dashed guide lines are drawn. A
#'   name labels its line: `x_lines = c(Listed = "2026-10-21")`. `y_lines`
#'   are numbers across the value axis (y on a scatter or line, the value
#'   axis of a bar or boxplot either way round). `x_lines` run across x on a
#'   scatter, line or area, and across the category axis of a bar either way
#'   round: numbers, dates or date-times, or category values, matching x, as
#'   in `x_ranges`. On a bar over dates, a date between two bars marks the bar
#'   whose period holds it.
#' @param ymin,ymax Columns holding the lower and upper end of an interval per
#'   row. On a line they draw a shaded band, on a scatter error bars. Several
#'   columns draw several intervals, the first the outermost: `ymin =
#'   c("lo95", "lo80"), ymax = c("hi95", "hi80")` is a forecast fan. A single
#'   line's band is grey; with a colour split each band takes its line's
#'   colour.
#' @param linetype Column whose values set a line's dash: the first value (in
#'   the order the rows carry them) is solid, the others dashed. A segment
#'   takes the style of the point it leads to, so a forecast is dashed from
#'   the last observed point on. Line and area.
#' @param x_ranges Named stretches of x shaded behind the marks, a named list
#'   of `c(from, to)`: `list(Recession = c("2008-01-01", "2009-06-30"))`.
#'   Dates, date-times, numbers or category values, matching x. On a bar they
#'   shade whole bars, along the category axis.
#' @param legend Where the colour legend goes: `"bottom"` (default, under the
#'   plot), `"top"` (between the subtitle and the plot), `"marks"` (the names
#'   written on the marks: at a line's or an area's end, beside the last bar
#'   of a stack, at the centre of a scatter group, on a pie slice) or
#'   `"none"`. Where a type has no place for names on its marks (dodged bars,
#'   a histogram, a boxplot), `"marks"` keeps the legend under the plot. The
#'   legend is titled only when the colour column has a label.
#' @param highlight One column and some of its values, as
#'   `list(country = "Switzerland")`. Marks whose rows hold one of the values
#'   keep their colour and every other mark is drawn grey behind them. Without
#'   `color` the highlighted marks take the chart's single colour; with it,
#'   they keep their colour level, and the palette is spread over the
#'   highlighted levels only. Highlighted marks are named: in semibold on a
#'   bar's axis, beside a point (when its value picks out one or two points),
#'   at a line's end. The legend lists only the highlighted levels. A title
#'   can say it with `{highlight}`: `subtitle = "...[, {highlight}
#'   highlighted]"`. Bar, scatter, line and area.
#' @param smoother Trend line on a scatter: `"none"` (default), `"lm"` or
#'   `"loess"`, fit per colour or series group and per facet panel.
#' @param identity_line `TRUE` draws a dashed y = x line on a scatter.
#' @param box_points `"none"` (default) or `"outliers"`: the points beyond the
#'   whiskers of a boxplot.
#' @param summary Statistic of a boxplot's body or a pointrange's interval:
#'   `"median_q1_q3"`, `"mean_sd"`, `"mean_2sd"`, `"mean_se"`, `"p5_p95"`,
#'   `"p10_p90"` or `"min_max"`. `NULL` (default) is `"median_q1_q3"` for a
#'   boxplot and `"mean_se"` for a pointrange.
#' @param whiskers A boxplot's outer interval: `"tukey"` (1.5 x IQR, clipped to
#'   the data) or any `summary` value. `NULL` (default) is `"tukey"` for a
#'   boxplot and `"p10_p90"` for a band.
#' @param connect_centers `TRUE` joins a pointrange's centers in category
#'   order.
#' @param band_window,band_size,band_min_n The band (`type = "band"`): a
#'   running quantile band of `y` over `x` across individuals. With
#'   `band_window = "adaptive"` (default) the window at each point grows until
#'   it holds `band_size` distinct individuals (default 45); with `"fixed"`,
#'   `band_size` is a half-width in x units. Points with fewer than
#'   `band_min_n` individuals (default 12) are not drawn.
#' @param id Column that identifies an individual. The band counts it
#'   distinct, and so do the N labels (`show_n`). `NULL` counts rows.
#' @param ref_min,ref_max Columns holding a lower and upper reference limit per
#'   row, drawn as dashed lines at their median and labelled with their
#'   spread.
#' @param waterfall_totals Steps of a waterfall drawn as total bars, from zero
#'   to the running total.
#' @param bins Histogram only: about how many bins `x` is cut into (default
#'   30). The breaks are rounded to readable values ([pretty()]). A click on
#'   a bin filters `x` to its range.
#' @param show_n Observation counts in labels, e.g. "Female (12)": `"none"`
#'   (default), `"axis"`, `"facet"` or `"both"`.
#' @param missing Rows whose category is missing: `"level"` (default) gives
#'   them their own category, `"drop"` removes them from the categories but
#'   keeps them in the panel, so they still count toward `pct_distinct`.
#' @param percent_of The role whose levels a `pct_distinct` denominator is taken
#'   within: `"facet"` (default), `"category"` or `"color"`, or `"total"`,
#'   the whole chart across all panels.
#' @param facet_scales `"fixed"` (default) shares one scale across the panels,
#'   `"free"` lets each panel size itself, `"free_y"` frees y only (scatter,
#'   line and band).
#' @param facet_cols Panels per row; `NULL` (default) fits as many as the card
#'   is wide. Defaults to the option `blockr.viz.facet_cols`.
#' @param title,subtitle,caption Chart text. `NULL` uses the input data's
#'   label, subtitle or caption attribute, `""` shows nothing, and any other
#'   string is a template resolved against the current data: `{col}` (the
#'   column's distinct values), `{label(col)}`, `{n}`, `{n_distinct(col)}`.
#'   `{@arg}` prints one of the block's own settings and makes the word a
#'   control the reader can click (`{label(@y)}`, `{@func in sum, mean}`); a
#'   clause in `[...]` disappears when its setting is empty. `caption`
#'   defaults to the option `blockr.viz.default_caption`.
#' @param download `TRUE` (default) offers the chart for download (image,
#'   HTML, PowerPoint and the plotted numbers).
#' @param drill_target,drill_table Beta. The block id of a value filter block
#'   that a drill click is also sent to, over the board's control channel
#'   (see [ctrl_send()] and [new_ctrl_bridge_extension()]), and the table in
#'   its dm the conditions apply to. `"auto"` finds the board's drill
#'   destination. With a target set, a click is an event: the chart holds no
#'   selection.
#' @param script,script_values Experimental. R code run on the incoming data
#'   before the chart sees it; its top-level plain-value assignments become
#'   controls on the block, and `script_values` holds their settings.
#' @param selection What a click or brush selected. Written by the chart; leave
#'   it `NULL`.
#' @param ... Arguments of [blockr.core::new_block()] (`block_name`, ...),
#'   and the argument names of blockr.viz < 0.3.0. Arguments after `...`
#'   match by their full name only.
#'
#' @return A transform block of class `chart_block`.
#' @examples
#' # counts by class, horizontal bars
#' new_chart_block(type = "bar", y = "class")
#'
#' # mean highway mileage by class and drive train, side by side
#' new_chart_block(
#'   type = "bar", x = "class", y = "hwy", func = "mean",
#'   color = "drv", position = "dodge"
#' )
#'
#' # a scatter with a linear trend per group
#' new_chart_block(
#'   type = "scatter", x = "displ", y = "hwy", color = "drv",
#'   smoother = "lm"
#' )
#'
#' # a subtitle with a control in it
#' new_chart_block(
#'   type = "bar", x = "class", y = "hwy", func = "mean",
#'   subtitle = "{@func in mean, median} of {label(@y)}"
#' )
#' @export
# viz: long the formals mirror the spec, one argument per line
new_chart_block <- function(
    type = "bar",
    ...,
    x = NULL,
    y = NULL,
    func = "count",
    color = NULL,
    facet = NULL,
    series = NULL,
    xend = NULL,
    label = NULL,
    orientation = NULL,
    position = "stack",
    sort_by = NULL,
    sort_dir = NULL,
    value_labels = FALSE,
    format = "number",
    unit = "",
    y_transform = "identity",
    x_transform = "identity",
    tooltip = NULL,
    drill = NULL,
    line_width = 1,
    point_size = 1,
    connect = "straight",
    y_lines = NULL,
    x_lines = NULL,
    ymin = NULL,
    ymax = NULL,
    linetype = NULL,
    x_ranges = NULL,
    legend = "bottom",
    highlight = NULL,
    smoother = "none",
    identity_line = FALSE,
    box_points = "none",
    summary = NULL,
    whiskers = NULL,
    connect_centers = FALSE,
    band_window = "adaptive",
    band_size = 45,
    band_min_n = 12L,
    id = NULL,
    ref_min = NULL,
    ref_max = NULL,
    waterfall_totals = NULL,
    bins = 30L,
    show_n = "none",
    missing = "level",
    percent_of = "facet",
    facet_scales = "fixed",
    facet_cols = getOption("blockr.viz.facet_cols"),
    download = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = getOption("blockr.viz.default_caption"),
    drill_target = "",
    drill_table = "",
    script = NULL,
    script_values = list(),
    selection = NULL) {

  spec <- viz_block_spec("chart")
  given <- mget(intersect(as.character(names(match.call())[-1L]),
                          names(spec$args)))
  dots <- list(...)
  core <- c("block_name", "block_metadata", "ctor", "ctor_pkg")
  args <- legacy_ctor_args("chart", c(given, dots[setdiff(names(dots), core)]))
  values <- viz_check(spec, args)
  if (length(values$ymin) != length(values$ymax)) {
    stop("`ymin` and `ymax` must name the same number of columns.",
         call. = FALSE)
  }
  values <- chart_normalize(values)
  # blockr.core reads the starting state from this frame, by argument name.
  list2env(values, environment())

  res <- blockr.core::new_transform_block(
    server = function(id, data) chart_server(id, data, values),
    ui = chart_ui,
    dat_valid = validate_annotated_df_input,
    allow_empty_state = viz_empty_ok(spec),
    external_ctrl = viz_ctrl_args(spec),
    expr_type = "bquoted",
    class = "chart_block",
    ...
  )
  # blockr.core keeps other `...` as attributes, and would save old names
  # back into every board.
  for (nm in setdiff(names(dots), core)) attr(res, nm) <- NULL
  res
}

chart_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    viz_echarts_dep(),
    drilldown_chart_dep(),
    viz_output(ns("chart"), "chart"),
    # A Shiny output, hoisted into the gear header by the chart.
    shiny::div(class = "dd-chart-dl-host", style = "display:none",
               shiny::uiOutput(ns("chart_download"), inline = TRUE))
  )
}
