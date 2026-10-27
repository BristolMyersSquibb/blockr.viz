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
#' `bar_mode`, ...) are still accepted, with a warning, and boards saved with
#' them load unchanged.
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
#'   `pct_of`). With a `color` split, `"count_distinct"` counts an entity
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
#' @param tooltip Extra columns shown in each mark's tooltip.
#' @param drill Column a click filters downstream on, `"auto"` for the type's
#'   natural target, or `NULL` (default) for no filtering.
#' @param line_width,point_size Line width and marker size relative to the
#'   default (1). Range 0.5 to 3.
#' @param connect How a line connects its points: `"straight"` (default),
#'   `"monotone"` (smoothed without overshoot), `"step-start"`,
#'   `"step-middle"` or `"step-end"`. A saved chart keeps the value it was
#'   saved with.
#' @param y_lines,x_lines Numbers at which dashed guide lines are drawn.
#'   `y_lines` run across the value axis (y on a scatter or line, the value
#'   axis of a bar or boxplot either way round); `x_lines` across a numeric x.
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
#'   Dates, date-times or numbers, matching x.
#' @param legend Where the colour legend goes: `"bottom"` (default, under the
#'   plot), `"top"` (between the subtitle and the plot), `"marks"` (the names
#'   written on the marks: at a line's or an area's end, beside the last bar
#'   of a stack, at the centre of a scatter group, on a pie slice) or
#'   `"none"`. Where a type has no place for names on its marks (dodged bars,
#'   a histogram, a boxplot), `"marks"` keeps the legend under the plot. The
#'   legend is titled only when the colour column has a label.
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
#' @param ref_lo,ref_hi Columns holding a lower and upper reference limit per
#'   row, drawn as dashed lines at their median and labelled with their
#'   spread.
#' @param waterfall_totals Steps of a waterfall drawn as total bars, from zero
#'   to the running total.
#' @param bins Histogram only: about how many bins `x` is cut into (default
#'   30). The breaks are rounded to readable values ([pretty()]). A click on
#'   a bin filters `x` to its range.
#' @param show_n Observation counts in labels, e.g. "Female (12)": `"off"`
#'   (default), `"axis"`, `"facet"` or `"both"`.
#' @param missing Rows whose category is missing: `"level"` (default) gives
#'   them their own category, `"drop"` removes them from the categories but
#'   keeps them in the panel, so they still count toward `pct_distinct`.
#' @param pct_of The role whose levels a `pct_distinct` denominator is taken
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
#' @param ... Forwarded to [blockr.core::new_transform_block()].
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
new_chart_block <- function(
    type = "bar",
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
    smoother = "none",
    identity_line = FALSE,
    box_points = "none",
    summary = NULL,
    whiskers = NULL,
    connect_centers = FALSE,
    band_window = "adaptive",
    band_size = 45,
    band_min_n = 12,
    id = NULL,
    ref_lo = NULL,
    ref_hi = NULL,
    waterfall_totals = NULL,
    bins = 30,
    show_n = "off",
    missing = "level",
    pct_of = "facet",
    facet_scales = "fixed",
    # Read once, at construction. A deployment that wants one answer for
    # every chart sets the option; a saved chart keeps its own value.
    facet_cols = getOption("blockr.viz.facet_cols"),
    title = NULL,
    subtitle = NULL,
    # A board that wants every chart to say which filters produced it sets
    # the option to "{filters}", which also reaches the charts an assistant
    # adds.
    caption = getOption("blockr.viz.default_caption"),
    download = TRUE,
    drill_target = "",
    drill_table = "",
    # The prepare script (experimental). Deliberately absent from the
    # registry's argument spec, so the assistant never reaches for a script
    # where a mapping would do. It stays externally controllable for MCP and
    # restore.
    script = NULL,
    script_values = list(),
    # What a click or a brush selected. Written by the chart, not by the
    # caller; a formal because blockr.core saves a block from its formals.
    selection = NULL,
    ...) {

  # Code written for blockr.viz < 0.3.0 passes the old names through `...`.
  # Translate them (R/chart-args.R) and say so. Saved boards take the same
  # route silently, in blockr_deser.chart_block().
  dots <- list(...)
  # R matches a formal before `...` by a unique prefix, so an old `value =`
  # lands in `value_labels`. Take it back.
  call_names <- names(sys.call())
  if ("value" %in% call_names && !"value_labels" %in% call_names) {
    dots$value <- value_labels
    value_labels <- FALSE
  }
  old <- intersect(names(dots), CHART_LEGACY_ARGS)
  if (length(old)) {
    pre <- setdiff(old, "direct_labels")
    if (length(pre)) {
      warning(
        "new_chart_block(): ", paste0("`", pre, "`", collapse = ", "),
        if (length(pre) > 1L) " are" else " is",
        " from blockr.viz < 0.3.0. See ?new_chart_block for the current ",
        "names.",
        call. = FALSE
      )
    }
    if ("direct_labels" %in% old) {
      warning(
        "new_chart_block(): `direct_labels = TRUE` is now ",
        "`legend = \"marks\"`.",
        call. = FALSE
      )
    }
    fn <- sys.function()
    given <- intersect(names(match.call())[-1L], names(formals(fn)))
    cur <- mget(given, envir = environment())
    pub <- chart_legacy_args(c(cur, dots[old]))
    for (nm in intersect(names(pub), names(formals(fn)))) {
      assign(nm, pub[[nm]])
    }
  }
  if (!is.null(dots$expose)) {
    warning(
      "new_chart_block(): `expose` is retired. Name the setting in ",
      "`subtitle` instead, e.g. \"[, coloured by {label(@color)}]\".",
      call. = FALSE
    )
  }

  # A pre-#144 DAG copy/paste wrote NULL state as `{}`, which arrives here as
  # an empty list(); a board saved in that state keeps re-emitting it (see
  # R/state-normalize.R). Heal every optional slot back to NULL. Column roles
  # normalize to character; numeric and free-form slots only collapse the
  # empty list, keeping their type. A script may arrive as a character
  # vector of lines; collapse it here, the one place every construction
  # passes through.
  type <- as.character(unlist(type %||% "bar"))[1L]
  if (!type %in% CHART_TYPES) {
    stop("`type` must be one of ", paste0("\"", CHART_TYPES, "\"",
         collapse = ", "), ". Got \"", type, "\".", call. = FALSE)
  }
  script <- cb_script_text(script)
  x <- chr_state(x)
  y <- chr_state(y)
  orientation <- chr_state(orientation)
  if (!is.null(orientation) &&
        !orientation %in% c("horizontal", "vertical")) {
    stop("`orientation` must be NULL, \"horizontal\" or \"vertical\".",
         call. = FALSE)
  }
  position <- as.character(unlist(position %||% "stack"))[1L]
  if (!position %in% names(CHART_POSITIONS)) {
    stop("`position` must be \"stack\", \"dodge\" or \"fill\".",
         call. = FALSE)
  }
  color <- chr_state(color)
  facet <- chr_state(facet)
  xend <- chr_state(xend)
  series <- chr_state(series)
  label <- chr_state(label)
  drill <- chr_state(drill)
  selection <- chart_selection_state(selection)
  sort_by <- chr_state(sort_by)
  # Several columns are several intervals, the first the outermost.
  ymin <- chr_vec_state(ymin)
  ymax <- chr_vec_state(ymax)
  if (length(ymin) != length(ymax)) {
    stop("`ymin` and `ymax` must name the same number of columns.",
         call. = FALSE)
  }
  linetype <- chr_state(linetype)
  x_ranges <- chart_ranges_state(x_ranges)
  legend <- chr_state(legend) %||% "bottom"
  if (!legend %in% CHART_LEGENDS) {
    stop("new_chart_block(): `legend` must be one of ",
         paste0('"', CHART_LEGENDS, '"', collapse = ", "), ".", call. = FALSE)
  }
  connect <- match.arg(
    connect,
    c("monotone", "straight", "step-start", "step-middle", "step-end")
  )
  tooltip <- chr_vec_state(tooltip)
  waterfall_totals <- chr_vec_state(waterfall_totals)
  # NOT chr_state: "" is a real value on these (explicitly no title, as
  # against NULL = auto) and chr_state would drop it. See title-template.R.
  title <- title_state(title)
  subtitle <- title_state(subtitle)
  caption <- title_state(caption)
  id <- chr_state(id)
  show_n <- as.character(unlist(show_n %||% "off"))[1L]
  missing <- match.arg(as.character(missing %||% "level")[1L],
                       c("level", "drop"))
  # chr_vec_state, not chr_state: the engine takes several roles even though
  # the gear offers one, and truncating here would silently drop the rest.
  pct_of <- chr_vec_state(pct_of) %||% "facet"
  bad <- setdiff(pct_of, names(CHART_PCT_OF))
  if (length(bad)) {
    stop("`pct_of` must be facet, category, color or total. Got ",
         paste0("\"", bad, "\"", collapse = ", "), ".", call. = FALSE)
  }
  facet_scales <- match.arg(
    as.character(facet_scales %||% "fixed")[1L],
    c("fixed", "free_y", "free")
  )
  # NULL (auto) or a whole number as a string; junk heals to auto rather than
  # erroring, because this value also arrives from MCP and from saved state.
  facet_cols <- facet_cols_state(facet_cols)
  y_lines <- num_vec_state(y_lines)
  x_lines <- num_vec_state(x_lines)
  # Logicals. The gear's segmented controls send "on"/"off", and boards
  # saved before these were logicals stored that string.
  identity_line <- bool_state(identity_line)
  connect_centers <- bool_state(connect_centers)
  value_labels <- bool_state(value_labels)
  # box_points lost its "all" option; anything outside the enum (old saved
  # boards) degrades to "none".
  box_points <- if (identical(box_points, "outliers")) "outliers" else "none"
  # NULL = the per-mark default, resolved where it is consumed ("tukey" for
  # the boxplot, "p10_p90" for the band; see SUMMARY_STATS in chart/roles.js).
  summary <- chr_state(summary)
  whiskers <- chr_state(whiskers)
  script_values <- if (is.list(script_values)) script_values else list()
  format <- chart_format_state(format %||% "number")
  if (is.null(format)) {
    stop("`format` must be \"number\", \"compact\", \"percent\" or a ",
         "whole number of decimals from 0 to 6.", call. = FALSE)
  }
  unit <- as.character(unlist(unit %||% ""))[1L] %|NA|% ""
  bins <- suppressWarnings(as.numeric(unlist(bins))[1L])
  if (!isTRUE(bins >= 1)) bins <- 30
  # The module server below takes `id` (its own namespace id), which would
  # shadow the id column.
  id_col <- id


  res <- blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # The busy cue (inst/js/busy-cue.js). The message goes out FIRST and
        # the input is read after it, so a rerun -- which is what an
        # invalidated input causes -- tells the browser before the upstream
        # recompute this read then triggers. Ahead of every other observer,
        # core's evaluation included. The chart ends the cue once it has drawn;
        # the push observer below ends it when no new picture is coming.
        shiny::observe({
          busy_cue_start(session, ns("drilldown_block"), "drawing")
          tryCatch(data(), error = function(e) {
            busy_cue_done(session, ns("drilldown_block"))
          })
        }, priority = 200L)

        # Input coercion, ONCE per upstream change, shared by the render
        # path and the title auto tier: a plain data frame is kept verbatim
        # (byte-identical passthrough, dotted columns included); a
        # table-producing object (composer et al., per the shared input
        # contract) is coerced via as_annotated_df(), with coercion failures
        # mapped to NULL so reader reactives park on their
        # `req(is.data.frame(d))` guards. The `coerced` flag records which
        # branch ran -- only coerced inputs get the reserved `.`-annotation
        # columns stripped below (a chart of a structured frame charts its
        # data columns; the row-structure rendering of principle 6 belongs
        # to the table).
        r_input <- shiny::reactive({
          d <- data()
          if (is.data.frame(d)) {
            list(df = d, coerced = FALSE)
          } else {
            list(
              df = tryCatch(as_annotated_df(d), error = function(e) NULL),
              coerced = TRUE
            )
          }
        })

        # What the render paths consume (same contract as coerce_plain_df,
        # minus the second coercion). Mirrors the table block's ann_data
        # reactive.
        raw_data <- shiny::reactive({
          inp <- r_input()
          d <- inp$df
          if (!is.data.frame(d)) return(NULL)
          if (!inp$coerced) return(d)
          drop <- names(d) %in% annotation_cols_in(d)
          out <- d[, !drop, drop = FALSE]
          # Column subsetting drops data-frame-level attributes on a base
          # data.frame (it does not on a tibble, which is why this is easy to
          # miss). The `{filters}` token reads the filter trail off whatever
          # frame it is handed, so carry it across rather than making every
          # caller thread it in. Same hazard the display attributes above are
          # captured before coercion to avoid.
          attr(out, "blockr_filters") <- attr(d, "blockr_filters", exact = TRUE)
          out
        })

        # --- prepare script ------------------------------------------------
        # Parse is cheap and pure; everything else derives from it.
        r_parsed <- shiny::reactive(cb_parse(r_script()))

        # Declarations are evaluated against the live upstream data, so a
        # factor's levels track the data instead of being frozen at authoring
        # time. NEVER DEGRADE THE SPECS WHILE THE DATA IS MOMENTARILY GONE:
        # `data()` does not merely go stale when a panel is hidden, the
        # visibility gate makes it throw, so `raw_data()` becomes NULL. A
        # declaration like `site <- factor(unique(data$SITE))` then cannot be
        # evaluated, yields an error spec, and cb_expr() drops it as unusable --
        # the compiled body loses its substitution and refers to a symbol
        # nothing defines. Coming back flips it to the good expression again,
        # so the block genuinely re-evaluates on every tab switch. Keeping the
        # last good specs removes the flip-flop. (Learned the hard way in
        # blockr.extra's code block; same fix, same reason.)
        last_specs <- new.env(parent = emptyenv())
        last_specs$value <- list()
        r_specs <- shiny::reactive({
          d <- tryCatch(raw_data(), error = function(e) NULL)
          parsed <- r_parsed()
          if (is.null(d) && length(last_specs$value)) {
            return(last_specs$value)
          }
          out <- cb_specs(parsed, d)
          if (!identical(out, last_specs$value)) {
            last_specs$value <- out
          }
          last_specs$value
        })

        # A script failure has to be visible. It rides the config payload to
        # the band rather than being swallowed into an unexplained empty chart
        # (reference_dead_block_looks_like_a_loading_view).
        r_prepared <- shiny::reactive({
          dd_prepare_run(raw_data(), r_parsed(), r_specs(), r_values())
        })

        # The prepared frame, one row per subject: what the block's own
        # filter and its drill claims read. With no script this is raw_data()
        # unchanged, which is what the other nineteen chart blocks in twenty
        # see.
        prepared_data <- shiny::reactive({
          prep <- r_prepared()
          if (is.null(prep$error)) {
            return(prep$data)
          }
          # A failed script draws NOTHING rather than the untransformed frame:
          # the chart would otherwise show data the block's own output does not
          # have, and say nothing about it. An EMPTY frame carrying the
          # upstream's columns, not NULL, because every render path guards with
          # `req(is.data.frame(d))` -- a NULL halts the push, and then the
          # error never reaches the band and the block just looks like it is
          # still loading (reference_dead_block_looks_like_a_loading_view).
          d <- tryCatch(raw_data(), error = function(e) NULL)
          if (is.data.frame(d)) d[0, , drop = FALSE] else NULL
        })

        # What every render path consumes: the prepared frame, expanded by
        # the group definition of any column a splitting role is bound to
        # (expand_groups()). A subject in two groups is drawn in both. A
        # chart that does not split by such a column gets prepared_data()
        # unchanged, so nothing that counts across groups sees a duplicate.
        plain_data <- shiny::reactive({
          d <- expand_role_groups(prepared_data(), chart_split_roles(
            group = if (!identical(r_chart_type(), "histogram")) r_group(),
            color = r_color(), facet = r_facet(),
            series = r_series(), x = r_x(), y = r_y()
          ))
          hist_bins(d)
        })

        # A histogram: `x` binned into its bin column, which the browser
        # draws as a bar. The bins come from all of the panel's rows, before
        # any click narrows them.
        hist_x <- function(d) {
          x <- r_x_pub()
          if (identical(r_chart_type(), "histogram") && is.data.frame(d) &&
                length(x) && x %in% names(d) && is.numeric(d[[x]])) x
        }
        hist_bins <- function(d) {
          x <- hist_x(d)
          if (is.null(x)) {
            return(d)
          }
          bin <- chart_bins(d[[x]], r_bins())$bin
          # The bins speak for x: a click reads "price = 1000-1500" in the
          # footer, not the bin column's own name.
          attr(bin, "label") <- attr(d[[x]], "label") %||% x
          d[[chart_bin_col(x)]] <- bin
          d
        }
        # The same breaks (they depend on the range of x only), for the
        # filter a click on a bin emits.
        r_hist_edges <- shiny::reactive({
          d <- tryCatch(prepared_data(), error = function(e) NULL)
          x <- hist_x(d)
          if (is.null(x)) NULL else chart_bins(d[[x]], r_bins())$edges
        })

        # Auto-tier sources: the input's label / subtitle / caption
        # attributes, read from the shared coercion above (NOT plain_data:
        # its column subset is base subsetting, which drops data-frame-level
        # attributes -- see input_display_attrs). Previously this re-ran
        # as_annotated_df() a second time for every non-df input.
        r_data_titles <- shiny::reactive({
          inp <- tryCatch(r_input(), error = function(e) NULL)
          input_display_attrs(if (is.list(inp)) inp$df else NULL)
        })

        # --- state -------------------------------------------------------
        # The block's state is held under the PUBLIC argument names (the
        # reactiveVals below that `state` lists). The rest of this server and
        # the browser speak the internal names; where those differ from the
        # public ones they are facades over the public state (R/chart-args.R).

        # Config state (orthogonal aesthetics)
        r_chart_type <- shiny::reactiveVal(type)
        r_x_pub <- shiny::reactiveVal(x)
        r_y_pub <- shiny::reactiveVal(y)
        r_orientation_pub <- shiny::reactiveVal(orientation)
        r_func <- shiny::reactiveVal(func)
        r_color <- shiny::reactiveVal(color)
        r_facet <- shiny::reactiveVal(facet)
        r_xend <- shiny::reactiveVal(xend)
        r_series <- shiny::reactiveVal(series)
        r_label <- shiny::reactiveVal(label)
        # Extra columns to append to each mark's hover tooltip. A character
        # vector or NULL. Optional display-only role, so a value dropped
        # upstream is silently omitted (not a hard invalid-state).
        r_tt_fields <- shiny::reactiveVal(tooltip)
        r_drill <- shiny::reactiveVal(drill)
        r_sort_by <- shiny::reactiveVal(sort_by)
        r_sort_dir <- shiny::reactiveVal(sort_dir)
        r_position <- shiny::reactiveVal(position)
        r_value_labels <- shiny::reactiveVal(value_labels)

        # Whether a column of the prepared data is numeric, for the
        # orientation of a chart whose `orientation` is not given. The last
        # known answer is kept while the data is momentarily gone (a hidden
        # panel makes `data()` throw), so the chart does not flip on a view
        # switch.
        num_cols <- new.env(parent = emptyenv())
        r_num_cols <- shiny::reactive({
          d <- tryCatch(prepared_data(), error = function(e) NULL)
          if (is.data.frame(d) && ncol(d)) {
            num_cols$known <- names(d)
            num_cols$num <- names(d)[vapply(d, is.numeric, logical(1L))]
          }
          list(known = num_cols$known, num = num_cols$num)
        })
        is_num <- function(col) {
          nc <- r_num_cols()
          if (!col %in% nc$known) NA else col %in% nc$num
        }
        is_num_now <- function(col) shiny::isolate(is_num(col))

        # Public x / y / orientation -> the internal roles.
        r_roles <- shiny::reactive({
          chart_roles_internal(r_chart_type(), r_x_pub(), r_y_pub(),
                               r_func(), r_orientation_pub(), is_num)
        })
        r_group <- function() r_roles()$group
        is_hist <- function() identical(r_chart_type(), "histogram")
        r_value <- function() r_roles()$value
        r_x <- function() r_roles()$x
        r_y <- function() r_roles()$y
        r_orientation <- function() r_roles()$orientation
        r_bar_mode <- chart_facade(
          function() chart_position_internal(r_position()),
          function(v) r_position(chart_position_public(v))
        )
        # A waterfall is its own type; the gear's "Bars: cumulative" on a bar
        # is translated to it in the config handler.
        r_baseline <- function() "zero"

        # What a click or a brush selected (transport for the emitted
        # downstream filter), one public `selection` list.
        r_selection <- shiny::reactiveVal(selection)
        sel_get <- function(key, default = NULL) {
          s <- r_selection()
          if (is.null(s)) default else s[[key]] %||% default
        }
        sel_set <- function(key, v) {
          s <- shiny::isolate(r_selection()) %||% list(type = "categorical")
          s[key] <- list(v)
          r_selection(chart_selection(s$type, s$filters, s$range, s$point))
        }
        r_filter_type <- chart_facade(
          function() sel_get("type", "categorical"),
          function(v) sel_set("type", v)
        )
        r_filters <- chart_facade(
          function() sel_get("filters"), function(v) sel_set("filters", v)
        )
        r_filter_range <- chart_facade(
          function() sel_get("range"), function(v) sel_set("range", v)
        )
        r_filter_point <- chart_facade(
          function() sel_get("point"), function(v) sel_set("point", v)
        )

        # External-control send (beta): the drill also pushes its claim into
        # a value filter block (gear section "Send to filter"). Empty target
        # = off, today's behaviour.
        r_ctrl_target  <- shiny::reactiveVal(drill_target %||% "")
        r_ctrl_table   <- shiny::reactiveVal(drill_table %||% "")
        r_ctrl_choices <- dd_ctrl_choices()

        # Transient drill. With a target set, a categorical click is an EVENT
        # sent to that target, not a selection this chart holds: nothing is
        # latched in `r_filter_*`, so the chart does not filter its own output,
        # the board saves no selection, and clicking the same mark twice sends
        # twice instead of toggling off. The undo lives at the target (the
        # cohort pill in the patient profile), which is the only place the
        # effect is visible -- the control channel has no back-edge, so a
        # selection held here goes stale the moment the target is reset by
        # anyone else.
        # NULL = no click this session: HOLD, never clear (see dd_ctrl_claims'
        # "cannot resolve is not an un-drill"). The sender in this mode only
        # ever sends.
        r_drill_claim <- shiny::reactiveVal(NULL)
        transient_drill <- function() {
          nzchar(trimws(r_ctrl_target() %||% ""))
        }

        # Theming state
        r_line_width_mult <- shiny::reactiveVal(line_width)
        r_dot_size_mult <- shiny::reactiveVal(point_size)
        # Line-connect mode (straight / monotone / step-*), the single gear
        # select that replaced step + smooth. Fixed-option, so never empty.
        r_connect <- shiny::reactiveVal(connect)
        r_value_lines <- shiny::reactiveVal(y_lines)
        r_x_lines <- shiny::reactiveVal(x_lines)
        r_smoother <- shiny::reactiveVal(smoother)
        r_identity_line <- shiny::reactiveVal(identity_line)
        r_box_points <- shiny::reactiveVal(box_points)
        # Distribution statistics (see constructor args).
        r_summary <- shiny::reactiveVal(summary)
        r_whiskers <- shiny::reactiveVal(whiskers)
        r_connect_centers <- shiny::reactiveVal(connect_centers)
        # Distribution band (see constructor args).
        r_band_window <- shiny::reactiveVal(band_window)
        r_band_size <- shiny::reactiveVal(band_size)
        r_band_min_n <- shiny::reactiveVal(band_min_n)
        # One id column: what the band counts and what N labels count.
        r_id <- shiny::reactiveVal(id_col)
        r_band_id <- chart_facade(function() r_id(), function(v) r_id(v))
        r_count_col <- chart_facade(function() r_id(), function(v) r_id(v))
        r_ref_hi <- shiny::reactiveVal(ref_hi)
        r_ref_lo <- shiny::reactiveVal(ref_lo)
        # Interval columns (error bars on a line or a scatter).
        r_lo <- shiny::reactiveVal(ymin)
        r_hi <- shiny::reactiveVal(ymax)
        r_linetype <- shiny::reactiveVal(linetype)
        r_x_ranges <- shiny::reactiveVal(x_ranges)
        r_legend <- shiny::reactiveVal(legend)
        r_waterfall_totals <- shiny::reactiveVal(waterfall_totals)
        r_bins <- shiny::reactiveVal(bins)
        # Number format of the values (axis, tooltips, labels), as on the
        # tile block.
        r_format <- shiny::reactiveVal(format)
        r_unit <- shiny::reactiveVal(unit)
        # Chart text. The public value is the template as written; the
        # internal one has its `{@arg}` slots renamed to what the resolver
        # and the gear know.
        r_title_pub <- shiny::reactiveVal(title)
        r_subtitle_pub <- shiny::reactiveVal(subtitle)
        r_caption_pub <- shiny::reactiveVal(caption)
        r_slot_alias <- shiny::reactive(
          chart_slot_alias(r_chart_type(), r_roles())
        )
        tpl_facade <- function(pub) {
          chart_facade(
            function() chart_template_internal(pub(), r_slot_alias()),
            function(v) pub(v)
          )
        }
        r_title <- tpl_facade(r_title_pub)
        r_subtitle <- tpl_facade(r_subtitle_pub)
        r_caption <- tpl_facade(r_caption_pub)
        # Observation-count labels (see constructor args). The browser does
        # the counting per label group.
        r_count_on <- shiny::reactiveVal(show_n)
        # Missing-key handling.
        r_na_group <- shiny::reactiveVal(missing)
        r_pct_of_pub <- shiny::reactiveVal(pct_of)
        r_pct_of <- chart_facade(
          function() chart_pct_of_internal(r_pct_of_pub()),
          function(v) r_pct_of_pub(chart_pct_of_public(v))
        )
        # Facet-grid panel scales and panels per row (see constructor args).
        r_facet_scales <- shiny::reactiveVal(facet_scales)
        r_facet_cols <- shiny::reactiveVal(facet_cols)
        r_download <- shiny::reactiveVal(isTRUE(download))
        # Prepare script. Externally controllable, so a write can arrive from
        # MCP or a restore as a vector of lines rather than one string;
        # cb_script_text() collapses it at the one place every write passes
        # through (the same shape blockr.extra's code block uses).
        r_script <- shiny::reactiveVal(cb_script_text(script))
        r_values <- shiny::reactiveVal(script_values)
        r_board_theme <- setup_drilldown_theme_sync(session)
        # Board scale map (NULL when the board has no "scale_map" option);
        # resolved per data push, never stored in block state.
        r_scale_map <- dd_board_scale_map()

        # Column metadata (computed once when data changes); see dd_col_meta().
        # Kept while the frame is identical: a panel visit hands over an
        # equal frame, and this walks every column (see last_value_cache()).
        col_meta_cache <- last_value_cache()
        r_col_meta <- shiny::reactive({
          d <- plain_data()
          shiny::req(is.data.frame(d))
          col_meta_cache(d, function() dd_col_meta(d))
        })

        # Columns needed by the chart (reactive -- changes when config
        # changes). drill and label are included so the browser has the
        # columns a click filters on / a mark is labelled by.
        # Columns the current mapping needs (so the whole wide flatten is
        # never shipped). Reads the config reactiveVals, so the enclosing
        # reactive (r_needed_cols below) invalidates when the mapping
        # changes -- same set as before the payload-cache refactor.
        needed_cols <- function(d) {
          # as.character(unlist(...)) so a config arg that arrives as an
          # empty list() -- e.g. a NULL aesthetic corrupted by DAG copy/paste
          # into `list()` -- does not coerce the whole vector to a list and
          # crash `d[, needed]` ("must be ... not a list").
          needed <- as.character(unlist(c(
            r_group(), r_color(), r_facet(), r_value(),
            r_x(), r_y(), r_xend(), r_series(),
            r_label(), r_tt_fields(), r_drill(), r_lo(), r_hi(),
            r_linetype(),
            # Count-label id column: not a mapped aesthetic, but the browser
            # needs its values to count distinct ids per label group.
            r_count_col(),
            # Band: the subject id backs the drill on an outlier point, and
            # the reference columns are reduced R-side but shipped so the
            # tooltip can quote them.
            r_band_id(), r_ref_hi(), r_ref_lo()
          )))
          sb <- as.character(unlist(r_sort_by()))
          if (length(sb) && !sb %in% c("onset", "alpha")) {
            needed <- c(needed, sb)
          }
          if (!is.data.frame(d)) return(character())
          if (identical(r_x(), "AVISIT") && "AVISITN" %in% names(d)) {
            needed <- c(needed, "AVISITN")
          }
          # Columns a READER can switch a role to, shipped up front.
          #
          # Only the columns the current mapping needs travel, so re-pointing
          # a role from the sentence would otherwise need a fresh
          # payload -- and until it lands the browser draws the new mapping
          # over rows without that column, which reads as the aesthetic
          # switching off and back on. Shipping the offer list with the data
          # makes the switch immediate.
          #
          # Bounded by the marks: this only fires when the frame carries
          # kinds (mark_column_kinds()), which is a curated set an author
          # chose -- on a clinical lab chain, ~16 columns of 40. An unmarked
          # frame offers EVERY column, so nothing extra is shipped there and
          # the client falls back to holding the last picture until the data
          # catches up.
          # A sentence can re-point any role it names, so the kinds every
          # role could offer have to travel with the data.
          if (length(r_subtitle()) && grepl("@", r_subtitle(), fixed = TRUE)) {
            needed <- c(needed, names(column_kinds(d)))
          }
          # Columns a sentence lists for a slot (`{@value in AVAL, CHG}`)
          # travel for the same reason, whatever their kind.
          needed <- c(needed, unlist(
            template_slot_choices(list(r_title(), r_subtitle(), r_caption())),
            use.names = FALSE
          ))
          needed <- unique(needed)
          needed[!is.null(needed) & needed != "" &
            needed != ".count" & needed %in% names(d)]
        }

        # Columns the current mapping ships, as their own reactive so the
        # JSON cache below only invalidates when the shipped-column set can
        # change (data or a mapping role), never on a presentation-only
        # edit (sort_dir, orientation, bar_mode, ...).
        r_needed_cols <- shiny::reactive({
          needed_cols(plain_data())
        })

        # Serialized data payload, cached. jsonlite::toJSON over all rows is
        # the expensive part of every push, and it depends only on the data
        # and the shipped columns -- so a presentation-only gear edit
        # re-sends the SAME payload instead of re-serializing the frame.
        # `rev` ticks on a change of CONTENT, not on a recompute: shiny
        # splices the json-classed string verbatim into the websocket
        # message (json_verbatim), so the browser receives a parsed OBJECT
        # and cannot use string identity -- an unchanged rev is its signal
        # to skip the row conversion (see setData in chart/view.js). Ticking
        # per recompute defeated that. Visiting a dock panel re-evaluates the
        # block, and an equal-but-new result invalidates this reactive, so
        # coming back to a chart shipped a fresh rev for bytes the browser
        # already held and made it rebuild every row object. Read
        # exclusively inside the gated observer below: a reactive is
        # pull-based, so it stays suspended with the observer for hidden
        # panels.
        # The shipped columns are the cache key: a panel visit (an equal
        # frame) or an edit that re-reads needed_cols() without changing the
        # set (subtitle, sort_by) returns the last payload without
        # serializing anything. The columns of `df_send` are the frame's own
        # vectors, so on an unchanged frame the comparison is by pointer.
        payload_rev <- 0L
        last_payload <- NULL
        payload_cache <- last_value_cache()
        r_data_json <- shiny::reactive({
          d <- plain_data()
          shiny::req(is.data.frame(d))
          needed <- r_needed_cols()
          df_send <- if (length(needed)) {
            d[, needed, drop = FALSE]
          } else {
            d[0]
          }
          payload_cache(df_send, function() {
            # Dictionary-encodes the low-cardinality string columns; see
            # chart-payload.R. Everything else is serialized exactly as it was.
            json <- chart_data_json(df_send)
            if (identical(json, last_payload$json)) {
              return(last_payload)
            }
            payload_rev <<- payload_rev + 1L
            last_payload <<- list(rev = payload_rev, json = json)
            last_payload
          })
        })

        # Distribution band, cached on exactly compute_band_series()'s
        # inputs. Same reasoning as the smoother below: the windowed pass is
        # the expensive part, so a presentation-only gear edit (title,
        # dot size, ...) must not re-run it. The early return keeps the
        # reactive off the data dependency entirely for every other chart
        # type, so nothing changes for existing charts.
        # A configured column name only counts when it is actually in the
        # data: a stale mapping (upstream filter dropped the column, a
        # restored board points at a renamed one) must degrade to "off"
        # rather than error out of the whole chart.
        col_in <- function(d, v) {
          if (!is.null(v) && length(v) && nzchar(v) && v %in% names(d)) v
          else NULL
        }
        # Keyed on the columns it reads and its settings, so a panel visit
        # does not re-run the windowed pass. Serialized here, once per
        # change: the band carries every outlier point, and Shiny would
        # otherwise serialize it again on every push.
        band_cache <- last_value_cache()
        r_band_series <- shiny::reactive({
          if (!identical(r_chart_type(), "band")) return(NULL)
          d <- plain_data()
          shiny::req(is.data.frame(d))
          args <- list(
            x_col = r_x(), y_col = r_y(), color_by = r_color(),
            series_by = r_series(), facet_by = col_in(d, r_facet()),
            summary = r_summary() %||% "median_q1_q3",
            whiskers = r_whiskers() %||% "p10_p90",
            window = r_band_window() %||% "adaptive",
            window_size = r_band_size() %||% 45,
            min_n = r_band_min_n() %||% 12,
            id_col = col_in(d, r_band_id())
          )
          cols <- intersect(as.character(unlist(args[c(
            "x_col", "y_col", "color_by", "series_by", "facet_by", "id_col"
          )])), names(d))
          band_cache(list(d[cols], args), function() {
            tryCatch(
              chart_config_json(do.call(compute_band_series,
                                        c(list(d), args))),
              error = function(e) {
                warning("drilldown_chart band computation failed: ",
                        conditionMessage(e), call. = FALSE)
                NULL
              }
            )
          })
        })

        r_band_note <- shiny::reactive({
          if (!identical(r_chart_type(), "band")) return(NULL)
          if (!is.null(r_band_series())) return(NULL)
          d <- plain_data()
          shiny::req(is.data.frame(d))
          band_empty_reason(d, r_x(), r_y(),
                            min_n = r_band_min_n() %||% 12,
                            id_col = col_in(d, r_band_id()))
        })

        # Reference lines reduced from their columns (see band_reference).
        r_band_refs <- shiny::reactive({
          if (!identical(r_chart_type(), "band")) return(NULL)
          d <- plain_data()
          shiny::req(is.data.frame(d))
          refs <- list(hi = band_reference(d, col_in(d, r_ref_hi())),
                       lo = band_reference(d, col_in(d, r_ref_lo())))
          refs <- refs[!vapply(refs, is.null, logical(1L))]
          if (length(refs) == 0L) NULL else refs
        })

        # Smoother overlay, cached on exactly compute_smoother_series()'s
        # inputs: the smoother kind, the data, and the x/y/color/series/facet
        # roles. A loess fit (surface = "direct") is O(n^2), so any other
        # gear edit must reuse the cached fit. With smoother "none" the
        # early return keeps the reactive off the data dependency entirely.
        # facet_by is not decoration: without it every panel was handed the
        # same pooled fit (see compute_smoother_series()).
        # Keyed like the band above, so a panel visit does not refit. Only a
        # scatter draws the line (chart/option-points.js), so no other type
        # pays for a fit.
        smoother_cache <- last_value_cache()
        r_smoother_series <- shiny::reactive({
          sm <- r_smoother()
          if (is.null(sm) || identical(sm, "none")) return(NULL)
          if (!identical(r_chart_type(), "scatter")) return(NULL)
          d <- plain_data()
          if (!is.data.frame(d)) return(NULL)
          args <- list(
            smoother = sm, x_col = r_x(), y_col = r_y(), color_by = r_color(),
            series_by = r_series(), facet_by = col_in(d, r_facet())
          )
          cols <- intersect(as.character(unlist(args[c(
            "x_col", "y_col", "color_by", "series_by", "facet_by"
          )])), names(d))
          smoother_cache(list(d[cols], args), function() {
            tryCatch(do.call(compute_smoother_series, c(list(d), args)),
                     error = function(e) {
              # Keep the NULL fallback (no overlay) but surface the failure
              # so a broken smoother fit is diagnosable instead of silent.
              warning("drilldown_chart smoother computation failed: ",
                      conditionMessage(e), call. = FALSE)
              NULL
            })
          })
        })

        # Resolved chart text, cached on exactly its inputs: the three
        # template states, the data, and the data-driven auto tier. Token
        # resolution walks whole columns (unique over as.character), so it
        # must not re-run on every unrelated gear edit -- the push observer
        # below fires per edit, and an uncached call there re-paid 3 column
        # scans per sort/step/width tweak. Same pattern as r_data_json /
        # r_smoother_series above.
        # The block's own arguments, reachable from a title template as
        # `{@color}` and friends. A word printed from one of these is a slot:
        # the face renders it as a control and clicking it opens that
        # argument's picker. See blockr.docs design-system/pinned-controls.md.
        title_args <- shiny::reactive({
          roles <- list(
            chart_type = r_chart_type(), group = r_group(), value = r_value(),
            func = r_func(), x = r_x(), y = r_y(), xend = r_xend(),
            series = r_series(), color = r_color(), facet = r_facet(),
            label = r_label(), sort_by = r_sort_by(), drill = r_drill(),
            # Not a mapping, but a setting a reader may want on the face: a
            # 12-panel grid is the one chart where the shape of the page is
            # the reader's call, not the author's.
            facet_cols = r_facet_cols(),
            # Presentation choices a reader may switch from the sentence
            # ("{@position}", "{@smoother in none, lm}").
            bar_mode = r_bar_mode(), smoother = r_smoother(),
            facet_scales = r_facet_scales(), bins = r_bins(),
            # Reference lines, typed in place on the word
            # ("[, target {@value_lines}]").
            value_lines = num_list_text(r_value_lines()),
            x_lines = num_list_text(r_x_lines())
          )
          # The prepare script's declared values are settings too, named in a
          # template by the variable the script uses: `{@param}` for
          # `param <- factor(...)`. See script_title_args().
          script_title_args(roles, r_specs(), r_values())
        })

        r_titles_resolved <- shiny::reactive({
          d <- plain_data()
          shiny::req(is.data.frame(d))
          auto <- r_data_titles()
          a <- title_args()
          # The subtitle is the block's sentence, so its pieces are read
          # twice: the words, and the settings whose clause dropped. One call.
          sub_parts <- block_title_parts(r_subtitle(), d, auto = auto$subtitle,
                                         args = a)
          title_parts <- block_title_parts(r_title(), d, auto = auto$label,
                                           args = a)
          caption_parts <- block_title_parts(r_caption(), d,
                                             auto = auto$caption, args = a)
          list(
            title = resolve_block_title(r_title(), d, auto = auto$label,
                                        args = a),
            subtitle = resolve_block_title(r_subtitle(), d,
                                           auto = auto$subtitle, args = a),
            caption = resolve_block_title(r_caption(), d,
                                          auto = auto$caption, args = a),
            title_parts = title_parts,
            subtitle_parts = sub_parts,
            caption_parts = caption_parts,
            # Settings the sentence names in a clause that dropped, so the
            # face can offer them: an unset facet has no word to click. Per
            # band, because the offer belongs where the word was -- a flag
            # switched off in the caption (`[{@scheduled} visits only. ]`)
            # takes its own word away, and without a chip beside the caption
            # there is nothing left to switch it back on with.
            subtitle_offers = attr(sub_parts, "offers", exact = TRUE),
            title_offers = attr(title_parts, "offers", exact = TRUE),
            caption_offers = attr(caption_parts, "offers", exact = TRUE),
            # Every setting the block's text speaks for, across all three
            # bands and including the ones only on offer. A prepare script's
            # declared value gets a control in the strip above the chart
            # BECAUSE it has nowhere else to go; once the sentence names it,
            # that control is a second live copy of one setting on one face,
            # which is a state you cannot explain to the person looking at it.
            sentence_args = sentence_args(
              title_parts, sub_parts, caption_parts
            ),
            # What `{@...}` can name, and what each one would print right now.
            # The gear's insert menu is built from this, so it cannot offer a
            # token the resolver has no answer for, and its value column comes
            # from the same function that prints the word. A named list, so it
            # arrives as an object rather than as a vector jsonlite might
            # unbox.
            title_arg_values = lapply(
              stats::setNames(nm = names(a)), arg_token_value, args = a
            ),
            # The choices a sentence lists for its words (`{@func in mean,
            # median}`), keyed by argument. Each as a list so one choice
            # still arrives as an array.
            slot_choices = lapply(
              template_slot_choices(list(r_title(), r_subtitle(), r_caption())),
              as.list
            )
          )
        })

        # Push data + config to JS. Single observe, exactly as before the
        # roles refactor: this shape is what the blockr.dock lazy-eval
        # card-probe pairing correctly suspends for hidden panels, so
        # off-view charts do not render. (The earlier observeEvent +
        # separate config channel rewrite escaped that gating and was the
        # AE-tab freeze.) Only the columns the current mapping needs are
        # shipped, never the whole wide flatten. The expensive pieces
        # (toJSON, smoother fit) live in the cached reactives above -- read
        # only from in here, so they inherit this observer's suspension.
        # Last message pushed, always WITH its rows: the `_need` reply and
        # the capture service both send it whole. `sent_rev` is the data_rev
        # of the last message that carried rows, i.e. the rows the client
        # holds.
        last_push <- new.env(parent = emptyenv())
        last_push$msg <- NULL
        last_push$sent_rev <- NULL

        # The payload the browser draws from, built on demand. It used to be
        # assembled inline in the observer below, which meant the ONLY copy
        # of it was whatever had last been pushed -- and a chart whose panel
        # nobody ever opened has never pushed, so an export could not draw
        # it. Now the capture service builds one when it needs one.
        # The board scale map resolved for the coloured column, serialized.
        # With a patient id coloured that is one hash per level and a 600 kB
        # entry (22k patients: 0.13 s to resolve, 0.27 s for Shiny to
        # serialize, on every push), so it is kept while the map, the column
        # and the palette are the same.
        scales_cache <- last_value_cache()
        r_scales <- function(d) {
          map <- r_scale_map()
          var <- dd_colored_var(r_chart_type(), r_color(), r_group())
          col <- if (!is.null(var) && var %in% names(d)) d[[var]]
          scales_cache(list(map, var, col, dd_palette()), function() {
            chart_config_json(dd_scales_config(
              map, r_chart_type(), color = r_color(), group = r_group(),
              data = d
            ))
          })
        }

        build_chart_msg <- function() {
          d <- plain_data()
          # No nrow gate: an upstream filter emptying the frame MUST push,
          # or the browser keeps rendering (clickable) stale marks over data
          # that no longer exists while the block's output is already the
          # empty frame. JS renders its own empty state for 0 rows.
          shiny::req(is.data.frame(d))
          # Reuse the column metadata reactive (same name/type/n_unique/label
          # shape) instead of recomputing it inline -- it is derived from the
          # same data() and was duplicated here.
          col_meta <- r_col_meta()
          payload <- r_data_json()
          # An OPTIONAL aesthetic mapped to a column not present in the current
          # data is sent as unmapped, so the chart draws no legend / no facet
          # strip instead of a single phantom "undefined" group. This is what
          # makes an upstream picker's "(none)" (which emits no such column)
          # actually turn the aesthetic off -- the picker changes data, never
          # this block's config, so the config must self-heal against the data.
          # It also drops phantom legends from a color/facet whose column was
          # removed upstream or typo'd. Mirrors needed_cols() and the scale-map
          # presence guard; REQUIRED roles (group/x/y/value) are left untouched
          # -- their absence is a broken chart, not an intentional "off".
          present_role <- function(v) {
            if (!is.null(v) && length(v) && nzchar(v) && v %in% names(d)) {
              v
            } else {
              NULL
            }
          }
          sel <- shiny::isolate(list(
            type = r_filter_type(), filters = r_filters(),
            range = r_filter_range()
          ))
          sel_one <- chart_filters_one_column(sel$filters, sel$type)
          chart_msg <- list(
            id = ns("drilldown_block"),
            columns = col_meta,
            data = payload$json,
            data_rev = payload$rev,
            config = list(
              group = r_group(),
              color = present_role(r_color()),
              facet = present_role(r_facet()),
              value = r_value(),
              func = if (is_hist()) "count" else r_func(),
              chart_type = r_chart_type(), x = r_x(), y = r_y(),
              # A number of decimals travels as text, the gear's value.
              bins = r_bins(), format = as.character(r_format()),
              unit = r_unit(),
              xend = r_xend(), series = present_role(r_series()),
              label = r_label(),
              # Extra tooltip columns. as.list() keeps a length-1 vector a JSON
              # array; NULL when empty so the JS "+" role stays hidden until the
              # user adds it (an empty [] would read as "present").
              tt_fields = if (length(r_tt_fields())) as.list(r_tt_fields()) else NULL,
              drill = r_drill(),
              # A histogram's bins are in data order, left to right.
              sort_by = if (is_hist()) "data" else r_sort_by(),
              # Click-filter state, ISOLATED: reading it reactively would
              # re-pump the whole data frame on every click (the render-storm
              # guard above). Only a genuine re-send (restore at session
              # start, config/data change) carries it -- exactly what the JS
              # restore branch needs to re-select the mark and label the
              # footer. The chart reads `filters` (and a range with the
              # panel it was taken in); the same filter also goes in the
              # older one-column form when it fits one. as.list() so a
              # length-1 value stays a JSON array.
              filter_type = sel$type,
              filters = chart_filters_json(sel$filters),
              filter_range = if (identical(sel$type, "range")) sel$range,
              filter_column = sel_one$column,
              filter_values = as.list(sel_one$values),
              sort_dir = if (is_hist()) "asc" else r_sort_dir(),
              orientation = r_orientation(),
              bar_mode = r_bar_mode(),
              # Logical in state, "on"/"off" for the gear's switch, like
              # identity_line below.
              value_labels = if (isTRUE(r_value_labels())) "on" else "off",
              line_width_mult = r_line_width_mult(),
              dot_size_mult = r_dot_size_mult(), connect = r_connect(),
              value_lines = as.list(unname(r_value_lines())),
              x_lines = as.list(unname(r_x_lines())),
              value_line_names = as.list(names(r_value_lines())),
              x_line_names = as.list(names(r_x_lines())),
              smoother = r_smoother(),
              # The gear's segmented control speaks "on"/"off"; the R state is
              # a logical (bool_state). Convert on the way OUT so the control
              # shows the right pill, and back again in the observer below.
              identity_line = if (isTRUE(r_identity_line())) "on" else "off",
              box_points = r_box_points(),
              # Distribution statistics: NULL summary = per-mark default
              # (resolved JS-side); connect_centers speaks the segmented
              # control's "on"/"off", like identity_line above.
              summary = r_summary(),
              # Send the RESOLVED whisker rule, not the NULL. The gear's select
              # falls back to its first option when the value is null, so an
              # unset band would have shown "tukey" while R computed p10_p90 --
              # a gear disagreeing with the canvas is worse than either alone.
              # Same idea as _ensureDistributionMetric resolving `summary` into
              # the config JS-side.
              # Only for the types that draw whiskers: a resolved value on
              # any other type would come back as a pick after a switch.
              whiskers = r_whiskers() %||% switch(r_chart_type() %||% "",
                band = "p10_p90", boxplot = "tukey", NULL),
              connect_centers = if (isTRUE(r_connect_centers())) "on" else "off",
              # Distribution band. `band_series` carries the computed band
              # itself (the JS side draws it, it does not derive it -- same
              # split as `smoother_series` above); the four settings ride
              # along so the gear controls show the right values.
              band_window = r_band_window(),
              band_size = r_band_size(),
              band_min_n = r_band_min_n(),
              band_id = present_role(r_band_id()),
              ref_hi = present_role(r_ref_hi()),
              ref_lo = present_role(r_ref_lo()),
              band_series = r_band_series(),
              band_refs = r_band_refs(),
              band_note = r_band_note(),
              # Observation-count labels: which surface(s) get the "(n)" and the
              # DISTINCT id column to count (browser-side, per label group).
              count_on = r_count_on(), count_col = r_count_col(),
              # Missing group keys and the percent denominator, read by
              # chart/model-aggregated.js.
              na_group = r_na_group(), pct_of = r_pct_of(),
              # Facet-grid panel scales: shared numeric domain + shared
              # category set across the panels ("fixed"), or per-panel
              # ("free" / "free_y").
              facet_scales = r_facet_scales(),
              # Panels per row: null = auto (the stylesheet's auto-fit grid).
              facet_cols = r_facet_cols(),
              # Downloads: the gear's toggle, and what decides whether the
              # hoisted control renders at all.
              download = if (isTRUE(r_download())) "on" else "off",
              # Opening the download menu makes the canvas compose itself
              # and send the bitmap up; the files carry that
              # (R/chart-capture.R).
              capture_export = TRUE,
              capture_ratio = canvas_capture_ratio(),
              # Bar baseline mode. chart_type "waterfall" implies "cumulative"
              # on the JS side (sugar); also send the flag explicitly so a plain
              # bar can opt into the cumulative bridge, and pass the optional
              # total-bar step names. as.list() so a length-1 char vector still
              # serializes as a JSON array.
              baseline = r_baseline(),
              waterfall_totals = as.list(r_waterfall_totals()),
              # Chart text. Raw state feeds the gear's inputs (the template
              # the user typed, or null = auto / "" = none); *_resolved is
              # what the browser renders (tokens substituted against the
              # current data, auto tier applied -- title-template.R).
              # The templates as written: the gear edits these.
              title = r_title_pub(),
              subtitle = r_subtitle_pub(),
              caption = r_caption_pub(),
              title_resolved = r_titles_resolved()$title,
              subtitle_resolved = r_titles_resolved()$subtitle,
              caption_resolved = r_titles_resolved()$caption,
              # The same three, as pieces, so the face can draw the words a
              # reader may set as controls. Absent when the text holds no
              # `@arg` token, which is every board written before this.
              title_parts = r_titles_resolved()$title_parts,
              subtitle_parts = r_titles_resolved()$subtitle_parts,
              caption_parts = r_titles_resolved()$caption_parts,
              subtitle_offers = as.list(r_titles_resolved()$subtitle_offers),
              title_offers = as.list(r_titles_resolved()$title_offers),
              caption_offers = as.list(r_titles_resolved()$caption_offers),
              sentence_args = as.list(r_titles_resolved()$sentence_args),
              # What `{@...}` can name, and what each one prints right now.
              # The gear's `@` menu is built from this, so it cannot offer a
              # token the resolver has no answer for.
              title_arg_values = r_titles_resolved()$title_arg_values,
              slot_choices = r_titles_resolved()$slot_choices,
              smoother_series = r_smoother_series(),
              # as.list(): a pair of intervals stays a JSON array.
              lo = as.list(r_lo()), hi = as.list(r_hi()),
              linetype = present_role(r_linetype()),
              x_ranges = chart_ranges_json(r_x_ranges()),
              legend = r_legend(),
              # Board scale map, resolved for the chart type's colored role
              # (NULL when no map / no binding / no colored role -- JS then
              # keeps palette cycling).
              scales = r_scales(d),
              # The series pool, resolved through the board theme. Rides the
              # config rather than its own custom message: an unhandled
              # message is dropped silently, and this one has to arrive before
              # the first render. JS keeps its own copy as the fallback, so an
              # older R side that omits the field still draws.
              palette = as.list(dd_palette()),
              # External-control send (beta): current target/table + the
              # board's candidate targets for the gear's picker. The choices
              # reactiveVal skips identical sets, so this re-pumps only when
              # value filter blocks come, go or get renamed.
              ctrl_target = r_ctrl_target(),
              ctrl_table = r_ctrl_table(),
              ctrl_choices = dd_ctrl_choices_list(r_ctrl_choices()),
              # Mapping roles the face carries. as.list() so a single role
              # ships as a JSON array rather than a bare string (auto_unbox).
              # Prepare script: the text the gear's textarea edits, the
              # controls it declares (in the engine's own role vocabulary, so
              # the band draws them with the same _buildControl() a mapping row
              # uses), and why a knob is missing when one is.
              script = r_script(),
              script_inputs = dd_script_roles(r_specs()),
              # NB: the `sv_*` value entries are spliced onto this list below,
              # since their names are only known once the script is parsed.
              script_error = r_prepared()$error
            )
            # NB: the registry _arguments() prose is intentionally NOT sent to
            # the browser. LLM prompts live in the registry only; popover help
            # is a UI-layer concern (terse labels + the live `name (label)`
            # convention). See blockr.design/open/block-config-ui.
          )
          # Script control values, keyed by declared name. Spliced rather than
          # written inline: the keys come out of the script, so they cannot be
          # part of the literal above.
          chart_msg$config <- c(chart_msg$config,
                                dd_script_cfg(r_specs(), r_values()))
          chart_msg
        }

        # Rows travel only when the client does not already hold them. A
        # gear edit changes the config and leaves the payload's rev alone, so
        # it goes out without `data` (the rows were 2.7 MB of the 2.7 MB
        # message on a 120k-row bar chart). The chart keeps the rows it has
        # for that rev, or asks for them through `_need` when it has none (a
        # panel the dock re-mounted).
        send_chart_msg <- function(msg) {
          if (identical(msg$data_rev, last_push$sent_rev)) {
            msg$data <- NULL
          } else {
            last_push$sent_rev <- msg$data_rev
          }
          session$sendCustomMessage("drilldown-data", msg)
        }

        # An IDENTICAL message is not sent again. The observer runs whenever
        # anything it reads invalidates, and a dock panel visit invalidates
        # the block's result without changing it, so returning to a chart
        # used to re-ship the whole payload for the picture already on
        # screen (651 kB for the six Vital Signs charts, second pass). A
        # client that never got the first copy is covered by the _ready
        # handshake below.
        shiny::observe({
          # A settings edit reaches here without touching the input, so the
          # cue starts here too (a no-op while one is already running). Any
          # exit that sends no picture ends it.
          busy_cue_start(session, ns("drilldown_block"), "drawing")
          sent <- FALSE
          on.exit(
            if (!sent) busy_cue_done(session, ns("drilldown_block"))
          )
          chart_msg <- build_chart_msg()
          if (identical(chart_msg, last_push$msg)) {
            return()
          }
          last_push$msg <- chart_msg
          send_chart_msg(chart_msg)
          sent <- TRUE
        })

        # The client got a message without rows for a rev it does not hold.
        # Answer with the whole last message, which is at least as new as
        # the one that prompted the request.
        shiny::observeEvent(input$drilldown_block_need, {
          msg <- last_push$msg
          if (is.null(msg)) {
            return()
          }
          last_push$sent_rev <- msg$data_rev
          session$sendCustomMessage("drilldown-data", msg)
        })

        # Register this chart with the session's capture service, so an
        # export can ask for a picture of it whether or not its panel is
        # open. The payload is the one the browser already renders from;
        # the box comes from whoever is exporting (the slide, say).
        # Keyed by the chart's own element id, which is the only thing
        # unique per block here: `id` is the module id every chart block
        # shares ("expr"), so registering under it would leave one entry
        # for the whole board.
        register_chart_capture(ns("drilldown_block"), function(token, width,
                                                               height) {
          # The last payload when there is one, a fresh one when there is
          # not: a chart in a view nobody opened has pushed nothing, and
          # that is exactly the chart an export cannot afford to skip.
          msg <- last_push$msg %||% tryCatch(
            shiny::isolate(build_chart_msg()),
            error = function(e) NULL
          )
          if (is.null(msg)) {
            stop("chart block '", ns("drilldown_block"),
                 "' has no data to draw yet", call. = FALSE)
          }
          session$sendCustomMessage("drilldown-capture", c(
            msg[c("columns", "data", "data_rev", "config")],
            list(req = token, width = width, height = height,
                 ratio = canvas_capture_ratio())
          ))
        })

        # The client announces itself when it binds with no payload waiting.
        # `pendingData` (chart/binding.js) only catches a message that
        # arrived while the SCRIPT was already loaded; Shiny drops a custom
        # message that has no registered handler at all, and the chart
        # scripts only load with the first chart block UI in the page. On a
        # board whose opening view carries no chart -- a config or population
        # view, typical of clinical boards -- every chart's startup payload
        # was dropped and nothing re-sent it, so the chart stayed blank until
        # an unrelated edit re-pumped it.
        # The reply is only the rev. A push already in flight reaches the
        # client before it (one websocket, in order), and a client that got it
        # does nothing; re-sending the whole message here drew the same picture
        # a second time. A client without those rows asks through `_need`.
        shiny::observeEvent(input$drilldown_block_ready, {
          if (!is.null(last_push$msg)) {
            session$sendCustomMessage("drilldown-ready", list(
              id = ns("drilldown_block"),
              data_rev = last_push$msg$data_rev
            ))
          }
        })

        # Send board theme to JS (separate observer so theme changes don't
        # re-pump the data frame)
        shiny::observe({
          session$sendCustomMessage("drilldown-theme", list(
            id = ns("drilldown_block"),
            theme = r_board_theme()
          ))
        })

        # JS -> R: config or filter changes.
        #
        # `_sendConfig()` echoes the FULL config on any popover change,
        # so most fields arrive unchanged every time. A reactiveVal
        # invalidates even when set to an identical value; since the
        # data-send observer transitively depends on these (via
        # r_needed_cols / the config list), a blind set would re-pump the
        # whole (potentially large) data frame and re-render every chart
        # on each echo -- a render storm that hangs heavy views. Only
        # write when the value actually changes. This is the
        # "prevent R->JS->R loops" guard from
        # blockr.docs/patterns/js-driven-blocks.md.
        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }
        nn <- function(v) if (is.null(v) || identical(v, "")) NULL else v
        # OPTIONAL healed roles (color/facet/series): the data push sends
        # them through present_role(), so when the mapped column is absent
        # from the current data JS holds NULL -- and `_sendConfig()` echoes
        # that back as "". Committing that echo would erase the user's pick
        # from state (and from any board saved afterwards) on the first
        # unrelated gear edit. Skip the clear while the stored column is
        # absent (it is the heal's echo, not a user action); a genuine gear
        # clear of a present column still lands.
        upd_healed <- function(rv, msg_val) {
          v <- nn(msg_val)
          if (is.null(v)) {
            cur <- shiny::isolate(rv())
            d <- shiny::isolate(plain_data())
            if (!is.null(cur) && is.data.frame(d) && !(cur %in% names(d))) {
              return(invisible(NULL))
            }
          }
          upd(rv, v)
        }

        shiny::observeEvent(input$drilldown_block_action, {
          msg <- input$drilldown_block_action
          if (is.null(msg)) return()

          action <- msg$action
          if (action == "config") {
            # The roles. The gear sends every role as the browser holds it
            # (internal names: group/value/orientation for an aggregated
            # chart, x/y otherwise); they become the public x / y /
            # orientation together, so no field is read against another's
            # stale value. "Bars: cumulative" on a bar is a waterfall.
            type_new <- msg$chart_type %||% shiny::isolate(r_chart_type())
            if (identical(type_new, "bar") &&
                  identical(msg$baseline, "cumulative")) {
              type_new <- "waterfall"
            }
            func_new <- msg$func %||% shiny::isolate(r_func())
            roles <- chart_roles_public(
              type_new, msg$group, msg$value, msg$x, msg$y, msg$orientation,
              func_new,
              prev = list(x = shiny::isolate(r_x_pub()),
                          y = shiny::isolate(r_y_pub()),
                          orientation = shiny::isolate(r_orientation_pub())),
              is_num = is_num_now
            )
            upd(r_func, func_new)
            upd(r_chart_type, type_new)
            upd(r_x_pub, roles$x)
            upd(r_y_pub, roles$y)
            upd(r_orientation_pub, roles$orientation)
            if (!is.null(msg$color))      upd_healed(r_color, msg$color)
            if (!is.null(msg$facet))      upd_healed(r_facet, msg$facet)
            if (!is.null(msg$xend))       upd(r_xend, nn(msg$xend))
            if (!is.null(msg$series))     upd_healed(r_series, msg$series)
            if (!is.null(msg$label))      upd(r_label, nn(msg$label))
            if (!is.null(msg$tt_fields)) {
              # Arrives as a JSON array (possibly empty). Flatten to a
              # character vector; an empty selection becomes NULL.
              tf <- as.character(unlist(msg$tt_fields))
              tf <- tf[nzchar(tf)]
              upd(r_tt_fields, if (length(tf)) tf else NULL)
            }
            if (!is.null(msg$drill))      upd(r_drill, nn(msg$drill))
            # A histogram sorts its bins itself; the gear's echo of that is
            # not the bar's sort.
            if (!identical(type_new, "histogram")) {
              if (!is.null(msg$sort_by))  upd(r_sort_by, nn(msg$sort_by))
              if (!is.null(msg$sort_dir)) upd(r_sort_dir, msg$sort_dir)
            }
            if (!is.null(msg$format)) {
              v <- chart_format_state(msg$format)
              if (!is.null(v)) upd(r_format, v)
            }
            if (!is.null(msg$unit)) upd(r_unit, trimws(as.character(msg$unit)))
            if (!is.null(msg$bins)) {
              v <- suppressWarnings(as.numeric(msg$bins))
              if (isTRUE(v >= 1)) upd(r_bins, v)
            }
            if (!is.null(msg$bar_mode))   upd(r_bar_mode, msg$bar_mode)
            # "on"/"off" from the gear -> logical in state (see bool_state).
            if (!is.null(msg$value_labels)) {
              upd(r_value_labels, bool_state(msg$value_labels))
            }
            if (!is.null(msg$connect))    upd(r_connect, msg$connect)
            if (!is.null(msg$smoother))   upd(r_smoother, msg$smoother)
            # "on"/"off" from the gear -> logical in state (see bool_state).
            if (!is.null(msg$identity_line)) {
              upd(r_identity_line, bool_state(msg$identity_line))
            }
            # Helper lines: the numlist control sends a comma-separated string
            # ("2, 5"); num_vec_state parses it and drops non-numeric junk. ""
            # is a real value here (clearing every line), so it must reach the
            # slot as NULL rather than being skipped -- hence no nn() guard.
            if (!is.null(msg$value_lines)) {
              upd(r_value_lines, num_vec_state(msg$value_lines))
            }
            if (!is.null(msg$x_lines)) {
              upd(r_x_lines, num_vec_state(msg$x_lines))
            }
            # Shaded ranges as [{name, from, to}]; [] clears them.
            if (!is.null(msg$x_ranges)) {
              upd(r_x_ranges, chart_ranges_state(msg$x_ranges))
            }
            if (!is.null(msg$box_points)) upd(r_box_points, msg$box_points)
            if (!is.null(msg$summary))    upd(r_summary, msg$summary)
            if (!is.null(msg$whiskers))   upd(r_whiskers, msg$whiskers)
            # "on"/"off" from the gear -> logical in state (see bool_state).
            if (!is.null(msg$connect_centers)) {
              upd(r_connect_centers, bool_state(msg$connect_centers))
            }
            # Distribution band. band_size / band_min_n arrive as strings
            # from a number control; a non-numeric edit must not poison the
            # window, so a failed parse keeps the current value.
            if (!is.null(msg$band_window)) {
              upd(r_band_window, msg$band_window)
            }
            if (!is.null(msg$band_size)) {
              v <- suppressWarnings(as.numeric(msg$band_size))
              if (is.finite(v) && v > 0) upd(r_band_size, v)
            }
            if (!is.null(msg$band_min_n)) {
              v <- suppressWarnings(as.numeric(msg$band_min_n))
              if (is.finite(v) && v >= 1) upd(r_band_min_n, v)
            }
            # One id column behind two gear rows (the band's and the N
            # labels'). The gear sends both; the one that differs from the
            # current id is the edit. nn(): "" (picker cleared) means "no id
            # column" (the band and the labels count rows).
            if (!is.null(msg$band_id) || !is.null(msg$count_col)) {
              cur_id <- shiny::isolate(r_id())
              bid <- nn(msg$band_id)
              cid <- nn(msg$count_col)
              # The band row is sent through present_role(): an id column
              # the current data lacks comes back as "", which is not an
              # edit (the upd_healed() rule).
              d_now <- shiny::isolate(plain_data())
              if (is.null(bid) && !is.null(cur_id) && is.data.frame(d_now) &&
                    !(cur_id %in% names(d_now))) {
                bid <- cur_id
              }
              new_id <- if (!is.null(msg$band_id) && !identical(bid, cur_id)) {
                bid
              } else if (!is.null(msg$count_col) && !identical(cid, cur_id)) {
                cid
              } else {
                cur_id
              }
              upd(r_id, new_id)
            }
            if (!is.null(msg$ref_hi))  upd(r_ref_hi, nn(msg$ref_hi))
            if (!is.null(msg$ref_lo))  upd(r_ref_lo, nn(msg$ref_lo))
            # The steps drawn as totals; [] clears them.
            if (!is.null(msg$waterfall_totals)) {
              upd(r_waterfall_totals, chr_vec_state(msg$waterfall_totals))
            }
            if (!is.null(msg$count_on))   upd(r_count_on, msg$count_on)
            if (!is.null(msg$na_group))   upd(r_na_group, msg$na_group)
            if (!is.null(msg$pct_of))     upd(r_pct_of, msg$pct_of)
            if (!is.null(msg$facet_scales)) {
              upd(r_facet_scales, msg$facet_scales)
            }
            # nn(): "" (the select's Auto row) means no pinned row count.
            if (!is.null(msg$facet_cols)) {
              upd(r_facet_cols, facet_cols_state(msg$facet_cols))
            }
            if (!is.null(msg$download)) {
              upd(r_download, identical(as.character(msg$download)[[1L]], "on"))
            }
            if (!is.null(msg$lo)) upd(r_lo, chr_vec_state(nn(msg$lo)))
            if (!is.null(msg$hi)) upd(r_hi, chr_vec_state(nn(msg$hi)))
            if (!is.null(msg$linetype)) upd_healed(r_linetype, msg$linetype)
            if (!is.null(msg$legend)) {
              v <- chr_state(msg$legend)
              if (!is.null(v) && v %in% CHART_LEGENDS) upd(r_legend, v)
            }
            # Chart text: "" is a real value (explicitly no title -- it
            # suppresses the auto/data-label tier), so no nn(). null = auto
            # is preserved because JS sends null, which skips the guard.
            if (!is.null(msg$title))    upd(r_title, title_state(msg$title))
            if (!is.null(msg$subtitle)) upd(r_subtitle, title_state(msg$subtitle))
            if (!is.null(msg$caption))  upd(r_caption, title_state(msg$caption))
            # "" is a real value here (un-targeting the sender), so no nn().
            if (!is.null(msg$ctrl_target)) {
              upd(r_ctrl_target, trimws(as.character(msg$ctrl_target)))
            }
            if (!is.null(msg$ctrl_table)) {
              upd(r_ctrl_table, trimws(as.character(msg$ctrl_table)))
            }
            # The prepare script, committed from the gear's textarea (on blur
            # or the Apply chip, never per keystroke: every write re-runs the
            # script and re-serializes the whole frame for the browser). "" is
            # a real value here, clearing the script, so it must not be
            # confused with an absent key.
            if (!is.null(msg$script)) {
              upd(r_script, cb_script_text(as.character(msg$script)))
            }
            # Script control values. They ride the ordinary config channel
            # under an `sv_` prefix, keyed by declared name, so a script
            # variable called `color` cannot write the chart's colour mapping.
            # Read against the CURRENT specs, which know each declaration's
            # type and drive the coercion back out of the DOM.
            sv <- dd_script_values(msg, r_specs(), r_values())
            if (!identical(sv, r_values())) {
              r_values(sv)
            }
          } else if (action == "set_mults") {
            if (!is.null(msg$line_width_mult)) {
              upd(r_line_width_mult, as.numeric(msg$line_width_mult))
            }
            if (!is.null(msg$dot_size_mult)) {
              upd(r_dot_size_mult, as.numeric(msg$dot_size_mult))
            }
          } else if (action == "filter") {
            # Filter-state writes use the same identical()-guard (`upd`) as
            # the config writes above: the data-send observer transitively
            # depends on the filter reactiveVals via the expr reactive, so a
            # blind set to an unchanged value would invalidate (and re-pump)
            # needlessly on an echoed filter.
            #
            # A categorical filter arrives as `filters` (a named list, a
            # JSON null for a missing value) or as the older `column` +
            # `values`; chart_msg_filters() reads both. A range may carry
            # the facet panel it was taken in as `filters`.
            ft <- msg$filter_type %||% "categorical"

            if (transient_drill() && ft == "categorical") {
              # The event path. Only a real claim lands here: an empty
              # filter (the gear clearing the filter after a mapping change)
              # is inert, because there is no local selection to clear and
              # the target's cohort is not this block's to drop.
              f <- chart_msg_filters(msg)
              if (length(f)) {
                r_drill_claim(list(
                  filters = f,
                  nonce = as.numeric(msg$nonce %||% 0)
                ))
              }
            } else if (ft == "categorical") {
              upd(r_filters, chart_msg_filters(msg))
              upd(r_filter_range, NULL)
              upd(r_filter_point, NULL)
              upd(r_filter_type, "categorical")
            } else if (ft == "point") {
              # Click on a single dot with no drill column: drill to the
              # observation(s) at that exact x/y coordinate.
              if (!is.null(msg$x_col) && !is.null(msg$y_col)) {
                upd(r_filters, NULL)
                upd(r_filter_range, NULL)
                upd(r_filter_point, list(
                  x_col = msg$x_col,
                  y_col = msg$y_col,
                  x_val = msg$x_val,
                  y_val = msg$y_val
                ))
                upd(r_filter_type, "point")
              }
            } else if (ft == "range") {
              if (!is.null(msg$x_col) && !is.null(msg$x_range)) {
                xr <- as.numeric(unlist(msg$x_range))
                yr <- if (!is.null(msg$y_range)) as.numeric(unlist(msg$y_range))
                # A degenerate range (xlo == xhi) is a point click: a
                # one-point selection, the observation via between(x, v, v).
                upd(r_filters, chart_filters_state(msg$filters))
                upd(r_filter_point, NULL)
                upd(r_filter_range, list(
                  x_col = msg$x_col,
                  y_col = msg$y_col,
                  x_range = xr,
                  y_range = yr
                ))
                upd(r_filter_type, "range")
              } else {
                upd(r_filters, NULL)
                upd(r_filter_range, NULL)
                upd(r_filter_point, NULL)
                upd(r_filter_type, "categorical")
              }
            }
          }
        })

        # What the drill claims: only a CATEGORICAL click is a claim (one
        # value is a decision). Range / point / brush selections are
        # deliberately not claims -- same rule as drill_claim_columns(). The
        # filter state records the columns actually drilled at click time
        # (JS resolves drill = "auto" there), so no user-facing claim field.
        # Several columns AND together; a column whose value is missing
        # narrows the rows the others are claimed from, but is not claimed
        # itself (a missing value is no value to filter a target on).
        r_ctrl_claims <- shiny::reactive({
          # The unexpanded frame: a claim names the rows upstream holds, and
          # dd_ctrl_claims() maps a group name to its members itself.
          d <- tryCatch(prepared_data(), error = function(e) NULL)
          claim <- r_drill_claim()

          # Transient mode: the claim is the last click, and no click yet is a
          # HOLD (NULL), not an un-drill. `list()` -- the one value that clears
          # the target -- is unreachable from here by design.
          if (transient_drill()) {
            if (is.null(claim)) {
              return(NULL)
            }
            return(dd_ctrl_claims(d, r_ctrl_table(), claim$filters,
                                 subject = dd_ctrl_subject(session)))
          }

          filters <- if (identical(r_filter_type(), "categorical")) r_filters()
          dd_ctrl_claims(d, r_ctrl_table(), filters %||% list(),
                         subject = dd_ctrl_subject(session))
        })

        dd_ctrl_sender(
          r_ctrl_target,
          r_ctrl_claims,
          # Startup suppression, over the LATCHED state -- which transient
          # mode never writes, so the latch would stay pristine forever and
          # swallow every send. A transient claim exists only after a real
          # click, so it ends pristine on its own. The latch is still read on
          # every pass: that is what holds the dependency on the filter state
          # (see dd_ctrl_pristine).
          local({
            latch <- dd_ctrl_pristine(
              function() {
                list(r_filter_type(), r_filters(), r_filter_range(),
                     r_filter_point())
              },
              list(selection$type %||% "categorical", selection$filters,
                   selection$range, selection$point)
            )
            function() {
              still <- latch()
              is.null(r_drill_claim()) && still
            }
          }),
          session,
          # Re-clicking the same mark must send again; a board re-evaluation
          # must not. The counter is minted per click in the browser.
          r_nonce = function() {
            claim <- r_drill_claim()
            if (is.null(claim)) NULL else claim$nonce
          }
        )

        # Build filter expression. With expr_type = "bquoted", `.(data)` is
        # substituted by blockr.core with the upstream block's id at eval
        # time -- so the no-filter branch passes the upstream data frame
        # straight through, keeping the lazy eval chain intact.

        # The click/brush filter expr, over `.(data)` as-is. Kept in its own
        # function so the expr reactive below can wrap the data slot in an
        # as_plain_df() coercion for non-data-frame inputs while plain data
        # frames keep this exact (byte-identical) emitted code.
        # A click on a pooled group of an overlap definition filters on the
        # group's members (dd_group_filters_members()).
        filter_members <- dd_group_filters_members(r_filters, raw_data)

        # The click filter's conditions, AND-ed (chart_filter_conds()), and
        # the columns each reads; a range ANDs with the facet panel it was
        # taken in. No conditions is `filter(.(data), TRUE)`.
        filter_parts <- function() {
          ft <- r_filter_type()
          filters <- r_filters()
          # A click on a histogram's bins is a range on `x`: the bin column
          # exists only in what the chart draws, not in the data it passes on.
          bin_cond <- NULL
          bcol <- if (is_hist()) chart_bin_col(r_x_pub())
          if (length(bcol) && bcol %in% names(filters)) {
            bin_cond <- chart_bin_cond(r_x_pub(), filters[[bcol]],
                                       r_hist_edges())
            filters[[bcol]] <- NULL
          }
          conds <- c(
            if (length(filters)) {
              chart_filter_conds(filters, filter_members(filters))
            },
            if (!is.null(bin_cond)) list(bin_cond)
          )
          # each condition's columns, in the same order: a bin reads `x`
          cols <- c(as.list(names(filters)),
                    if (!is.null(bin_cond)) list(r_x_pub()))
          none <- list(conds = list(), cols = list())
          if (ft == "categorical") {
            list(conds = as.list(conds), cols = cols)
          } else if (ft == "point") {
            pt <- r_filter_point()
            if (is.null(pt) || is.null(pt$x_col) || is.null(pt$y_col)) {
              return(none)
            }
            # A point on a time axis comes back in ms; read it as a date.
            d_now <- tryCatch(raw_data(), error = function(e) NULL)
            xv <- pt$x_val
            if (is.data.frame(d_now) && pt$x_col %in% names(d_now)) {
              xv <- chart_time_literal(xv, d_now[[pt$x_col]])
            }
            cond <- bquote(
              .data[[.(xc)]] == .(xv) & .data[[.(yc)]] == .(yv),
              list(xc = pt$x_col, yc = pt$y_col, xv = xv, yv = pt$y_val)
            )
            list(conds = list(cond), cols = list(c(pt$x_col, pt$y_col)))
          } else if (ft == "range") {
            rng <- r_filter_range()
            rc <- chart_range_cond(
              rng, tryCatch(raw_data(), error = function(e) NULL)
            )
            if (is.null(rc)) {
              return(none)
            }
            rcols <- c(rng$x_col, if (!is.null(rng$y_range)) rng$y_col)
            list(conds = c(list(rc), conds), cols = c(list(rcols), cols))
          } else {
            none
          }
        }

        # --- downloads ------------------------------------------------------
        #
        # The png, web page and PowerPoint downloads carry the chart as the
        # browser drew it. The canvas composes itself and posts the bitmap
        # when the download menu opens, so by the time a format is picked the
        # picture is here. Kept OUT of the reactive graph: it is an artifact
        # of the last render, not state anything should recompute from.
        capture <- shiny::reactiveVal(NULL)

        shiny::observeEvent(input$drilldown_block_capture, {
          msg <- input$drilldown_block_capture
          capture(new_chart_capture(chart_capture_decode(msg$png),
                                    msg$width, msg$height))
        })

        # No picture when a format is clicked: the chart had not drawn when
        # the menu opened, or the bitmap is still on its way. The capture
        # service then draws it offscreen. Its reply cannot land while the
        # download is in flight (Shiny holds inputs back until it returns),
        # so this click says so and the next one finds the picture.
        capture_token <- shiny::reactiveVal(NULL)

        shiny::observe({
          tok <- capture_token()
          shiny::req(tok)
          cap <- tryCatch(chart_capture_collect(tok), error = function(e) {
            capture_token(NULL)
            NULL
          })
          shiny::req(cap)
          capture_token(NULL)
          if (is.null(shiny::isolate(capture()))) {
            capture(cap)
          }
        })

        # With the board's download footer under it (R/download-footer.R).
        dl_chart <- function() {
          cap <- capture()
          if (!is.null(cap)) {
            return(download_footer_stamp(cap, data(), session))
          }
          if (is.null(shiny::isolate(capture_token()))) {
            tok <- tryCatch(
              chart_capture_request(ns("drilldown_block"), 960, 540),
              error = function(e) NULL
            )
            if (is.null(tok)) {
              stop("the chart has no data yet; open its view and try again",
                   call. = FALSE)
            }
            capture_token(tok)
          }
          stop("the chart has not been drawn yet; try again in a moment",
               call. = FALSE)
        }

        dl_formats <- shiny::reactive({
          if (!isTRUE(r_download())) {
            return(list())
          }
          Filter(
            function(s) isTRUE(s$ok),
            list(
              list(id = "dl_xlsx", ext = "xlsx", label = "Excel",
                   ok = requireNamespace("openxlsx", quietly = TRUE)),
              list(id = "dl_html", ext = "html", label = "Web page",
                   ok = TRUE),
              list(id = "dl_pptx", ext = "pptx", label = "PowerPoint",
                   ok = requireNamespace("officer", quietly = TRUE)),
              list(id = "dl_png", ext = "png", label = "Image",
                   ok = TRUE)
            )
          )
        })

        output$chart_download <- shiny::renderUI(
          dl_control_ui(ns, dl_formats())
        )

        # Named for the title the chart shows (see R/download-filename.R).
        dl_name <- function(ext) {
          dl_filename(
            tryCatch(dl_dataset(data()), error = function(e) NULL),
            tryCatch(r_titles_resolved()$title, error = function(e) NULL),
            "chart", ext
          )
        }

        output$dl_xlsx <- shiny::downloadHandler(
          filename = function() dl_name("xlsx"),
          content = function(file) {
            dl_guard("Excel", {
              # The numbers the chart shows: one row per bar (or slice,
              # cell, spoke) for the aggregated types, the rows themselves
              # for the rest.
              d <- chart_xlsx_frame(
                plain_data(), chart_type = r_chart_type(),
                group = r_group(), color = r_color(), facet = r_facet(),
                func = r_func(), value = r_value(), na_group = r_na_group()
              )
              shiny::req(is.data.frame(d))
              auto <- r_data_titles()
              write_annotated_xlsx(
                d, file,
                title = resolve_block_title(r_title(), plain_data(),
                                            auto = auto$label),
                subtitle = resolve_block_title(r_subtitle(), plain_data(),
                                               auto = auto$subtitle),
                # Ends with the board's download footer
                # (R/download-footer.R), as under the chart's pictures.
                caption = download_footer_caption(
                  resolve_block_title(r_caption(), plain_data(),
                                      auto = auto$caption),
                  data(), session
                )
              )
            })
          }
        )
        output$dl_html <- shiny::downloadHandler(
          filename = function() dl_name("html"),
          content = function(file) {
            dl_guard("web page", {
              p <- dl_chart()
              auto <- r_data_titles()
              write_exhibit_html(
                p, file,
                title = resolve_block_title(r_title(), plain_data(),
                                            auto = auto$label),
                subtitle = NULL
              )
            })
          }
        )
        output$dl_pptx <- shiny::downloadHandler(
          filename = function() dl_name("pptx"),
          content = function(file) {
            dl_guard("PowerPoint", {
              p <- dl_chart()
              auto <- r_data_titles()
              write_exhibit_pptx(
                p, file,
                title = resolve_block_title(r_title(), plain_data(),
                                            auto = auto$label)
              )
            })
          }
        )
        output$dl_png <- shiny::downloadHandler(
          filename = function() dl_name("png"),
          content = function(file) {
            dl_guard("image", {
              chart_capture_file(dl_chart(), file)
            })
          }
        )


        # A download link inside a display:none host is a HIDDEN output, and
        # Shiny suspends those: the handler is never registered, so the click
        # fetches /session/<id>/download/<name> and Connect answers 404 "Not
        # Found". The browser saves that response as
        # "<block>-expr-dl_pptx.htm" and reports "Couldn't download".
        #
        # Which is exactly what prod does. It never shows up locally, because
        # there the host is hoisted into the gear header (chart/chrome.js
        # _hoistDownload) before the output would have been suspended, so the
        # registration happens anyway.
        #
        # The UI slot needs it too: a suspended renderUI never draws the link
        # the JS then hoists.
        shiny::outputOptions(output, "chart_download", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "dl_xlsx", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "dl_html", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "dl_pptx", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "dl_png", suspendWhenHidden = FALSE)

        # See the identity guard at the end of the expr reactive below.
        last_expr <- NULL

        list(
          expr = shiny::reactive({
            # The expr's only job is the data transform: the click/brush
            # filter. Aesthetic mappings (group/color/x/y/...) are NOT part of
            # it -- they are presentation config the JS renderer consumes, so a
            # mapped column that was renamed or dropped upstream leaves the
            # filter (and the downstream data) perfectly valid. That is a
            # presentation concern, surfaced by the renderer's own in-canvas
            # message (see chart/model-common.js: "Mapped column not in data
            # ... re-pick it in the gear"), NOT an expr-level failure.
            # Validating aesthetics here would fail a correct expression; a
            # broken *filter* column, by contrast, fails hard on its own when
            # the emitted filter is evaluated (caught by core's
            # capture_conditions("eval")).
            d <- data()
            # Non-data-frame input under the shared contract (a composer
            # table et al.): the emitted code must coerce the same way the
            # renderer does, so downstream receives the same plain frame the
            # chart draws. NULL input (no upstream yet) stays unwrapped.
            coerce <- !is.null(d) && !is.data.frame(d)
            # The prepare script wraps the filter, so the click filter still
            # applies to the SOURCE rows: a mark is identified by an upstream
            # column, which the script is free not to keep. Downstream then
            # receives what the chart draws, filtered. A condition on a column
            # only the script makes is filtered after it (dd_drill_post()).
            parts <- filter_parts()
            sx <- cb_expr(r_parsed(), r_specs(), r_values(), slot = TRUE)
            src <- if (!is.null(sx) && length(parts$conds)) {
              tryCatch(names(raw_data()), error = function(e) NULL)
            }
            post <- dd_drill_post(parts$cols, src)
            ex <- chart_filter_call(parts$conds[!post])
            if (coerce) ex <- wrap_plain_df_input(ex)
            out <- if (is.null(sx)) ex else dd_splice_slot(sx, ex)
            if (any(post)) {
              out <- dd_splice_slot(chart_filter_call(parts$conds[post]), out)
            }
            # Hand back the SAME object when the emitted code has not changed.
            # blockr.core skips a re-evaluation on object identity
            # (same_ref, block-server.R), and this reactive reads `data()`
            # above -- so a view switch, which lands its visibility across
            # several flushes, invalidates it, it re-runs, and bbquote mints a
            # freshly allocated but equal call. That fails same_ref and every
            # re-fronted chart re-evaluates. The calls compared here are plain
            # language and literals (no closures, no environments), so
            # identical() is cheap and has nothing environment-sensitive to
            # walk.
            if (!is.null(last_expr) && identical(out, last_expr)) {
              return(last_expr)
            }
            last_expr <<- out
            out
          }),
          # The public arguments, in the constructor's order. Every entry is
          # a reactiveVal: blockr.core requires that of `external_ctrl`.
          state = list(
            type = r_chart_type,
            x = r_x_pub,
            y = r_y_pub,
            func = r_func,
            color = r_color,
            facet = r_facet,
            series = r_series,
            xend = r_xend,
            label = r_label,
            orientation = r_orientation_pub,
            position = r_position,
            sort_by = r_sort_by,
            sort_dir = r_sort_dir,
            value_labels = r_value_labels,
            format = r_format,
            unit = r_unit,
            tooltip = r_tt_fields,
            drill = r_drill,
            line_width = r_line_width_mult,
            point_size = r_dot_size_mult,
            connect = r_connect,
            y_lines = r_value_lines,
            x_lines = r_x_lines,
            ymin = r_lo,
            ymax = r_hi,
            linetype = r_linetype,
            x_ranges = r_x_ranges,
            legend = r_legend,
            smoother = r_smoother,
            identity_line = r_identity_line,
            box_points = r_box_points,
            summary = r_summary,
            whiskers = r_whiskers,
            connect_centers = r_connect_centers,
            band_window = r_band_window,
            band_size = r_band_size,
            band_min_n = r_band_min_n,
            id = r_id,
            ref_lo = r_ref_lo,
            ref_hi = r_ref_hi,
            waterfall_totals = r_waterfall_totals,
            bins = r_bins,
            show_n = r_count_on,
            missing = r_na_group,
            pct_of = r_pct_of_pub,
            facet_scales = r_facet_scales,
            facet_cols = r_facet_cols,
            title = r_title_pub,
            subtitle = r_subtitle_pub,
            caption = r_caption_pub,
            download = r_download,
            drill_target = r_ctrl_target,
            drill_table = r_ctrl_table,
            # One string, not the code block's array-of-lines: a state entry
            # that reshapes on the way out cannot be a reactiveVal. The ctor
            # and the config handler still accept a character vector and
            # collapse it (cb_script_text).
            script = r_script,
            script_values = r_values,
            selection = r_selection
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        viz_echarts_dep(),
        viz_block_css_dep(),
        drilldown_chart_dep(),
        # The prepare script's control strip. Always in the DOM, empty and
        # display:none until a script declares a value -- the chart fills it.
        # It used to hold promoted mapping rows too (`expose`, retired): a
        # mapping is named in the block's sentence now.
        shiny::div(id = ns("mapping_band"), class = "dd-mapping-band",
                   style = "display:none"),
        shiny::div(
          id = ns("drilldown_block"),
          class = "drilldown-chart-container"
        ),
        # The download control is rendered HERE and hoisted into the gear
        # header by chart/chrome.js -- the same shape summarize-table.js uses
        # for the search box. It has to be a Shiny output (download links are
        # server-driven), and the gear header is built by the widget's JS.
        shiny::div(class = "dd-chart-dl-host", style = "display:none",
                   shiny::uiOutput(ns("chart_download"), inline = TRUE))
      )
    },
    # Shared input contract (see validate_annotated_df_input): a data frame,
    # or a table-producing object coerced via as_annotated_df() -- the chart
    # then charts the coerced frame's data columns (as_plain_df()).
    dat_valid = validate_annotated_df_input,
    # Every optional slot whose default is empty MUST be listed: a field
    # outside this list holding NULL wedges the block (state_ready never goes
    # TRUE and result() stays NULL; reference_blockr_allow_empty_state_wedge).
    # `y` is emptied by the gear mid-config (the aggregation changed and the
    # old column no longer fits). The fixed-option fields (type, func,
    # position, show_n, missing, pct_of, facet_scales, connect, ...) and the
    # logicals are never empty.
    allow_empty_state = c("x", "y", "color", "facet", "series", "xend",
      "label", "orientation", "sort_by", "sort_dir", "tooltip", "drill",
      "y_lines", "x_lines", "ymin", "ymax", "linetype", "x_ranges",
      "smoother", "identity_line", "box_points", "summary", "whiskers", "id", "ref_lo", "ref_hi",
      "waterfall_totals", "facet_cols", "title", "subtitle", "caption",
      "drill_target", "drill_table", "script", "script_values",
      "selection", "unit"),
    # Everything is externally controllable (MCP, restore). `script` is
    # deliberately NOT on the AI surface: it is absent from the registry
    # argument spec, so the assistant never sees it.
    external_ctrl = c("type", "x", "y", "func", "color", "facet", "series",
      "xend", "label", "orientation", "position", "sort_by", "sort_dir",
      "value_labels", "format", "unit", "tooltip", "drill", "line_width",
      "point_size",
      "connect", "y_lines", "x_lines", "ymin", "ymax", "linetype", "x_ranges",
      "legend", "smoother",
      "identity_line", "box_points", "summary", "whiskers",
      "connect_centers", "band_window", "band_size", "band_min_n", "id",
      "ref_lo", "ref_hi", "waterfall_totals", "bins", "show_n", "missing",
      "pct_of",
      "facet_scales", "facet_cols", "title", "subtitle", "caption",
      "drill_target", "drill_table", "script", "script_values", "selection"),
    expr_type = "bquoted",
    class = "chart_block",
    ...
  )

  # Take the old names back OFF the block. They arrive through `...`, which
  # is forwarded above, and blockr.core stores unrecognised dots as block
  # ATTRIBUTES that `blockr_ser.block()` writes back into the payload: a
  # re-save would carry the old names forever, and every open would warn
  # again. Making them formals would serialize them as state instead. Their
  # values were translated at the top of this function.
  for (nm in c(CHART_LEGACY_ARGS, "expose", "vlines", "hlines", "ref_x",
               "ref_y")) {
    attr(res, nm) <- NULL
  }

  res
}

#' Compute smoother line points per group for a scatter chart
#'
#' Returns a named list keyed by group level. Each entry is a list with
#' numeric vectors `x` and `y` covering a 100-point line across the group's
#' x range. Used by the chart's JS-side renderer to draw a regression
#' overlay without doing the math in the browser.
#'
#' With `facet_by` set the result is nested one level deeper, keyed by facet
#' level then by group. **A fit belongs to its panel**: a faceted chart draws
#' one panel per level, so a single fit over the pooled rows is an estimate of
#' a relationship no panel shows — it can carry a slope, an x range, and even a
#' sign that contradict every panel it is drawn into (Simpson's paradox). This
#' mirrors `ggplot2::geom_smooth()` under `facet_wrap()`.
#'
#' @param data Data frame.
#' @param smoother One of `"none"`, `"lm"`, `"loess"`.
#' @param x_col,y_col Numeric column names.
#' @param color_by,series_by Grouping column names; smoother is fit per
#'   `series_by` if non-NULL else `color_by` else no grouping.
#' @param facet_by Facet column name, or `NULL` for an unfaceted chart. When
#'   given, each panel is fit on its own rows and the result is keyed by facet
#'   level first.
#' @param loess_max Most points a loess fit is run on. A loess fit with
#'   `surface = "direct"` costs the square of its points (2 s at 12k rows,
#'   minutes at 120k). A group with more rows is cut, in x order, into
#'   `loess_max` bins of equal count, and the fit runs on each bin's mean x
#'   and mean y with `surface = "interpolate"`. Every row still counts, and
#'   equal counts keep the x density, so the span (a share of the points)
#'   covers the same stretch of x. On 12k noisy rows the line moved by at
#'   most 0.3 where the exact fit's own error was 1.8; keeping every k-th
#'   point instead moved it by 0.9. No randomness, so the same data draws the
#'   same line. A group at or below the limit is fit exactly as before.
#' @return A named list or `NULL`.
#' @examples
#' compute_smoother_series(
#'   mtcars, "lm", "wt", "mpg",
#'   color_by = NULL, series_by = NULL
#' )
#'
#' # one fit per panel, keyed by facet level then group
#' compute_smoother_series(
#'   mtcars, "lm", "wt", "mpg",
#'   color_by = NULL, series_by = NULL, facet_by = "cyl"
#' )
#' @keywords internal
#' @export
compute_smoother_series <- function(data, smoother, x_col, y_col,
                                     color_by, series_by, facet_by = NULL,
                                     loess_max = 5000L) {
  if (is.null(smoother) || identical(smoother, "none")) return(NULL)
  if (is.null(data) || nrow(data) == 0) return(NULL)
  if (is.null(x_col) || is.null(y_col)) return(NULL)
  if (!all(c(x_col, y_col) %in% names(data))) return(NULL)
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) return(NULL)

  split_col <- series_by %||% color_by
  # Only the columns the fits read, so the splits below copy two to four
  # columns rather than the whole frame.
  data <- data[intersect(c(x_col, y_col, split_col, facet_by), names(data))]

  fit_one <- function(d) {
    d <- d[!is.na(d[[x_col]]) & !is.na(d[[y_col]]), , drop = FALSE]
    if (nrow(d) < 3L) return(NULL)
    xv <- d[[x_col]]
    yv <- d[[y_col]]
    if (length(unique(xv)) < 2L) return(NULL)
    rng <- range(xv, na.rm = TRUE)
    xs <- seq(rng[1L], rng[2L], length.out = 100L)
    surface <- "direct"
    at <- xs
    if (identical(smoother, "loess") && length(xv) > loess_max) {
      o <- order(xv)
      bin <- ((seq_along(o) - 1) * loess_max) %/% length(o)
      cnt <- tabulate(bin + 1L)
      xv <- rowsum(xv[o], bin, reorder = FALSE)[, 1L] / cnt
      yv <- rowsum(yv[o], bin, reorder = FALSE)[, 1L] / cnt
      surface <- "interpolate"
      # An interpolated surface is NA outside the fitted x, and the outer
      # bin means sit just inside the data's range. Read the ends at the
      # nearest fitted x so the line still spans the whole range.
      at <- pmin(pmax(xs, min(xv)), max(xv))
    }
    ys <- tryCatch({
      if (identical(smoother, "lm")) {
        coefs <- stats::coef(stats::lm(yv ~ xv))
        coefs[1L] + coefs[2L] * xs
      } else if (identical(smoother, "loess")) {
        fit <- stats::loess(yv ~ xv, span = 0.75,
                            control = stats::loess.control(surface = surface))
        as.numeric(stats::predict(fit, newdata = data.frame(xv = at)))
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (is.null(ys) || all(is.na(ys))) return(NULL)
    list(x = as.list(xs), y = as.list(unname(ys)))
  }

  drop_empty <- function(x) {
    x <- x[!vapply(x, is.null, logical(1L))]
    if (length(x) == 0L) NULL else x
  }

  fit_groups <- function(d) {
    if (nrow(d) == 0L) return(NULL)
    if (!is.null(split_col) && split_col %in% names(d)) {
      groups <- split(d, as.character(d[[split_col]]))
    } else {
      groups <- list(`__all__` = d)
    }
    drop_empty(lapply(groups, fit_one))
  }

  if (is.null(facet_by) || !facet_by %in% names(data)) {
    return(fit_groups(data))
  }

  # One fit per panel. split() drops NA levels, so a panel whose facet value
  # is missing gets no entry and the renderer draws no line there -- better
  # than lending it another panel's fit.
  drop_empty(lapply(split(data, as.character(data[[facet_by]])), fit_groups))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# What the chart's Excel download holds. The aggregated types draw one mark
# per (facet, group, colour) cell, so the sheet carries those cells, built by
# the same engine the table block and the JS golden tests use. Distribution
# marks, points, lines and bars on a timeline draw rows, so the sheet does too.
#' @noRd
chart_xlsx_frame <- function(data, chart_type, group = NULL, color = NULL,
                             facet = NULL, func = "count", value = NULL,
                             na_group = "level") {
  if (!is.data.frame(data)) {
    return(NULL)
  }
  aggregated <- c("bar", "waterfall", "pie", "treemap", "radar")
  keys <- intersect(c(facet, group, color), names(data))
  if (!isTRUE(chart_type %in% aggregated) ||
        !length(intersect(group, names(data)))) {
    return(data)
  }
  func <- func %||% "count"
  summaries <- if (identical(func, "count") || !length(value) ||
                     !value %in% names(data)) {
    list(list(func = "count", cols = character()))
  } else {
    list(list(func = func, cols = value))
  }
  dd_table_aggregate(data, unique(keys), summaries,
                     na_group = na_group %||% "level")$data
}
