#' Drill-Down Chart Block
#'
#' A configurable chart that can also act as a filter. Arguments are
#' grammar-of-graphics aesthetics: each one names a **data column**, never
#' a literal value. Roles are orthogonal -- `color` only colours, `series`
#' only splits into series, `label` only writes on-mark text, position is
#' `x`/`y`/`xend` or `group`.
#'
#' Interactivity is explicit and opt-in via `drill`. A click identifies a
#' mark; the mark maps to one or more source rows; the downstream filter
#' is `drill %in% (distinct values of drill over those rows)`. When
#' `drill` is unset the click is inert (no filter emitted). Brush / drag
#' range selection on scatter/line is a separate gesture and is unchanged.
#'
#' Three chart families share this block (an internal render-dispatch
#' detail that never changes what an argument means):
#' aggregated (bar, waterfall, pie, treemap, boxplot, radar), individual
#' (scatter, line), timeline (gantt). A waterfall is a bar with a cumulative
#' baseline (each bar floats from the running total). A radar puts the `group`
#' levels on the spokes,
#' draws one shape per `color` level, and each vertex is
#' `agg_fn(metric)` for that cell; clicking a shape drills on its `color`
#' value. For spokes from several numeric columns, pivot longer upstream
#' and map the name column to `group`.
#'
#' @param group Column for the categorical axis (aggregated charts)
#' @param color Column mapped to colour (optional, all families)
#' @param facet Column to facet by -- one small panel per level (optional)
#' @param value Column for the value (aggregated only). Must match
#'   `func`: ".count" for `"count"` (row count -- the value is otherwise
#'   ignored), any column for `"count_distinct"` (e.g. a subject id to count
#'   patients instead of records), a numeric column for the numeric
#'   aggregations. (Was `metric`.)
#' @param func Aggregation function: `"count"`, `"count_distinct"`, `"mean"`,
#'   `"median"`, `"sum"`, `"min"`, `"max"`. With a `color` split,
#'   `"count_distinct"` counts an entity once per colour level it appears
#'   under; deduplicate upstream if segments must sum to the per-group
#'   distinct count. (Was `agg_fn`.)
#' @param chart_type Chart type: "bar", "waterfall", "scatter", "line",
#'   "pie", "treemap", "boxplot", "radar", "gantt". "waterfall" is a bar with
#'   a cumulative baseline (sugar for `bar` + `baseline = "cumulative"`).
#' @param x X-axis column (individual / timeline charts)
#' @param y Y-axis column (individual / timeline charts)
#' @param series Column whose distinct values split rows into separate
#'   series (individual: one line/scatter group per value; timeline:
#'   per-bar label). Independent of `color`. High cardinality is fine.
#' @param xend Interval end column (timeline only; NA rows render as
#'   dots at `x`)
#' @param label Column whose value is written on each mark. Default unset
#'   (no on-mark text). For `pie`/`treemap`, when unset the label falls
#'   back to `group` (a label-less pie is unusable).
#' @param tt_fields Character vector of extra column names appended to each
#'   mark's hover tooltip, beyond the mapped roles (gantt). Default `NULL`
#'   (none). Display-only -- does not affect the plot; a listed column dropped
#'   upstream is silently omitted.
#' @param drill Column a click filters downstream on. Default unset (a
#'   click is inert). When set, clicking a mark emits a categorical
#'   filter on this column's value(s) for the clicked mark. For an
#'   aggregated chart this should be a column constant within a group.
#' @param sort_by Category ordering on the axis. Allowed values depend on
#'   the chart family:
#'   * Aggregated: `"value"` (default), `"alpha"`, or a column name.
#'   * Timeline: `"onset"` (default), `"alpha"`, or a column name.
#'   * Individual: ignored.
#' @param sort_dir `"asc"` or `"desc"`. Reverses the `sort_by` ordering.
#' @param orientation Bar orientation: `"horizontal"` (default; category on the
#'   y-axis, best for long labels) or `"vertical"`. Presentation property -- the
#'   mapping (Group/Metric) is unchanged. Bar charts only.
#' @param bar_mode Layout for a color-split bar: `"stacked"` (default -- color
#'   segments stack into one bar per group), `"grouped"` (segments sit
#'   side-by-side / dodged, for comparing absolute values), or `"percent"`
#'   (stacked but each group normalized to 100%, for comparing composition).
#'   No effect without a `color` split; ignored when `baseline = "cumulative"`
#'   (waterfall). Bar charts only.
#' @param filter_type,filter_column,filter_values,filter_range,filter_point
#'   Runtime click/brush filter state (transport for the emitted filter;
#'   normally left at defaults at creation).
#' @param baseline Bar baseline mode: `"zero"` (default -- every bar starts at
#'   0) or `"cumulative"` (a waterfall/bridge -- each bar floats from the
#'   running cumulative of the bars before it; the step axis honors data order,
#'   each `metric` value is a delta). `chart_type = "waterfall"` implies
#'   `"cumulative"`. Bar family only.
#' @param waterfall_totals Character vector of `group` (step) values rendered
#'   as total/subtotal bars in a cumulative-baseline bar: their baseline resets
#'   to 0 and they draw the absolute running cumulative. Default `NULL` (every
#'   bar is a relative delta).
#' @param line_width_mult Multiplier on the default line width for line
#'   charts. Default `1.0`. Range `0.5`-`3.0`. Individual family only.
#' @param dot_size_mult Multiplier on the default marker size. Default
#'   `1.0`. Range `0.5`-`3.0`. Individual family only.
#' @param step Step-line mode for line charts. `NULL` (default) draws a
#'   straight line; a step mode (e.g. `"start"`/`"middle"`/`"end"`) draws a
#'   stepped line. Consumed by the JS renderer.
#' @param ref_x,ref_y Optional reference-line overlays at a fixed x or y
#'   value (vertical / horizontal guide line). Default `NULL` (no overlay).
#' @param smoother Trend overlay for scatter charts: one of `"none"`
#'   (default), `"lm"`, or `"loess"`. Fit per `color`/`series` group via
#'   [compute_smoother_series()].
#' @param identity_line Identity-line overlay for scatter charts: `"off"`
#'   (default) or `"on"` draws a dashed 45-degree y = x guide line across
#'   the overlap of the x and y ranges (shift / agreement plots).
#' @param box_points Observation overlay for boxplots: one of `"none"`
#'   (default, box only), `"outliers"` (plot only the points beyond the
#'   1.5x IQR whiskers) or `"all"` (jittered strip of every observation on
#'   top of the box). No-op for other chart types.
#' @param lo,hi Optional lower / upper value bounds used by the renderer to
#'   clamp or annotate the value axis. Default `NULL` (auto).
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `chart_block`
#' @examplesIf interactive()
#' new_chart_block()
#' @export
new_chart_block <- function(
    group = NULL,
    color = NULL,
    facet = NULL,
    value = ".count",
    func = "count",
    chart_type = "bar",
    x = NULL,
    y = NULL,
    series = NULL,
    xend = NULL,
    label = NULL,
    tt_fields = NULL,
    drill = NULL,
    sort_by = NULL,
    sort_dir = NULL,
    orientation = "horizontal",
    # Color-split bar layout. "stacked" (default) = segments stack into one
    # bar per group; "grouped" = segments sit side-by-side (dodged); "percent"
    # = stacked but each group normalized to 100% (composition view). No-op
    # without a `color` split, and ignored when baseline = "cumulative".
    bar_mode = "stacked",
    # --- Runtime filter transport (NOT creation-time config) -------------
    # These five hold the emitted click/brush filter state. They are set by
    # interaction at runtime, normally left at defaults at creation. They
    # MUST stay in the constructor signature: blockr.core serializes a block
    # from its constructor formals (`block_ctor_inputs`) and restores by
    # re-calling the constructor with those saved values, so dropping them
    # from the signature would break filter-state save/restore. See the
    # `state = list(...)` block below.
    filter_type = "categorical",
    filter_column = NULL,
    filter_values = NULL,
    filter_range = NULL,
    filter_point = NULL,
    # ---------------------------------------------------------------------
    line_width_mult = 1.0,
    dot_size_mult = 1.0,
    step = NULL,
    ref_x = NULL,
    ref_y = NULL,
    smoother = "none",
    identity_line = "off",
    # Boxplot observation overlay: "none" (box only), "outliers" (only the
    # points past the 1.5x IQR whiskers) or "all" (jittered strip of every
    # observation). No-op for non-boxplot charts.
    box_points = "none",
    lo = NULL,
    hi = NULL,
    # Bar baseline mode. "zero" (default) = a normal bar (every bar starts at
    # 0); "cumulative" = a waterfall/bridge (each bar floats from the running
    # cumulative). `chart_type = "waterfall"` is sugar for bar + cumulative.
    # `waterfall_totals` names the steps rendered as total/subtotal bars
    # (baseline reset to 0, drawn as the absolute running cumulative).
    baseline = "zero",
    waterfall_totals = NULL,
    ...) {

  # ARG-RENAME (see dev/unified-arg-naming.md): `metric`/`agg_fn` are the
  # pre-rename names of `value`/`func`. Taken from `...` rather than made
  # formals -- a formal would demand a matching `state` entry and re-serialize
  # under the old name -- so saved boards (which pass the old names on restore)
  # still map onto the new args. Remove after one release cycle.
  .dep <- list(...)
  if (!is.null(.dep$metric)) {
    warning("new_chart_block(): `metric` is deprecated, use `value`.",
            call. = FALSE)
    value <- .dep$metric
  }
  if (!is.null(.dep$agg_fn)) {
    warning("new_chart_block(): `agg_fn` is deprecated, use `func`.",
            call. = FALSE)
    func <- .dep$agg_fn
  }

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Config state (orthogonal aesthetics)
        r_group <- shiny::reactiveVal(group)
        r_color <- shiny::reactiveVal(color)
        r_facet <- shiny::reactiveVal(facet)
        r_value <- shiny::reactiveVal(value)
        r_func <- shiny::reactiveVal(func)
        r_chart_type <- shiny::reactiveVal(chart_type)
        r_x <- shiny::reactiveVal(x)
        r_y <- shiny::reactiveVal(y)
        r_xend <- shiny::reactiveVal(xend)
        r_series <- shiny::reactiveVal(series)
        r_label <- shiny::reactiveVal(label)
        # Extra columns to append to each mark's hover tooltip (gantt). A
        # character vector or NULL. Optional display-only role, so a value
        # dropped upstream is silently omitted (not a hard invalid-state).
        r_tt_fields <- shiny::reactiveVal(tt_fields)
        r_drill <- shiny::reactiveVal(drill)
        r_sort_by <- shiny::reactiveVal(sort_by)
        r_sort_dir <- shiny::reactiveVal(sort_dir)
        r_orientation <- shiny::reactiveVal(orientation)
        r_bar_mode <- shiny::reactiveVal(bar_mode)

        # Filter state (transport for the emitted downstream filter)
        r_filter_type <- shiny::reactiveVal(filter_type)
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        r_filter_range <- shiny::reactiveVal(filter_range)
        r_filter_point <- shiny::reactiveVal(filter_point)

        # Theming state
        r_line_width_mult <- shiny::reactiveVal(line_width_mult)
        r_dot_size_mult <- shiny::reactiveVal(dot_size_mult)
        # Overlay options. `step`, `ref_x`, `ref_y` have no gear-popover
        # control but ARE consumed by the JS renderer (step-mode line, and
        # vertical/horizontal reference-line overlays). They flow through the
        # config payload below and via external_ctrl, so leave them wired.
        r_step <- shiny::reactiveVal(step)
        r_ref_x <- shiny::reactiveVal(ref_x)
        r_ref_y <- shiny::reactiveVal(ref_y)
        r_smoother <- shiny::reactiveVal(smoother)
        r_identity_line <- shiny::reactiveVal(identity_line)
        r_box_points <- shiny::reactiveVal(box_points)
        r_lo <- shiny::reactiveVal(lo)
        r_hi <- shiny::reactiveVal(hi)
        # Bar baseline mode + waterfall total-bar steps (see constructor args).
        r_baseline <- shiny::reactiveVal(baseline)
        r_waterfall_totals <- shiny::reactiveVal(waterfall_totals)
        r_board_theme <- setup_drilldown_theme_sync(session)
        # Board scale map (NULL when the board has no "scale_map" option);
        # resolved per data push, never stored in block state.
        r_scale_map <- dd_board_scale_map()

        # Column metadata (computed once when data changes)
        r_col_meta <- shiny::reactive({
          d <- data()
          shiny::req(is.data.frame(d), nrow(d) > 0)
          lapply(names(d), function(col) {
            vals <- d[[col]]
            lbl <- attr(vals, "label")
            res <- list(
              name = col,
              type = if (is.numeric(vals)) "numeric" else "categorical",
              n_unique = length(unique(vals))
            )
            if (!is.null(lbl) && nzchar(lbl)) res$label <- lbl
            # Factor level order travels to JS as the category/legend order
            # (the data-level "order lives in factors" contract).
            if (is.factor(vals)) res$levels <- as.list(levels(vals))
            res
          })
        })

        # Columns needed by the chart (reactive -- changes when config
        # changes). drill and label are included so the browser has the
        # columns a click filters on / a mark is labelled by.
        # Columns the current mapping needs (so the whole wide flatten is
        # never shipped). Reads the config reactiveVals, so the enclosing
        # observe re-runs when the mapping changes -- same as before the
        # roles refactor.
        needed_cols <- function(d) {
          # as.character(unlist(...)) so a config arg that arrives as an
          # empty list() -- e.g. a NULL aesthetic corrupted by DAG copy/paste
          # into `list()` -- does not coerce the whole vector to a list and
          # crash `d[, needed]` ("must be ... not a list").
          needed <- as.character(unlist(c(
            r_group(), r_color(), r_facet(), r_value(),
            r_x(), r_y(), r_xend(), r_series(),
            r_label(), r_tt_fields(), r_drill(), r_lo(), r_hi()
          )))
          sb <- as.character(unlist(r_sort_by()))
          if (length(sb) && !sb %in% c("onset", "alpha")) {
            needed <- c(needed, sb)
          }
          if (!is.data.frame(d)) return(character())
          if (identical(r_x(), "AVISIT") && "AVISITN" %in% names(d)) {
            needed <- c(needed, "AVISITN")
          }
          needed <- unique(needed)
          needed[!is.null(needed) & needed != "" &
            needed != ".count" & needed %in% names(d)]
        }

        # Push data + config to JS. Single observe, exactly as before the
        # roles refactor: this shape is what the blockr.dock lazy-eval
        # card-probe pairing correctly suspends for hidden panels, so
        # off-view charts do not render. (The earlier observeEvent +
        # separate config channel rewrite escaped that gating and was the
        # AE-tab freeze.) Only the columns the current mapping needs are
        # shipped, never the whole wide flatten.
        shiny::observe({
          d <- data()
          shiny::req(is.data.frame(d), nrow(d) > 0)
          needed <- needed_cols(d)
          df_send <- if (length(needed)) {
            d[, needed, drop = FALSE]
          } else {
            d[0]
          }
          # Reuse the column metadata reactive (same name/type/n_unique/label
          # shape) instead of recomputing it inline -- it is derived from the
          # same data() and was duplicated here.
          col_meta <- r_col_meta()
          session$sendCustomMessage("drilldown-data", list(
            id = ns("drilldown_block"),
            columns = col_meta,
            data = jsonlite::toJSON(df_send, dataframe = "columns",
                                    digits = NA),
            config = list(
              group = r_group(), color = r_color(), facet = r_facet(),
              value = r_value(), func = r_func(),
              chart_type = r_chart_type(), x = r_x(), y = r_y(),
              xend = r_xend(), series = r_series(), label = r_label(),
              # Extra tooltip columns. as.list() keeps a length-1 vector a JSON
              # array; NULL when empty so the JS "+" role stays hidden until the
              # user adds it (an empty [] would read as "present").
              tt_fields = if (length(r_tt_fields())) as.list(r_tt_fields()) else NULL,
              drill = r_drill(), sort_by = r_sort_by(),
              sort_dir = r_sort_dir(), orientation = r_orientation(),
              bar_mode = r_bar_mode(),
              line_width_mult = r_line_width_mult(),
              dot_size_mult = r_dot_size_mult(), step = r_step(),
              ref_x = as.list(r_ref_x()), ref_y = as.list(r_ref_y()),
              smoother = r_smoother(),
              identity_line = r_identity_line(),
              box_points = r_box_points(),
              # Bar baseline mode. chart_type "waterfall" implies "cumulative"
              # on the JS side (sugar); also send the flag explicitly so a plain
              # bar can opt into the cumulative bridge, and pass the optional
              # total-bar step names. as.list() so a length-1 char vector still
              # serializes as a JSON array.
              baseline = r_baseline(),
              waterfall_totals = as.list(r_waterfall_totals()),
              smoother_series = tryCatch(compute_smoother_series(
                d, r_smoother(), r_x(), r_y(), r_color(), r_series()
              ), error = function(e) {
                # Keep the NULL fallback (no overlay) but surface the failure
                # so a broken smoother fit is diagnosable instead of silent.
                warning("drilldown_chart smoother computation failed: ",
                        conditionMessage(e), call. = FALSE)
                NULL
              }),
              lo = r_lo(), hi = r_hi(),
              # Board scale map, resolved for the chart type's colored role
              # (NULL when no map / no binding / no colored role -- JS then
              # keeps palette cycling).
              scales = dd_scales_config(
                r_scale_map(), r_chart_type(),
                color = r_color(), group = r_group(), data = d
              )
            )
            # NB: the registry _arguments() prose is intentionally NOT sent to
            # the browser. LLM prompts live in the registry only; popover help
            # is a UI-layer concern (terse labels + the live `name (label)`
            # convention). See blockr.design/open/block-config-ui.
          ))
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

        shiny::observeEvent(input$drilldown_block_action, {
          msg <- input$drilldown_block_action
          if (is.null(msg)) return()

          action <- msg$action
          if (action == "config") {
            if (!is.null(msg$group))      upd(r_group, nn(msg$group))
            if (!is.null(msg$color))      upd(r_color, nn(msg$color))
            if (!is.null(msg$facet))      upd(r_facet, nn(msg$facet))
            if (!is.null(msg$value))     upd(r_value, msg$value)
            if (!is.null(msg$func))     upd(r_func, msg$func)
            if (!is.null(msg$chart_type)) upd(r_chart_type, msg$chart_type)
            if (!is.null(msg$x))          upd(r_x, msg$x)
            if (!is.null(msg$y))          upd(r_y, msg$y)
            if (!is.null(msg$xend))       upd(r_xend, nn(msg$xend))
            if (!is.null(msg$series))     upd(r_series, nn(msg$series))
            if (!is.null(msg$label))      upd(r_label, nn(msg$label))
            if (!is.null(msg$tt_fields)) {
              # Arrives as a JSON array (possibly empty). Flatten to a
              # character vector; an empty selection becomes NULL.
              tf <- as.character(unlist(msg$tt_fields))
              tf <- tf[nzchar(tf)]
              upd(r_tt_fields, if (length(tf)) tf else NULL)
            }
            if (!is.null(msg$drill))      upd(r_drill, nn(msg$drill))
            if (!is.null(msg$sort_by))    upd(r_sort_by, nn(msg$sort_by))
            if (!is.null(msg$sort_dir))   upd(r_sort_dir, msg$sort_dir)
            if (!is.null(msg$orientation)) upd(r_orientation, msg$orientation)
            if (!is.null(msg$bar_mode))   upd(r_bar_mode, msg$bar_mode)
            if (!is.null(msg$baseline))   upd(r_baseline, msg$baseline)
            if (!is.null(msg$smoother))   upd(r_smoother, msg$smoother)
            if (!is.null(msg$identity_line)) {
              upd(r_identity_line, msg$identity_line)
            }
            if (!is.null(msg$box_points)) upd(r_box_points, msg$box_points)
            if (!is.null(msg$lo))         upd(r_lo, nn(msg$lo))
            if (!is.null(msg$hi))         upd(r_hi, nn(msg$hi))
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
            # needlessly on an echoed filter. The click-vs-brush race logic
            # (the `is_point` no-op below, the `!is.null` gating) is
            # unchanged -- only the actual writes are now guarded.
            ft <- msg$filter_type %||% "categorical"

            if (ft == "categorical") {
              upd(r_filter_column, msg$column)
              upd(r_filter_values, msg$values)
              upd(r_filter_range, NULL)
              upd(r_filter_point, NULL)
              upd(r_filter_type, "categorical")
            } else if (ft == "point") {
              # Click on a single dot with no drill column: drill to the
              # observation(s) at that exact x/y coordinate.
              if (!is.null(msg$x_col) && !is.null(msg$y_col)) {
                upd(r_filter_column, NULL)
                upd(r_filter_values, NULL)
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
                xr <- as.numeric(msg$x_range)
                yr <- if (!is.null(msg$y_range)) as.numeric(msg$y_range)
                # A degenerate range (xlo == xhi) is now legitimate: it is a
                # scatter-auto CLICK (a one-point selection -> the observation
                # via between(x, v, v)). The old click-vs-brush race that this
                # guarded against is gone -- within each drill mode click and
                # brush emit the SAME filter type (override -> categorical,
                # auto -> range), so there is nothing to clobber.
                upd(r_filter_column, NULL)
                upd(r_filter_values, NULL)
                upd(r_filter_point, NULL)
                upd(r_filter_range, list(
                  x_col = msg$x_col,
                  y_col = msg$y_col,
                  x_range = xr,
                  y_range = yr
                ))
                upd(r_filter_type, "range")
              } else {
                upd(r_filter_column, NULL)
                upd(r_filter_values, NULL)
                upd(r_filter_range, NULL)
                upd(r_filter_point, NULL)
                upd(r_filter_type, "categorical")
              }
            }
          }
        })

        # Build filter expression. With expr_type = "bquoted", `.(data)` is
        # substituted by blockr.core with the upstream block's id at eval
        # time -- so the no-filter branch passes the upstream data frame
        # straight through, keeping the lazy eval chain intact.
        # Configured aesthetic columns that must exist in the upstream data.
        # If an upstream block renames/drops a mapped column, the chart would
        # otherwise read NA silently and mis-render -- surface a clear invalid
        # state instead. `.count` is the synthetic count metric (no column),
        # `drill = "auto"` is a sentinel resolved at click time from the clicked
        # shape (not a literal column), and the runtime sort_by sentinels are
        # not data columns either. The data passes through unchanged, so this
        # only guards the JS-side mapping.
        mapped_cols <- function(d) {
          drill <- r_drill()
          if (identical(drill, "auto")) drill <- NULL
          cols <- c(r_group(), r_color(), r_facet(), r_value(),
            r_x(), r_y(), r_xend(), r_series(), r_label(), drill)
          cols <- unique(cols[!is.null(cols) & nzchar(cols) & cols != ".count"])
          cols[!cols %in% names(d)]
        }

        list(
          expr = shiny::reactive({
            d <- data()
            if (is.data.frame(d)) {
              missing_cols <- mapped_cols(d)
              shiny::validate(shiny::need(
                length(missing_cols) == 0,
                paste0(
                  "Column", if (length(missing_cols) > 1) "s" else "", " not found: ",
                  paste(missing_cols, collapse = ", "),
                  " (renamed or dropped upstream?)"
                )
              ))
            }
            ft <- r_filter_type()

            if (ft == "categorical") {
              col <- r_filter_column()
              vals <- r_filter_values()

              if (is.null(col) || is.null(vals) || length(vals) == 0) {
                blockr.core::bbquote(dplyr::filter(.(data), TRUE))
              } else if (length(vals) == 1) {
                blockr.core::bbquote(
                  dplyr::filter(.(data), .data[[.(col)]] == .(val)),
                  list(col = col, val = vals[[1]])
                )
              } else {
                blockr.core::bbquote(
                  dplyr::filter(.(data), .data[[.(col)]] %in% .(vals)),
                  list(col = col, vals = vals)
                )
              }
            } else if (ft == "point") {
              pt <- r_filter_point()
              if (is.null(pt) || is.null(pt$x_col) || is.null(pt$y_col)) {
                return(blockr.core::bbquote(dplyr::filter(.(data), TRUE)))
              }
              blockr.core::bbquote(
                dplyr::filter(.(data),
                  .data[[.(xc)]] == .(xv) & .data[[.(yc)]] == .(yv)
                ),
                list(xc = pt$x_col, yc = pt$y_col,
                  xv = pt$x_val, yv = pt$y_val)
              )
            } else if (ft == "range") {
              rng <- r_filter_range()
              if (is.null(rng) || is.null(rng$x_range)) {
                return(blockr.core::bbquote(dplyr::filter(.(data), TRUE)))
              }
              xc <- rng$x_col
              xr <- rng$x_range
              yr <- rng$y_range
              if (!is.null(yr) && !is.null(rng$y_col)) {
                # 2D brush (scatter): filter on both x and y
                yc <- rng$y_col
                blockr.core::bbquote(
                  dplyr::filter(.(data),
                    dplyr::between(.data[[.(xc)]], .(xlo), .(xhi)) &
                      dplyr::between(.data[[.(yc)]], .(ylo), .(yhi))
                  ),
                  list(xc = xc, yc = yc,
                    xlo = xr[1], xhi = xr[2],
                    ylo = yr[1], yhi = yr[2])
                )
              } else {
                # 1D brush (line): filter on x only
                blockr.core::bbquote(
                  dplyr::filter(.(data),
                    dplyr::between(.data[[.(xc)]], .(xlo), .(xhi))
                  ),
                  list(xc = xc, xlo = xr[1], xhi = xr[2])
                )
              }
            } else {
              blockr.core::bbquote(dplyr::filter(.(data), TRUE))
            }
          }),
          state = list(
            group = r_group,
            color = r_color,
            facet = r_facet,
            value = r_value,
            func = r_func,
            chart_type = r_chart_type,
            x = r_x,
            y = r_y,
            xend = r_xend,
            series = r_series,
            label = r_label,
            tt_fields = r_tt_fields,
            drill = r_drill,
            sort_by = r_sort_by,
            sort_dir = r_sort_dir,
            orientation = r_orientation,
            bar_mode = r_bar_mode,
            filter_type = r_filter_type,
            filter_column = r_filter_column,
            filter_values = r_filter_values,
            filter_range = r_filter_range,
            filter_point = r_filter_point,
            line_width_mult = r_line_width_mult,
            dot_size_mult = r_dot_size_mult,
            step = r_step,
            ref_x = r_ref_x,
            ref_y = r_ref_y,
            smoother = r_smoother,
            identity_line = r_identity_line,
            box_points = r_box_points,
            lo = r_lo,
            hi = r_hi,
            baseline = r_baseline,
            waterfall_totals = r_waterfall_totals
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
        shiny::div(id = ns("drilldown_block"), class = "drilldown-chart-container")
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame")
    },
    allow_empty_state = c("group", "color", "facet", "filter_column",
      "filter_values", "x", "y", "xend", "series", "label", "tt_fields",
      "drill", "sort_by", "sort_dir", "filter_range", "filter_point",
      "step", "ref_x", "ref_y", "smoother", "identity_line", "box_points",
      "lo", "hi", "waterfall_totals"),
    external_ctrl = c("group", "color", "facet", "value", "func",
      "chart_type", "x", "y", "xend", "series", "label", "tt_fields", "drill",
      "sort_by", "sort_dir", "orientation", "bar_mode", "filter_type",
      "filter_column",
      "filter_values", "filter_range", "filter_point", "line_width_mult",
      "dot_size_mult", "step", "ref_x", "ref_y", "smoother", "identity_line",
      "box_points", "lo", "hi", "baseline", "waterfall_totals"),
    expr_type = "bquoted",
    class = "chart_block",
    ...
  )
}

#' Compute smoother line points per group for a scatter chart
#'
#' Returns a named list keyed by group level. Each entry is a list with
#' numeric vectors `x` and `y` covering a 100-point line across the group's
#' x range. Used by the chart's JS-side renderer to draw a regression
#' overlay without doing the math in the browser.
#'
#' @param data Data frame.
#' @param smoother One of `"none"`, `"lm"`, `"loess"`.
#' @param x_col,y_col Numeric column names.
#' @param color_by,series_by Grouping column names; smoother is fit per
#'   `series_by` if non-NULL else `color_by` else no grouping.
#' @return A named list or `NULL`.
#' @examples
#' compute_smoother_series(
#'   mtcars, "lm", "wt", "mpg",
#'   color_by = NULL, series_by = NULL
#' )
#' @keywords internal
#' @export
compute_smoother_series <- function(data, smoother, x_col, y_col,
                                     color_by, series_by) {
  if (is.null(smoother) || identical(smoother, "none")) return(NULL)
  if (is.null(data) || nrow(data) == 0) return(NULL)
  if (is.null(x_col) || is.null(y_col)) return(NULL)
  if (!all(c(x_col, y_col) %in% names(data))) return(NULL)
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) return(NULL)

  split_col <- series_by %||% color_by
  if (!is.null(split_col) && split_col %in% names(data)) {
    groups <- split(data, as.character(data[[split_col]]))
  } else {
    groups <- list(`__all__` = data)
  }

  fit_one <- function(d) {
    d <- d[!is.na(d[[x_col]]) & !is.na(d[[y_col]]), , drop = FALSE]
    if (nrow(d) < 3L) return(NULL)
    xv <- d[[x_col]]
    yv <- d[[y_col]]
    if (length(unique(xv)) < 2L) return(NULL)
    rng <- range(xv, na.rm = TRUE)
    xs <- seq(rng[1L], rng[2L], length.out = 100L)
    ys <- tryCatch({
      if (identical(smoother, "lm")) {
        coefs <- stats::coef(stats::lm(yv ~ xv))
        coefs[1L] + coefs[2L] * xs
      } else if (identical(smoother, "loess")) {
        fit <- stats::loess(yv ~ xv, span = 0.75,
                            control = stats::loess.control(surface = "direct"))
        as.numeric(stats::predict(fit, newdata = data.frame(xv = xs)))
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (is.null(ys) || all(is.na(ys))) return(NULL)
    list(x = as.list(xs), y = as.list(unname(ys)))
  }
  res <- lapply(groups, fit_one)
  res <- res[!vapply(res, is.null, logical(1L))]
  if (length(res) == 0L) NULL else res
}

`%||%` <- function(a, b) if (is.null(a)) b else a
