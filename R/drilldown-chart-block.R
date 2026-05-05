#' Drill-Down Chart Block
#'
#' A configurable chart block that also acts as a filter. Three chart families:
#'
#' **Aggregated** (bar, pie, treemap, boxplot): group-by + metric, click a
#' group to filter the raw rows behind it.
#'
#' **Individual** (scatter, line): x/y columns, brush-drag a region to filter
#' rows within that range. Clicking a line or a scatter dot emits a
#' categorical filter on its series (the `series_by` column when set,
#' otherwise `color_by`). Useful for "one dot per entity → click to drill
#' to that entity" patterns (e.g. one dot per policy in a property
#' workbench, click → filter downstream blocks to that single policy).
#'
#' **Timeline** (gantt): interval rows with start / optional end and a
#' categorical y axis (e.g. AE term). Clicking a bar emits a `USUBJID`
#' filter. `sort_by` controls y-axis category ordering.
#'
#' @param group_by Column to group by (aggregated charts)
#' @param color_by Column to color/stack by (optional, both families)
#' @param facet_by Column to facet by (optional, both families)
#' @param metric Column for metric (".count" for row count, aggregated only)
#' @param agg_fn Aggregation: `"count"`, `"count_distinct"`, `"mean"`,
#'   `"median"`, `"sum"`, `"min"`, `"max"`
#' @param chart_type Chart type: "bar", "scatter", "line", "pie", "treemap",
#'   "boxplot", "gantt"
#' @param x_col X-axis column (individual / timeline charts)
#' @param y_col Y-axis column (individual / timeline charts)
#' @param series_by Column whose distinct values define separate series in
#'   the individual family (e.g. `"USUBJID"` for one line per patient).
#'   Independent from `color_by`: `series_by` controls how rows are split
#'   into series; `color_by` controls how those series are colored. High
#'   cardinality (hundreds of patients) is expected — lines degrade to low
#'   opacity automatically. When `series_by` is unset, the chart falls
#'   back to splitting by `color_by` (legacy behavior).
#' @param x_end_col Interval end column (timeline only; NA rows render as
#'   dots at `x_col`)
#' @param sort_by Category ordering on the axis. The allowed values depend
#'   on the chart family:
#'   * Aggregated: `"alpha"` (group name, default), `"value"` (computed
#'     metric), or a column name (ascending min of that column per group).
#'   * Timeline: `"onset"` (default — by ascending min of `x_col` per term),
#'     `"alpha"`, or a column name.
#'   * Individual: ignored (points plotted in raw order).
#' @param sort_dir `"asc"` (default) or `"desc"`. Reverses the ordering
#'   produced by `sort_by`. Ignored by individual charts.
#' @param filter_type Filter mode: "categorical" or "range"
#' @param filter_column Currently filtered column (aggregated, set by click)
#' @param filter_values Currently filtered values (aggregated, set by click)
#' @param filter_range Range filter list with x_col, y_col, x_range, y_range
#'   (individual, set by brush)
#' @param line_width_mult Multiplier on the default line width for line charts.
#'   Defaults to `1.0`. Range `0.5`–`3.0`. Applies to the individual family only.
#' @param dot_size_mult Multiplier on the default marker size (scatter points
#'   and line markers). Defaults to `1.0`. Range `0.5`–`3.0`. Applies to the
#'   individual family only.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `drilldown_chart_block`
#' @export
new_drilldown_chart_block <- function(
    group_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    metric = ".count",
    agg_fn = "count",
    chart_type = "bar",
    x_col = NULL,
    y_col = NULL,
    series_by = NULL,
    x_end_col = NULL,
    sort_by = NULL,
    sort_dir = "asc",
    filter_type = "categorical",
    filter_column = NULL,
    filter_values = NULL,
    filter_range = NULL,
    line_width_mult = 1.0,
    dot_size_mult = 1.0,
    ...) {

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Config state
        r_group_by <- shiny::reactiveVal(group_by)
        r_color_by <- shiny::reactiveVal(color_by)
        r_facet_by <- shiny::reactiveVal(facet_by)
        r_metric <- shiny::reactiveVal(metric)
        r_agg_fn <- shiny::reactiveVal(agg_fn)
        r_chart_type <- shiny::reactiveVal(chart_type)
        r_x_col <- shiny::reactiveVal(x_col)
        r_y_col <- shiny::reactiveVal(y_col)
        r_x_end_col <- shiny::reactiveVal(x_end_col)
        r_sort_by <- shiny::reactiveVal(sort_by)
        r_sort_dir <- shiny::reactiveVal(sort_dir)
        r_series_by <- shiny::reactiveVal(series_by)

        # Filter state
        r_filter_type <- shiny::reactiveVal(filter_type)
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        r_filter_range <- shiny::reactiveVal(filter_range)

        # Theming state
        r_line_width_mult <- shiny::reactiveVal(line_width_mult)
        r_dot_size_mult <- shiny::reactiveVal(dot_size_mult)
        r_board_theme <- setup_drilldown_theme_sync(session)

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
            res
          })
        })

        # Columns needed by the chart (reactive — changes when config changes)
        r_needed_cols <- shiny::reactive({
          needed <- c(
            r_group_by(), r_color_by(), r_facet_by(), r_metric(),
            r_x_col(), r_y_col(), r_x_end_col(), r_series_by()
          )
          # Include the column named by sort_by (keywords are ignored)
          sb <- r_sort_by()
          if (!is.null(sb) && !sb %in% c("onset", "alpha")) {
            needed <- c(needed, sb)
          }
          d <- data()
          shiny::req(is.data.frame(d))
          # Auto-include USUBJID so click-to-filter on trajectory / gantt
          # always has the subject id column present.
          if ("USUBJID" %in% names(d)) needed <- c(needed, "USUBJID")
          # When visiting AVISIT, pull AVISITN so JS can order categories.
          if (identical(r_x_col(), "AVISIT") && "AVISITN" %in% names(d)) {
            needed <- c(needed, "AVISITN")
          }
          needed <- unique(needed)
          needed[!is.null(needed) & needed != "" &
            needed != ".count" & needed %in% names(d)]
        })

        # Send data to JS whenever upstream data or needed columns change
        # NOTE: do NOT read filter reactives here — that would cause
        # re-sends on every brush/click, destroying the chart state.
        shiny::observe({
          d <- data()
          shiny::req(is.data.frame(d), nrow(d) > 0)
          col_meta <- r_col_meta()
          needed <- r_needed_cols()
          df_send <- d[, needed, drop = FALSE]

          # Pre-encode as columnar JSON (fast, no double-encoding by Shiny)
          data_json <- jsonlite::toJSON(df_send, dataframe = "columns")

          # Read config (not filter state) for initial chart setup
          session$sendCustomMessage("drilldown-data", list(
            id = ns("drilldown_block"),
            columns = col_meta,
            data = data_json,
            config = list(
              group_by = r_group_by(),
              color_by = r_color_by(),
              facet_by = r_facet_by(),
              metric = r_metric(),
              agg_fn = r_agg_fn(),
              chart_type = r_chart_type(),
              x_col = r_x_col(),
              y_col = r_y_col(),
              x_end_col = r_x_end_col(),
              sort_by = r_sort_by(),
              sort_dir = r_sort_dir(),
              series_by = r_series_by(),
              line_width_mult = r_line_width_mult(),
              dot_size_mult = r_dot_size_mult()
            )
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

        # JS -> R: config or filter changes
        shiny::observeEvent(input$drilldown_block_action, {
          msg <- input$drilldown_block_action
          if (is.null(msg)) return()

          action <- msg$action
          if (action == "config") {
            if (!is.null(msg$group_by)) r_group_by(msg$group_by)
            if (!is.null(msg$color_by)) {
              r_color_by(if (msg$color_by == "") NULL else msg$color_by)
            }
            if (!is.null(msg$facet_by)) {
              r_facet_by(if (msg$facet_by == "") NULL else msg$facet_by)
            }
            if (!is.null(msg$metric)) r_metric(msg$metric)
            if (!is.null(msg$agg_fn)) r_agg_fn(msg$agg_fn)
            if (!is.null(msg$chart_type)) r_chart_type(msg$chart_type)
            if (!is.null(msg$x_col)) r_x_col(msg$x_col)
            if (!is.null(msg$y_col)) r_y_col(msg$y_col)
            if (!is.null(msg$x_end_col)) {
              r_x_end_col(if (msg$x_end_col == "") NULL else msg$x_end_col)
            }
            if (!is.null(msg$sort_by)) {
              r_sort_by(if (msg$sort_by == "") NULL else msg$sort_by)
            }
            if (!is.null(msg$sort_dir)) r_sort_dir(msg$sort_dir)
            if (!is.null(msg$series_by)) {
              r_series_by(if (msg$series_by == "") NULL else msg$series_by)
            }
          } else if (action == "set_mults") {
            if (!is.null(msg$line_width_mult)) {
              r_line_width_mult(as.numeric(msg$line_width_mult))
            }
            if (!is.null(msg$dot_size_mult)) {
              r_dot_size_mult(as.numeric(msg$dot_size_mult))
            }
          } else if (action == "filter") {
            ft <- msg$filter_type %||% "categorical"

            if (ft == "categorical") {
              r_filter_column(msg$column)
              r_filter_values(msg$values)
              r_filter_range(NULL)
              r_filter_type(
                if (!is.null(msg$column) && !is.null(msg$values)) "categorical"
                else "categorical"
              )
            } else if (ft == "range") {
              r_filter_column(NULL)
              r_filter_values(NULL)
              if (!is.null(msg$x_col) && !is.null(msg$x_range)) {
                r_filter_range(list(
                  x_col = msg$x_col,
                  y_col = msg$y_col,
                  x_range = as.numeric(msg$x_range),
                  y_range = if (!is.null(msg$y_range)) as.numeric(msg$y_range)
                ))
                r_filter_type("range")
              } else {
                r_filter_range(NULL)
                r_filter_type("categorical")
              }
            }
          }
        })

        # Build filter expression. With expr_type = "bquoted", `.(data)` is
        # substituted by blockr.core with the upstream block's id at eval
        # time — so the no-filter branch passes the upstream data frame
        # straight through, keeping the lazy eval chain intact.
        list(
          expr = shiny::reactive({
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
            group_by = r_group_by,
            color_by = r_color_by,
            facet_by = r_facet_by,
            metric = r_metric,
            agg_fn = r_agg_fn,
            chart_type = r_chart_type,
            x_col = r_x_col,
            y_col = r_y_col,
            x_end_col = r_x_end_col,
            sort_by = r_sort_by,
            sort_dir = r_sort_dir,
            series_by = r_series_by,
            filter_type = r_filter_type,
            filter_column = r_filter_column,
            filter_values = r_filter_values,
            filter_range = r_filter_range,
            line_width_mult = r_line_width_mult,
            dot_size_mult = r_dot_size_mult
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
    allow_empty_state = c("group_by", "color_by", "facet_by", "filter_column",
      "filter_values", "x_col", "y_col", "x_end_col", "sort_by", "series_by",
      "filter_range"),
    external_ctrl = c("group_by", "color_by", "facet_by", "metric", "agg_fn",
      "chart_type", "x_col", "y_col", "x_end_col", "sort_by", "sort_dir",
      "series_by", "filter_type", "filter_column", "filter_values",
      "filter_range", "line_width_mult", "dot_size_mult"),
    expr_type = "bquoted",
    class = "drilldown_chart_block",
    ...
  )
}
