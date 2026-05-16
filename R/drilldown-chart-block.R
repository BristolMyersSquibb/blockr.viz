#' Drill-Down Chart Block
#'
#' A configurable chart block that also acts as a filter. Three chart families:
#'
#' **Aggregated** (bar, pie, treemap, boxplot): group-by + metric, click a
#' group to filter the raw rows behind it.
#'
#' **Individual** (scatter, line): x/y columns, brush-drag a region to filter
#' rows within that range. Clicking a line or a scatter dot emits a
#' categorical filter on its series (`series_by` when set, otherwise
#' `color_by`). When neither is set, a click instead drills to the
#' clicked observation itself — filtering the row(s) at that exact
#' (x, y) point. Useful for "one dot per entity → click to drill to
#' that entity" patterns (e.g. one dot per policy in a property
#' workbench, click → filter downstream blocks to that single policy).
#'
#' **Timeline** (gantt): interval rows with start / optional end and a
#' categorical y axis (e.g. AE term). Clicking a bar emits a categorical
#' filter on `series_by` (else `color_by`); with neither set the click is
#' a no-op. `sort_by` controls y-axis category ordering.
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
#'   * Aggregated: `"value"` (computed metric, default), `"alpha"` (group
#'     name), or a column name (ascending min of that column per group).
#'   * Timeline: `"onset"` (default — by ascending min of `x_col` per term),
#'     `"alpha"`, or a column name.
#'   * Individual: ignored (points plotted in raw order).
#' @param sort_dir `"asc"` or `"desc"`. Reverses the ordering produced by
#'   `sort_by`. When unset, defaults to `"desc"` for aggregated charts and
#'   `"asc"` for timelines. Ignored by individual charts.
#' @param filter_type Filter mode: "categorical" or "range"
#' @param filter_column Currently filtered column (aggregated, set by click)
#' @param filter_values Currently filtered values (aggregated, set by click)
#' @param filter_range Range filter list with x_col, y_col, x_range, y_range
#'   (individual, set by brush)
#' @param filter_point Point filter list with x_col, y_col, x_val, y_val
#'   (individual scatter/line, set by clicking a single dot when no
#'   entity/id column resolves — drills to the row(s) at that point)
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
    sort_dir = NULL,
    filter_type = "categorical",
    filter_column = NULL,
    filter_values = NULL,
    filter_range = NULL,
    filter_point = NULL,
    line_width_mult = 1.0,
    dot_size_mult = 1.0,
    step = NULL,
    ref_x = NULL,
    ref_y = NULL,
    smoother = "none",
    lo_col = NULL,
    hi_col = NULL,
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
        r_filter_point <- shiny::reactiveVal(filter_point)

        # Theming state
        r_line_width_mult <- shiny::reactiveVal(line_width_mult)
        r_dot_size_mult <- shiny::reactiveVal(dot_size_mult)
        # Overlay options
        r_step <- shiny::reactiveVal(step)
        r_ref_x <- shiny::reactiveVal(ref_x)
        r_ref_y <- shiny::reactiveVal(ref_y)
        r_smoother <- shiny::reactiveVal(smoother)
        r_lo_col <- shiny::reactiveVal(lo_col)
        r_hi_col <- shiny::reactiveVal(hi_col)
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
            r_x_col(), r_y_col(), r_x_end_col(), r_series_by(),
            r_lo_col(), r_hi_col()
          )
          # Include the column named by sort_by (keywords are ignored)
          sb <- r_sort_by()
          if (!is.null(sb) && !sb %in% c("onset", "alpha")) {
            needed <- c(needed, sb)
          }
          d <- data()
          shiny::req(is.data.frame(d))
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

          # Pre-encode as columnar JSON (fast, no double-encoding by Shiny).
          # digits = NA keeps full double precision: click-to-filter sends
          # a point's value back and we match it server-side, so the
          # transmitted value must round-trip exactly. The default
          # digits = 4 rounded it and the equality matched zero rows.
          data_json <- jsonlite::toJSON(
            df_send, dataframe = "columns", digits = NA
          )

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
              dot_size_mult = r_dot_size_mult(),
              step = r_step(),
              ref_x = as.list(r_ref_x()),
              ref_y = as.list(r_ref_y()),
              smoother = r_smoother(),
              smoother_series = tryCatch(compute_smoother_series(
                d, r_smoother(), r_x_col(), r_y_col(),
                r_color_by(), r_series_by()
              ), error = function(e) NULL),
              lo_col = r_lo_col(),
              hi_col = r_hi_col()
            ),
            # Per-argument help text for the settings popover. Single
            # source of truth with the LLM-facing API metadata; see
            # drilldown_chart_arguments(). as.list() keeps each string a
            # JSON scalar (avoids the auto_unbox collapse trap).
            arguments = as.list(
              stats::setNames(
                as.character(drilldown_chart_arguments()),
                names(drilldown_chart_arguments())
              )
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
            if (!is.null(msg$smoother)) r_smoother(msg$smoother)
            if (!is.null(msg$lo_col)) {
              r_lo_col(if (msg$lo_col == "") NULL else msg$lo_col)
            }
            if (!is.null(msg$hi_col)) {
              r_hi_col(if (msg$hi_col == "") NULL else msg$hi_col)
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
              r_filter_point(NULL)
              r_filter_type(
                if (!is.null(msg$column) && !is.null(msg$values)) "categorical"
                else "categorical"
              )
            } else if (ft == "point") {
              # Click on a single dot with no entity/id column: drill to
              # the observation(s) at that exact x/y coordinate.
              if (!is.null(msg$x_col) && !is.null(msg$y_col)) {
                r_filter_column(NULL)
                r_filter_values(NULL)
                r_filter_range(NULL)
                r_filter_point(list(
                  x_col = msg$x_col,
                  y_col = msg$y_col,
                  x_val = msg$x_val,
                  y_val = msg$y_val
                ))
                r_filter_type("point")
              }
            } else if (ft == "range") {
              if (!is.null(msg$x_col) && !is.null(msg$x_range)) {
                xr <- as.numeric(msg$x_range)
                yr <- if (!is.null(msg$y_range)) as.numeric(msg$y_range)
                # Defensive: a 1-pixel range (xlo == xhi and either no
                # y-range or ylo == yhi) is almost always the click-on-a-dot
                # race — a click handler sent a categorical filter, and
                # ECharts' brush mode then fired a brushSelected on the
                # clicked point. Treat as a no-op so the categorical
                # filter survives. (JS also disables brush when series_by
                # is set, but this is the belt-and-braces R-side guard.)
                is_point <- length(xr) == 2L && xr[1L] == xr[2L] &&
                  (is.null(yr) || (length(yr) == 2L && yr[1L] == yr[2L]))
                if (!is_point) {
                  r_filter_column(NULL)
                  r_filter_values(NULL)
                  r_filter_point(NULL)
                  r_filter_range(list(
                    x_col = msg$x_col,
                    y_col = msg$y_col,
                    x_range = xr,
                    y_range = yr
                  ))
                  r_filter_type("range")
                }
              } else {
                r_filter_column(NULL)
                r_filter_values(NULL)
                r_filter_range(NULL)
                r_filter_point(NULL)
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
            filter_point = r_filter_point,
            line_width_mult = r_line_width_mult,
            dot_size_mult = r_dot_size_mult,
            step = r_step,
            ref_x = r_ref_x,
            ref_y = r_ref_y,
            smoother = r_smoother,
            lo_col = r_lo_col,
            hi_col = r_hi_col
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
      "filter_values", "x_col", "y_col", "x_end_col", "sort_by", "sort_dir",
      "series_by", "filter_range", "filter_point",
      "step", "ref_x", "ref_y", "smoother", "lo_col", "hi_col"),
    external_ctrl = c("group_by", "color_by", "facet_by", "metric", "agg_fn",
      "chart_type", "x_col", "y_col", "x_end_col", "sort_by", "sort_dir",
      "series_by", "filter_type", "filter_column", "filter_values",
      "filter_range", "filter_point", "line_width_mult", "dot_size_mult",
      "step", "ref_x", "ref_y", "smoother", "lo_col", "hi_col"),
    expr_type = "bquoted",
    class = "drilldown_chart_block",
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
    xv <- d[[x_col]]; yv <- d[[y_col]]
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
      } else NULL
    }, error = function(e) NULL)
    if (is.null(ys) || all(is.na(ys))) return(NULL)
    list(x = as.list(xs), y = as.list(unname(ys)))
  }
  res <- lapply(groups, fit_one)
  res <- res[!vapply(res, is.null, logical(1L))]
  if (length(res) == 0L) NULL else res
}

`%||%` <- function(a, b) if (is.null(a)) b else a
