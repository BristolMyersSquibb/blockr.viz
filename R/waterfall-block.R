#' @importFrom blockr.core block_output block_ui
NULL

# Global variable definitions for echarts4r (suppress R CMD check NOTEs)
utils::globalVariables(c("step", "helper", "positive", "negative", "total"))

#' Waterfall Block
#'
#' A waterfall/bridge chart block for visualizing sequential value progression.
#' Shows how a value builds from one measure to another (e.g., Revenue -> Costs ->
#' Profit, or Q1 -> Q2 -> Q3 -> Q4).
#'
#' Common use cases include P&L statements, budget variance analysis, revenue
#' bridges, and any sequential measure comparison.
#'
#' @param measures Character vector. The numeric columns representing steps in
#'   the waterfall, in order. Each subsequent measure should include the previous.
#' @param colors Named list. Colors for increase/decrease/total bars.
#'   Default: increase = "#009E73" (Okabe-Ito green), decrease = "#dc2626" (red), total = "#bbbbbb" (gray).
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#' @param id Module ID (for S3 methods)
#' @param x Block object (for S3 methods)
#' @param result Evaluation result (for S3 methods)
#' @param session Shiny session object (for S3 methods)
#'
#' @return A blockr transform block that displays a waterfall chart
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.bi)
#'
#'   # P&L waterfall example
#'   pnl_data <- data.frame(
#'     Revenue = 1000000,
#'     Gross_Profit = 600000,
#'     Operating_Income = 400000,
#'     Net_Income = 300000
#'   )
#'
#'   serve(
#'     new_waterfall_block(
#'       measures = c("Revenue", "Gross_Profit", "Operating_Income", "Net_Income")
#'     ),
#'     data = list(data = pnl_data)
#'   )
#' }
new_waterfall_block <- function(
    measures = character(),
    colors = list(increase = "#009E73", decrease = "#dc2626", total = "#bbbbbb"),
    ...
) {
  lifecycle::deprecate_soft(
    "0.0.0", "new_waterfall_block()", "new_chart_block()",
    details = paste0(
      "Unregistered; constructor kept so existing boards still load. ",
      "Use chart_type='waterfall'."
    )
  )
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          r_measures <- shiny::reactiveVal(measures)
          r_colors <- shiny::reactiveVal(colors)
          r_initialized <- shiny::reactiveVal(FALSE)

          # Detect numeric columns for measures
          numeric_cols <- shiny::reactive({
            df <- data()
            if (!is.data.frame(df) || ncol(df) == 0) {
              return(character())
            }
            is_numeric <- vapply(df, is.numeric, logical(1))
            names(df)[is_numeric]
          })

          # One-time initialization (runs once when data first arrives)
          shiny::observe({
            if (!r_initialized() && length(numeric_cols()) > 0) {
              choices <- numeric_cols()
              if (length(r_measures()) == 0 && length(choices) >= 2) {
                r_measures(utils::head(choices, 3))
              }
              shiny::updateSelectizeInput(
                session, "measures",
                choices = choices,
                selected = r_measures()
              )
              r_initialized(TRUE)
            }
          })

          # Data-change handler (preserves current user selection)
          shiny::observeEvent(numeric_cols(), {
            if (r_initialized()) {
              shiny::updateSelectizeInput(
                session, "measures",
                choices = numeric_cols(),
                selected = r_measures()
              )
            }
          })

          # Update state from UI
          shiny::observeEvent(input$measures, {
            r_measures(input$measures)
          }, ignoreInit = TRUE)

          # Return the aggregated data for waterfall
          list(
            expr = shiny::reactive({
              meas <- r_measures()
              shiny::req(length(meas) >= 2)
              # Guard against an upstream rename/drop: the summarise expr
              # references each measure by name, so a missing column would
              # error opaquely at eval time. Surface a clear invalid state.
              d <- data()
              if (is.data.frame(d)) {
                missing_cols <- meas[!meas %in% names(d)]
                shiny::validate(shiny::need(
                  length(missing_cols) == 0,
                  paste0(
                    "Column", if (length(missing_cols) > 1) "s" else "",
                    " not found: ", paste(missing_cols, collapse = ", "),
                    " (renamed or dropped upstream?)"
                  )
                ))
              }
              build_waterfall_expr(meas)
            }),
            state = list(
              measures = r_measures,
              colors = r_colors
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          class = "waterfall-settings",
          style = "padding: 10px;",

          # Measures (ordered multi-select)
          shiny::selectizeInput(
            ns("measures"),
            label = "Measures (in order)",
            choices = measures,
            selected = measures,
            multiple = TRUE,
            options = list(
              placeholder = "Select measures in waterfall order...",
              plugins = list("remove_button", "drag_drop")
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    allow_empty_state = c("measures", "colors"),
    class = "waterfall_block",
    colors = colors,
    ...
  )
}

#' Build expression for waterfall aggregation
#' @noRd
build_waterfall_expr <- function(measures) {
  # Build summarise expression - aggregate all measures

  summarise_args <- lapply(measures, function(m) {
    m_sym <- as.name(m)
    bquote(sum(.(m_sym), na.rm = TRUE))
  })
  names(summarise_args) <- measures

  as.call(c(
    list(quote(dplyr::summarise), quote(data)),
    summarise_args
  ))
}

#' Build waterfall data from aggregated measures
#'
#' @param data Aggregated data frame with measures as columns
#' @param measures Character vector of measure names in order
#' @return Data frame ready for waterfall chart
#' @noRd
build_waterfall_data <- function(data, measures) {
  # Ensure we extract values in the correct order
  values <- numeric(length(measures))
  for (i in seq_along(measures)) {
    val <- data[[measures[i]]]
    values[i] <- if (length(val) > 0 && is.numeric(val)) val[1] else NA_real_
  }
  n <- length(values)
  deltas <- c(values[1], diff(values))

  helper <- numeric(n)
  positive <- numeric(n)
  negative <- numeric(n)

  cumsum_val <- 0

  for (i in seq_len(n)) {
    if (i == 1) {
      # First bar: starts from 0
      helper[i] <- 0
      positive[i] <- values[i]
      negative[i] <- 0
      cumsum_val <- values[i]
    } else {
      # Middle bars: show delta (floating)
      delta <- deltas[i]
      if (delta >= 0) {
        helper[i] <- cumsum_val
        positive[i] <- delta
        negative[i] <- 0
      } else {
        helper[i] <- cumsum_val + delta
        positive[i] <- 0
        negative[i] <- abs(delta)
      }
      cumsum_val <- cumsum_val + delta
    }
  }

  # Create nice labels
  labels <- gsub("_", " ", measures)

  data.frame(
    step = factor(labels, levels = labels),
    total = values,
    delta = deltas,
    helper = helper,
    positive = positive,
    negative = negative,
    stringsAsFactors = FALSE
  )
}

#' @rdname new_waterfall_block
#' @export
block_ui.waterfall_block <- function(id, x, ...) {
  shiny::tagList(
    echarts4r::echarts4rOutput(shiny::NS(id, "result"), height = "350px")
  )
}

#' @rdname new_waterfall_block
#' @export
block_output.waterfall_block <- function(x, result, session) {
  # Get colors from block attributes
  colors <- attr(x, "colors")
  if (is.null(colors)) {
    colors <- list(increase = "#009E73", decrease = "#dc2626", total = "#bbbbbb")
  }

  # Get user-selected measures from session input (preserves order)
  selected_measures <- session$input[["expr-measures"]]

  # Board-level echarts theme (reactive — re-renders on change)
  r_theme <- setup_drilldown_theme_sync()

  echarts4r::renderEcharts4r({
    if (!is.data.frame(result) || ncol(result) == 0) {
      return(NULL)
    }

    # Use selected measures in order, or fall back to numeric columns
    if (!is.null(selected_measures) && length(selected_measures) >= 2) {
      # Filter to only columns that exist in result
      measures <- intersect(selected_measures, names(result))
    } else {
      # Fallback: get all numeric columns
      measures <- names(result)[vapply(result, is.numeric, logical(1))]
    }

    if (length(measures) < 2) {
      return(NULL)
    }

    # Build waterfall data
    wf_data <- build_waterfall_data(result, measures)

    # Render waterfall chart
    render_waterfall(wf_data, colors, r_theme())
  })
}

#' Render waterfall chart
#'
#' Styling tracks the drill-down design system (see chart.js):
#' Open Sans, `#666` labels, `#ccc` axis line, dashed `#f3f4f6` splitlines.
#' Increase/decrease/total colors are semantic and override theme palettes.
#' @noRd
render_waterfall <- function(wf_data, colors, theme = "default") {
  blockr_font <- "'Open Sans', system-ui, sans-serif"

  e <- wf_data |>
    echarts4r::e_charts(step) |>
    # Transparent helper for floating effect
    echarts4r::e_bar(
      helper,
      stack = "waterfall",
      name = " ",
      itemStyle = list(
        color = "transparent",
        borderColor = "transparent"
      ),
      emphasis = list(disabled = TRUE)
    ) |>
    # Positive values (green)
    echarts4r::e_bar(
      positive,
      stack = "waterfall",
      name = "Increase",
      itemStyle = list(
        color = colors$increase,
        borderRadius = c(4, 4, 0, 0)
      )
    ) |>
    # Negative values (red)
    echarts4r::e_bar(
      negative,
      stack = "waterfall",
      name = "Decrease",
      itemStyle = list(
        color = colors$decrease,
        borderRadius = c(4, 4, 0, 0)
      )
    ) |>
    # Total markers (dots only, no connecting line)
    echarts4r::e_line(
      total,
      name = "Total",
      symbol = "circle",
      symbolSize = 7,
      lineStyle = list(width = 0),
      itemStyle = list(color = colors$total)
    ) |>
    echarts4r::e_tooltip(
      trigger = "axis",
      valueFormatter = htmlwidgets::JS("function(v) { return Math.round(v); }")
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_y_axis(
      name = "Value",
      nameLocation = "middle",
      nameGap = 50,
      nameTextStyle = list(color = "#666", fontFamily = blockr_font),
      axisLabel = list(
        color = "#666",
        fontSize = 11,
        fontFamily = blockr_font,
        formatter = htmlwidgets::JS("
          function(value) {
            if (value == null || isNaN(value)) return '';
            if (Math.abs(value) >= 1000000) {
              return (value/1000000).toFixed(1) + 'M';
            } else if (Math.abs(value) >= 1000) {
              return (value/1000).toFixed(0) + 'k';
            }
            return value;
          }
        ")
      ),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      splitLine = list(lineStyle = list(color = "#f3f4f6", type = "dashed"))
    ) |>
    echarts4r::e_x_axis(
      axisLabel = list(
        color = "#666",
        rotate = 0,
        fontSize = 11,
        fontFamily = blockr_font,
        overflow = "break",
        width = 100
      ),
      axisLine = list(lineStyle = list(color = "#ccc")),
      axisTick = list(show = FALSE),
      splitLine = list(show = FALSE)
    ) |>
    echarts4r::e_grid(
      left = "12%",
      right = "5%",
      bottom = "10%",
      top = "10%"
    ) |>
    echarts4r::e_text_style(fontFamily = "Open Sans") |>
    echarts4r::e_toolbox(
      right = 8, top = 4, itemSize = 11,
      iconStyle = list(borderColor = "#bbb")
    ) |>
    echarts4r::e_toolbox_feature(
      feature = "saveAsImage",
      title = "Save",
      pixelRatio = 2
    )

  if (!identical(theme, "default") && nzchar(theme)) {
    e <- echarts4r::e_theme(e, theme)
  }
  e
}
