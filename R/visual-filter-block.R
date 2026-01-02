#' @importFrom blockr.core new_transform_block block_output block_ui block_render_trigger
#' @importFrom utils head
NULL

#' Visual Filter Block
#'
#' A visual filter block using dc.js-style interactive charts. Shows multiple
#' bar charts for different dimensions. Clicking on a bar filters the data
#' to only rows matching that selection. Returns the filtered data frame.
#'
#' @param dimensions Character vector. Which columns to show as dimension charts.
#'   If NULL, auto-detects non-numeric columns (up to 4).
#' @param measure Character. Which numeric column to aggregate in the charts.
#'   Default is the first numeric column found.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that returns filtered data
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.bi)
#'
#'   # Use with demo data
#'   serve(
#'     new_visual_filter_block(),
#'     data = list(data = bi_demo_data())
#'   )
#' }
new_visual_filter_block <- function(dimensions = NULL, measure = NULL, ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # Detect dimensions and measures from data
          column_info <- shiny::reactive({
            df <- data()
            if (!is.data.frame(df) || ncol(df) == 0) {
              return(list(dimensions = character(), measures = character()))
            }

            is_numeric <- vapply(df, is.numeric, logical(1))
            list(
              dimensions = names(df)[!is_numeric],
              measures = names(df)[is_numeric]
            )
          })

          # Determine which dimensions to show (max 4)
          active_dimensions <- shiny::reactive({
            info <- column_info()
            dims <- if (!is.null(dimensions)) {
              intersect(dimensions, info$dimensions)
            } else {
              info$dimensions
            }
            # Limit to first 4 dimensions
            head(dims, 4)
          })

          # Determine which measure to use
          active_measure <- shiny::reactive({
            info <- column_info()
            if (!is.null(measure) && measure %in% info$measures) {
              measure
            } else if (length(info$measures) > 0) {
              info$measures[1]
            } else {
              NULL
            }
          })

          # State: active filters per dimension
          r_filters <- shiny::reactiveVal(list())

          # Clear all filters
          shiny::observeEvent(input$clear_filters, {
            r_filters(list())
          })

          # Filtered data based on active filters (for chart rendering)
          # Uses dplyr::filter for database backend compatibility
          filtered_data <- shiny::reactive({
            df <- data()
            shiny::req(is.data.frame(df))

            filters <- r_filters()
            for (dim in names(filters)) {
              val <- filters[[dim]]
              if (!is.null(val) && dim %in% names(df)) {
                df <- dplyr::filter(df, .data[[dim]] == val)
              }
            }
            df
          })

          # Render the charts grid UI
          output$charts_grid <- shiny::renderUI({
            dims <- active_dimensions()

            if (length(dims) == 0) {
              return(shiny::div(
                style = "padding: 20px; text-align: center; color: #666;",
                "No dimension columns found in data"
              ))
            }

            # Create a row of charts
            shiny::div(
              class = "charts-row",
              style = "display: flex; flex-wrap: wrap; gap: 10px;",
              lapply(dims, function(dim) {
                shiny::div(
                  style = "flex: 1; min-width: 200px;",
                  echarts4r::echarts4rOutput(ns(paste0(dim, "_chart")), height = "250px")
                )
              })
            )
          })

          # Track created observers to avoid duplicates
          created_observers <- shiny::reactiveVal(character())

          # Create click handlers for each dimension (only once per dimension)
          shiny::observe({
            dims <- active_dimensions()
            existing <- created_observers()
            new_dims <- setdiff(dims, existing)

            if (length(new_dims) > 0) {
              for (dim in new_dims) {
                local({
                  my_dim <- dim
                  input_id <- paste0(my_dim, "_chart_clicked_data")

                  shiny::observeEvent(input[[input_id]], {
                    clicked <- input[[input_id]]

                    # echarts4r returns value as c(category_name, numeric_value)
                    clicked_value <- clicked$value[1]

                    if (!is.null(clicked_value) && clicked_value != "") {
                      current <- r_filters()

                      # Toggle: if already selected, remove; else set
                      if (identical(current[[my_dim]], clicked_value)) {
                        current[[my_dim]] <- NULL
                      } else {
                        current[[my_dim]] <- clicked_value
                      }
                      r_filters(current)
                    }
                  }, ignoreInit = TRUE)
                })
              }
              created_observers(c(existing, new_dims))
            }
          })

          # Render each dimension chart
          shiny::observe({
            dims <- active_dimensions()
            meas <- active_measure()
            shiny::req(length(dims) > 0, meas)

            lapply(dims, function(dim) {
              output[[paste0(dim, "_chart")]] <- echarts4r::renderEcharts4r({
                df <- filtered_data()
                shiny::req(nrow(df) > 0)

                # Aggregate by dimension using dplyr (DB-compatible)
                agg <- dplyr::summarise(
                  df,
                  !!meas := sum(.data[[meas]], na.rm = TRUE),
                  .by = dplyr::all_of(dim)
                )

                # Sort by measure descending
                agg <- dplyr::arrange(agg, dplyr::desc(.data[[meas]]))

                # Get current filter for highlighting
                current_filter <- r_filters()[[dim]]

                # Create colors based on selection
                colors <- if (!is.null(current_filter)) {
                  ifelse(agg[[dim]] == current_filter, "#5470c6", "#91cc75")
                } else {
                  "#5470c6"
                }

                # Create chart
                agg |>
                  echarts4r::e_charts_(dim) |>
                  echarts4r::e_bar_(meas, name = meas, itemStyle = list(color = htmlwidgets::JS(
                    sprintf("function(params) { var colors = %s; return colors[params.dataIndex] || colors; }",
                            jsonlite::toJSON(colors))
                  ))) |>
                  echarts4r::e_tooltip(trigger = "item") |>
                  echarts4r::e_title(text = dim, left = "center", top = 5,
                                     textStyle = list(fontSize = 14)) |>
                  echarts4r::e_legend(show = FALSE) |>
                  echarts4r::e_grid(top = 40, bottom = 60, left = 60, right = 20) |>
                  echarts4r::e_x_axis(
                    axisLabel = list(rotate = 45, fontSize = 10, interval = 0)
                  ) |>
                  echarts4r::e_y_axis(
                    axisLabel = list(
                      formatter = htmlwidgets::JS(
                        "function(value) { return value >= 1e6 ? (value/1e6).toFixed(1) + 'M' : value >= 1e3 ? (value/1e3).toFixed(0) + 'K' : value; }"
                      )
                    )
                  )
              })
            })
          })

          # Active filters display
          output$active_filters <- shiny::renderText({
            filters <- r_filters()
            nrows <- nrow(filtered_data())
            total <- nrow(data())

            if (length(filters) == 0) {
              paste0("No filters active - click on bars to filter (", format(total, big.mark = ","), " rows)")
            } else {
              filter_text <- paste(
                names(filters),
                "=",
                vapply(filters, function(x) paste(x, collapse = ", "), character(1)),
                collapse = " | "
              )
              paste0(filter_text, " (", format(nrows, big.mark = ","), " / ", format(total, big.mark = ","), " rows)")
            }
          })

          # Build filter expression that returns filtered data
          list(
            expr = shiny::reactive({
              filters <- r_filters()

              if (length(filters) == 0) {
                return(quote(identity(data)))
              }

              # Build filter conditions for each dimension
              conditions <- lapply(names(filters), function(dim) {
                val <- filters[[dim]]
                call("==", as.name(dim), val)
              })

              # Combine conditions with &
              if (length(conditions) == 1) {
                combined <- conditions[[1]]
              } else {
                combined <- Reduce(function(a, b) call("&", a, b), conditions)
              }

              # Return dplyr::filter expression
              as.call(list(quote(dplyr::filter), quote(data), combined))
            }),
            state = list(
              dimensions = active_dimensions,
              measure = active_measure
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)

      shiny::tagList(
        shiny::div(
          class = "visual-filter-container",
          style = "padding: 10px;",

          # Header with clear button
          shiny::div(
            class = "visual-filter-header",
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            shiny::tags$h5("Visual Filter", style = "margin: 0;"),
            shiny::actionButton(
              ns("clear_filters"),
              "Clear Filters",
              class = "btn-sm btn-outline-secondary"
            )
          ),

          # Active filters display
          shiny::div(
            class = "active-filters",
            style = "background: #f8f9fa; padding: 8px; border-radius: 4px; margin-bottom: 15px; font-size: 12px;",
            shiny::textOutput(ns("active_filters"))
          ),

          # Charts grid (populated by server)
          shiny::uiOutput(ns("charts_grid"))
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    class = "visual_filter_block",
    ...
  )
}


#' @method block_output visual_filter_block
#' @export
block_output.visual_filter_block <- function(x, result, session) {
  # Output is handled by the server's renderUI for charts_grid
  NULL
}


#' @method block_ui visual_filter_block
#' @export
block_ui.visual_filter_block <- function(id, x, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("charts_grid"))
  )
}


#' @method block_render_trigger visual_filter_block
#' @export
block_render_trigger.visual_filter_block <- function(x, session = blockr.core::get_session()) {
  NULL
}
