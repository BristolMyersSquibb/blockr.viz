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
#'   Default is the first numeric column found. User can change via dropdown.
#' @param chart_types Named list. Chart type per dimension ("bar", "pie", or "row").
#'   Default is "bar" for all dimensions.
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
new_visual_filter_block <- function(dimensions = NULL, measure = NULL, chart_types = "bar", ...) {
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
              return(list(
                all_columns = character(),
                suggested_dimensions = character(),
                measures = character()
              ))
            }

            is_numeric <- vapply(df, is.numeric, logical(1))
            list(
              all_columns = names(df),
              suggested_dimensions = names(df)[!is_numeric],
              measures = names(df)[is_numeric]
            )
          })

          # State: selected dimensions
          r_dimensions <- shiny::reactiveVal(dimensions)

          # State: selected measure
          r_measure <- shiny::reactiveVal(measure)

          # Update dimension choices when data changes
          shiny::observeEvent(column_info(), {
            info <- column_info()

            # Update dimensions
            current_dims <- r_dimensions()
            if (is.null(current_dims) || !all(current_dims %in% info$all_columns)) {
              # Default to suggested (non-numeric) dimensions, max 4
              current_dims <- head(info$suggested_dimensions, 4)
              r_dimensions(current_dims)
            }

            shiny::updateSelectizeInput(
              session, "dimensions",
              choices = info$all_columns,
              selected = current_dims
            )
          })

          # Update measure choices when data or dimensions change
          # Exclude selected dimensions from measure choices
          shiny::observe({
            info <- column_info()
            dims <- r_dimensions()

            # Available measures = numeric columns minus selected dimensions
            available_measures <- setdiff(info$measures, dims)

            current_meas <- r_measure()
            if (is.null(current_meas) || !(current_meas %in% available_measures)) {
              current_meas <- if (length(available_measures) > 0) available_measures[1] else NULL
              r_measure(current_meas)
            }

            shiny::updateSelectInput(
              session, "measure",
              choices = available_measures,
              selected = current_meas
            )
          })

          # State: chart type - either "bar"/"pie"/"row" for all, or named list per dimension
          r_chart_types <- shiny::reactiveVal(chart_types)

          # Update state from UI
          shiny::observeEvent(input$dimensions, {
            r_dimensions(input$dimensions)
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$measure, {
            r_measure(input$measure)
          }, ignoreInit = TRUE)

          # Active dimensions (from user selection, max 4)
          active_dimensions <- shiny::reactive({
            dims <- r_dimensions()
            head(dims, 4)
          })

          # Active measure (reactive wrapper)
          active_measure <- shiny::reactive({
            r_measure()
          })

          # Get chart type for a dimension
          get_chart_type <- function(dim) {
            types <- r_chart_types()
            # If it's a string, use for all dimensions
            if (is.character(types) && length(types) == 1) return(types)
            # If it's a list, look up the dimension
            if (is.list(types) && dim %in% names(types)) return(types[[dim]])
            "bar"
          }

          # State: active filters per dimension
          r_filters <- shiny::reactiveVal(list())

          # Clear all filters
          shiny::observeEvent(input$clear_filters, {
            r_filters(list())
          })

          # Filtered data based on active filters (for output/downstream)
          # Uses dplyr::filter for database backend compatibility
          filtered_data <- shiny::reactive({
            df <- data()
            shiny::req(is.data.frame(df))

            filters <- r_filters()
            for (dim in names(filters)) {
              val <- filters[[dim]]
              if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
                df <- dplyr::filter(df, .data[[dim]] %in% val)
              }
            }
            df
          })

          # Crossfilter: data filtered by OTHER dimensions (excluding one)
          # This allows each chart to show all its values while respecting other filters
          crossfilter_data <- function(exclude_dim) {
            df <- data()
            shiny::req(is.data.frame(df))

            filters <- r_filters()
            for (dim in names(filters)) {
              if (dim == exclude_dim) next
              val <- filters[[dim]]
              if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
                df <- dplyr::filter(df, .data[[dim]] %in% val)
              }
            }
            df
          }

          # Render the charts grid UI
          output$charts_grid <- shiny::renderUI({
            dims <- active_dimensions()

            if (length(dims) == 0) {
              return(shiny::div(
                style = "padding: 20px; text-align: center; color: #666;",
                "No dimension columns found in data"
              ))
            }

            # Create a row of charts with per-chart type selectors
            shiny::div(
              class = "charts-row",
              style = "display: flex; flex-wrap: wrap; gap: 10px;",
              lapply(dims, function(dim) {
                current_type <- get_chart_type(dim)
                shiny::div(
                  style = "flex: 1; min-width: 200px;",
                  # Chart type dropdown (compact)
                  shiny::div(
                    style = "display: flex; justify-content: flex-end; margin-bottom: 2px;",
                    shiny::tags$select(
                      id = ns(paste0(dim, "_chart_type")),
                      class = "form-select form-select-sm",
                      style = "width: auto; padding: 2px 24px 2px 6px; font-size: 0.75rem;",
                      onchange = sprintf("Shiny.setInputValue('%s', this.value)", ns(paste0(dim, "_chart_type"))),
                      shiny::tags$option(value = "bar", selected = if (current_type == "bar") "selected" else NULL, "Bar"),
                      shiny::tags$option(value = "pie", selected = if (current_type == "pie") "selected" else NULL, "Pie"),
                      shiny::tags$option(value = "row", selected = if (current_type == "row") "selected" else NULL, "Row")
                    )
                  ),
                  echarts4r::echarts4rOutput(ns(paste0(dim, "_chart")), height = "220px")
                )
              })
            )
          })

          # Track created observers to avoid duplicates
          created_observers <- shiny::reactiveVal(character())

          # Create click handlers and chart type handlers for each dimension
          shiny::observe({
            dims <- active_dimensions()
            existing <- created_observers()
            new_dims <- setdiff(dims, existing)

            if (length(new_dims) > 0) {
              for (dim in new_dims) {
                local({
                  my_dim <- dim
                  input_id <- paste0(my_dim, "_chart_clicked_data")
                  type_id <- paste0(my_dim, "_chart_type")

                  # Chart type change handler
                  shiny::observeEvent(input[[type_id]], {
                    new_type <- input[[type_id]]
                    if (!is.null(new_type)) {
                      current <- r_chart_types()
                      # Convert string to list if needed
                      if (!is.list(current)) current <- list()
                      current[[my_dim]] <- new_type
                      r_chart_types(current)
                    }
                  }, ignoreInit = TRUE)

                  shiny::observeEvent(input[[input_id]], {
                    clicked <- input[[input_id]]

                    # echarts4r returns data differently for different chart types:
                    # - Bar charts: value = c(category_name, numeric_value)
                    # - Pie charts: name = category_name, value = numeric_value
                    clicked_value <- if (!is.null(clicked$name)) {
                      clicked$name  # Pie chart format
                    } else {
                      clicked$value[1]  # Bar chart format
                    }

                    if (!is.null(clicked_value) && clicked_value != "") {
                      current <- r_filters()
                      current_vals <- current[[my_dim]]

                      # Toggle: if already in selection, remove; else add
                      if (clicked_value %in% current_vals) {
                        current_vals <- setdiff(current_vals, clicked_value)
                        current[[my_dim]] <- if (length(current_vals) == 0) NULL else current_vals
                      } else {
                        current[[my_dim]] <- c(current_vals, clicked_value)
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
            chart_types <- r_chart_types()  # Dependency to trigger re-render
            shiny::req(length(dims) > 0, meas)

            lapply(dims, function(dim) {
              output[[paste0(dim, "_chart")]] <- echarts4r::renderEcharts4r({
                # Get chart type for this dimension
                chart_type <- get_chart_type(dim)

                # Crossfilter: show all values of this dim, filtered by OTHER dims
                df <- crossfilter_data(dim)
                shiny::req(nrow(df) > 0)

                # Aggregate by dimension using dplyr (DB-compatible)
                agg <- dplyr::summarise(
                  df,
                  !!meas := sum(.data[[meas]], na.rm = TRUE),
                  .by = dplyr::all_of(dim)
                )

                # Convert dimension to character (so numeric dims like Year show as categories)
                agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))

                # Sort by measure descending
                agg <- dplyr::arrange(agg, dplyr::desc(.data[[meas]]))

                # Get current filter for highlighting
                current_filter <- r_filters()[[dim]]

                # Create colors based on selection (supports multi-select)
                # Selected: solid blue, unselected: very light blue with transparency
                colors <- if (!is.null(current_filter) && length(current_filter) > 0) {
                  ifelse(agg[[dim]] %in% current_filter, "#5470c6", "rgba(84, 112, 198, 0.2)")
                } else {
                  "#5470c6"
                }

                # Build chart based on type
                chart <- agg |> echarts4r::e_charts_(dim)

                if (chart_type == "pie") {
                  # Pie chart - needs colors in data for proper slice coloring
                  # Use a color palette, with selected items highlighted
                  palette <- c("#5470c6", "#91cc75", "#fac858", "#ee6666",
                               "#73c0de", "#3ba272", "#fc8452", "#9a60b4",
                               "#ea7ccc", "#48b8d0")
                  if (!is.null(current_filter) && length(current_filter) > 0) {
                    # Dim unselected slices
                    pie_colors <- ifelse(
                      agg[[dim]] %in% current_filter,
                      palette[seq_len(nrow(agg)) %% length(palette) + 1],
                      "rgba(200, 200, 200, 0.3)"
                    )
                  } else {
                    pie_colors <- palette[seq_len(nrow(agg)) %% length(palette) + 1]
                  }
                  agg$pie_color <- pie_colors

                  # Rebuild chart with pie_color column included
                  chart <- agg |>
                    echarts4r::e_charts_(dim) |>
                    echarts4r::e_pie_(meas, name = dim,
                      radius = c("20%", "70%"),
                      label = list(
                        show = TRUE,
                        minAngle = 20,  # Only show labels for slices > 20 degrees
                        formatter = "{b}",
                        fontSize = 10
                      ),
                      labelLine = list(
                        show = TRUE,
                        length = 5,
                        length2 = 5
                      ),
                      emphasis = list(label = list(show = TRUE, fontSize = 12))
                    ) |>
                    echarts4r::e_add_nested("itemStyle", color = pie_color)
                } else if (chart_type == "row") {
                  # Row chart (horizontal bars)
                  chart <- chart |>
                    echarts4r::e_bar_(meas, name = meas,
                      itemStyle = list(color = htmlwidgets::JS(
                        sprintf("function(params) { var colors = %s; return colors[params.dataIndex] || colors; }",
                                jsonlite::toJSON(colors))
                      ))
                    ) |>
                    echarts4r::e_flip_coords() |>
                    echarts4r::e_grid(top = 40, bottom = 20, left = 80, right = 20) |>
                    echarts4r::e_y_axis(
                      axisLabel = list(fontSize = 10, interval = 0)
                    ) |>
                    echarts4r::e_x_axis(
                      axisLabel = list(
                        formatter = htmlwidgets::JS(
                          "function(value) { return value >= 1e6 ? (value/1e6).toFixed(1) + 'M' : value >= 1e3 ? (value/1e3).toFixed(0) + 'K' : value; }"
                        )
                      )
                    )
                } else {
                  # Bar chart (default)
                  chart <- chart |>
                    echarts4r::e_bar_(meas, name = meas,
                      itemStyle = list(color = htmlwidgets::JS(
                        sprintf("function(params) { var colors = %s; return colors[params.dataIndex] || colors; }",
                                jsonlite::toJSON(colors))
                      ))
                    ) |>
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
                }

                # Common options
                chart |>
                  echarts4r::e_tooltip(trigger = "item") |>
                  echarts4r::e_title(text = dim, left = "center", top = 5,
                                     textStyle = list(fontSize = 14)) |>
                  echarts4r::e_legend(show = FALSE)
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

              # Build filter conditions for each dimension (supports multi-select)
              conditions <- lapply(names(filters), function(dim) {
                val <- filters[[dim]]
                if (length(val) == 1) {
                  call("==", as.name(dim), val)
                } else {
                  call("%in%", as.name(dim), val)
                }
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
              dimensions = r_dimensions,
              measure = r_measure,
              chart_types = r_chart_types
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)

      shiny::tagList(
        # CSS for advanced toggle
        shiny::tags$style(shiny::HTML(sprintf(
          "
          #%s {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.3s ease-out;
          }
          #%s.expanded {
            max-height: 500px;
            overflow: visible;
            transition: max-height 0.5s ease-in;
          }
          .visual-filter-advanced-toggle {
            cursor: pointer;
            user-select: none;
            padding: 4px 0;
            display: flex;
            align-items: center;
            gap: 6px;
            font-size: 0.75rem;
            color: #6c757d;
          }
          .visual-filter-chevron {
            transition: transform 0.2s;
            display: inline-block;
            font-size: 12px;
            font-weight: bold;
          }
          .visual-filter-chevron.rotated {
            transform: rotate(90deg);
          }
          ",
          ns("advanced-options"),
          ns("advanced-options")
        ))),

        shiny::div(
          class = "visual-filter-container",
          style = "padding: 10px;",

          # Active filters display (always visible)
          shiny::div(
            class = "text-muted",
            style = "font-size: 0.8rem; margin-bottom: 10px;",
            shiny::textOutput(ns("active_filters"))
          ),

          # Charts grid (always visible)
          shiny::uiOutput(ns("charts_grid")),

          # Toggle for advanced options
          shiny::div(
            class = "visual-filter-advanced-toggle",
            id = ns("advanced-toggle"),
            onclick = sprintf(
              "
              const section = document.getElementById('%s');
              const chevron = document.querySelector('#%s .visual-filter-chevron');
              section.classList.toggle('expanded');
              chevron.classList.toggle('rotated');
              ",
              ns("advanced-options"),
              ns("advanced-toggle")
            ),
            shiny::tags$span(class = "visual-filter-chevron", "\u203A"),
            "Settings"
          ),

          # Advanced options (collapsed by default)
          shiny::div(
            id = ns("advanced-options"),
            style = "padding-top: 10px;",
            shiny::div(
              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
              shiny::div(
                style = "display: flex; align-items: center; gap: 5px;",
                shiny::tags$label("Dimensions:", style = "margin: 0; font-size: 12px;"),
                shiny::selectizeInput(
                  ns("dimensions"),
                  label = NULL,
                  choices = NULL,
                  multiple = TRUE,
                  width = "250px",
                  options = list(plugins = list("remove_button"))
                )
              ),
              shiny::div(
                style = "display: flex; align-items: center; gap: 5px;",
                shiny::tags$label("Measure:", style = "margin: 0; font-size: 12px;"),
                shiny::selectInput(
                  ns("measure"),
                  label = NULL,
                  choices = NULL,
                  width = "150px"
                )
              ),
              shiny::actionButton(
                ns("clear_filters"),
                "Clear Filters",
                class = "btn-sm btn-outline-secondary"
              )
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
    class = "visual_filter_block",
    ...
  )
}
