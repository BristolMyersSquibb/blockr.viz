#' @importFrom blockr.core new_transform_block block_output block_ui block_render_trigger
#' @importFrom rlang .data :=
#' @importFrom stats setNames
#' @importFrom utils head
NULL

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(".count", ".selected"))

#' Table Filter Block
#'
#' A table-based crossfilter block using reactable. Shows multiple sortable,
#' searchable tables for different dimensions with inline bar charts.
#' Clicking on a row filters the data to only rows matching that selection.
#' Returns the filtered data frame.
#'
#' @param dimensions Character vector. Which columns to show as dimension tables.
#'   If NULL, auto-detects non-numeric columns (up to 4).
#' @param measure Character. Which numeric column to aggregate in the tables.
#'   Default is the first numeric column found. User can change via dropdown.
#' @param filters Named list. Active filters per dimension. Each element is a character
#'   vector of selected values. Default is empty list (no filters). This is saved as
#'   part of block state and restored when the block is restored.
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
#'     new_table_filter_block(),
#'     data = list(data = bi_demo_data())
#'   )
#' }
new_table_filter_block <- function(dimensions = NULL, measure = NULL, filters = list(), ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # Detect dimensions and measures from data
          # Numeric columns with few unique values (<=10) are treated as dimensions
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

            # Low-cardinality numeric columns are dimensions (e.g., Year, Quarter)
            is_low_cardinality <- vapply(df, function(col) {
              length(unique(col)) <= 10
            }, logical(1))

            # Dimension: non-numeric OR (numeric AND low-cardinality)
            is_dimension <- !is_numeric | (is_numeric & is_low_cardinality)

            list(
              all_columns = names(df),
              suggested_dimensions = names(df)[is_dimension],
              measures = names(df)[is_numeric & !is_low_cardinality]
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
          # Always include "Count" as an option
          shiny::observe({
            info <- column_info()
            dims <- r_dimensions()

            # Available measures = numeric columns minus selected dimensions
            numeric_measures <- setdiff(info$measures, dims)

            # Always add Count as first option
            available_measures <- c("Count" = ".count", stats::setNames(numeric_measures, numeric_measures))

            current_meas <- r_measure()
            if (is.null(current_meas) || !(current_meas %in% available_measures)) {
              # Default to first numeric measure if available, otherwise Count
              current_meas <- if (length(numeric_measures) > 0) numeric_measures[1] else ".count"
              r_measure(current_meas)
            }

            shiny::updateSelectInput(
              session, "measure",
              choices = available_measures,
              selected = current_meas
            )
          })

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

          # State: active filters per dimension (initialized from parameter)
          r_filters <- shiny::reactiveVal(filters)

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
          # This allows each table to show all its values while respecting other filters
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

          # Handle row clicks from any table
          shiny::observeEvent(input$table_click, {
            click <- input$table_click
            if (is.null(click)) return()

            dim <- click$dim
            value <- click$value

            if (!is.null(value) && value != "") {
              current <- r_filters()
              current_vals <- current[[dim]]

              # Toggle: if already in selection, remove; else add
              if (value %in% current_vals) {
                current_vals <- setdiff(current_vals, value)
                current[[dim]] <- if (length(current_vals) == 0) NULL else current_vals
              } else {
                current[[dim]] <- c(current_vals, value)
              }
              r_filters(current)
            }
          }, ignoreInit = TRUE)

          # Helper to format numbers compactly
          format_number <- function(x) {
            ifelse(
              x >= 1e6, paste0(round(x / 1e6, 1), "M"),
              ifelse(x >= 1e3, paste0(round(x / 1e3, 0), "K"), format(round(x), big.mark = ","))
            )
          }

          # Build a single filter table for a dimension
          build_filter_table <- function(dim, meas) {
            # Crossfilter: show all values of this dim, filtered by OTHER dims
            df <- crossfilter_data(dim)
            shiny::req(nrow(df) > 0)

            # Aggregate by dimension
            if (meas == ".count") {
              agg <- dplyr::summarise(
                df,
                .count = dplyr::n(),
                .by = dplyr::all_of(dim)
              )
              value_col <- ".count"
            } else {
              agg <- dplyr::summarise(
                df,
                !!meas := sum(.data[[meas]], na.rm = TRUE),
                .by = dplyr::all_of(dim)
              )
              value_col <- meas
            }

            # Convert dimension to character
            agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))

            # Sort by value descending
            agg <- dplyr::arrange(agg, dplyr::desc(.data[[value_col]]))

            # Get current filter for highlighting
            current_filter <- r_filters()[[dim]]
            has_filter <- !is.null(current_filter) && length(current_filter) > 0

            # Add selection indicator
            agg$.selected <- if (has_filter) {
              agg[[dim]] %in% current_filter
            } else {
              TRUE  # All selected when no filter
            }

            # Calculate max for bar scaling
            max_val <- max(agg[[value_col]], na.rm = TRUE)
            if (is.na(max_val) || max_val == 0) max_val <- 1

            # Build columns list with dynamic names
            columns_list <- list()
            columns_list[[".selected"]] <- reactable::colDef(show = FALSE)

            # Label column (dimension)
            columns_list[[dim]] <- reactable::colDef(
              name = dim,
              minWidth = 120,
              cell = function(value, index) {
                is_selected <- agg$.selected[index]
                style <- if (has_filter && !is_selected) {
                  "color: #999;"
                } else {
                  "font-weight: 500;"
                }
                shiny::tags$span(style = style, value)
              }
            )

            # Value column with inline bar
            columns_list[[value_col]] <- reactable::colDef(
              name = if (meas == ".count") "Count" else meas,
              minWidth = 120,
              align = "right",
              cell = function(value, index) {
                is_selected <- agg$.selected[index]
                pct <- value / max_val * 100
                bar_color <- if (has_filter && !is_selected) {
                  "rgba(84, 112, 198, 0.2)"
                } else {
                  "#5470c6"
                }
                text_color <- if (has_filter && !is_selected) "#999" else "#333"

                # Bar + number, right-aligned
                shiny::div(
                  style = "display: flex; align-items: center; justify-content: flex-end; gap: 6px;",
                  # Bar container
                  shiny::div(
                    style = "flex: 1; max-width: 80px; height: 14px; background: #f0f0f0; border-radius: 2px; overflow: hidden;",
                    shiny::div(
                      style = sprintf(
                        "height: 100%%; width: %.1f%%; background: %s;",
                        pct, bar_color
                      )
                    )
                  ),
                  shiny::span(
                    style = sprintf("color: %s; font-size: 12px; width: 38px; text-align: right;", text_color),
                    format_number(value)
                  )
                )
              }
            )

            # Create reactable (no checkboxes - just click rows to filter)
            reactable::reactable(
              agg,
              columns = columns_list,
              onClick = htmlwidgets::JS(sprintf(
                "function(rowInfo, column) {
                  Shiny.setInputValue('%s', {dim: '%s', value: rowInfo.row['%s']}, {priority: 'event'});
                }",
                ns("table_click"), dim, dim
              )),
              searchable = TRUE,
              compact = TRUE,
              borderless = TRUE,
              highlight = TRUE,
              height = 200,
              pagination = FALSE,
              theme = reactable::reactableTheme(
                searchInputStyle = list(fontSize = "12px", padding = "4px 8px"),
                headerStyle = list(fontSize = "12px", fontWeight = "600")
              )
            )
          }

          # Render the tables grid UI
          output$tables_grid <- shiny::renderUI({
            dims <- active_dimensions()
            meas <- active_measure()

            if (length(dims) == 0) {
              return(shiny::div(
                style = "padding: 20px; text-align: center; color: #666;",
                "No dimension columns found in data"
              ))
            }

            shiny::req(meas)

            # Create a row of tables
            shiny::div(
              class = "tables-row",
              style = "display: flex; flex-wrap: wrap; gap: 16px;",
              lapply(dims, function(dim) {
                shiny::div(
                  style = "flex: 1; min-width: 280px; max-width: 450px;",
                  shiny::tags$div(
                    style = "font-weight: 600; font-size: 14px; margin-bottom: 8px; color: #333;",
                    dim
                  ),
                  build_filter_table(dim, meas)
                )
              })
            )
          })

          # Active filters display with conditional Clear button
          output$filter_status <- shiny::renderUI({
            filters <- r_filters()
            nrows <- nrow(filtered_data())
            total <- nrow(data())

            has_filters <- length(filters) > 0

            if (has_filters) {
              filter_text <- paste(
                names(filters),
                "=",
                vapply(filters, function(x) paste(x, collapse = ", "), character(1)),
                collapse = " | "
              )
              status_text <- paste0(filter_text, " (", format(nrows, big.mark = ","), " / ", format(total, big.mark = ","), " rows)")
            } else {
              status_text <- paste0("No filters active - click on rows to filter (", format(total, big.mark = ","), " rows)")
            }

            shiny::div(
              style = "display: flex; align-items: center; gap: 10px; margin: 12px 0 8px 0;",
              shiny::span(
                class = "text-muted",
                style = "font-size: 0.8rem;",
                status_text
              ),
              if (has_filters) {
                shiny::actionButton(
                  ns("clear_filters"),
                  "Remove Filter",
                  class = "btn btn-outline-secondary btn-sm",
                  style = "font-size: 0.7rem; padding: 1px 6px; opacity: 0.6;"
                )
              }
            )
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
              filters = r_filters
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)

      shiny::tagList(
        # CSS for advanced toggle (consistent with blockr.dplyr)
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
          .block-advanced-toggle {
            cursor: pointer;
            user-select: none;
            padding: 8px 0;
            margin-bottom: 0;
            display: flex;
            align-items: center;
            gap: 6px;
            font-size: 0.8125rem;
          }
          .block-chevron {
            transition: transform 0.2s;
            display: inline-block;
            font-size: 14px;
            font-weight: bold;
          }
          .block-chevron.rotated {
            transform: rotate(90deg);
          }
          ",
          ns("advanced-options"),
          ns("advanced-options")
        ))),

        shiny::div(
          class = "table-filter-container",
          style = "padding: 10px;",

          # Tables grid (always visible)
          shiny::uiOutput(ns("tables_grid")),

          # Filter status with conditional clear button
          shiny::uiOutput(ns("filter_status")),

          # Advanced options toggle (consistent with blockr.dplyr)
          shiny::div(
            class = "block-advanced-toggle text-muted",
            id = ns("advanced-toggle"),
            onclick = sprintf(
              "
              const section = document.getElementById('%s');
              const chevron = document.querySelector('#%s .block-chevron');
              section.classList.toggle('expanded');
              chevron.classList.toggle('rotated');
              ",
              ns("advanced-options"),
              ns("advanced-toggle")
            ),
            shiny::tags$span(class = "block-chevron", "\u203A"),
            "Show advanced options"
          ),

          # Advanced options section (collapsed by default)
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
    allow_empty_state = c("dimensions", "measure", "filters"),
    class = "table_filter_block",
    ...
  )
}
