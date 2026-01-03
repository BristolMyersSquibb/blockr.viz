#' Pivot Table Block
#'
#' A flexible pivot table block for slice & dice analysis. Map columns to
#' rows, columns, or leave them to be aggregated over. Inspired by Excel pivot
#' tables and dc.js-style dimensional analysis.
#'
#' @param rows Character vector. Columns to use as row headers (vertical axis).
#' @param cols Character vector. Columns to pivot into column headers (horizontal axis).
#'   If empty and multiple measures selected, measures become columns instead.
#' @param measures Character vector. Numeric columns to aggregate. Multiple measures
#'   are supported - if no column dimension is set, measures become columns.
#' @param agg_fun Character. Aggregation function: "sum", "mean", "median", "min", "max", "n".
#' @param digits Character or integer. Number of decimal places to round numeric results.
#'   Empty string "" (default) means no rounding.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that returns a pivoted data frame
#'
#' @details
#' The pivot table block provides a flexible way to reshape and aggregate data:
#'
#' - **Rows**: Dimensions that become row labels (like Region, Country)
#' - **Columns**: Dimension to pivot into columns (like Category, Year)
#' - **Measures**: Numeric values to aggregate (Revenue, Profit, etc.)
#' - **Aggregation**: How to combine values (sum, mean, etc.)
#'
#' Dimensions not placed in rows or columns are automatically aggregated over.
#'
#' **Multiple measures behavior:**
#' - No column dimension + 1 measure: Simple grouped table
#' - No column dimension + N measures: Measures become columns
#' - Column dimension + 1 measure: Dimension values become columns
#' - Column dimension + N measures: Nested columns (Dimension_Measure)
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.bi)
#'
#'   # Single measure pivoted by dimension
#'   serve(
#'     new_pivot_table_block(
#'       rows = "Region",
#'       cols = "Category",
#'       measures = "Revenue"
#'     ),
#'     data = list(data = bi_demo_data())
#'   )
#'
#'   # Multiple measures as columns (no column dimension)
#'   serve(
#'     new_pivot_table_block(
#'       rows = "Region",
#'       measures = c("Revenue", "Profit", "Quantity")
#'     ),
#'     data = list(data = bi_demo_data())
#'   )
#' }
new_pivot_table_block <- function(
    rows = character(),
    cols = character(),
    measures = character(),
    agg_fun = "sum",
    digits = "",
    ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # State
          r_rows <- shiny::reactiveVal(rows)
          r_cols <- shiny::reactiveVal(cols)
          r_measures <- shiny::reactiveVal(measures)
          r_agg_fun <- shiny::reactiveVal(agg_fun)
          r_digits <- shiny::reactiveVal(digits)

          # Detect column types from data
          # Numeric columns with few unique values (<=10) are treated as dimensions
          column_info <- shiny::reactive({
            df <- data()
            if (!is.data.frame(df) || ncol(df) == 0) {
              return(list(dimensions = character(), measures = character()))
            }

            is_numeric <- vapply(df, is.numeric, logical(1))

            # Low-cardinality numeric columns are dimensions (e.g., Year, Quarter)
            is_low_cardinality <- vapply(df, function(col) {
              length(unique(col)) <= 10
            }, logical(1))

            # Dimension: non-numeric OR (numeric AND low-cardinality)
            is_dimension <- !is_numeric | (is_numeric & is_low_cardinality)

            list(
              dimensions = names(df)[is_dimension],
              measures = names(df)[is_numeric & !is_low_cardinality]
            )
          })

          # Update UI choices when data changes
          shiny::observeEvent(column_info(), {
            info <- column_info()

            # Current selections - keep only valid ones
            current_rows <- intersect(r_rows(), info$dimensions)
            current_cols <- intersect(r_cols(), info$dimensions)
            current_meas <- intersect(r_measures(), info$measures)

            # Default to first measure if none selected
            if (length(current_meas) == 0 && length(info$measures) > 0) {
              current_meas <- info$measures[1]
              r_measures(current_meas)
            }

            # Update dropdowns
            shiny::updateSelectizeInput(
              session, "rows",
              choices = info$dimensions,
              selected = current_rows
            )

            shiny::updateSelectizeInput(
              session, "cols",
              choices = info$dimensions,
              selected = current_cols
            )

            shiny::updateSelectizeInput(
              session, "measures",
              choices = info$measures,
              selected = current_meas
            )
          })

          # Update state from UI
          shiny::observeEvent(input$rows, {
            r_rows(input$rows)
          }, ignoreNULL = FALSE)

          shiny::observeEvent(input$cols, {
            r_cols(input$cols)
          }, ignoreNULL = FALSE)

          shiny::observeEvent(input$measures, {
            r_measures(input$measures)
          }, ignoreNULL = FALSE)

          shiny::observeEvent(input$agg_fun, {
            r_agg_fun(input$agg_fun)
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$digits, {
            r_digits(input$digits)
          }, ignoreInit = TRUE)

          # Show what's being aggregated over
          output$aggregating_over <- shiny::renderText({
            info <- column_info()
            used <- c(r_rows(), r_cols())
            unused <- setdiff(info$dimensions, used)

            if (length(unused) == 0) {
              "All dimensions mapped"
            } else {
              paste("Aggregating over:", paste(unused, collapse = ", "))
            }
          })

          list(
            expr = shiny::reactive({
              row_cols <- r_rows()
              col_cols <- r_cols()
              meas_cols <- r_measures()
              agg <- r_agg_fun()
              digs_raw <- r_digits()

              # Convert empty string to NULL, otherwise to integer
              digs <- if (is.null(digs_raw) || digs_raw == "") {
                NULL
              } else {
                suppressWarnings(as.integer(digs_raw))
              }

              shiny::req(length(meas_cols) > 0)

              # Build pivot table expression
              build_pivot_expr(row_cols, col_cols, meas_cols, agg, digs)
            }),
            state = list(
              rows = r_rows,
              cols = r_cols,
              measures = r_measures,
              agg_fun = r_agg_fun,
              digits = r_digits
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        # CSS for responsive grid and advanced toggle
        shiny::tags$style(shiny::HTML(sprintf(
          "
          .pivot-form-grid {
            display: grid;
            gap: 15px;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          }
          #%s {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.3s ease-out;
          }
          #%s.expanded {
            max-height: 200px;
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
          class = "pivot-table-block",
          style = "padding: 10px;",

          # Responsive grid for inputs
          shiny::div(
            class = "pivot-form-grid",

            # Rows selector
            shiny::selectizeInput(
              ns("rows"),
              label = "Rows",
              choices = rows,
              selected = rows,
              multiple = TRUE,
              options = list(
                placeholder = "Row dimensions...",
                plugins = list("remove_button")
              )
            ),

            # Columns selector
            shiny::selectizeInput(
              ns("cols"),
              label = "Columns",
              choices = cols,
              selected = cols,
              multiple = TRUE,
              options = list(
                placeholder = "Column dimensions...",
                plugins = list("remove_button")
              )
            ),

            # Measures selector
            shiny::selectizeInput(
              ns("measures"),
              label = "Measures",
              choices = measures,
              selected = measures,
              multiple = TRUE,
              options = list(
                placeholder = "Select measures...",
                plugins = list("remove_button")
              )
            ),

            # Aggregation selector
            shiny::selectInput(
              ns("agg_fun"),
              label = "Aggregation",
              choices = c("Sum" = "sum", "Mean" = "mean", "Median" = "median",
                          "Min" = "min", "Max" = "max", "Count" = "n"),
              selected = agg_fun
            )
          ),

          # Show what's being aggregated over
          shiny::div(
            class = "text-muted",
            style = "font-size: 0.8rem; margin-top: 5px;",
            shiny::textOutput(ns("aggregating_over"))
          ),

          # Advanced options toggle (with more top spacing)
          shiny::div(
            class = "block-advanced-toggle text-muted",
            id = ns("advanced-toggle"),
            style = "margin-top: 10px;",
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
              style = "display: flex; align-items: center; gap: 10px;",
              shiny::tags$label("Round to", style = "margin: 0; font-size: 12px;"),
              shiny::textInput(
                ns("digits"),
                label = NULL,
                value = digits,
                width = "60px",
                placeholder = ""
              ),
              shiny::tags$span(
                class = "text-muted",
                style = "font-size: 12px;",
                "digits (empty = no rounding)"
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
    allow_empty_state = c("rows", "cols", "measures", "digits"),
    class = "pivot_table_block",
    ...
  )
}


#' Build dplyr expression for pivot table
#'
#' @param rows Character vector of row dimension columns
#' @param cols Character vector of column dimension columns
#' @param measures Character vector of measure column names (supports multiple)
#' @param agg_fun Character, aggregation function
#' @param digits Integer or NULL, number of decimal places for rounding
#'
#' @return An unevaluated R expression
#' @noRd
build_pivot_expr <- function(rows, cols, measures, agg_fun, digits = NULL) {
  # Helper to wrap result with rounding if digits specified
  wrap_with_round <- function(expr) {
    if (is.null(digits)) return(expr)
    bquote(
      dplyr::mutate(.(expr), dplyr::across(dplyr::where(is.numeric), ~ round(.x, .(digits))))
    )
  }

  # All grouping columns (rows + cols)
  group_cols <- c(rows, cols)

  # Get properly namespaced aggregation function
  # median is in stats package, others are in base
  agg_sym <- if (agg_fun == "median") {
    quote(stats::median)
  } else {
    as.name(agg_fun)
  }

  # Handle count aggregation specially
  if (agg_fun == "n") {
    if (length(group_cols) == 0) {
      return(wrap_with_round(quote(dplyr::summarise(data, Count = dplyr::n()))))
    }
    group_syms <- lapply(group_cols, as.name)
    group_call <- as.call(c(quote(dplyr::group_by), quote(data), group_syms))
    return(wrap_with_round(bquote(
      dplyr::summarise(.(group_call), Count = dplyr::n(), .groups = "drop")
    )))
  }

  # Build aggregation expressions for each measure
  # Result: summarise(grouped_data, meas1 = sum(meas1), meas2 = sum(meas2), ...)
  build_agg_call <- function(grouped_data_expr) {
    agg_exprs <- lapply(measures, function(m) {
      bquote(.(agg_sym)(.(as.name(m)), na.rm = TRUE))
    })
    names(agg_exprs) <- measures

    # Build: dplyr::summarise(data, m1 = agg(m1), m2 = agg(m2), ..., .groups = "drop")
    args <- c(list(grouped_data_expr), agg_exprs, list(.groups = "drop"))
    as.call(c(quote(dplyr::summarise), args))
  }

  # Case 1: No grouping - just aggregate all measures
  if (length(group_cols) == 0) {
    agg_exprs <- lapply(measures, function(m) {
      bquote(.(agg_sym)(.(as.name(m)), na.rm = TRUE))
    })
    names(agg_exprs) <- measures
    args <- c(list(quote(data)), agg_exprs)
    return(wrap_with_round(as.call(c(quote(dplyr::summarise), args))))
  }

  # Group by rows + cols
  group_syms <- lapply(group_cols, as.name)
  group_call <- as.call(c(quote(dplyr::group_by), quote(data), group_syms))
  agg_call <- build_agg_call(group_call)

  # Case 2: No column dimensions - measures stay as columns
  if (length(cols) == 0) {
    return(wrap_with_round(agg_call))
  }

  # Case 3 & 4: Column dimensions present - need to pivot
  # First pivot to long format (measure names as column), then pivot wider

  # For single measure: pivot cols to columns
  # For multiple measures: pivot cols × measures to columns

  if (length(measures) == 1) {
    # Single measure - simple pivot
    meas_sym <- as.name(measures)
    if (length(cols) == 1) {
      pivot_call <- bquote(
        tidyr::pivot_wider(
          .(agg_call),
          names_from = .(as.name(cols)),
          values_from = .(meas_sym),
          values_fill = 0
        )
      )
    } else {
      # Multiple col dimensions: unite first
      unite_call <- bquote(
        tidyr::unite(.(agg_call), "_pivot_col", dplyr::all_of(.(cols)), sep = " | ")
      )
      if (length(rows) > 0) {
        select_call <- bquote(
          dplyr::select(.(unite_call), dplyr::all_of(.(rows)), `_pivot_col`, .(meas_sym))
        )
      } else {
        select_call <- bquote(
          dplyr::select(.(unite_call), `_pivot_col`, .(meas_sym))
        )
      }
      pivot_call <- bquote(
        tidyr::pivot_wider(
          .(select_call),
          names_from = `_pivot_col`,
          values_from = .(meas_sym),
          values_fill = 0
        )
      )
    }
  } else {
    # Multiple measures - pivot to long first, then wide with combined names
    # Step 1: Aggregate (done above)
    # Step 2: Pivot measures to long format
    long_call <- bquote(
      tidyr::pivot_longer(
        .(agg_call),
        cols = dplyr::all_of(.(measures)),
        names_to = "_measure",
        values_to = "_value"
      )
    )

    # Step 3: Unite column dimensions + measure name
    if (length(cols) == 1) {
      unite_call <- bquote(
        tidyr::unite(.(long_call), "_pivot_col", .(as.name(cols)), `_measure`, sep = " | ")
      )
    } else {
      # Multiple cols: unite all cols + measure
      all_to_unite <- c(cols, "_measure")
      unite_call <- bquote(
        tidyr::unite(.(long_call), "_pivot_col", dplyr::all_of(.(all_to_unite)), sep = " | ")
      )
    }

    # Step 4: Select only rows + pivot col + value, then pivot wide
    if (length(rows) > 0) {
      select_call <- bquote(
        dplyr::select(.(unite_call), dplyr::all_of(.(rows)), `_pivot_col`, `_value`)
      )
    } else {
      select_call <- bquote(
        dplyr::select(.(unite_call), `_pivot_col`, `_value`)
      )
    }

    pivot_call <- bquote(
      tidyr::pivot_wider(
        .(select_call),
        names_from = `_pivot_col`,
        values_from = `_value`,
        values_fill = 0
      )
    )
  }

  wrap_with_round(pivot_call)
}
