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
#' The UI uses the shared `Blockr.Select` widget library (also used by
#' `blockr.dplyr` blocks) with a gear-icon popover for rounding precision.
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
  init_state <- list(
    rows = as.character(rows),
    cols = as.character(cols),
    measures = as.character(measures),
    agg_fun = agg_fun,
    digits = if (is.null(digits)) "" else as.character(digits)
  )

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns
          r_state <- shiny::reactiveVal(init_state)

          self_write <- new.env(parent = emptyenv())
          self_write$active <- FALSE

          # Detect column types. Numeric columns with <= 10 unique values are
          # dimensions (e.g. Year, Quarter); others follow is.numeric.
          column_info <- shiny::reactive({
            df <- data()
            if (!is.data.frame(df) || ncol(df) == 0) {
              return(list(dimensions = character(), measures = character()))
            }
            is_numeric <- vapply(df, is.numeric, logical(1))
            is_low_cardinality <- vapply(df, function(col) {
              length(unique(col)) <= 10
            }, logical(1))
            is_dimension <- !is_numeric | (is_numeric & is_low_cardinality)
            list(
              dimensions = names(df)[is_dimension],
              measures = names(df)[is_numeric & !is_low_cardinality]
            )
          })

          shiny::observeEvent(column_info(), {
            info <- column_info()
            session$sendCustomMessage("pivot-table-columns", list(
              id = ns("pivot_input"),
              dimensions = as.list(info$dimensions),
              measures = as.list(info$measures)
            ))
            # Re-send state so the JS can reconcile selections against the
            # new option set (e.g. measures default to first available).
            s <- r_state()
            if (length(s$measures) == 0 && length(info$measures) > 0) {
              s$measures <- info$measures[1]
              r_state(s)
            }
            session$sendCustomMessage("pivot-table-update", list(
              id = ns("pivot_input"),
              state = r_state()
            ))
          })

          shiny::observeEvent(input$pivot_input, {
            self_write$active <- TRUE
            new_state <- input$pivot_input
            new_state$rows <- as.character(new_state$rows %||% character())
            new_state$cols <- as.character(new_state$cols %||% character())
            new_state$measures <- as.character(new_state$measures %||% character())
            new_state$agg_fun <- new_state$agg_fun %||% "sum"
            new_state$digits <- as.character(new_state$digits %||% "")
            r_state(new_state)
          })

          shiny::observeEvent(r_state(), {
            if (self_write$active) {
              self_write$active <- FALSE
            } else {
              session$sendCustomMessage("pivot-table-update", list(
                id = ns("pivot_input"),
                state = r_state()
              ))
            }

            # Push the "Aggregating over: …" hint to JS.
            info <- column_info()
            used <- c(r_state()$rows, r_state()$cols)
            unused <- setdiff(info$dimensions, used)
            txt <- if (length(info$dimensions) == 0) {
              ""
            } else if (length(unused) == 0) {
              "All dimensions mapped"
            } else {
              paste("Aggregating over:", paste(unused, collapse = ", "))
            }
            session$sendCustomMessage("pivot-table-aggregating-over", list(
              id = ns("pivot_input"),
              text = txt
            ))
          })

          list(
            expr = shiny::reactive({
              s <- r_state()
              shiny::req(length(s$measures) > 0)

              digs_raw <- s$digits
              digs <- if (is.null(digs_raw) || digs_raw == "") {
                NULL
              } else {
                suppressWarnings(as.integer(digs_raw))
              }

              build_pivot_expr(s$rows, s$cols, s$measures, s$agg_fun, digs)
            }),
            state = list(
              rows     = shiny::reactive(r_state()$rows),
              cols     = shiny::reactive(r_state()$cols),
              measures = shiny::reactive(r_state()$measures),
              agg_fun  = shiny::reactive(r_state()$agg_fun),
              digits   = shiny::reactive(r_state()$digits)
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      htmltools::tagList(
        blockr_core_js_dep(),
        blockr_blocks_css_dep(),
        blockr_select_dep(),
        pivot_table_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(
            id = ns("pivot_input"),
            class = "pivot-table-block-container"
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

pivot_table_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "pivot-table-block-js",
      version = utils::packageVersion("blockr.bi"),
      src = system.file("js", package = "blockr.bi"),
      script = "pivot-table-block.js"
    ),
    htmltools::htmlDependency(
      name = "pivot-table-block-css",
      version = utils::packageVersion("blockr.bi"),
      src = system.file("css", package = "blockr.bi"),
      stylesheet = "pivot-table-block.css"
    )
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
