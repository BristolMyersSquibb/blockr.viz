#' Aggregate Block
#'
#' A block for exploring data by grouping and summarizing. Select "drill down"
#' columns to group by and "value" columns to aggregate.
#'
#' @param drill_down Character vector. Columns to group by. If NULL, auto-detected
#'   as non-numeric columns.
#' @param values Character vector. Columns to summarize. If NULL, auto-detected
#'   as numeric columns.
#' @param agg_fun Character. Aggregation function: "sum", "mean", "median", "min", "max".
#'   Default is "sum".
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block for aggregate analysis
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.bi)
#'   serve(
#'     new_aggregate_block(),
#'     data = list(data = bi_demo_data())
#'   )
#' }
new_aggregate_block <- function(
    drill_down = character(),
    values = character(),
    agg_fun = "sum",
    ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # State
          r_drill_down <- shiny::reactiveVal(drill_down)
          r_values <- shiny::reactiveVal(values)
          r_agg_fun <- shiny::reactiveVal(agg_fun)

          # Auto-detect column types
          column_info <- shiny::reactive({
            df <- data()
            if (!is.data.frame(df) || ncol(df) == 0) {
              return(list(drill_down = character(), values = character()))
            }

            # Detect drill down (non-numeric) and value (numeric) columns
            is_numeric <- vapply(df, is.numeric, logical(1))
            list(
              drill_down = names(df)[!is_numeric],
              values = names(df)[is_numeric]
            )
          })

          # Update UI choices when data changes
          shiny::observeEvent(column_info(), {
            info <- column_info()

            # Current selections
            current_dd <- r_drill_down()
            current_val <- r_values()

            # Valid selections (intersect with available)
            valid_dd <- intersect(current_dd, info$drill_down)
            valid_val <- intersect(current_val, info$values)

            # If no valid selections, use defaults
            if (length(valid_dd) == 0 && length(info$drill_down) > 0) {
              # Select first drill down column by default
              valid_dd <- info$drill_down[1]
            }
            if (length(valid_val) == 0 && length(info$values) > 0) {
              # Select all value columns by default
              valid_val <- info$values
            }

            shiny::updateSelectizeInput(
              session, "drill_down",
              choices = info$drill_down,
              selected = valid_dd
            )

            shiny::updateSelectizeInput(
              session, "values",
              choices = info$values,
              selected = valid_val
            )
          })

          # Update state from UI
          shiny::observeEvent(input$drill_down, {
            r_drill_down(input$drill_down)
          }, ignoreNULL = FALSE)

          shiny::observeEvent(input$values, {
            r_values(input$values)
          }, ignoreNULL = FALSE)

          shiny::observeEvent(input$agg_fun, {
            r_agg_fun(input$agg_fun)
          }, ignoreInit = TRUE)

          list(
            expr = shiny::reactive({
              dd_cols <- r_drill_down()
              val_cols <- r_values()
              agg <- r_agg_fun()

              shiny::req(length(val_cols) > 0)

              # Build the dplyr expression
              build_aggregate_expr(dd_cols, val_cols, agg)
            }),
            state = list(
              drill_down = r_drill_down,
              values = r_values,
              agg_fun = r_agg_fun
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::tags$p(
            class = "text-muted mb-2",
            "Group by drill down columns and summarize value columns."
          ),
          shiny::selectizeInput(
            ns("drill_down"),
            label = "Drill down by",
            choices = drill_down,
            selected = drill_down,
            multiple = TRUE,
            options = list(
              placeholder = "Select columns to group by...",
              plugins = list("remove_button")
            )
          ),
          shiny::selectizeInput(
            ns("values"),
            label = "Summarize",
            choices = values,
            selected = values,
            multiple = TRUE,
            options = list(
              placeholder = "Select columns to summarize...",
              plugins = list("remove_button")
            )
          ),
          shiny::selectInput(
            ns("agg_fun"),
            label = "Aggregation",
            choices = c("Sum" = "sum", "Mean" = "mean", "Median" = "median",
                        "Min" = "min", "Max" = "max", "Count" = "n"),
            selected = agg_fun
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    allow_empty_state = c("drill_down"),
    class = "aggregate_block",
    ...
  )
}


#' Build dplyr expression for aggregate
#'
#' @param drill_down Character vector of columns to group by
#' @param values Character vector of columns to summarize
#' @param agg_fun Character, aggregation function name
#'
#' @return An unevaluated R expression
#' @noRd
build_aggregate_expr <- function(drill_down, values, agg_fun) {
  # If no drill down columns, just summarize everything
  if (length(drill_down) == 0) {
    # Build: summarise(data, across(c(col1, col2), sum, na.rm = TRUE), Count = n())
    value_syms <- lapply(values, as.name)
    cols_call <- as.call(c(quote(c), value_syms))

    if (agg_fun == "n") {
      # Count only
      return(quote(dplyr::summarise(data, Count = dplyr::n())))
    }

    agg_fn_sym <- as.name(agg_fun)
    return(bquote(
      dplyr::summarise(
        data,
        dplyr::across(.(cols_call), ~ .(agg_fn_sym)(.x, na.rm = TRUE)),
        Count = dplyr::n()
      )
    ))
  }

  # With drill down columns: use nested calls instead of pipe
  dd_syms <- lapply(drill_down, as.name)
  value_syms <- lapply(values, as.name)
  cols_call <- as.call(c(quote(c), value_syms))

  # Build group_by call: dplyr::group_by(data, col1, col2, ...)
  group_call <- as.call(c(quote(dplyr::group_by), quote(data), dd_syms))

  # Build summarise call wrapping the group_by
  agg_fn_sym <- as.name(agg_fun)
  if (agg_fun == "n") {
    bquote(
      dplyr::summarise(
        .(group_call),
        Count = dplyr::n(),
        .groups = "drop"
      )
    )
  } else {
    bquote(
      dplyr::summarise(
        .(group_call),
        dplyr::across(.(cols_call), ~ .(agg_fn_sym)(.x, na.rm = TRUE)),
        Count = dplyr::n(),
        .groups = "drop"
      )
    )
  }
}
