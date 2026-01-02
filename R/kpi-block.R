#' @importFrom blockr.core block_output block_ui block_render_trigger
NULL

#' KPI Block
#'
#' A block for displaying one or more key performance indicators (KPIs) as
#' prominent numbers. Useful for dashboard headlines like "Revenue: $67M".
#' Select multiple measures to display them side by side with auto-assigned colors.
#'
#' @param measures Character vector. The numeric columns to aggregate.
#' @param agg_fun Character. Aggregation function: "sum", "mean", "median", "min", "max", "n".
#' @param prefix Character. Text before each number (e.g., "$").
#' @param suffix Character. Text after each number (e.g., "%", "M").
#' @param digits Integer. Decimal places for rounding. Default 0.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that displays KPIs
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.bi)
#'   library(blockr.dag)
#'
#'   # Multiple KPIs in one block
#'   serve(
#'     new_kpi_block(
#'       measures = c("Revenue", "Profit", "Transactions"),
#'       prefix = "$"
#'     ),
#'     data = list(data = bi_demo_data())
#'   )
#'
#'   # Simple dashboard
#'   run_app(
#'     blocks = c(
#'       data = new_static_block(bi_demo_data()),
#'       kpis = new_kpi_block(measures = c("Revenue", "Profit", "Transactions"))
#'     ),
#'     links = c(
#'       new_link("data", "kpis", "data")
#'     ),
#'     extensions = list(new_dag_extension())
#'   )
#' }
new_kpi_block <- function(
    measures = character(),
    agg_fun = "sum",
    prefix = "",
    suffix = "",
    digits = "0",
    ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # State
          r_measures <- shiny::reactiveVal(measures)
          r_agg_fun <- shiny::reactiveVal(agg_fun)
          r_prefix <- shiny::reactiveVal(prefix)
          r_suffix <- shiny::reactiveVal(suffix)
          r_digits <- shiny::reactiveVal(digits)

          # Detect numeric columns
          measure_choices <- shiny::reactive({
            df <- data()
            if (!is.data.frame(df) || ncol(df) == 0) {
              return(character())
            }
            is_numeric <- vapply(df, is.numeric, logical(1))
            names(df)[is_numeric]
          })

          # Update measures dropdown when data changes
          shiny::observeEvent(measure_choices(), {
            choices <- measure_choices()
            current <- r_measures()

            # Keep valid selections
            valid <- intersect(current, choices)
            if (length(valid) == 0 && length(choices) > 0) {
              # Select first few measures by default
              valid <- utils::head(choices, min(3, length(choices)))
            }
            r_measures(valid)

            shiny::updateSelectizeInput(
              session, "measures",
              choices = choices,
              selected = valid
            )
          })

          # Update state from UI
          shiny::observeEvent(input$measures, {
            r_measures(input$measures)
          }, ignoreNULL = FALSE, ignoreInit = TRUE)

          shiny::observeEvent(input$agg_fun, {
            r_agg_fun(input$agg_fun)
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$prefix, {
            r_prefix(input$prefix)
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$suffix, {
            r_suffix(input$suffix)
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$digits, {
            r_digits(input$digits)
          }, ignoreInit = TRUE)

          # Return summarized values for all measures
          list(
            expr = shiny::reactive({
              meas <- r_measures()
              agg <- r_agg_fun()
              digs_raw <- r_digits()

              digs <- suppressWarnings(as.integer(digs_raw))
              if (is.na(digs)) digs <- 0

              shiny::req(length(meas) > 0)

              # Build expression for multiple measures
              build_kpi_expr(meas, agg, digs)
            }),
            state = list(
              measures = r_measures,
              agg_fun = r_agg_fun,
              prefix = r_prefix,
              suffix = r_suffix,
              digits = r_digits
            )
          )
        }
      )
    },
    # Input view: settings only
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::div(
        class = "kpi-block-settings",
        style = "padding: 10px;",

        # Row 1: Measures (multi-select)
        shiny::selectizeInput(
          ns("measures"),
          label = "Measures",
          choices = measures,
          selected = measures,
          multiple = TRUE,
          options = list(
            placeholder = "Select measures to display...",
            plugins = list("remove_button")
          )
        ),

        # Row 2: Aggregation, Prefix, Suffix, Digits
        shiny::div(
          style = "display: flex; gap: 10px; flex-wrap: wrap;",
          shiny::div(
            style = "flex: 1; min-width: 100px;",
            shiny::selectInput(
              ns("agg_fun"),
              label = "Aggregation",
              choices = c("Sum" = "sum", "Mean" = "mean", "Median" = "median",
                          "Min" = "min", "Max" = "max", "Count" = "n"),
              selected = agg_fun
            )
          ),
          shiny::div(
            style = "flex: 1; min-width: 70px;",
            shiny::textInput(
              ns("prefix"),
              label = "Prefix",
              value = prefix,
              placeholder = "$"
            )
          ),
          shiny::div(
            style = "flex: 1; min-width: 70px;",
            shiny::textInput(
              ns("suffix"),
              label = "Suffix",
              value = suffix,
              placeholder = "%"
            )
          ),
          shiny::div(
            style = "flex: 1; min-width: 60px;",
            shiny::textInput(
              ns("digits"),
              label = "Digits",
              value = digits
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
    allow_empty_state = c("measures", "prefix", "suffix", "digits"),
    class = "kpi_block",
    ...
  )
}

#' Build expression for KPI aggregation
#' @noRd
build_kpi_expr <- function(measures, agg_fun, digits) {
  if (agg_fun == "n") {
    # Count only - just one value
    return(quote(dplyr::summarise(data, Count = dplyr::n())))
  }

  # Build summarise call for multiple measures
  agg_sym <- as.name(agg_fun)

  # Create named expressions for each measure
  summarise_args <- lapply(measures, function(m) {
    m_sym <- as.name(m)
    bquote(round(.(agg_sym)(.(m_sym), na.rm = TRUE), .(digits)))
  })
  names(summarise_args) <- measures

  # Build the call
  as.call(c(
    list(quote(dplyr::summarise), quote(data)),
    summarise_args
  ))
}

#' @rdname new_kpi_block
#' @export
block_ui.kpi_block <- function(id, x, ...) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "result"))
  )
}

#' @rdname new_kpi_block
#' @param session Shiny session object
#' @export
block_render_trigger.kpi_block <- function(x, session = shiny::getDefaultReactiveDomain()) {
  # Trigger re-render when display parameters change
  list(
    session$input[["expr-prefix"]],
    session$input[["expr-suffix"]],
    session$input[["expr-digits"]]
  )
}

# Color palette for KPI values (auto-cycled)
kpi_color_palette <- function() {
  c("#0d6efd", "#198754", "#fd7e14", "#dc3545", "#6f42c1", "#20c997", "#6c757d")
}

#' @rdname new_kpi_block
#' @export
block_output.kpi_block <- function(x, result, session) {
  # Get display params from expr module inputs
  prefix <- session$input[["expr-prefix"]]
  suffix <- session$input[["expr-suffix"]]

  # Handle NULL values
  if (is.null(prefix)) prefix <- ""
  if (is.null(suffix)) suffix <- ""

  # Get color palette

  colors <- kpi_color_palette()

  shiny::renderUI({
    if (!is.data.frame(result) || ncol(result) == 0) {
      return(shiny::div(
        style = "text-align: center; padding: 30px; color: #6c757d;",
        "Select measures to display"
      ))
    }

    # Build KPI cards for each measure
    measure_names <- names(result)
    kpi_cards <- lapply(seq_along(measure_names), function(i) {
      name <- measure_names[i]
      val <- result[[name]][1]
      color <- colors[((i - 1) %% length(colors)) + 1]

      # Format the value
      formatted <- if (!is.na(val)) {
        format(val, big.mark = ",", scientific = FALSE)
      } else {
        "—"
      }

      shiny::div(
        class = "kpi-card",
        style = "flex: 1; min-width: 120px; text-align: center; padding: 20px 10px;",
        shiny::tags$div(
          class = "kpi-title",
          style = "font-size: 0.9rem; color: #6c757d; margin-bottom: 6px;",
          name
        ),
        shiny::tags$div(
          class = "kpi-value",
          style = sprintf("font-size: 2rem; font-weight: bold; color: %s;", color),
          paste0(prefix, formatted, suffix)
        )
      )
    })

    shiny::div(
      class = "kpi-container",
      style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: center;",
      kpi_cards
    )
  })
}
