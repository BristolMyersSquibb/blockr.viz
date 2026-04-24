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
#' @param titles Named character vector. Custom titles for the colored pill labels.
#'   Names should match measure names. If NULL or missing for a measure, uses the
#'   measure name as title.
#' @param subtitles Named character vector. Optional subtitles shown below each value.
#'   Names should match measure names. If NULL or missing for a measure, no subtitle shown.
#' @param colors Named character vector. Custom colors for each measure's pill.
#'   Names should match measure names. Values should be hex colors.
#'   If NULL or missing for a measure, auto-assigns from palette.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#' @param id Module ID (for S3 methods)
#' @param x Block object (for S3 methods)
#' @param result Evaluation result (for S3 methods)
#' @param session Shiny session object (for S3 methods)
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
    titles = NULL,
    subtitles = NULL,
    colors = NULL,
    ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # State - track if measures were specified by user
          specified_measures <- measures  # Store original specification
          r_measures <- shiny::reactiveVal(measures)
          r_agg_fun <- shiny::reactiveVal(agg_fun)
          r_prefix <- shiny::reactiveVal(prefix)
          r_suffix <- shiny::reactiveVal(suffix)
          r_digits <- shiny::reactiveVal(digits)
          # Convert named vectors to lists for consistent behavior
          r_titles <- shiny::reactiveVal(if (!is.null(titles)) as.list(titles) else list())
          r_subtitles <- shiny::reactiveVal(if (!is.null(subtitles)) as.list(subtitles) else list())
          r_colors <- shiny::reactiveVal(if (!is.null(colors)) as.list(colors) else list())

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

            # If measures were specified, check if they now exist
            if (length(specified_measures) > 0) {
              valid <- intersect(specified_measures, choices)
              if (length(valid) > 0) {
                r_measures(specified_measures)  # Restore full specification
              }
              shiny::updateSelectizeInput(
                session, "measures",
                choices = choices,
                selected = valid
              )
            } else {
              # Auto-detect mode
              current <- r_measures()
              if (length(current) == 0 && length(choices) > 0) {
                current <- utils::head(choices, min(3, length(choices)))
                r_measures(current)
              }
              shiny::updateSelectizeInput(
                session, "measures",
                choices = choices,
                selected = intersect(current, choices)
              )
            }
          })

          # Update state from UI - only if user manually changes
          shiny::observeEvent(input$measures, {
            # Only update if this is a real user change, not our programmatic update
            if (length(specified_measures) == 0 || length(input$measures) > 0) {
              r_measures(input$measures)
            }
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

          # Render dynamic title/subtitle inputs for each measure
          output$measure_labels <- shiny::renderUI({
            meas <- r_measures()
            if (length(meas) == 0) return(NULL)

            current_titles <- r_titles()
            current_subtitles <- r_subtitles()

            # Ensure they're lists for proper indexing
            if (is.null(current_titles)) current_titles <- list()
            if (is.null(current_subtitles)) current_subtitles <- list()

            shiny::div(
              lapply(meas, function(m) {
                title_val <- current_titles[[m]] %||% ""
                subtitle_val <- current_subtitles[[m]] %||% ""

                shiny::div(
                  style = "display: flex; gap: 10px; margin-bottom: 8px; align-items: center;",
                  shiny::tags$span(
                    style = "min-width: 100px; font-size: 0.85rem; color: #6b7280;",
                    m
                  ),
                  shiny::textInput(
                    ns(paste0("title_", m)),
                    label = NULL,
                    value = title_val,
                    placeholder = "Title",
                    width = "120px"
                  ),
                  shiny::textInput(
                    ns(paste0("subtitle_", m)),
                    label = NULL,
                    value = subtitle_val,
                    placeholder = "Subtitle",
                    width = "200px"
                  )
                )
              })
            )
          })

          # Update titles/subtitles from dynamic inputs when they change
          # Only update state if inputs actually exist (to preserve constructor values)
          shiny::observe({
            meas <- r_measures()
            if (length(meas) == 0) return()

            # Check if any dynamic input exists
            any_input_exists <- FALSE
            for (m in meas) {
              if (!is.null(input[[paste0("title_", m)]]) ||
                  !is.null(input[[paste0("subtitle_", m)]])) {
                any_input_exists <- TRUE
                break
              }
            }

            # Don't update if inputs haven't been rendered yet
            if (!any_input_exists) return()

            new_titles <- list()
            new_subtitles <- list()

            for (m in meas) {
              title_input <- input[[paste0("title_", m)]]
              subtitle_input <- input[[paste0("subtitle_", m)]]

              if (!is.null(title_input) && nzchar(title_input)) {
                new_titles[[m]] <- title_input
              }
              if (!is.null(subtitle_input) && nzchar(subtitle_input)) {
                new_subtitles[[m]] <- subtitle_input
              }
            }

            # Only update if changed to avoid loops
            if (!identical(new_titles, r_titles())) {
              r_titles(new_titles)
            }
            if (!identical(new_subtitles, r_subtitles())) {
              r_subtitles(new_subtitles)
            }
          })

          # Store titles/subtitles in session userData for block_output access
          # Using a unique key based on the module namespace
          userData_key <- paste0("kpi_", id)
          shiny::observe({
            titles_data <- r_titles()
            subtitles_data <- r_subtitles()

            # Store in parent session's userData
            parent_session <- session$rootScope()
            if (is.null(parent_session$userData[[userData_key]])) {
              parent_session$userData[[userData_key]] <- shiny::reactiveValues()
            }
            parent_session$userData[[userData_key]]$titles <- titles_data
            parent_session$userData[[userData_key]]$subtitles <- subtitles_data
          })

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
              digits = r_digits,
              titles = r_titles,
              subtitles = r_subtitles,
              colors = r_colors
            )
          )
        }
      )
    },
    # Input view: settings only
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

          # Aggregation selector (always visible)
          shiny::selectInput(
            ns("agg_fun"),
            label = "Aggregation",
            choices = c("Sum" = "sum", "Mean" = "mean", "Median" = "median",
                        "Min" = "min", "Max" = "max", "Count" = "n"),
            selected = agg_fun
          ),

          # Advanced options toggle
          shiny::div(
            class = "block-advanced-toggle text-muted",
            id = ns("advanced-toggle"),
            style = "margin-top: 5px;",
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

            # Prefix, Suffix, Digits row
            shiny::div(
              style = "display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 15px;",
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
            ),

            # Dynamic title/subtitle/color inputs per measure
            shiny::uiOutput(ns("measure_labels"))
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
      numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
      if (length(numeric_cols) == 0) {
        stop(
          "KPI block needs at least one numeric column to aggregate. ",
          "Input has ", ncol(data), " column(s), none numeric (",
          paste(vapply(data, function(col) class(col)[1], character(1)),
                collapse = ", "),
          "). Cast character/date columns upstream with a mutate block ",
          "(e.g. `as.numeric(`ID Number`)`), or wire the KPI to a ",
          "downstream block that produces numerics."
        )
      }
    },
    allow_empty_state = c("measures", "prefix", "suffix", "digits", "titles", "subtitles", "colors"),
    class = "kpi_block",
    # Pass titles/subtitles to blockr.core - they become block attributes
    titles = titles,
    subtitles = subtitles,
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

  # Get properly namespaced aggregation function
  # median is in stats package, others are in base
  agg_sym <- if (agg_fun == "median") {
    quote(stats::median)
  } else {
    as.name(agg_fun)
  }

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
  userData_key <- "kpi_expr"
  userData <- session$userData[[userData_key]]

  list(
    session$input[["expr-prefix"]],
    session$input[["expr-suffix"]],
    session$input[["expr-digits"]],
    # Trigger on userData changes (titles/subtitles)
    if (!is.null(userData)) userData$titles,
    if (!is.null(userData)) userData$subtitles
  )
}

# Color palette for KPI pills (Okaidia-inspired, auto-cycled)
# Blue, Green, Orange, Red, Purple, Teal, Gray
kpi_color_palette <- function() {
  c("#3b82f6", "#22c55e", "#f97316", "#ef4444", "#8b5cf6", "#14b8a6", "#6b7280")
}

#' @rdname new_kpi_block
#' @export
block_output.kpi_block <- function(x, result, session) {
  # Get display params from expr module inputs
  prefix <- session$input[["expr-prefix"]]
  suffix <- session$input[["expr-suffix"]]
  titles_json <- session$input[["expr-titles_json"]]
  subtitles_json <- session$input[["expr-subtitles_json"]]

  # Handle NULL values
  if (is.null(prefix)) prefix <- ""
  if (is.null(suffix)) suffix <- ""

  # Get titles/subtitles - first try session userData (for dynamic updates)
  # then fall back to block attributes (for initial values)
  userData_key <- "kpi_expr"  # Module ID is typically "expr"
  userData <- session$userData[[userData_key]]

  if (!is.null(userData)) {
    titles <- userData$titles
    subtitles <- userData$subtitles
  } else {
    # Fall back to block attributes from constructor
    titles <- attr(x, "titles")
    subtitles <- attr(x, "subtitles")
  }

  # Convert named vectors to lists if needed
  if (!is.null(titles)) titles <- as.list(titles) else titles <- list()
  if (!is.null(subtitles)) subtitles <- as.list(subtitles) else subtitles <- list()

  # Get color palette
  colors <- kpi_color_palette()

  shiny::renderUI({
    if (!is.data.frame(result) || ncol(result) == 0) {
      return(shiny::div(
        style = "text-align: center; padding: 30px; color: #6b7280;",
        "Select measures to display"
      ))
    }

    # Build KPI cards for each measure
    measure_names <- names(result)
    kpi_cards <- lapply(seq_along(measure_names), function(i) {
      name <- measure_names[i]
      val <- result[[name]][1]
      color <- colors[((i - 1) %% length(colors)) + 1]

      # Get custom title or use measure name
      title <- if (!is.null(titles[[name]])) titles[[name]] else name

      # Get subtitle (may be NULL)
      subtitle <- subtitles[[name]]

      # Format the value
      formatted <- if (!is.na(val)) {
        format(val, big.mark = ",", scientific = FALSE)
      } else {
        "\u2014"
      }

      shiny::div(
        class = "kpi-card",
        style = paste0(
          "flex: 1; min-width: 150px; ",
          "background: white; ",
          "border: 1px solid #e5e7eb; ",
          "border-radius: 1rem; ",
          "padding: 1.5rem; "
        ),
        # Colored pill label
        shiny::tags$div(
          class = "kpi-label",
          style = sprintf(
            "display: inline-block; background: %s; color: white; ",
            color
          ) |> paste0(
            "font-size: 0.75rem; ",
            "padding: 0.25rem 0.75rem; ",
            "border-radius: 9999px; ",
            "margin-bottom: 1rem;"
          ),
          title
        ),
        # Value
        shiny::tags$div(
          class = "kpi-value",
          style = "font-size: 2.25rem; font-weight: 600; color: #111827; margin-bottom: 0.75rem;",
          paste0(prefix, formatted, suffix)
        ),
        # Subtitle (only if provided)
        if (!is.null(subtitle) && nzchar(subtitle)) {
          shiny::tags$div(
            class = "kpi-subtitle",
            style = "font-size: 0.875rem; color: #6b7280;",
            subtitle
          )
        }
      )
    })

    shiny::div(
      class = "kpi-container",
      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 1.5rem;",
      kpi_cards
    )
  })
}
