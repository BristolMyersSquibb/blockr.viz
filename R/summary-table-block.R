#' Summary Table Block
#'
#' Blockr transform block wrapping [summary_table()]. Multi-variable
#' descriptive summary where each `vars` entry becomes a row-section
#' (one row for compact numerics, six rows for expanded numerics,
#' one row per level for categoricals, one row per flag for logicals).
#'
#' Output is a plain tibble with dotted section columns
#' (`.section_1, ..., .section_k, .var, .label`), consumable by
#' [new_gt_table_block()] or any renderer that understands the
#' convention.
#'
#' @param state Initial state list. Fields:
#'   - `vars` — character, variables to summarise.
#'   - `sections` — character, outer section columns (0..N).
#'   - `by` — character, column-split dimensions (0..2).
#'   - `stats` — `"compact"` or `"expanded"`.
#'   - `add_overall` — logical, append an overall column.
#'   - `overall_label` — label for the overall column, default `"Total"`.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @details
#' v1 uses stock Shiny `selectizeInput` widgets for the selection
#' slots. A JS-widget (`blockr-select` / `blockr-pill`) rewrite is
#' planned for a follow-up release that will bring pivot_table_block
#' and summary_table_block onto a shared widget library.
#'
#' Spec: `blockr.design/open/table-blocks/`.
#'
#' @export
new_summary_table_block <- function(
  state = list(
    vars = character(),
    sections = character(),
    by = character(),
    stats = "compact",
    add_overall = FALSE,
    overall_label = "Total",
    indent_details = TRUE,
    nest_hierarchies = FALSE
  ),
  ...
) {
  # Backfill defaults for partial state lists (e.g. from cedx-poc.R or
  # older serialized boards that predate new fields).
  defaults <- list(
    vars = character(), sections = character(), by = character(),
    stats = "compact", add_overall = FALSE, overall_label = "Total",
    indent_details = TRUE, nest_hierarchies = FALSE
  )
  for (nm in names(defaults)) {
    if (is.null(state[[nm]])) state[[nm]] <- defaults[[nm]]
  }
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- shiny::reactiveVal(state)

        # Populate column choices when data changes
        shiny::observeEvent(data(), {
          d <- data()
          shiny::req(d, is.data.frame(d))
          all_cols <- names(d)
          num_cols <- all_cols[vapply(d, is.numeric, logical(1))]
          log_cols <- all_cols[vapply(d, is.logical, logical(1))]
          cat_cols <- setdiff(all_cols, c(num_cols, log_cols))
          var_cols <- c(num_cols, log_cols, cat_cols)  # any type valid for vars

          s <- shiny::isolate(r_state())
          shiny::updateSelectizeInput(session, "vars",
            choices = var_cols,
            selected = intersect(s$vars, var_cols))
          shiny::updateSelectizeInput(session, "sections",
            choices = cat_cols,
            selected = intersect(s$sections, cat_cols))
          shiny::updateSelectizeInput(session, "by",
            choices = cat_cols,
            selected = intersect(s$by, cat_cols))
        })

        # Sync UI → r_state. Use ignoreInit = TRUE to avoid clobbering
        # the initial state before the user has touched anything.
        update_state <- function(field, value) {
          s <- shiny::isolate(r_state())
          s[[field]] <- value
          r_state(s)
        }
        shiny::observeEvent(input$vars,
          update_state("vars", input$vars),
          ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$sections,
          update_state("sections", input$sections),
          ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$by,
          update_state("by", input$by),
          ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$stats,
          update_state("stats", input$stats),
          ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$add_overall,
          update_state("add_overall", isTRUE(input$add_overall)),
          ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$overall_label,
          update_state("overall_label", input$overall_label),
          ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$indent_details,
          update_state("indent_details", isTRUE(input$indent_details)),
          ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input$nest_hierarchies,
          update_state("nest_hierarchies", isTRUE(input$nest_hierarchies)),
          ignoreNULL = FALSE, ignoreInit = TRUE)

        list(
          expr = shiny::reactive({
            s <- r_state()
            shiny::req(length(s$vars) > 0)
            bquote(
              blockr.bi::summary_table(
                data,
                vars             = .(s$vars),
                sections         = .(s$sections),
                by               = .(s$by),
                stats            = .(s$stats),
                add_overall      = .(s$add_overall),
                overall_label    = .(s$overall_label),
                indent_details   = .(s$indent_details),
                nest_hierarchies = .(s$nest_hierarchies)
              )
            )
          }),
          state = list(state = r_state)
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          class = "summary-table-block-ui",
          shiny::fluidRow(
            shiny::column(6,
              shiny::selectizeInput(
                ns("vars"),
                label = "Variables",
                choices = NULL,
                selected = state$vars,
                multiple = TRUE,
                options = list(placeholder = "Columns to summarise")
              )
            ),
            shiny::column(6,
              shiny::selectizeInput(
                ns("sections"),
                label = "Sections (outer grouping)",
                choices = NULL,
                selected = state$sections,
                multiple = TRUE,
                options = list(placeholder = "Optional outer section columns")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::selectizeInput(
                ns("by"),
                label = "By (column split)",
                choices = NULL,
                selected = state$by,
                multiple = TRUE,
                options = list(
                  placeholder = "Up to 2 categorical columns",
                  maxItems = 2
                )
              )
            ),
            shiny::column(6,
              shiny::selectInput(
                ns("stats"),
                label = "Stats preset",
                choices = c(
                  "Compact (Mean (SD) per row)" = "compact",
                  "Expanded (N / Mean / SD / Median / Q1,Q3 / Min,Max)" = "expanded"
                ),
                selected = state$stats
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::checkboxInput(
                ns("add_overall"),
                label = "Add overall column",
                value = state$add_overall
              )
            ),
            shiny::column(6,
              shiny::textInput(
                ns("overall_label"),
                label = "Overall column label",
                value = state$overall_label
              )
            )
          ),
          shiny::tags$details(
            shiny::tags$summary("Advanced options"),
            shiny::fluidRow(
              shiny::column(6,
                shiny::checkboxInput(
                  ns("indent_details"),
                  label = "Indent detail rows",
                  value = isTRUE(state$indent_details)
                )
              ),
              shiny::column(6,
                shiny::checkboxInput(
                  ns("nest_hierarchies"),
                  label = "Nest hierarchies",
                  value = isTRUE(state$nest_hierarchies)
                )
              )
            )
          )
        )
      )
    },
    class = c("summary_table_block", "transform_block", "block"),
    ...
  )
}
