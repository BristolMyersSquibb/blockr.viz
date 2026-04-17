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
#' The UI uses the shared `Blockr.Select` widget library (also used by
#' `blockr.dplyr` blocks) with a gear-icon popover for advanced options
#' (stats preset, overall column, indent, nest hierarchies).
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

        # Gate to avoid circular JS <-> R updates.
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        send_columns <- function(d) {
          all_cols <- names(d)
          num_cols <- all_cols[vapply(d, is.numeric, logical(1))]
          log_cols <- all_cols[vapply(d, is.logical, logical(1))]
          cat_cols <- setdiff(all_cols, c(num_cols, log_cols))
          var_cols <- c(num_cols, log_cols, cat_cols)
          session$sendCustomMessage("summary-table-columns", list(
            id = ns("summary_input"),
            var_cols = as.list(var_cols),
            cat_cols = as.list(cat_cols)
          ))
        }

        # Send initial state + column choices once data arrives.
        shiny::observeEvent(data(), {
          d <- data()
          shiny::req(is.data.frame(d))
          send_columns(d)
          session$sendCustomMessage("summary-table-update", list(
            id = ns("summary_input"),
            state = r_state()
          ))
        })

        # JS -> R: user edited the block.
        shiny::observeEvent(input$summary_input, {
          self_write$active <- TRUE
          new_state <- input$summary_input
          # Coerce nulls to typed empties so downstream bquote is stable.
          new_state$vars <- as.character(new_state$vars %||% character())
          new_state$sections <- as.character(new_state$sections %||% character())
          new_state$by <- as.character(new_state$by %||% character())
          new_state$stats <- new_state$stats %||% "compact"
          new_state$add_overall <- isTRUE(new_state$add_overall)
          new_state$overall_label <- new_state$overall_label %||% "Total"
          new_state$indent_details <- isTRUE(new_state$indent_details)
          new_state$nest_hierarchies <- isTRUE(new_state$nest_hierarchies)
          r_state(new_state)
        })

        # R -> JS: external state change (e.g. restore from serialized board).
        shiny::observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage("summary-table-update", list(
              id = ns("summary_input"),
              state = r_state()
            ))
          }
        })

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
      htmltools::tagList(
        blockr_core_js_dep(),
        blockr_blocks_css_dep(),
        blockr_select_dep(),
        summary_table_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(
            id = ns("summary_input"),
            class = "summary-table-block-container"
          )
        )
      )
    },
    class = c("summary_table_block", "transform_block", "block"),
    allow_empty_state = c("vars", "sections", "by", "overall_label"),
    ...
  )
}

summary_table_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "summary-table-block-js",
      version = utils::packageVersion("blockr.bi"),
      src = system.file("js", package = "blockr.bi"),
      script = "summary-table-block.js"
    ),
    htmltools::htmlDependency(
      name = "summary-table-block-css",
      version = utils::packageVersion("blockr.bi"),
      src = system.file("css", package = "blockr.bi"),
      stylesheet = "summary-table-block.css"
    )
  )
}
