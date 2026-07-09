#' Summary Table Block
#'
#' Blockr transform block wrapping [summary_table()]. Multi-variable
#' descriptive summary where each `vars` entry becomes a row-section
#' (one row per selected stat for numerics, one row per level for
#' categoricals, one row per flag for logicals).
#'
#' Output is the wide annotated data frame [summary_table()] produces
#' (dotted structure columns `.section_1, ..., .section_k, .label,
#' .indent, .strong` plus one formatted column per by-group level),
#' consumable by [new_gt_table_block()] or any renderer that
#' understands the annotated-data-frame convention.
#'
#' @param vars Character, variables to summarise (each becomes a row-section).
#' @param sections Character, outer section columns that contain `vars` (0..N).
#' @param by Character, column-split dimensions (0..2).
#' @param stats Character vector of stat keys emitted for numeric variables
#'   (see [summary_table()]): any combination of `"n"`, `"n_pct"`, `"mean"`,
#'   `"sd"`, `"mean_sd"`, `"median"`, `"median_q1_q3"`, `"q1_q3"`,
#'   `"min_max"`. One key = a single row per variable; several = one row per
#'   stat. Legacy `"compact"` / `"expanded"` presets are still accepted.
#' @param add_overall Logical, append an overall column across all `by` levels.
#' @param overall_label Label for the overall column, default `"Total"`.
#' @param indent_details Logical, indent detail rows under their variable
#'   header. Default `TRUE`.
#' @param nest_hierarchies Logical, advanced row-side drill-down for adjacent
#'   functionally-dependent categorical vars. Default `FALSE`.
#' @param id_var Optional subject-identifier column name. When set, column N
#'   values and percentages are computed over distinct values of this column
#'   instead of row counts. Useful when each row is an event and multiple rows
#'   can belong to the same entity (e.g. patient, order, session).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @details
#' The UI has two main fields (Summarize, Split by) and a gear popover
#' with advanced options (stat picker pills, overall column, nest
#' hierarchy, group-by sections, count-distinct-by).
#'
#' Spec: `blockr.design/open/table-blocks/`.
#'
#' @return A blockr transform block of class `summary_table_block`.
#' @examplesIf interactive()
#' new_summary_table_block()
#' @export
new_summary_table_block <- function(
  vars = character(),
  sections = character(),
  by = character(),
  stats = "mean_sd",
  add_overall = FALSE,
  overall_label = "Total",
  indent_details = TRUE,
  nest_hierarchies = FALSE,
  id_var = NULL,
  ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # One reactiveVal per field is the source of truth. Required because
        # the flat constructor formals are externally controllable:
        # blockr.core resolves external_ctrl_vars() to block_ctor_inputs()
        # (the flat formals) and every such variable must be a reactiveVal.
        # The JS widget still emits/consumes a single state blob, decomposed
        # into these on the way in and recombined on the way out.
        r_vars             <- shiny::reactiveVal(as.character(vars))
        r_sections         <- shiny::reactiveVal(as.character(sections))
        r_by               <- shiny::reactiveVal(as.character(by))
        # Normalizing here maps legacy "compact"/"expanded" (old serialized
        # boards restore through the ctor) to canonical catalog keys, so
        # both the JS widget and re-serialized state only ever see keys.
        r_stats            <- shiny::reactiveVal(
          normalize_summary_stats(stats %||% "mean_sd")
        )
        r_add_overall      <- shiny::reactiveVal(isTRUE(add_overall))
        r_overall_label    <- shiny::reactiveVal(overall_label %||% "Total")
        r_indent_details   <- shiny::reactiveVal(isTRUE(indent_details))
        r_nest_hierarchies <- shiny::reactiveVal(isTRUE(nest_hierarchies))
        r_id_var           <- shiny::reactiveVal(
          if (is.null(id_var) || !nzchar(id_var)) NULL else as.character(id_var)
        )

        # Recombine the per-field reactiveVals into the single blob the JS
        # widget and the expr consume.
        cur_state <- shiny::reactive(
          list(
            vars             = r_vars(),
            sections         = r_sections(),
            by               = r_by(),
            stats            = r_stats(),
            add_overall      = r_add_overall(),
            overall_label    = r_overall_label(),
            indent_details   = r_indent_details(),
            nest_hierarchies = r_nest_hierarchies(),
            id_var           = r_id_var()
          )
        )

        # Gate to avoid circular JS <-> R updates.
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        send_columns <- function(d) {
          all_cols <- names(d)
          num_cols <- all_cols[vapply(d, is.numeric, logical(1))]
          log_cols <- all_cols[vapply(d, is.logical, logical(1))]
          cat_cols <- setdiff(all_cols, c(num_cols, log_cols))
          var_cols <- c(num_cols, log_cols, cat_cols)
          # Send {value, label} where the column carries a `label` attribute
          # (e.g. ADaM/SDTM datasets), bare name otherwise. The JS select
          # renders the label as a muted hint, like the chart block does.
          col_opt <- function(col) {
            lbl <- attr(d[[col]], "label", exact = TRUE)
            if (!is.null(lbl) && is.character(lbl) && nzchar(lbl[1L])) {
              list(value = col, label = lbl[1L])
            } else {
              col
            }
          }
          session$sendCustomMessage("summary-table-columns", list(
            id = ns("summary_input"),
            var_cols = lapply(var_cols, col_opt),
            cat_cols = lapply(cat_cols, col_opt)
          ))
        }

        # Shiny's sendCustomMessage serializes with auto_unbox = TRUE, which
        # collapses length-1 character vectors to JSON scalars. The JS side
        # checks Array.isArray() on vars/sections/by and drops non-arrays,
        # which then round-trips back to R as empty and breaks the pipeline.
        # Wrap as.list() to keep them as JSON arrays regardless of length.
        normalize_state_for_js <- function(state) {
          for (k in c("vars", "sections", "by", "stats")) {
            state[[k]] <- as.list(state[[k]] %||% character())
          }
          state$id_var <- state$id_var %||% ""
          state
        }

        # Send initial state + column choices once data arrives.
        shiny::observeEvent(data(), {
          d <- data()
          shiny::req(is.data.frame(d))
          send_columns(d)
          session$sendCustomMessage("summary-table-update", list(
            id = ns("summary_input"),
            state = normalize_state_for_js(cur_state())
          ))
        })

        # JS -> R: user edited the block. Fan the single blob out to the
        # per-field reactiveVals (coercing nulls to typed empties so the
        # downstream bquote is stable).
        shiny::observeEvent(input$summary_input, {
          self_write$active <- TRUE
          new_state <- input$summary_input
          r_vars(as.character(new_state$vars %||% character()))
          r_sections(as.character(new_state$sections %||% character()))
          r_by(as.character(new_state$by %||% character()))
          # The JS widget only emits catalog keys and never an empty
          # selection; the filter + fallback just keep malformed input
          # (e.g. via external_ctrl) from wedging the block.
          stats_in <- intersect(
            names(SUMMARY_STATS_CATALOG),
            as.character(new_state$stats %||% character())
          )
          r_stats(if (length(stats_in)) stats_in else "mean_sd")
          r_add_overall(isTRUE(new_state$add_overall))
          r_overall_label(new_state$overall_label %||% "Total")
          r_indent_details(isTRUE(new_state$indent_details))
          r_nest_hierarchies(isTRUE(new_state$nest_hierarchies))
          id_v <- new_state$id_var
          r_id_var(if (is.null(id_v) || !nzchar(id_v)) NULL else as.character(id_v))
        })

        # R -> JS: external/field state change (e.g. restore from a serialized
        # board or external_ctrl). The per-field writes above land in one
        # flush, so cur_state() invalidates once per user edit.
        shiny::observeEvent(cur_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage("summary-table-update", list(
              id = ns("summary_input"),
              state = normalize_state_for_js(cur_state())
            ))
          }
        })

        list(
          expr = shiny::reactive({
            s <- cur_state()
            # `vars` is the one required field. While it is empty, pass the
            # input through unchanged instead of erroring: this keeps the block
            # in a valid, non-error state (no hard red banner) while the
            # Summarize field is highlighted amber -- the design-system "needs a
            # value" affordance used by the chart block. The expr must be a call
            # (blockr requires `typeof(expr) == "language"`), so `data` is
            # wrapped rather than returned as a bare symbol.
            if (!length(s$vars)) {
              return(quote(identity(data)))
            }
            if (!is.null(s$id_var) && nzchar(s$id_var)) {
              bquote(
                blockr.viz::summary_table(
                  data,
                  vars             = .(s$vars),
                  sections         = .(s$sections),
                  by               = .(s$by),
                  stats            = .(s$stats),
                  add_overall      = .(s$add_overall),
                  overall_label    = .(s$overall_label),
                  indent_details   = .(s$indent_details),
                  nest_hierarchies = .(s$nest_hierarchies),
                  subject_var      = .(s$id_var)
                )
              )
            } else {
              bquote(
                blockr.viz::summary_table(
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
            }
          }),
          state = list(
            vars             = r_vars,
            sections         = r_sections,
            by               = r_by,
            stats            = r_stats,
            add_overall      = r_add_overall,
            overall_label    = r_overall_label,
            indent_details   = r_indent_details,
            nest_hierarchies = r_nest_hierarchies,
            id_var           = r_id_var
          )
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
    allow_empty_state = c("vars", "sections", "by", "overall_label", "id_var"),
    external_ctrl = c(
      "vars", "sections", "by", "stats", "add_overall", "overall_label",
      "indent_details", "nest_hierarchies", "id_var"
    ),
    ...
  )
}

summary_table_block_dep <- function() {
  htmltools::tagList(
    settings_band_dep(),
    htmltools::htmlDependency(
      name = "summary-table-block-js",
      version = paste0(utils::packageVersion("blockr.viz"), ".2"),
      src = system.file("js", package = "blockr.viz"),
      script = "summary-table-block.js"
    ),
    htmltools::htmlDependency(
      name = "summary-table-block-css",
      version = utils::packageVersion("blockr.viz"),
      src = system.file("css", package = "blockr.viz"),
      stylesheet = "summary-table-block.css"
    )
  )
}
