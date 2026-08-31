# new_heatmap_block(): the matrix heatmap ENGINE. Long event rows in (one
# row per event), the rendered row x column matrix out -- the block
# aggregates ITSELF (cell = event count + worst level of `color`), so no
# upstream reshape block is needed. Data output is a passthrough filter
# (drill: a row click hands the clicked row identity downstream), the tile
# block's model. Renderer: R/heatmap-html.R; JS: inst/js/heatmap-block.js.
#
# Deliberately NOT in this package's registry: the registered surface is
# blockr.pharma::new_ae_heatmap_block() -- same formals, AE defaults, and
# it stamps ITSELF as the serialized ctor (via the ctor/ctor_pkg
# passthrough below), so study boards restore against blockr.pharma. This
# ctor stays exported for the pharma delegation and for non-pharma reuse
# from code.

#' Heatmap block
#'
#' Renders long event rows (e.g. one row per adverse event) as a row x
#' column matrix: the cell DISPLAYS the event count and is PAINTED by the
#' worst level of `color` -- two channels, the old-CDEx AE heatmap form.
#' Columns are capped to the top-N by event count (the "Top n" slider on
#' the block toolbar, next to the cell-numbers toggle -- both deliberately
#' block-level controls, not gear entries). Rows order by `group` (factor
#' order), then total burden descending; each group is marked by a rotated
#' rail tile on the right, the table analogue of the chart's facet strip.
#'
#' @param row Column identifying a matrix ROW (e.g. `"USUBJID"`).
#' @param col Column identifying a matrix COLUMN (e.g. the preferred term).
#' @param color Optional column whose WORST level per cell drives the
#'   paint. An ordered factor keeps its own level order (vocabularies live
#'   in the data); numeric grades order numerically; a bare character
#'   column falls back to sorted unique values. Empty: cells paint by
#'   count instead.
#' @param group Optional column grouping the rows (one group value per row
#'   identity, e.g. the treatment arm); rendered as the right-hand rail.
#' @param top_n Cap on the number of columns, most frequent first
#'   (default 25). The toolbar slider edits this.
#' @param cell_numbers Show the count in each cell (default `TRUE`). Off,
#'   the matrix reads as a pure color heatmap. The toolbar checkbox.
#' @param drill Logical (default `FALSE`): a row click filters downstream
#'   on the `row` column (click again to clear).
#' @param download Logical (default `FALSE`), exposed in the gear: the
#'   toolbar grows the shared download control ([dt_download_control()]) --
#'   the matrix (row identity, group, count columns) written as xlsx /
#'   html / pptx.
#' @param filter_column,filter_values Persisted drill state (restore).
#' @param max_height Scroll container height (default `"600px"`).
#' @param ctrl_target,ctrl_table Character(1), beta: as in
#'   [new_table_block()] -- push the drill claim into a value filter block.
#' @param class Optional subclass(es) prepended to `"heatmap_block"` -- how
#'   a delegating surface (blockr.pharma's AE heatmap) gets its own class,
#'   which is what the registry keys metadata on.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A transform block of class `heatmap_block`.
#' @examplesIf interactive()
#' new_heatmap_block(row = "USUBJID", col = "AEDECOD", color = "AESEV")
#' @export
new_heatmap_block <- function(row = character(),
                              col = character(),
                              color = character(),
                              group = character(),
                              top_n = 25L,
                              cell_numbers = TRUE,
                              drill = FALSE,
                              download = FALSE,
                              filter_column = NULL,
                              filter_values = NULL,
                              max_height = "600px",
                              ctrl_target = "",
                              ctrl_table = "",
                              class = character(),
                              ...) {
  row <- chr_state(row)
  col <- chr_state(col)
  color <- chr_state(color)
  group <- chr_state(group)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        plain_data <- shiny::reactive(coerce_plain_df(data()))

        r_row      <- shiny::reactiveVal(row)
        r_col      <- shiny::reactiveVal(col)
        r_color    <- shiny::reactiveVal(color)
        r_group    <- shiny::reactiveVal(group)
        r_top_n    <- shiny::reactiveVal(as.integer(top_n %||% 25L))
        r_numbers  <- shiny::reactiveVal(isTRUE(cell_numbers))
        r_drill    <- shiny::reactiveVal(isTRUE(drill))
        r_download <- shiny::reactiveVal(isTRUE(download))
        r_filter_column <- shiny::reactiveVal(filter_column)
        r_filter_values <- shiny::reactiveVal(filter_values)
        r_ctrl_target  <- shiny::reactiveVal(ctrl_target %||% "")
        r_ctrl_table   <- shiny::reactiveVal(ctrl_table %||% "")
        r_ctrl_choices <- dd_ctrl_choices()

        upd <- function(rv, v) {
          if (!identical(shiny::isolate(rv()), v)) rv(v)
        }
        blank <- function(v) {
          v <- as.character(v %||% "")
          if (!length(v) || !nzchar(v[1L]) || identical(v[1L], "(none)")) {
            character()
          } else {
            v[1L]
          }
        }

        shiny::observeEvent(input$heatmap_block_action, {
          msg <- input$heatmap_block_action
          if (is.null(msg)) return()
          act <- msg$action %||% "config"
          if (identical(act, "filter")) {
            upd(r_filter_column, msg$column)
            upd(r_filter_values, msg$values)
          } else if (identical(act, "config")) {
            p <- msg$param
            v <- msg$value
            switch(
              p,
              row     = upd(r_row, blank(v)),
              col     = upd(r_col, blank(v)),
              color   = upd(r_color, blank(v)),
              group   = upd(r_group, blank(v)),
              top_n   = upd(r_top_n, max(1L, as.integer(v %||% 25L))),
              cell_numbers = upd(r_numbers, isTRUE(v) || identical(v, "true")),
              drill   = upd(r_drill, isTRUE(v) || identical(v, "true")),
              download = upd(r_download,
                             isTRUE(v) || identical(v, "true") ||
                               identical(v, "on")),
              ctrl_target = upd(r_ctrl_target, trimws(as.character(v %||% ""))),
              ctrl_table  = upd(r_ctrl_table, trimws(as.character(v %||% ""))),
              NULL
            )
          }
        })

        r_ctrl_claims <- shiny::reactive({
          d <- tryCatch(plain_data(), error = function(e) NULL)
          col <- r_filter_column()
          vals <- as.character(unlist(r_filter_values()))
          filters <- if (!is.null(col) && length(vals)) {
            stats::setNames(list(vals), col)
          } else {
            list()
          }
          dd_ctrl_claims(d, r_ctrl_table(), filters)
        })
        dd_ctrl_sender(
          r_ctrl_target,
          r_ctrl_claims,
          dd_ctrl_pristine(
            function() list(r_filter_column(), r_filter_values()),
            list(filter_column, filter_values)
          ),
          session
        )

        # Downloads: the matrix as the reader sees it (row identity, group,
        # count columns), under the current mappings and Top-n. The shared
        # control (dt_download_control) supplies the markup and handlers.
        dl_exhibit <- function() {
          d <- tryCatch(plain_data(), error = function(e) NULL)
          p <- heatmap_prep(d, one_or_null(r_row()), one_or_null(r_col()),
                            one_or_null(r_color()), one_or_null(r_group()),
                            r_top_n())
          if (!is.null(p$err)) {
            return(list(data = data.frame(message = p$err)))
          }
          out <- stats::setNames(
            data.frame(p$rows, stringsAsFactors = FALSE,
                       check.names = FALSE),
            p$row_col
          )
          if (!is.null(p$group_of)) out[["Group"]] <- p$group_of
          for (tm in p$terms) out[[tm]] <- p$count[, tm]
          list(
            data = out,
            title = sprintf("Top %d %s by %s", length(p$terms), p$col_col,
                            p$row_col),
            caption = if (!is.null(p$color_col)) {
              paste0("Cells count events; on screen the color encodes the ",
                     "worst ", p$color_col, ".")
            }
          )
        }
        dl_slot <- dt_download_control(session, dl_exhibit,
                                       enabled = r_download,
                                       filename = "heatmap")

        # A row click must not redraw the matrix: the JS marks the row, and
        # only this small status line re-renders. The matrix render below
        # therefore ISOLATES the filter state (and the cell-numbers flag,
        # which the JS also applies instantly) -- a restore still reads the
        # current values on its first render.
        # Board scale map: the declared per-level colours (e.g. AETOXGR
        # grade 5 = red) the chart and the summarize table already honour.
        # Without one the block falls back to its sequential ramp.
        board_scale_map <- dd_board_scale_map()

        output$heatmap_status <- shiny::renderUI({
          hmb_status_tag(one_or_null(r_row()) %||% "row",
                         r_filter_values())
        })

        output$heatmap_result <- shiny::renderUI({
          d <- plain_data()
          shiny::req(is.data.frame(d))
          heatmap_html(
            d,
            row = one_or_null(r_row()), col = one_or_null(r_col()),
            color = one_or_null(r_color()), group = one_or_null(r_group()),
            top_n = r_top_n(),
            cell_numbers = shiny::isolate(r_numbers()),
            drill = r_drill(), download = r_download(),
            elem_id = ns("heatmap_block"),
            active_values = shiny::isolate(r_filter_values()),
            status = shiny::uiOutput(ns("heatmap_status"), inline = TRUE),
            download_slot = dl_slot,
            scale_map = board_scale_map(),
            ctrl = list(
              target = r_ctrl_target(),
              table = r_ctrl_table(),
              choices = dd_ctrl_choices_list(r_ctrl_choices())
            ),
            max_height = max_height
          )
        })

        list(
          expr = shiny::reactive({
            col  <- r_filter_column()
            vals <- r_filter_values()
            ex <- if (is.null(col) || is.null(vals) || length(vals) == 0) {
              blockr.core::bbquote(dplyr::filter(.(data), TRUE))
            } else if (length(vals) == 1) {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] == .(val)),
                list(col = col, val = vals[[1]])
              )
            } else {
              blockr.core::bbquote(
                dplyr::filter(.(data), .data[[.(col)]] %in% .(vals)),
                list(col = col, vals = vals)
              )
            }
            d <- tryCatch(data(), error = function(e) NULL)
            if (!is.null(d) && !is.data.frame(d)) {
              ex <- wrap_plain_df_input(ex)
            }
            ex
          }),
          state = list(
            row = r_row, col = r_col, color = r_color, group = r_group,
            top_n = r_top_n, cell_numbers = r_numbers, drill = r_drill,
            download = r_download,
            filter_column = r_filter_column,
            filter_values = r_filter_values,
            max_height = function() max_height,
            ctrl_target = r_ctrl_target,
            ctrl_table = r_ctrl_table
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(shiny::uiOutput(ns("heatmap_result")))
    },
    dat_valid = validate_annotated_df_input,
    allow_empty_state = c("row", "col", "color", "group", "drill",
      "filter_column", "filter_values", "ctrl_target", "ctrl_table",
      "class"),
    external_ctrl = c("row", "col", "color", "group", "top_n",
      "cell_numbers", "drill", "download", "filter_column", "filter_values",
      "ctrl_target", "ctrl_table"),
    expr_type = "bquoted",
    class = c(class, "heatmap_block"),
    # `ctor`/`ctor_pkg` deliberately ride `...` and are NOT formals: the
    # framework passes them itself (registry harvest, deser restore), and a
    # formal would both collide with that injection and leak into the
    # serialized state (initial_block_state = the recorded ctor's formals).
    # blockr.pharma's new_ae_heatmap_block stamps its own identity the same
    # way -- through `...`.
    ...
  )
}

#' First element or NULL -- the renderer wants a scalar or nothing.
#' @noRd
one_or_null <- function(x) {
  x <- as.character(x %||% character())
  if (length(x) && nzchar(x[1L])) x[1L] else NULL
}

#' @noRd
heatmap_block_dep <- memoise0(function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-blocks-css",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".3"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = c("blockr-blocks.css", "blockr-select.css")
    ),
    htmltools::htmlDependency(
      name = "blockr-select-js",
      version = paste0(utils::packageVersion("blockr.dplyr"), ".3"),
      src = system.file("js", package = "blockr.dplyr"),
      script = c("blockr-core.js", "blockr-select.js")
    ),
    settings_band_dep(),
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "heatmap-block",
      version = paste0(utils::packageVersion("blockr.viz"), ".3"),
      src = system.file(package = "blockr.viz"),
      script = "js/heatmap-block.js",
      stylesheet = "css/heatmap-block.css"
    )
  )
})
