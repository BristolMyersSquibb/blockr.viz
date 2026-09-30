# The table download control, in one place.
#
# Three renderers offer the same downloads off the same annotated frame: the
# table block, the summarize (rank) table, and now a function block drawing a
# composer table through blockr.sandbox's block_result_output() methods. Each
# had its own copy of the icon, the format list and the button-or-menu rule,
# which is how two of them ended up with slightly different markup for what
# the reader sees as one control.
#
# So the markup and the handlers live here, and a caller supplies the only
# thing that differs: which frame to write, and under which titles.

#' The download glyph. Inline SVG rather than an icon font, so the control
#' carries no dependency and inherits `currentColor` from the tool.
#' @noRd
dt_dl_icon <- function() {
  htmltools::HTML(paste0(
    '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true" ',
    'fill="none" stroke="currentColor" stroke-width="1.6" ',
    'stroke-linecap="round" stroke-linejoin="round">',
    '<path d="M8 2.5 V10 M4.8 7 L8 10.2 L11.2 7"/>',
    '<path d="M2.5 11.5 V12.8 A1.2 1.2 0 0 0 3.7 14 H12.3 ',
    'A1.2 1.2 0 0 0 13.5 12.8 V11.5"/></svg>'
  ))
}

#' Downloads on = every format this machine can write, in menu order.
#'
#' The formats are not a per-board choice: "can people take this table away"
#' is one decision, and which file the reader wants is theirs.
#'
#' A format whose writer is missing is left out rather than shown disabled.
#' Nobody asked for PowerPoint specifically, the download toggle did, so an
#' entry that only ever explains itself is noise. HTML is always there: its
#' renderer is this package's own.
#' @noRd
dt_dl_specs <- function() {
  specs <- list(
    list(id = "dl_xlsx", ext = "xlsx", label = "Excel",
         ok = dt_has_openxlsx()),
    list(id = "dl_html", ext = "html", label = "Web page",
         ok = TRUE),
    list(id = "dl_pptx", ext = "pptx", label = "PowerPoint",
         ok = dt_has_officer())
  )
  Filter(function(s) isTRUE(s$ok), specs)
}

#' The download control of the table, the summarize table and the chart.
#'
#' One format is a tool that downloads; several are a tool that opens an
#' action menu with one row per format, the extension as meta text (design
#' system, Menus). Both are blockr.ui's controls, so every block shows the
#' same 26px tool and the same menu. `specs` are lists of `id` (the
#' download handler's output id), `ext` and `label`.
#' @noRd
dl_control_ui <- function(ns, specs) {
  if (!length(specs)) {
    return(NULL)
  }
  if (length(specs) == 1L) {
    return(dl_tool(ns, specs[[1L]]))
  }
  do.call(blockr.ui::action_menu, c(
    list(blockr.ui::tool_button(dt_dl_icon(), "Download")),
    lapply(specs, function(s) {
      blockr.ui::menu_item(
        shiny::downloadLink(ns(s$id), s$label),
        meta = paste0(".", s$ext)
      )
    })
  ))
}

#' A tool that downloads one format. blockr.ui's tool, as a Shiny download
#' link: a download needs an `<a>`, which [blockr.ui::tool_button()] is not.
#' @noRd
dl_tool <- function(ns, spec) {
  tip <- paste0("Download as ", spec$label, " (.", spec$ext, ")")
  htmltools::attachDependencies(
    shiny::downloadLink(
      ns(spec$id), dt_dl_icon(), class = "blockr-tool",
      `aria-label` = tip, `data-blockr-tooltip` = tip
    ),
    htmltools::findDependencies(blockr.ui::controls_dep()),
    append = TRUE
  )
}

#' Install a table's download control
#'
#' Registers the download handlers for every format this machine can write and
#' returns the slot to drop on the table's toolbar (`dt_chrome()`'s
#' `download_slot`, which [drilldown_table()] takes as `download_slot`). Each
#' format writes the SAME frame the table shows, through that format's own
#' writer: [write_annotated_xlsx()] for the spreadsheet,
#' [write_exhibit_html()] for a self-contained page, [write_exhibit_pptx()]
#' for a deck. That last pair is the exhibit machinery blockr.outline's report
#' and deck exports use, so a table downloaded here and the same table in a
#' deck are one artifact.
#'
#' Registration is guarded per session and slot, because a caller may run on
#' every re-render: `block_result_output()` does, once per evaluation of its
#' block, and re-registering a handler each time would leak one output per
#' render.
#'
#' @param session The calling module's Shiny session. Handlers are registered
#'   on its `output`, so the caller must own the namespace.
#' @param exhibit A function of no arguments returning the list the writers
#'   are called with: `data` (the annotated frame), `title`, `subtitle`,
#'   `caption`, and the `collapsible` / `sortable` display toggles the HTML
#'   page carries over. Read at click time, so a stale snapshot is never
#'   written.
#' @param enabled A function of no arguments returning `TRUE` when downloads
#'   are on. `NULL` (the default) means always on, which is the answer for a
#'   caller with no gear to switch them off in.
#' @param slot_id Output id for the control itself.
#' @param filename Base name for the written file, without extension.
#'
#' @return A [shiny::uiOutput()] to place on the toolbar.
#' @export
dt_download_control <- function(session, exhibit, enabled = NULL,
                                slot_id = "dt_download",
                                filename = "table") {
  stopifnot(is.function(exhibit))
  ns <- session$ns
  slot <- shiny::uiOutput(ns(slot_id), inline = TRUE)

  key <- paste0("blockr_dt_download:", ns(slot_id))
  if (isTRUE(session$userData[[key]])) {
    return(slot)
  }
  session$userData[[key]] <- TRUE

  # The format set is reactive on `enabled` so a gear toggle takes effect
  # without rebuilding the chrome around it.
  specs <- shiny::reactive({
    if (!is.null(enabled) && !isTRUE(enabled())) list() else dt_dl_specs()
  })

  session$output[[slot_id]] <- shiny::renderUI(dl_control_ui(ns, specs()))

  # One handler per format, registered whether or not the format is currently
  # offered: what is installed does not change inside a session, and a handler
  # nobody can click costs nothing.
  session$output$dl_xlsx <- shiny::downloadHandler(
    filename = function() paste0(filename, ".xlsx"),
    content = function(file) {
            dl_guard("Excel", {
              e <- exhibit()
              write_annotated_xlsx(e$data, file, title = e$title,
                                   subtitle = e$subtitle, caption = e$caption)
            })
          }
        )
  session$output$dl_html <- shiny::downloadHandler(
    filename = function() paste0(filename, ".html"),
    content = function(file) {
            dl_guard("web page", {
              e <- exhibit()
              write_exhibit_html(e$data, file, title = e$title,
                                 subtitle = e$subtitle, caption = e$caption,
                                 collapsible = !identical(e$collapsible, FALSE),
                                 sortable = !identical(e$sortable, FALSE))
            })
          }
        )
  session$output$dl_pptx <- shiny::downloadHandler(
    filename = function() paste0(filename, ".pptx"),
    content = function(file) {
            dl_guard("PowerPoint", {
              e <- exhibit()
              write_exhibit_pptx(e$data, file, title = e$title,
                                 subtitle = e$subtitle, caption = e$caption)
            })
          }
        )

  # The control lives wherever its host puts it, and the table's host is
  # display:none until the JS hoists it. A hidden output is a SUSPENDED
  # output, so the download handler never registers and the click comes back
  # 404 -- the prod failure that saves "<block>-expr-dl_pptx.htm".
  for (nm in c(slot_id, "dl_xlsx", "dl_html", "dl_pptx")) {
    try(
      shiny::outputOptions(session$output, nm, suspendWhenHidden = FALSE),
      silent = TRUE
    )
  }

  slot
}

# -- Download failures, said out loud -----------------------------------------
#
# A download that fails on a deployment is a mystery. Shiny answers the
# request with an HTML error page, so the browser saves
# "<session>-<block>-dl_pptx.htm" and reports "Couldn't download", while the
# reason stays in a Connect log nobody watching the app can reach. Two prod
# bugs have now been diagnosed by guessing at that file.
#
# This puts the message on screen at click time and keeps the failure: the
# error still propagates, it just stops being anonymous.
#' @noRd
dl_guard <- function(what, expr) {
  withCallingHandlers(
    expr,
    error = function(e) {
      msg <- conditionMessage(e)
      try(
        shiny::showNotification(
          paste0("The ", what, " download failed: ", msg),
          type = "error", duration = NULL
        ),
        silent = TRUE
      )
      message("[blockr.viz] ", what, " download failed: ", msg)
    }
  )
}
