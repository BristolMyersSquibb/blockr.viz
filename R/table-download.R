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
#' carries no dependency and inherits `currentColor` from the toolbar.
#' @noRd
dt_dl_icon <- function() {
  htmltools::HTML(paste0(
    '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ',
    'stroke="currentColor" stroke-width="1.6" stroke-linecap="round" ',
    'stroke-linejoin="round">',
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
    list(id = "dl_xlsx", ext = "xlsx", label = "Excel (.xlsx)",
         ok = dt_has_openxlsx()),
    list(id = "dl_html", ext = "html", label = "Web page (.html)",
         ok = TRUE),
    list(id = "dl_pptx", ext = "pptx", label = "PowerPoint (.pptx)",
         ok = dt_has_officer())
  )
  Filter(function(s) isTRUE(s$ok), specs)
}

#' One download link.
#'
#' Hand-built (the `shiny-download-link` class is what shiny's download
#' binding attaches to) instead of shiny::downloadButton, so it renders as a
#' quiet design-system control rather than a stock Bootstrap .btn with a
#' FontAwesome icon.
#' @noRd
dt_dl_link <- function(ns, spec, menu = FALSE) {
  htmltools::tags$a(
    id = ns(spec$id),
    class = paste(if (menu) "blockr-dl-item" else "blockr-dl-xlsx",
                  "shiny-download-link"),
    href = "",
    target = "_blank",
    download = NA,
    title = paste0("Download as ", spec$label),
    `aria-label` = paste0("Download as ", spec$label),
    if (menu) spec$label else dt_dl_icon()
  )
}

#' One writable format is a button; several are a menu. Both are the same 30px
#' icon control, so a machine with officer installed does not get a
#' differently-shaped toolbar. The button just gains somewhere to open.
#'
#' `<details>` rather than a scripted popover: the open / close behaviour, the
#' keyboard handling and the focus order are the browser's, so the menu needs
#' no JS of its own and cannot fall out of step with the table's own script.
#' @noRd
dt_dl_ui <- function(ns, specs) {
  if (!length(specs)) {
    return(NULL)
  }
  if (length(specs) == 1L) {
    return(dt_dl_link(ns, specs[[1L]]))
  }
  htmltools::tags$details(
    class = "blockr-dl-menu",
    htmltools::tags$summary(
      class = "blockr-dl-xlsx",
      title = "Download",
      `aria-label` = "Download",
      dt_dl_icon()
    ),
    htmltools::tags$div(
      class = "blockr-dl-menu-list", role = "menu",
      lapply(specs, function(s) dt_dl_link(ns, s, menu = TRUE))
    )
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

  session$output[[slot_id]] <- shiny::renderUI(dt_dl_ui(ns, specs()))

  # One handler per format, registered whether or not the format is currently
  # offered: what is installed does not change inside a session, and a handler
  # nobody can click costs nothing.
  session$output$dl_xlsx <- shiny::downloadHandler(
    filename = function() paste0(filename, ".xlsx"),
    content = function(file) {
      e <- exhibit()
      write_annotated_xlsx(e$data, file, title = e$title,
                           subtitle = e$subtitle, caption = e$caption)
    }
  )
  session$output$dl_html <- shiny::downloadHandler(
    filename = function() paste0(filename, ".html"),
    content = function(file) {
      e <- exhibit()
      write_exhibit_html(e$data, file, title = e$title,
                         subtitle = e$subtitle, caption = e$caption,
                         collapsible = !identical(e$collapsible, FALSE),
                         sortable = !identical(e$sortable, FALSE))
    }
  )
  session$output$dl_pptx <- shiny::downloadHandler(
    filename = function() paste0(filename, ".pptx"),
    content = function(file) {
      e <- exhibit()
      write_exhibit_pptx(e$data, file, title = e$title,
                         subtitle = e$subtitle, caption = e$caption)
    }
  )

  slot
}
