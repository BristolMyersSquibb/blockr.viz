# The table download control, in one place.
#
# Three renderers offer the same downloads off the same annotated frame: the
# table block, the summarize table, and now a function block drawing a
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
#'   page carries over. An optional `digits` rounds the numbers the way the
#'   table block's screen does: the web page and the deck show the rounded
#'   text, the spreadsheet keeps the full value under a rounding number
#'   format. Read at click time, so a stale snapshot is never written.
#' @param enabled A function of no arguments returning `TRUE` when downloads
#'   are on. `NULL` (the default) means always on, which is the answer for a
#'   caller with no gear to switch them off in.
#' @param slot_id Output id for the control itself.
#' @param filename Word the file is named by when the exhibit has no title.
#'   The full name is the dataset in the data's `blockr_provenance` attribute
#'   when it has one,
#'   the title or this word, and the time:
#'   `AQ-001_Ozone_by_month_2026-10-02_1432.xlsx`. An exhibit whose `data`
#'   is a frame built for the export, without the input's attributes, returns
#'   the dataset name as `dataset`.
#' @param picture Optional function of no arguments returning the block as
#'   the browser drew it (a `chart_capture`, see R/chart-capture.R), or
#'   `NULL` while there is none. When given, the menu gains an Image entry,
#'   and the web page and the deck carry the picture instead of a re-typeset
#'   table. The heatmap passes one: it is a graphic table, exported like the
#'   chart and the summarize table. The spreadsheet always gets values.
#'
#' @return A [shiny::uiOutput()] to place on the toolbar.
#' @export
dt_download_control <- function(session, exhibit, enabled = NULL,
                                slot_id = "dt_download",
                                filename = "table", picture = NULL) {
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
    if (!is.null(enabled) && !isTRUE(enabled())) {
      list()
    } else if (is.function(picture)) {
      c(dt_dl_specs(),
        list(list(id = "dl_png", ext = "png", label = "Image", ok = TRUE)))
    } else {
      dt_dl_specs()
    }
  })
  pic <- function() if (is.function(picture)) picture()
  # A name that cannot be worked out is a plain one, never a failed
  # download: the content handler reports its own errors (dl_guard below).
  dl_name <- function(ext) {
    e <- tryCatch(exhibit(), error = function(e) list())
    dl_filename(e$dataset %||% dl_dataset(e$data), e$title, filename, ext)
  }

  session$output[[slot_id]] <- shiny::renderUI(dl_control_ui(ns, specs()))

  # One handler per format, registered whether or not the format is currently
  # offered: what is installed does not change inside a session, and a handler
  # nobody can click costs nothing.
  session$output$dl_xlsx <- shiny::downloadHandler(
    filename = function() dl_name("xlsx"),
    content = function(file) {
            dl_guard("Excel", {
              e <- exhibit()
              write_annotated_xlsx(e$data, file, title = e$title,
                                   subtitle = e$subtitle, caption = e$caption,
                                   digits = e$digits)
            })
          }
        )
  session$output$dl_html <- shiny::downloadHandler(
    filename = function() dl_name("html"),
    content = function(file) {
            dl_guard("web page", {
              p <- pic()
              if (!is.null(p)) {
                e <- exhibit()
                return(write_exhibit_html(p, file, title = e$title))
              }
              e <- exhibit()
              write_exhibit_html(dt_format_digits(e$data, e$digits), file,
                                 title = e$title,
                                 subtitle = e$subtitle, caption = e$caption,
                                 collapsible = !identical(e$collapsible, FALSE),
                                 sortable = !identical(e$sortable, FALSE))
            })
          }
        )
  session$output$dl_pptx <- shiny::downloadHandler(
    filename = function() dl_name("pptx"),
    content = function(file) {
            dl_guard("PowerPoint", {
              p <- pic()
              if (!is.null(p)) return(write_exhibit_pptx(p, file))
              e <- exhibit()
              write_exhibit_pptx(dt_format_digits(e$data, e$digits), file,
                                 title = e$title,
                                 subtitle = e$subtitle, caption = e$caption)
            })
          }
        )
  session$output$dl_png <- shiny::downloadHandler(
    filename = function() dl_name("png"),
    content = function(file) {
            dl_guard("image", {
              p <- pic()
              if (is.null(p)) {
                stop("the picture is not ready yet; open the menu again",
                     call. = FALSE)
              }
              chart_capture_file(p, file)
            })
          }
        )

  # The control lives wherever its host puts it, and the table's host is
  # display:none until the JS hoists it. A hidden output is a SUSPENDED
  # output, so the download handler never registers and the click comes back
  # 404 -- the prod failure that saves "<block>-expr-dl_pptx.htm".
  for (nm in c(slot_id, "dl_xlsx", "dl_html", "dl_pptx", "dl_png")) {
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
