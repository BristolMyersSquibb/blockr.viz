# dt_download_control(): a download menu for blocks outside this package.
# The chart and table blocks do not use it (their menu is the kit's,
# viz_downloads()); blockr.sandbox's composer block does. It writes through
# the same exporters (R/export-registry.R).

#' The download glyph. Inline SVG, so the control carries no dependency and
#' inherits `currentColor` from the tool.
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

#' The control: one format is a download tool, several are an action menu with
#' one row per format and the extension as meta text. `specs` are lists of
#' `id` (the handler's output id), `ext` and `label`.
#' @noRd
dl_control_ui <- function(ns, specs) {
  if (length(specs) == 1L) {
    spec <- specs[[1L]]
    tip <- paste0("Download as ", spec$label, " (.", spec$ext, ")")
    return(htmltools::attachDependencies(
      shiny::downloadLink(
        ns(spec$id), dt_dl_icon(), class = "blockr-tool",
        `aria-label` = tip, `data-blockr-tooltip` = tip
      ),
      htmltools::findDependencies(blockr.ui::controls_dep()),
      append = TRUE
    ))
  }
  if (!length(specs)) {
    return(NULL)
  }
  do.call(blockr.ui::action_menu, c(
    list(blockr.ui::tool_button(dt_dl_icon(), "Download")),
    lapply(specs, function(s) {
      blockr.ui::menu_item(shiny::downloadLink(ns(s$id), s$label),
                           meta = paste0(".", s$ext))
    })
  ))
}

#' Install a table's download control
#'
#' Registers a download handler for each format this machine can write and
#' returns the slot to drop on the table's toolbar. Each format writes the
#' table through the package's own exporters ([write_annotated_xlsx()],
#' [write_exhibit_html()], [write_exhibit_pptx()]), the same artifact a deck
#' or report export gets. Registration is guarded per session and slot, so a
#' caller may run on every re-render.
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
#'   are on. `NULL` (the default) means always on.
#' @param slot_id Output id for the control itself.
#' @param filename Word the file is named by when the exhibit has no title.
#'   The full name is the dataset in the data's `blockr_provenance` attribute
#'   when it has one, the title or this word, and the time:
#'   `AQ-001_Ozone_by_month_2026-10-02_1432.xlsx`. An exhibit whose `data`
#'   is a frame built for the export, without the input's attributes, returns
#'   the dataset name as `dataset`.
#' @param picture Optional function of no arguments returning the block as
#'   the browser drew it (a `chart_capture`, see R/chart-capture.R), or
#'   `NULL` while there is none. When given, the menu gains an Image entry,
#'   and the web page and the deck carry the picture instead of a re-typeset
#'   table. The spreadsheet always gets values.
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

  # The exhibit the exporters read, with the board's download footer
  # (R/download-footer.R) as the last line of the caption. `source`, when the
  # exhibit has one, is the frame the footer's provenance is read from.
  viz <- function() {
    e <- exhibit()
    new_viz_exhibit(
      e$data, picture = if (is.function(picture)) picture(), render = e$cards,
      title = e$title, subtitle = e$subtitle,
      caption = download_footer_caption(e$caption, e$source %||% e$data,
                                        session),
      meta = list(digits = e$digits, collapsible = e$collapsible,
                  sortable = e$sortable)
    )
  }
  formats <- list(
    list(ext = "xlsx", label = "Excel", fun = export_xlsx,
         ok = dt_has_openxlsx()),
    list(ext = "html", label = "Web page", fun = export_html, ok = TRUE),
    list(ext = "pptx", label = "PowerPoint", fun = export_pptx,
         ok = dt_has_officer()),
    list(ext = "png", label = "Image", fun = export_png,
         ok = is.function(picture))
  )
  specs <- shiny::reactive({
    if (!is.null(enabled) && !isTRUE(enabled())) {
      return(list())
    }
    on <- Filter(function(f) f$ok, formats)
    lapply(on, function(f) c(f[c("ext", "label")], id = paste0("dl_", f$ext)))
  })
  session$output[[slot_id]] <- shiny::renderUI(dl_control_ui(ns, specs()))

  # One handler per format, whether or not it is offered. A hidden output is
  # suspended, and the table's host is hidden until the JS hoists it, so the
  # handlers must not suspend or the click comes back 404.
  lapply(formats, function(f) {
    session$output[[paste0("dl_", f$ext)]] <- shiny::downloadHandler(
      filename = function() {
        # A name that cannot be worked out is a plain one.
        e <- tryCatch(exhibit(), error = function(e) list())
        dl_filename(e$dataset %||% dl_dataset(e$data), e$title, filename,
                    f$ext)
      },
      content = function(file) dl_guard(f$label, f$fun(viz(), file))
    )
  })
  for (nm in c(slot_id, paste0("dl_", vapply(formats, `[[`, "", "ext")))) {
    try(shiny::outputOptions(session$output, nm, suspendWhenHidden = FALSE),
        silent = TRUE)
  }
  slot
}

# Whether a download format's packages are installed: system.file(), since
# requireNamespace() would load officer and flextable (0.5 s, 53 MB) just to
# grey out a menu entry. A seam, so tests can mock it.
dt_pkg_installed <- function(...) {
  all(vapply(c(...), function(p) nzchar(system.file(package = p)), logical(1L)))
}

dt_has_openxlsx <- function() {
  dt_pkg_installed("openxlsx")
}

dt_has_officer <- function() {
  dt_pkg_installed("officer", "flextable")
}
