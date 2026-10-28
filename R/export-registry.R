# The download formats, registered by whoever can write them. A block's
# download menu lists every registered format that can write the block's
# exhibit and whose packages are installed.

viz_exporter_env <- new.env(parent = emptyenv())

#' Register a download format
#'
#' @param format A short id: the download output is `dl_<format>`.
#' @param label The menu text ("Excel").
#' @param fun A function `(exhibit, file)` that writes the file. It reads the
#'   exhibit with the [exhibit-accessors] only.
#' @param ext The file extension.
#' @param accepts A function of an exhibit: whether this format can write
#'   it (Image needs a picture).
#' @param needs Packages the format needs; it is left out of the menu when
#'   one is not installed.
#' @param order The position in the menu, smallest first.
#' @return `format`, invisibly. A format registered again replaces the old.
#' @seealso [new_viz_exhibit()]
#' @export
register_viz_exporter <- function(format, label, fun, ext = format,
                                  accepts = function(x) TRUE,
                                  needs = character(), order = 50) {
  stopifnot(
    is.character(format), length(format) == 1L, grepl("^[a-z0-9]+$", format),
    is.character(label), length(label) == 1L, is.function(fun),
    is.character(ext), length(ext) == 1L, is.function(accepts),
    is.character(needs), is.numeric(order), length(order) == 1L
  )
  assign(format, list(format = format, label = label, fun = fun, ext = ext,
                      accepts = accepts, needs = needs, order = order),
         envir = viz_exporter_env)
  invisible(format)
}

# The registered formats, in menu order.
viz_exporters <- function() {
  all <- mget(ls(viz_exporter_env), envir = viz_exporter_env)
  all[order(vapply(all, `[[`, numeric(1L), "order"), names(all))]
}

# The formats that can write `x` on this machine.
viz_export_formats <- function(x, exporters = viz_exporters()) {
  Filter(function(f) {
    all(vapply(f$needs, requireNamespace, logical(1L), quietly = TRUE)) &&
      isTRUE(tryCatch(f$accepts(x), error = function(e) FALSE))
  }, exporters)
}

#' A block's downloads
#'
#' Registers one download handler per registered format, `dl_<format>` in
#' the block's namespace, and returns the menu entries for the view's
#' payload (`derived$downloads`). The exhibit is read when a format is
#' clicked, and the board's download footer is put on it before the exporter
#' writes it.
#'
#' @param exhibit A function of no arguments returning a `viz_exhibit`.
#' @param enabled A function returning whether downloads are on.
#' @param session The block's Shiny session.
#' @param filename The word a file is named by when the exhibit has no
#'   title.
#' @param before A function of no arguments run when a format is clicked,
#'   before the exhibit is read (the kit's capture sync).
#' @return A reactive: a list of `list(id, label, ext)`, empty when
#'   downloads are off.
#' @noRd
viz_downloads <- function(exhibit, enabled = function() TRUE,
                          session = shiny::getDefaultReactiveDomain(),
                          filename = "table", before = function() NULL) {
  formats <- viz_exporters()
  for (f in formats) {
    viz_download_handler(f, exhibit, session, filename, before)
  }
  shiny::reactive({
    if (!isTRUE(enabled())) {
      return(list())
    }
    x <- tryCatch(exhibit(), error = function(e) NULL)
    if (!inherits(x, "viz_exhibit")) {
      return(list())
    }
    unname(lapply(viz_export_formats(x, formats), function(f) {
      list(id = session$ns(paste0("dl_", f$format)), label = f$label,
           ext = f$ext)
    }))
  })
}

# One format's handler. It is not suspended when hidden, because the menu that
# links to it is drawn by the view.
viz_download_handler <- function(f, exhibit, session, filename,
                                 before = function() NULL) {
  id <- paste0("dl_", f$format)
  session$output[[id]] <- shiny::downloadHandler(
    # The file name is asked for first, so the picture is taken here.
    filename = function() {
      before()
      x <- tryCatch(exhibit(), error = function(e) NULL)
      if (!inherits(x, "viz_exhibit")) {
        return(paste0(filename, ".", f$ext))
      }
      dl_filename(x$meta$dataset %||% dl_dataset(x$data), x$titles$title,
                  filename, f$ext)
    },
    content = function(file) {
      before()
      dl_guard(f$label, f$fun(exhibit_footed(exhibit(), session), file))
    }
  )
  shiny::outputOptions(session$output, id, suspendWhenHidden = FALSE)
}

# The formats blockr.viz writes itself. Called from .onLoad.
viz_register_exporters <- function() {
  register_viz_exporter("xlsx", "Excel", export_xlsx, needs = "openxlsx",
                        order = 10)
  register_viz_exporter("html", "Web page", export_html, order = 20)
  register_viz_exporter("png", "Image", export_png, accepts = png_accepts,
                        order = 40)
  register_pptx_exporter()
  invisible(NULL)
}

#' Say a download failure out loud: show it on screen at click time and keep
#' the error propagating. Shiny otherwise answers with an HTML error page that
#' the browser saves as "<session>-<block>-dl_pptx.htm".
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
