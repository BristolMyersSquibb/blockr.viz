# The chart as the browser drew it -------------------------------------------
#
# A chart's downloads and slides carry the picture the browser drew.
# `_downloadImage()` in chart/export.js composes the whole block on an
# offscreen canvas (title band, every facet panel at its grid position, facet
# labels, legend chips, caption) and sends the bitmap up; the png / html /
# pptx downloads write that. A chart whose panel was never drawn is drawn
# offscreen by the capture service further down.

# How many device pixels per CSS pixel the canvas composes with. 2 is what
# the screen wants; a bitmap headed for an 11.9in slide is placed four times
# larger than it was drawn, so a deck wants more. Nothing about the layout
# changes with it -- only how finely the same picture is drawn.
#' @noRd
canvas_capture_ratio <- function() {
  r <- suppressWarnings(as.numeric(
    getOption("blockr.viz.canvas_capture_ratio", 2)
  ))[1L]
  if (!is.finite(r) || r < 1) 2 else min(r, 6)
}

# A captured bitmap, with the CSS-pixel box it was composed at. Sized in
# inches at 96 dpi, the density the canvas draws in, so an exhibit lands at
# the size it had on screen. `pages`, a picture cut for slides, is a list of
# captures, one per slide.
#' @noRd
new_chart_capture <- function(png, width, height, pages = NULL) {

  w <- as.numeric(width %||% 0)[1L]
  h <- as.numeric(height %||% 0)[1L]

  if (!length(png) || !is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) {
    return(NULL)
  }

  structure(
    list(png = png, width = w / 96, height = h / 96,
         pages = if (length(pages) > 1L) pages),
    class = "chart_capture"
  )
}

# "data:image/png;base64,iVBOR..." -> raw. Anything else is not a capture.
#' @noRd
chart_capture_decode <- function(url) {

  url <- as.character(url %||% "")[1L]

  if (!grepl("^data:image/png;base64,", url)) {
    return(NULL)
  }

  out <- tryCatch(
    jsonlite::base64_dec(sub("^data:image/png;base64,", "", url)),
    error = function(e) NULL
  )

  if (!length(out)) NULL else out
}

# The bitmap on disk, for the writers that want a file.
#' @param file Where to write it.
#' @rdname chart_capture_ids
#' @export
chart_capture_file <- function(x, file = tempfile(fileext = ".png")) {
  writeBin(x$png, file)
  file
}

#' @export
html_exhibit.chart_capture <- function(x, title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       max_height = NULL,
                                       default_expanded = NULL, ...) {

  # The image inline as a data URI, so the page carries it. jsonlite's
  # base64_enc() wraps its output at 80 columns, and a data URI with newlines
  # does not load, so they are taken out.
  f <- chart_capture_file(x)
  on.exit(unlink(f), add = TRUE)

  htmltools::tags$img(
    class = "blockr-exhibit-img",
    src = paste0("data:image/png;base64,", gsub(
      "\n", "", jsonlite::base64_enc(readBin(f, "raw", file.size(f))),
      fixed = TRUE
    )),
    style = "max-width:100%;height:auto;display:block;"
  )
}


# -- The capture service, R side ---------------------------------------------
#
# A deck asks for pictures of charts it does not own, some of whose panels are
# closed. The block cannot be reached through its module (a closed panel has
# no UI in the page at all), so each chart block registers a REQUEST FUNCTION
# in the session while it lives, and the deck calls that.
#
# The exchange is asynchronous by nature -- the picture comes back over the
# websocket -- so it is request now, collect later: `chart_capture_request()`
# returns a token, the replies land in one session-level input, and
# `chart_captures_ready()` says when the set is complete. An export therefore
# runs in two steps: ask, then write the file once the pictures are in.

# The registry, created on first use. Keyed by block id, so a deck that knows
# its chart blocks can address them without knowing anything else.
#' @noRd
capture_registry <- function(session = shiny::getDefaultReactiveDomain()) {

  root <- session$rootScope()

  if (is.null(root$userData$blockr_viz_captures)) {
    root$userData$blockr_viz_captures <- shiny::reactiveValues()
  }

  root$userData$blockr_viz_captures
}

# Called by a chart block for itself. `send` takes (token, width, height) and
# posts the request to the browser.
#' @noRd
register_chart_capture <- function(id, send,
                                   session = shiny::getDefaultReactiveDomain()) {

  reg <- capture_registry(session)
  reg[[id]] <- send

  shiny::onStop(function() {
    reg[[id]] <- NULL
  }, session = session)

  invisible(id)
}

#' The Charts This Session Can Draw
#'
#' The capture service: a deck asks the browser for a picture of a chart
#' block, at a box it chooses, and gets the bitmap back. Every chart block
#' registers itself while it lives, so a block whose panel was never opened
#' can still be drawn (offscreen, at the export's size).
#'
#' The exchange is asynchronous, because the picture crosses the websocket:
#' `chart_capture_request()` returns a token and `chart_capture_collect()`
#' turns into an exhibit once the reply lands. An export therefore asks
#' first and writes the file when the pictures are in.
#'
#' @param block_id A block id, as the board knows it.
#' @param width,height The box to draw into, in CSS pixels.
#' @param token A token from `chart_capture_request()`.
#' @param session The Shiny session.
#'
#' @return `chart_capture_ids()` the registered keys; `chart_capture_for()`
#'   the key for one block, or `NULL`; `chart_capture_request()` a token;
#'   `chart_capture_collect()` `NULL` while the picture is in flight, and an
#'   exhibit once it arrives.
#'
#' @keywords internal
#' @export
chart_capture_ids <- function(session = shiny::getDefaultReactiveDomain()) {
  shiny::isolate(names(capture_registry(session)))
}

# The registry key for a block id. Keys are the chart's element id, which
# carries the block id as a segment ("<board>-block_<id>-expr-..."), because
# the module id inside a block server is the same string for every chart on
# the board.
#' @rdname chart_capture_ids
#' @export
chart_capture_for <- function(block_id,
                              session = shiny::getDefaultReactiveDomain()) {

  ids <- chart_capture_ids(session)
  hit <- grep(paste0("(^|-)block_", block_id, "-"), ids, value = TRUE)

  if (!length(hit)) NULL else hit[[1L]]
}

# Ask one registered chart for a picture at `width` x `height` CSS px. The
# token identifies the reply; a chart that is not registered (never mounted,
# a block that is not a chart) is an error rather than a silent omission --
# a deck with a chart missing is worse than a deck that says which one.
#' @rdname chart_capture_ids
#' @export
chart_capture_request <- function(id, width, height,
                                  session = shiny::getDefaultReactiveDomain()) {

  send <- shiny::isolate(capture_registry(session)[[id]])

  if (!is.function(send)) {
    stop("no chart capture registered for block '", id, "'", call. = FALSE)
  }

  token <- paste0("cap-", id, "-", as.integer(stats::runif(1, 1, 1e9)))
  send(token, width, height)
  token
}

# The replies, keyed by token. One session-level input carries all of them:
# the browser answers from the capture host, which belongs to no block.
#' @noRd
chart_capture_results <- function(session = shiny::getDefaultReactiveDomain()) {

  root <- session$rootScope()

  if (is.null(root$userData$blockr_viz_capture_results)) {
    store <- shiny::reactiveValues()
    root$userData$blockr_viz_capture_results <- store
    shiny::observeEvent(root$input$blockr_viz_capture_result, {
      msg <- root$input$blockr_viz_capture_result
      if (!is.null(msg$req)) {
        store[[msg$req]] <- msg
      }
    }, domain = root)
  }

  root$userData$blockr_viz_capture_results
}

# The capture for a token: NULL while it is still in flight, a chart_capture
# once it lands, an error when the browser could not draw it.
#' @rdname chart_capture_ids
#' @export
chart_capture_collect <- function(token,
                                  session = shiny::getDefaultReactiveDomain()) {

  msg <- chart_capture_results(session)[[token]]

  if (is.null(msg)) {
    return(NULL)
  }

  if (!is.null(msg$error)) {
    stop("the browser could not draw this chart: ", msg$error, call. = FALSE)
  }

  new_chart_capture(chart_capture_decode(msg$png), msg$width, msg$height)
}

# The bitmap as something officer can place. Sized in inches at the density
# it was composed with, and fitted into `max_width` / `max_height` when the
# caller has a box in mind, keeping the aspect.
#' @param x A capture, from `chart_capture_collect()`.
#' @param max_width,max_height Optional box, in inches.
#' @rdname chart_capture_ids
#' @export
chart_capture_img <- function(x, max_width = NULL, max_height = NULL) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("chart_capture_img() needs the 'officer' package.", call. = FALSE)
  }

  fit <- min(
    if (is.null(max_width)) Inf else max_width / x$width,
    if (is.null(max_height)) Inf else max_height / x$height,
    2
  )
  if (!is.finite(fit)) {
    fit <- 1
  }

  w <- x$width * fit
  h <- x$height * fit

  img <- officer::external_img(chart_capture_file(x), width = w, height = h)
  attr(img, "pptx_width") <- w
  attr(img, "pptx_height") <- h
  img
}
