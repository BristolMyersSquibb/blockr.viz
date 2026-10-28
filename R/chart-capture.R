# A block as the browser drew it ---------------------------------------------
#
# The png, html and pptx downloads and blockr.outline's slides carry the
# picture the browser drew. Two paths bring it to R:
#
# - Downloads: the block's own `<view>_capture` input. When the download menu
#   opens, the view composes itself (`_downloadImage()` in chart/export.js:
#   title band, facet panels, facet labels, legend chips, caption; the
#   groups table's capture() in table/groups.js) and sends the bitmap.
#   viz_capture_observer() (R/kit-server.R) decodes it with
#   chart_capture_read() and the exhibit reads it. Chart and table both.
# - Decks: the capture service at the end of this file. A deck asks for
#   pictures of charts it does not own, some with closed panels, so the
#   browser draws them offscreen from the block's payload and answers on one
#   session-level input. Charts only.
#
# Both end in a `chart_capture`, the class every exhibit writer takes.

# Device pixels per CSS pixel the canvas composes with: 2 for the screen, up
# to 6 for a deck. The layout does not change with it.
#' @noRd
canvas_capture_ratio <- function() {
  r <- suppressWarnings(as.numeric(
    getOption("blockr.viz.canvas_capture_ratio", 2)
  ))[1L]
  if (!is.finite(r) || r < 1) 2 else min(r, 6)
}

# A captured bitmap, sized in inches at 96 dpi from the CSS-pixel box it was
# composed at, so an exhibit lands at its on-screen size.
#' @noRd
new_chart_capture <- function(png, width, height) {

  w <- as.numeric(width %||% 0)[1L]
  h <- as.numeric(height %||% 0)[1L]

  if (!length(png) || !is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) {
    return(NULL)
  }

  structure(
    list(png = png, width = w / 96, height = h / 96),
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

# The browser's `{png, width, height}` message as a chart_capture, or NULL
# when it carries no PNG. The one place a capture message is decoded.
#' @noRd
chart_capture_read <- function(msg) {
  if (!is.list(msg)) {
    return(NULL)
  }
  new_chart_capture(chart_capture_decode(msg$png), msg$width, msg$height)
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

  htmltools::tags$img(
    class = "blockr-exhibit-img",
    src = png_data_uri(x$png),
    style = "max-width:100%;height:auto;display:block;"
  )
}

# PNG bytes as a data URI. base64_enc() wraps at 80 columns and a data URI
# with newlines does not load, so the newlines are removed.
#' @noRd
png_data_uri <- function(png) {
  paste0("data:image/png;base64,",
         gsub("\n", "", jsonlite::base64_enc(png), fixed = TRUE))
}

# How much a capture is scaled to fit a box in inches, keeping its aspect.
# The browser composes it at canvas_capture_ratio() device pixels per CSS
# pixel, at least 2, so it may grow to twice its on-screen size and stay
# sharp; no further.
#' @noRd
capture_fit <- function(x, max_width = NULL, max_height = NULL) {
  fit <- min(
    if (is.null(max_width)) Inf else max_width / x$width,
    if (is.null(max_height)) Inf else max_height / x$height,
    2
  )
  if (is.finite(fit)) fit else 1
}


# -- The capture service, R side ---------------------------------------------
#
# For blockr.outline decks; the block's downloads do not use it. A deck asks
# for pictures of charts it does not own, some with closed panels. A closed
# panel has no UI in the page, so the block cannot be reached through its
# module. Each chart block registers a request function in the session while
# it lives (chart_capture_service()), and the deck calls that.
#
# The picture comes back over the websocket, so an export runs in two steps:
# `chart_capture_request()` returns a token, the replies land in one
# session-level input, and the file is written once the pictures are in.

# The registry of request functions, created on first use and keyed by chart
# element id. It belongs to the root session: the first chart creates it
# from inside its own module, and since Shiny 1.14 a module's reactives die
# with the module, which would break every chart added after that one is
# removed.
#' @noRd
capture_registry <- function(session = shiny::getDefaultReactiveDomain()) {

  root <- session$rootScope()

  if (is.null(root$userData$blockr_viz_captures)) {
    root$userData$blockr_viz_captures <- shiny::withReactiveDomain(
      root, shiny::reactiveValues()
    )
  }

  root$userData$blockr_viz_captures
}

# A chart block's entry in the registry: the browser draws the picture
# offscreen from the block's payload, at the box the deck asks for and at
# the deck's pixel ratio (canvas_capture_ratio()), which the payload alone
# does not carry.
#' @noRd
chart_capture_service <- function(session, el, payload) {
  register_chart_capture(el, function(token, width, height) {
    msg <- tryCatch(shiny::isolate(payload()), error = function(e) NULL)
    if (is.null(msg)) {
      stop("chart block '", el, "' has no data to draw yet", call. = FALSE)
    }
    session$sendCustomMessage("blockr-viz:capture", c(
      msg, list(req = token, width = width, height = height,
                ratio = canvas_capture_ratio())
    ))
  }, session = session)
}

# Registers a chart block's request function. `send` takes (token, width,
# height) and posts the request to the browser. The entry goes when the
# block's module is destroyed (Shiny 1.14), so a removed chart's server and
# its last payload are not kept until the session ends; older Shiny drops it
# at session end. A later chart under the same id keeps its own entry.
#' @noRd
register_chart_capture <- function(id, send,
                                   session = shiny::getDefaultReactiveDomain()) {

  reg <- capture_registry(session)
  reg[[id]] <- send

  drop <- function() {
    if (identical(shiny::isolate(reg[[id]]), send)) reg[[id]] <- NULL
  }
  if (is.function(session$onDestroy)) session$onDestroy(drop)
  shiny::onStop(drop, session = session)

  invisible(id)
}

#' The Charts This Session Can Draw
#'
#' The capture service: a deck asks the browser for a picture of a chart
#' block, at a box it chooses, and gets the bitmap back. Every chart block
#' registers itself while it lives, so a block whose panel was never opened
#' can still be drawn offscreen at the export's size.
#'
#' The picture crosses the websocket, so the exchange is asynchronous:
#' `chart_capture_request()` returns a token, and `chart_capture_collect()`
#' returns an exhibit once the reply lands.
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
  # A removed chart leaves its key behind with a NULL entry.
  reg <- shiny::isolate(shiny::reactiveValuesToList(capture_registry(session)))
  names(Filter(is.function, reg))
}

# The registry key for a block id. A key is the chart's element id, which
# carries the block id as a segment ("<board>-block_<id>-expr-...").
#' @rdname chart_capture_ids
#' @export
chart_capture_for <- function(block_id,
                              session = shiny::getDefaultReactiveDomain()) {

  ids <- chart_capture_ids(session)
  hit <- grep(paste0("(^|-)block_", block_id, "-"), ids, value = TRUE)

  if (!length(hit)) NULL else hit[[1L]]
}

# Asks one registered chart for a picture at `width` x `height` CSS px. The
# token identifies the reply. A chart that is not registered is an error, so a
# deck never silently misses a chart.
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

# The replies, keyed by token. One session-level input carries all of them,
# because the browser answers from a capture host that belongs to no block.
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

# The capture for a token: NULL while in flight, a chart_capture once it lands,
# an error when the browser could not draw it.
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

  chart_capture_read(msg)
}

# The bitmap as an officer image, in inches, fitted into `max_width` and
# `max_height` when given (capture_fit()).
#' @param x A capture, from `chart_capture_collect()`.
#' @param max_width,max_height Optional box, in inches.
#' @rdname chart_capture_ids
#' @export
chart_capture_img <- function(x, max_width = NULL, max_height = NULL) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("chart_capture_img() needs the 'officer' package.", call. = FALSE)
  }

  fit <- capture_fit(x, max_width, max_height)
  w <- x$width * fit
  h <- x$height * fit

  img <- officer::external_img(chart_capture_file(x), width = w, height = h)
  attr(img, "pptx_width") <- w
  attr(img, "pptx_height") <- h
  img
}
