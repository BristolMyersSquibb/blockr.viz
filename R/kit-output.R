# What a block view sends the browser: one payload, computed by a Shiny
# output and sent as a message.
#
# The output decides when the payload is built: Shiny runs it when something
# it read changed. The payload itself goes out as a custom message the
# moment it is built.
# Shiny writes a message at once and an output's value only after every
# observer of the flush has run, which includes the block's evaluation and
# its preview, and on a board every other block. So each block draws as soon
# as its own payload is ready. The output's value is a small token.
#
# A hidden view is not built for, with one exception: the first payload is
# built before the browser has said whether the element shows. Shiny itself
# waits for that word, which costs a new block a round trip, and a board that
# is loading the time until every block has evaluated, before the first chart
# can draw. A block that never shows costs one payload and none after it
# (viz_output_hidden()).
#
# The binding (inst/js/core/binding.js) keeps the last payload and the rows per
# id: R leaves the rows out of a payload whose data revision the client holds,
# and a re-mounted panel draws from what the binding kept, without asking R.

#' The element a block view draws into
#'
#' @param id The output id.
#' @param view The view the shared binding builds for it (`"chart"`,
#'   `"table"`).
#' @param ... Further tags or attributes for the element.
#' @noRd
viz_output <- function(id, view, ...) {
  htmltools::div(id = id, class = "blockr-viz-output",
                 `data-viz-view` = view, ...)
}

#' A render function whose value is the payload, as R built it
#' @noRd
viz_render <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::installExprFunction(expr, "func", env, quoted,
                                     label = "viz_render")
  shiny::createRenderFunction(
    func,
    function(value, session, name, ...) value,
    function(outputId) viz_output(outputId, "")
  )
}

#' Send a block view's payload
#'
#' `build()` returns the payload from viz_payload(). Nothing goes out when it
#' is identical to the last one, or when it stops on a silent `req()` (a
#' block the dock has parked): the render cancels and the client keeps its
#' value and picture. The rows go out only when their revision changed, and
#' with rows the client holds the column description goes out only when it
#' changed: the client keeps both (inst/js/core/binding.js). A click or a
#' setting then sends the settings and `derived` alone. A client that holds
#' no rows for a payload's revision asks through `<id>_need`, and the next
#' render sends everything.
#'
#' The busy cue goes out before `build()` reads the input. A render that draws
#' nothing (cancelled, or an error) ends the cue here; otherwise the view ends
#' it once it has drawn.
#'
#' @param output,input The block's Shiny output and input.
#' @param id The output id.
#' @param build A function of no arguments returning the payload.
#' @param session,el The block's session and the element id the view draws
#'   into, for the busy cue and the message; without them no cue is sent and
#'   the payload is the output's value.
#' @return Invisibly, `NULL`.
#' @noRd
viz_payload_output <- function(output, input, id, build, session = NULL,
                               el = NULL) {
  sent <- new.env(parent = emptyenv())
  sent$n <- 0L
  full <- shiny::reactiveVal(0L)
  busy <- !is.null(session) && !is.null(el)
  post <- busy && viz_posts(session)
  done <- function() if (busy) busy_cue_done(session, el)
  output[[id]] <- viz_render({
    full()
    if (post) {
      # Not for an element known to be hidden, nor, once the first payload
      # is out, for one the browser has not spoken about. This reads the
      # browser's word, so the render runs again when it changes.
      hidden <- viz_output_hidden(session, el)
      if (isTRUE(hidden) || (is.null(hidden) && sent$n > 0L)) {
        shiny::req(FALSE, cancelOutput = TRUE)
      }
    }
    if (busy) busy_cue_start(session, el, "drawing")
    msg <- tryCatch(build(), shiny.silent.error = function(e) NULL,
                    error = function(e) {
                      done()
                      stop(e)
                    })
    if (is.null(msg) || identical(msg, sent$msg)) {
      done()
      shiny::req(FALSE, cancelOutput = TRUE)
    }
    sent$msg <- msg
    columns <- msg$columns
    if (identical(msg$data_rev, sent$data_rev)) {
      msg$data <- NULL
      # Left out, the key is absent; a block without columns sends `null`.
      if (isTRUE(sent$has_columns) && identical(columns, sent$columns)) {
        msg$columns <- NULL
      }
    }
    sent$data_rev <- sent$msg$data_rev
    sent$columns <- columns
    sent$has_columns <- TRUE
    if (!post) {
      return(msg)
    }
    session$sendCustomMessage("blockr-viz:payload",
                              list(id = el, payload = msg))
    sent$n <- sent$n + 1L
    list(posted = sent$n, state_rev = msg$state_rev, data_rev = msg$data_rev)
  })
  # Ahead of the block's other outputs (its preview), behind the observers
  # that settle what the payload reads (priority 50 and up).
  shiny::outputOptions(output, id, priority = 40)
  # The render decides about a hidden element itself (above).
  if (post) shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  shiny::observeEvent(input[[paste0(id, "_need")]], {
    sent$msg <- NULL
    sent$data_rev <- NULL
    sent$has_columns <- FALSE
    full(shiny::isolate(full()) + 1L)
  })
  invisible(NULL)
}

#' Whether a session's payloads go out as messages
#'
#' They do in a browser session. A test session has no browser to take a
#' message, so there the output's value is the payload, which is also what
#' the option `blockr.viz.post_payload = FALSE` gives.
#' @noRd
viz_posts <- function(session) {
  if (!isTRUE(getOption("blockr.viz.post_payload", TRUE))) {
    return(FALSE)
  }
  root <- if (is.function(session$rootScope)) session$rootScope() else session
  !inherits(root, "MockShinySession")
}

#' Whether the browser shows a view's element
#'
#' What Shiny's client reports for a bound output: `TRUE` hidden, `FALSE`
#' showing, `NULL` when it has not said yet (the element is not on the page,
#' or its first report is on its way). Read reactively.
#' @noRd
viz_output_hidden <- function(session, el) {
  session$clientData[[paste0("output_", el, "_hidden")]]
}

#' The payload of a block view
#'
#' @param spec The block's `viz_spec`.
#' @param values The block's values, as the state holds them.
#' @param rows The rows from a viz_rows() cache.
#' @param revs The block's revision counter.
#' @param derived What the server computes for the view (resolved titles,
#'   series, download formats, an empty state). The client does not send it
#'   back.
#' @param columns The column description the view reads, viz_wire_columns()
#'   shape. By default the shipped rows' columns; a view whose gear picks
#'   from every column of its input passes that frame's.
#' @return A list: `state_rev`, `data_rev`, `columns`, `config`, `derived`,
#'   `data`.
#' @noRd
viz_payload <- function(spec, values, rows, revs, derived = list(),
                        columns = rows$columns) {
  list(
    state_rev = revs$now(),
    data_rev = rows$rev,
    columns = columns,
    config = viz_wire(spec, values),
    derived = if (length(derived)) {
      derived
    } else {
      structure(list(), names = character())
    },
    data = rows$data
  )
}

#' Rows for the browser and their revision
#'
#' A function `(data, columns)` that returns both with a revision: the
#' chart's written rows (viz_wire_df()), the table's model. The revision goes
#' up only when `data` differs from the last call's, so a re-evaluation that
#' yields the same rows, or a click or a title edit, keeps the browser's
#' copy, sort and folds.
#' @noRd
viz_rows <- function() {
  last <- new.env(parent = emptyenv())
  last$rev <- 0L
  function(data, columns = NULL) {
    if (last$rev == 0L || !identical(data, last$data)) {
      last$rev <- last$rev + 1L
      last$data <- data
    }
    list(data = data, columns = columns, rev = last$rev)
  }
}

#' A one-slot cache keyed by identical()
#'
#' A dock panel visit hands the chart server an equal frame, which invalidates
#' every reactive that reads it. The costly ones (serializing the rows, the
#' column metadata, the band and smoother fits, the scale resolution) keep their
#' last answer and return it while their inputs are identical(). On the same
#' object that is a pointer comparison; on an equal copy it walks the values,
#' which is still much cheaper than the work it saves.
#'
#' @return A function `(key, compute)` that returns the last value when `key`
#'   is identical to the last key, and otherwise calls `compute()` and keeps
#'   its result. A `compute()` that errors leaves the cache as it was.
#' @noRd
last_value_cache <- function() {
  env <- new.env(parent = emptyenv())
  env$full <- FALSE
  function(key, compute) {
    if (env$full && identical(key, env$key)) {
      return(env$value)
    }
    value <- compute()
    env$key <- key
    env$value <- value
    env$full <- TRUE
    value
  }
}

# The busy cue's two messages (inst/js/core/busy.js).
# `id` is the element the cue dims, the view's element. Blocks outside this
# package that render through viz send the same two messages.

busy_cue_start <- function(session, id, label = "computing") {
  session$sendCustomMessage("blockr-busy", list(id = id, label = label))
}

busy_cue_done <- function(session, id) {
  session$sendCustomMessage("blockr-busy-done", list(id = id))
}
