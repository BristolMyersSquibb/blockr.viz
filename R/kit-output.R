# What a block view sends the browser: one payload through a Shiny output.
# Shiny hands an output's last value to the binding whenever an element under
# that id binds, so a re-mounted panel draws without asking R. The binding
# (inst/js/core/binding.js) keeps rows per id, because R leaves them out of a
# payload whose data revision the client holds.

# The element a block view draws into. `view` is the view the shared binding
# builds for it ("chart", "table").
viz_output <- function(id, view, ...) {
  htmltools::div(id = id, class = "blockr-viz-output",
                 `data-viz-view` = view, ...)
}

# A render function whose value is the payload, as R built it.
viz_render <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::installExprFunction(expr, "func", env, quoted,
                                     label = "viz_render")
  shiny::createRenderFunction(
    func,
    function(value, session, name, ...) value,
    function(outputId) viz_output(outputId, "")
  )
}

# Sends a block view's payload. `build()` returns the payload from
# viz_payload(). Nothing goes out when it is identical to the last one, or when
# it stops on a silent `req()` (a block the dock has parked): the render
# cancels and the client keeps its value and picture. The rows go out only when
# their revision changed. A client that holds no rows for a payload's revision
# asks through `<id>_need`, and the next render sends everything.
#
# The busy cue goes out before `build()` reads the input. A render that draws
# nothing (cancelled, or an error) ends the cue here; otherwise the view ends
# it once it has drawn. Without `session` and `el` no cue is sent.
viz_payload_output <- function(output, input, id, build, session = NULL,
                               el = NULL) {
  sent <- new.env(parent = emptyenv())
  full <- shiny::reactiveVal(0L)
  busy <- !is.null(session) && !is.null(el)
  done <- function() if (busy) busy_cue_done(session, el)
  output[[id]] <- viz_render({
    full()
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
    if (identical(msg$data_rev, sent$data_rev)) {
      msg$data <- NULL
    }
    sent$data_rev <- msg$data_rev
    msg
  })
  shiny::observeEvent(input[[paste0(id, "_need")]], {
    sent$msg <- NULL
    sent$data_rev <- NULL
    full(shiny::isolate(full()) + 1L)
  })
  invisible(NULL)
}

# The payload of a block view: a list of `state_rev`, `data_rev`, `columns`,
# `config`, `derived` and `data`. `rows` comes from a viz_rows() cache.
# `derived` is what the server computes for the view (resolved titles, series,
# download formats, an empty state); the client does not send it back.
viz_payload <- function(spec, values, rows, revs, derived = list()) {
  # An empty `derived` stays a named list, so it is an object on the wire.
  if (!length(derived)) derived <- structure(list(), names = character())
  list(
    state_rev = revs$now(),
    data_rev = rows$rev,
    columns = rows$columns,
    config = viz_wire(spec, values),
    derived = derived,
    data = rows$data
  )
}

# Rows for the browser, written once per content: a function of a data frame
# that returns viz_wire_df()'s columns and data plus a revision. An identical
# frame returns the last result without writing. The revision goes up only when
# the written rows differ, so a re-evaluation that yields the same rows keeps
# the client's copy.
viz_rows <- function() {
  last <- new.env(parent = emptyenv())
  last$rev <- 0L
  function(df) {
    if (!is.null(last$out) && identical(df, last$df)) {
      return(last$out)
    }
    w <- viz_wire_df(df)
    if (!identical(w$data, last$out$data)) {
      last$rev <- last$rev + 1L
    }
    last$df <- df
    last$out <- c(w, list(rev = last$rev))
    last$out
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
