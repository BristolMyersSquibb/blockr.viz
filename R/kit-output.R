# What a block view sends the browser: one payload through a Shiny output.
#
# Shiny keeps an output's last value on the client and hands it to the binding
# whenever an element under that id binds, so a re-mounted panel draws without
# asking R. The binding (inst/js/core/binding.js) keeps rows per id, because R
# leaves them out of a payload whose data revision the client holds.

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
#' value and picture. The rows go out only when their revision changed. A
#' client that holds no rows for a payload's revision asks through
#' `<id>_need`, and the next render sends everything.
#'
#' The busy cue goes out before `build()` reads the input. A render that draws
#' nothing (cancelled, or an error) ends the cue here; otherwise the view ends
#' it once it has drawn.
#'
#' @param output,input The block's Shiny output and input.
#' @param id The output id.
#' @param build A function of no arguments returning the payload.
#' @param session,el The block's session and the element id the view draws
#'   into, for the busy cue; without them no cue is sent.
#' @return Invisibly, `NULL`.
#' @noRd
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

#' Rows for the browser, written once per content
#'
#' A function of a data frame that returns viz_wire_df()'s columns and data
#' plus a revision. An identical frame returns the last result without
#' writing. The revision goes up only when the written rows differ, so a
#' re-evaluation that yields the same rows keeps the client's copy.
#' @noRd
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

# The busy cue's two messages (inst/js/busy-cue.js, loaded with the JS core).
# `id` is the element the cue dims, the view's element. Blocks outside this
# package that render through viz send the same two messages.

busy_cue_start <- function(session, id, label = "computing") {
  session$sendCustomMessage("blockr-busy", list(id = id, label = label))
}

busy_cue_done <- function(session, id) {
  session$sendCustomMessage("blockr-busy-done", list(id = id))
}
