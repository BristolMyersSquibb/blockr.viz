# The server side every block shares: the observers that take the browser's
# inputs into the block's state.
#
# The rule for all of them: an observer that receives input only checks it
# and writes state. Parsing or evaluating the prepare script, running the
# prepare step and any other side effect happen in reactives that read the
# state. On a frozen block blockr.core reverts writes to external_ctrl
# fields with a priority = Inf observer, which runs after the input observer
# returns; code evaluated inside the input observer would already have run.

#' Track changes to a block's state that do not come from its gear
#'
#' An observer, run ahead of the block's other observers, that bumps the
#' revision for every value that differs from the last known one. The gear's
#' own writes are known already (viz_apply()), so only changes from
#' elsewhere count: the assistant, a ctrl_send, core's revert of a frozen
#' block.
#'
#' @param state The block's state from viz_state().
#' @param revs Its counter from new_viz_revs().
#' @return The observer, invisibly.
#' @noRd
viz_rev_tracker <- function(state, revs) {
  obs <- shiny::observe({
    revs$track(lapply(state, function(rv) rv()))
  }, priority = 100)
  invisible(obs)
}

#' Take the gear's settings into the state
#'
#' Reads `<id>_set` (already checked by its input handler, see
#' kit-input.R), passes it through the lock, script and stale-message gates,
#' runs the block's `normalize()` over the result and writes what changed.
#'
#' @param input The block's Shiny input.
#' @param id The view's element id, without the `_set` suffix.
#' @param spec The block's `viz_spec`.
#' @param state,revs The block's state and revision counter.
#' @param normalize A pure function of the full value list that returns it
#'   with the rules between arguments applied (the chart's orientation, ...).
#' @return The observer, invisibly.
#' @noRd
viz_settings_observer <- function(input, id, spec, state, revs,
                                  normalize = identity) {
  name <- paste0(id, "_set")
  obs <- shiny::observeEvent(input[[name]], {
    msg <- input[[name]]
    viz_log_rejected(msg$rejected, spec$block)
    values <- viz_gate_set(msg, revs)
    if (!length(values)) {
      return()
    }
    merged <- utils::modifyList(viz_values(state), values, keep.null = TRUE)
    viz_apply(state, normalize(merged), revs)
  })
  invisible(obs)
}

# A message key the spec does not know or a value it refuses means the JS
# and the spec disagree. Logged once per block and key, not per message.
viz_log_rejected <- function(rejected, block) {
  for (key in rejected) {
    rlang::warn(
      paste0("The ", block, " gear sent a setting the spec refuses: `", key,
             "`."),
      .frequency = "once",
      .frequency_id = paste("blockr.viz rejected", block, key)
    )
  }
}

#' Take clicks and brushes into the block
#'
#' Reads `<id>_select`. A block that holds its selection writes it to the
#' `selection` state; a block whose clicks are events for a drill target
#' (`transient()` is TRUE) hands each one, with its nonce, to `claim`, and
#' holds nothing. A clear in that mode is inert: the target owns the cohort.
#'
#' @param input The block's Shiny input.
#' @param id The view's element id.
#' @param state,revs The block's state and revision counter.
#' @param transient A function returning whether clicks are events.
#' @param claim A reactiveVal for the last click in event mode.
#' @return The observer, invisibly.
#' @noRd
viz_selection_observer <- function(input, id, state, revs, transient,
                                   claim) {
  name <- paste0(id, "_select")
  obs <- shiny::observeEvent(input[[name]], {
    msg <- input[[name]]
    if (isTRUE(transient())) {
      if (!is.null(msg$selection)) claim(msg)
      return()
    }
    viz_apply(state, list(selection = msg$selection), revs)
  })
  invisible(obs)
}

#' Keep the browser's last picture of the view
#'
#' Reads `<id>_capture` into `capture`, outside the state: a picture is a
#' product of the last render, not a setting.
#'
#' @param input The block's Shiny input.
#' @param id The view's element id.
#' @param capture A reactiveVal.
#' @return The observer, invisibly.
#' @noRd
viz_capture_observer <- function(input, id, capture) {
  name <- paste0(id, "_capture")
  obs <- shiny::observeEvent(input[[name]], capture(input[[name]]))
  invisible(obs)
}
