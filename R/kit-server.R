# The server side every block shares: the observers that take the browser's
# inputs into the block's state.
#
# An observer that receives input only checks it and writes state. Parsing or
# evaluating the prepare script, running the prepare step and any other side
# effect happen in reactives that read the state. On a frozen block
# blockr.core reverts writes to external_ctrl fields with a priority = Inf
# observer, which runs after the input observer returns, so a side effect
# inside the input observer would already have run.

#' Track changes to a block's state that do not come from its gear
#'
#' An observer, run ahead of the block's other observers, that bumps the
#' revision for every value that differs from the last known one. The gear's
#' own writes are known already (viz_apply()), so only other changes count:
#' the assistant, a ctrl_send, core's revert of a frozen block.
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
#' Reads `<id>_set` (checked by its input handler, see kit-input.R), passes it
#' through the lock, script and stale-message gates, runs the block's
#' `normalize()` over the result and writes what changed.
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
    # Each setting replaces its value whole; modifyList() would merge lists.
    merged <- viz_values(state)
    for (nm in names(values)) merged[nm] <- list(values[[nm]])
    viz_apply(state, normalize(merged), revs)
  })
  invisible(obs)
}

# A message key the spec does not know, or a value it refuses, means the JS
# and the spec disagree. Warns once per block and key.
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
#' Reads `<id>_capture` (checked by its input handler, viz_read_capture())
#' into `capture`, outside the state: a picture is a product of the last
#' render, not a setting. The message is decoded here, once, so `capture()`
#' holds a `chart_capture` or NULL for every block's exhibit.
#'
#' This is the picture the downloads write. The browser sends it when the
#' download menu opens. blockr.outline decks use the capture service
#' instead (R/chart-capture.R).
#'
#' @param input The block's Shiny input.
#' @param id The view's element id.
#' @param capture A reactiveVal.
#' @return The observer, invisibly.
#' @noRd
viz_capture_observer <- function(input, id, capture) {
  name <- paste0(id, "_capture")
  obs <- shiny::observeEvent(input[[name]], {
    capture(chart_capture_read(input[[name]]))
  })
  invisible(obs)
}

#' The server every block view runs
#'
#' The skeleton of a block on the kit. It holds the block's state, one
#' reactiveVal per spec argument, and wires what every block has:
#'
#' - the revision tracker;
#' - the settings observer, which runs the block's `normalize()` over every
#'   change from the gear;
#' - the selection and capture observers, and the drop of a picture whose
#'   rows the view no longer draws (viz_with_shared());
#' - the payload output, with the busy cue while the input recomputes; the
#'   kit adds the entries every block's `derived` carries (viz_with_shared());
#' - the drill sender, when the block gives claims.
#'
#' Everything particular to the block comes from `setup(kit)`: it builds the
#' block's reactives from `kit` (`input`, `output`, `session`, `spec`,
#' `state`, `revs`, `el`, the element id the view draws into, `data`,
#' `claim` and `transient` for clicks sent to a drill target, and `capture`,
#' the browser's last picture as a `chart_capture`, or NULL). A spec without `drill_target` never goes
#' transient: it holds every click. `setup()` returns a list:
#'
#' - `payload`: a function of no arguments returning viz_payload()'s list;
#' - `expr`: the reactive filter expression the block emits;
#' - `exhibit`: a function of no arguments returning the block's
#'   new_viz_exhibit(); optional. The kit registers the download handlers
#'   (viz_downloads()) and puts the menu into the payload's
#'   `derived$downloads`;
#' - `normalize`: a function of the full value list, the block's rules
#'   between arguments (chart_normalize(), table_normalize()); optional;
#' - `claims`: a reactive of the conditions a drill target receives
#'   (ctrl_claims()); optional, no sender without it;
#' - `cond`: a reactiveValues of the block's own conditions (`error`,
#'   `warning`, `message`), which blockr.core shows with the block; optional;
#' - `script`: the block's viz_script_sources(); optional, no `script` entry
#'   in the payload without it.
#'
#' `setup()` wires anything else itself, with what `kit` holds.
#'
#' @param id The block's module id.
#' @param spec The block's `viz_spec`.
#' @param data The block's input, a reactive.
#' @param init The block's values at construction (viz_check()'s result).
#' @param view The view's id inside the module (`"chart"`, `"table"`): the
#'   output and the `<view>_set`, `<view>_select`, `<view>_capture` inputs.
#' @param setup A function of `kit`, as above.
#' @return blockr.core's `list(expr, state)`, and `cond` when `setup()`
#'   gives one.
#' @noRd
viz_block_server <- function(id, spec, data, init, view, setup) {
  shiny::moduleServer(id, function(input, output, session) {
    kit <- viz_kit(input, output, session, spec, data, init, view)
    parts <- setup(kit)
    stopifnot(is.function(parts$payload), shiny::is.reactive(parts$expr))
    viz_settings_observer(input, view, spec, kit$state, kit$revs,
                          normalize = parts$normalize %||% identity)
    viz_selection_observer(input, view, kit$state, kit$revs, kit$transient,
                           kit$claim)
    viz_capture_observer(input, view, kit$capture)
    viz_payload_output(output, input, view,
                       viz_with_shared(kit, parts, view),
                       session = session, el = kit$el)
    if (!is.null(parts$claims)) {
      viz_drill_sender(kit, parts$claims, init$selection)
    }
    c(list(expr = parts$expr, state = kit$state),
      if (!is.null(parts$cond)) list(cond = parts$cond))
  })
}

# What viz_block_server() hands a block's setup().
viz_kit <- function(input, output, session, spec, data, init, view) {
  state <- viz_state(init)
  revs <- new_viz_revs(init)
  viz_rev_tracker(state, revs)
  list(
    input = input, output = output, session = session, spec = spec,
    state = state, revs = revs, el = session$ns(view), data = data,
    # With a drill target set a click is an event, kept here instead of in
    # the selection. A block without the argument holds every click.
    claim = shiny::reactiveVal(NULL),
    transient = function() {
      !is.null(state$drill_target) && nzchar(state$drill_target() %||% "")
    },
    capture = shiny::reactiveVal(NULL)
  )
}

# The payload with the entries every block's `derived` carries beside its
# own: the download menu, when the block gives an exhibit; the prepare
# script's controls and error, when it gives one; the drill targets the gear
# offers; the ratio the browser composes a picture at. `derived` stays a
# named list, an object on the wire.
#
# A picture of rows the view no longer draws must not reach a download, so
# the capture is dropped when the payload's data revision moves. The view
# draws before its menu opens, so a download reads a fresh picture or none.
viz_with_shared <- function(kit, parts, view) {
  menu <- if (is.function(parts$exhibit)) {
    enabled <- if (is.null(kit$state$download)) {
      function() TRUE
    } else {
      function() isTRUE(kit$state$download())
    }
    viz_downloads(parts$exhibit, enabled = enabled, session = kit$session,
                  filename = view)
  }
  choices <- ctrl_choices(kit$session)
  drawn <- new.env(parent = emptyenv())
  function() {
    p <- parts$payload()
    if (is.null(p)) {
      return(p)
    }
    if (!is.null(drawn$rev) && !identical(p$data_rev, drawn$rev)) {
      kit$capture(NULL)
    }
    drawn$rev <- p$data_rev
    if (!is.null(menu)) p$derived$downloads <- menu()
    if (!is.null(parts$script)) p$derived$script <- parts$script$derived()
    p$derived$ctrl_choices <- ctrl_choices_list(choices())
    p$derived$capture_ratio <- canvas_capture_ratio()
    p
  }
}

# Sends the claims to a drill target over the control channel. With a target
# set each click is sent once, by its nonce. Without one the held selection is
# the claim, and nothing goes out while it is the one the block was built with.
viz_drill_sender <- function(kit, claims, sel0) {
  latch <- ctrl_pristine(function() kit$state$selection(), sel0)
  ctrl_sender(
    kit$state$drill_target, claims,
    function() {
      still <- latch()
      is.null(kit$claim()) && still
    },
    kit$session,
    r_nonce = function() {
      c1 <- kit$claim()
      if (is.null(c1)) NULL else c1$nonce
    }
  )
}
