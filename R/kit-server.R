# The server side every block shares: the observers that take the browser's
# inputs into the block's state. An input observer only checks and writes
# state. Side effects (parsing the prepare script, running the prepare step)
# happen in reactives that read the state, because on a frozen block
# blockr.core reverts writes with a priority = Inf observer that runs after
# the input observer returns.

# An observer, run ahead of the block's others, that bumps the revision for
# every value that differs from the last known one. The gear's own writes are
# known already (viz_apply()), so only other changes count: the assistant, a
# ctrl_send, core's revert of a frozen block.
viz_rev_tracker <- function(state, revs) {
  invisible(shiny::observe({
    revs$track(lapply(state, function(rv) rv()))
  }, priority = 100))
}

# Reads `<id>_set` (checked by its input handler, see kit-input.R), passes it
# through the lock, script and stale-message gates, runs the block's
# `normalize()` over the full value list and writes what changed.
viz_settings_observer <- function(input, id, spec, state, revs,
                                  normalize = identity) {
  name <- paste0(id, "_set")
  invisible(shiny::observeEvent(input[[name]], {
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
  }))
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

# Reads `<id>_select` into the `selection` state. When clicks are events for a
# drill target (`transient()` is TRUE) each one goes to `claim` with its nonce
# and nothing is held; a clear is inert, the target owns the cohort.
viz_selection_observer <- function(input, id, state, revs, transient,
                                   claim) {
  name <- paste0(id, "_select")
  invisible(shiny::observeEvent(input[[name]], {
    msg <- input[[name]]
    if (isTRUE(transient())) {
      if (!is.null(msg$selection)) claim(msg)
      return()
    }
    viz_apply(state, list(selection = msg$selection), revs)
  }))
}

# Reads `<id>_capture` into the reactiveVal `capture`, outside the state.
viz_capture_observer <- function(input, id, capture) {
  name <- paste0(id, "_capture")
  invisible(shiny::observeEvent(input[[name]], capture(input[[name]])))
}

# The server every block view runs: the state (one reactiveVal per spec
# argument), the observers above, the payload output with the busy cue, the
# download menu when the block gives an exhibit and the drill sender when it
# gives claims. `view` ("chart", "table") names the output and the
# `<view>_set`, `<view>_select`, `<view>_capture` inputs; `init` is
# viz_check()'s result. Returns blockr.core's `list(expr, state)`, plus `cond`.
#
# `setup(kit)` builds the block's reactives from `kit` (`input`, `output`,
# `session`, `spec`, `state`, `revs`, `el` the element id the view draws into,
# `data`, `claim` and `transient` for clicks sent to a drill target, `capture`
# the browser's last picture) and returns a list:
#   payload    function returning viz_payload()'s list
#   expr       the reactive filter expression the block emits
#   exhibit    optional function returning new_viz_exhibit(); the kit adds the
#              download menu to the payload's `derived$downloads`
#   normalize  optional rules between arguments (chart_normalize(), ...)
#   claims     optional reactive of the conditions a drill target receives
#              (dd_ctrl_claims()); no sender without it
#   cond       optional reactiveValues of the block's own conditions (`error`,
#              `warning`, `message`), which blockr.core shows with the block
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
                       viz_with_downloads(kit, parts, view),
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

# The payload with the download menu in `derived$downloads`, when the block
# gives an exhibit.
viz_with_downloads <- function(kit, parts, view) {
  if (!is.function(parts$exhibit)) {
    return(parts$payload)
  }
  enabled <- function() {
    is.null(kit$state$download) || isTRUE(kit$state$download())
  }
  menu <- viz_downloads(parts$exhibit, enabled = enabled,
                        session = kit$session, filename = view)
  function() {
    p <- parts$payload()
    if (!is.null(p)) p$derived$downloads <- menu()
    p
  }
}

# Sends the claims to a drill target over the control channel. With a target
# set each click is sent once, by its nonce. Without one the held selection is
# the claim, and nothing goes out while it is the one the block was built with.
viz_drill_sender <- function(kit, claims, sel0) {
  latch <- dd_ctrl_pristine(function() kit$state$selection(), sel0)
  dd_ctrl_sender(
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
