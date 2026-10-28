# The control channel: how one block sets another block's externally
# controllable arguments through the board's update reactive. The channel
# itself (ctrl_send(), ctrl_clear(), install_ctrl_send()), how a sender finds
# its target (ctrl_targets(), ctrl_auto_target()), and the dock extension that
# installs it on a board (new_ctrl_bridge_extension()).
#
# What a drill sends is built in R/ctrl-claims.R, the observer that sends it
# is R/ctrl-sender.R, and the card a hand-rolled sender returns is
# R/ctrl-receipt.R.

#' Cross-block external control
#'
#' Helpers that let one block set the externally controllable arguments (see
#' the `external_ctrl` argument to [blockr.core::new_block()]) of another block
#' on the same board, through the board's regular update channel. A drill-down
#' chart can feed a function block whose function calls `ctrl_send()` to push
#' the clicked patient into a patient-profile block. Sender and target share
#' no data link.
#'
#' `install_ctrl_send()` wires the channel up. Call it once per session from
#' any board component that holds the board's `update` reactive: a board
#' callback, a plugin or a dock extension server. It stores a send closure in
#' `session$userData`, where `ctrl_send()` finds it.
#'
#' Updates go through the board's full validation. Arguments that are not
#' externally controllable on the target are rejected with a notification (see
#' `blockr.core:::validate_board_update`).
#'
#' @section Semantics under lazy evaluation:
#' `ctrl_send()` writes state and never reads another block's result, so it
#' adds nothing to any evaluation closure. A function block calling it sends
#' only while it is on screen. Repeated sends of an unchanged value are no-ops
#' (`apply_block_mod_delta()` compares with `identical()`).
#'
#' @param target Character(1). Block id of the target block on the board.
#' @param ... Named values to set, one per externally controllable argument
#'   of the target block, e.g. `subject = "01-701-1015"`.
#' @param session Shiny session (defaults to the current reactive domain).
#'
#' @return `ctrl_send()` returns `TRUE` (invisibly) when the payload was
#'   handed to the update channel, `FALSE` when no channel is installed
#'   (a warning is emitted once per session). `ctrl_clear()` returns `TRUE`
#'   (invisibly) when the caller owned the target's last claim and the reset
#'   was sent, `FALSE` otherwise. `install_ctrl_send()` returns the send
#'   closure, invisibly.
#'
#' @examples
#' # Inside a function block fn (always qualify: fns are deparsed):
#' # function(data) {
#' #   ids <- unique(data$USUBJID)
#' #   if (length(ids) == 1L) {
#' #     blockr.viz::ctrl_send("profile", subject = ids)
#' #   }
#' #   data
#' # }
#'
#' @export
ctrl_send <- function(target, ..., session = shiny::getDefaultReactiveDomain()) {

  stopifnot(
    is.character(target), length(target) == 1L, nzchar(target),
    !is.null(session)
  )

  args <- list(...)

  if (length(args) == 0L || is.null(names(args)) || !all(nzchar(names(args)))) {
    stop("`ctrl_send()` expects named values, one per controllable argument.")
  }

  send <- session$userData$blockr_ctrl_send

  if (!is.function(send)) {
    if (!isTRUE(session$userData$blockr_ctrl_send_warned)) {
      session$userData$blockr_ctrl_send_warned <- TRUE
      warning(
        "ctrl_send(): no control channel installed for this board ",
        "(missing `install_ctrl_send()` in a board callback or extension); ",
        "ignoring.",
        call. = FALSE
      )
    }
    return(invisible(FALSE))
  }

  send(target, args, author = ctrl_author(session))

  invisible(TRUE)
}

#' @section Clearing (`ctrl_clear()`):
#' The undo side of `ctrl_send()`. It resets the target's controllable
#' arguments only if the last `ctrl_send()` to that target came from the same
#' block. A sender that re-evaluates without a drill (at startup, on a board
#' restore, or while a different sender's claim is active on the target) owns
#' nothing and does nothing. The values passed via `...` define "cleared" for
#' the target, e.g. `state = list(columns = list())` empties a value filter.
#'
#' @rdname ctrl_send
#' @export
ctrl_clear <- function(target, ..., session = shiny::getDefaultReactiveDomain()) {

  stopifnot(
    is.character(target), length(target) == 1L, nzchar(target),
    !is.null(session)
  )

  args <- list(...)

  if (length(args) == 0L || is.null(names(args)) || !all(nzchar(names(args)))) {
    stop("`ctrl_clear()` expects named reset values, one per controllable ",
         "argument.")
  }

  clear <- session$userData$blockr_ctrl_clear

  if (!is.function(clear)) {
    return(invisible(FALSE))
  }

  invisible(clear(target, args, author = ctrl_author(session)))
}

# The calling block's identity: the module namespace of its session. Scopes
# clears to their author.
ctrl_author <- function(session) {
  ns <- tryCatch(session$ns(""), error = function(e) NULL)
  if (is.null(ns) || !nzchar(ns)) "(root)" else ns
}

#' @param update The board update reactive, as handed to board callbacks,
#'   plugins and dock extension servers.
#' @param board The board reactive values, as handed to the same components
#'   (`board$board` is the current board). Optional: without it the channel
#'   still sends, but [ctrl_targets()] finds no candidates.
#'
#' @rdname ctrl_send
#' @export
install_ctrl_send <- function(update, board = NULL,
                              session = shiny::getDefaultReactiveDomain()) {

  stopifnot(is.function(update), !is.null(session))

  # Last author per target block id: `ctrl_send()` claims, `ctrl_clear()`
  # acts only for the current holder and releases it.
  authors <- new.env(parent = emptyenv())

  push <- function(target, args) {
    update(list(blocks = list(mod = stats::setNames(list(args), target))))
  }

  send <- function(target, args, author = NULL) {
    if (!is.null(author)) assign(target, author, envir = authors)
    push(target, args)
  }

  clear <- function(target, args, author = NULL) {
    owner <- if (exists(target, envir = authors)) get(target, envir = authors)
    if (is.null(author) || !identical(owner, author)) {
      return(FALSE)
    }
    rm(list = target, envir = authors)
    push(target, args)
    TRUE
  }

  # Reads `board$board` reactively, so a picker re-renders as blocks change.
  targets <- function(class = "value_filter_block") {

    if (is.null(board)) {
      return(character())
    }

    blks <- blockr.core::board_blocks(board$board)
    keep <- vapply(blks, inherits, logical(1L), what = class)

    if (!any(keep)) {
      return(character())
    }

    ids <- names(blks)[keep]

    labs <- vapply(
      blks[keep],
      function(b) {
        nme <- tryCatch(blockr.core::block_name(b), error = function(e) NULL)
        if (is.character(nme) && length(nme) == 1L && nzchar(nme)) nme else ""
      },
      character(1L)
    )

    labs[!nzchar(labs)] <- ids[!nzchar(labs)]

    stats::setNames(ids, labs)
  }

  session$userData$blockr_ctrl_send <- send
  session$userData$blockr_ctrl_clear <- clear
  session$userData$blockr_ctrl_targets <- targets

  invisible(send)
}

#' The board's drill destination, found by class
#'
#' What `ctrl_target = "auto"` resolves to: the one
#' `blockr.dm::new_drill_filter_block()` on the board or, on a board without
#' one, the one value filter block.
#'
#' Two or more candidates resolve to nothing, so the sender stays quiet until
#' the gear's target picker names one. A board that carries any drill filter
#' ignores value filters, even when its drill filters are ambiguous.
#'
#' @param session Shiny session (defaults to the current reactive domain).
#'
#' @return The block id, `""` when the board offers no single candidate.
#'
#' @export
ctrl_auto_target <- function(session = shiny::getDefaultReactiveDomain()) {

  drills <- ctrl_targets("drill_filter_block", session = session)

  if (length(drills)) {
    return(if (length(drills) == 1L) unname(drills) else "")
  }

  vals <- ctrl_targets("value_filter_block", session = session)

  if (length(vals) == 1L) unname(vals) else ""
}

# `ctrl_target` as a sender reads it: the block id, or the board's answer for
# the sentinel "auto".
#' @noRd
ctrl_resolve_target <- function(target, session) {
  target <- trimws(target %||% "")
  if (identical(target, "auto")) ctrl_auto_target(session) else target
}

#' Controllable blocks on the board
#'
#' The block ids a sender may point at: every block on the board of the given
#' class. It populates the target picker of the drill senders and is available
#' to a hand-rolled sender that wants one too.
#'
#' Returns `character()` when no channel is installed, or when
#' [install_ctrl_send()] was called without `board`.
#'
#' @param class Character(1). Block class to look for. The default is the value
#'   filter block, the target shape the senders in this package push (a
#'   `state` of filter conditions).
#' @param session Shiny session (defaults to the current reactive domain).
#'
#' @return A named character vector of block ids, named by block name (falling
#'   back to the id). Empty when there is no channel or no candidate.
#'
#' @export
ctrl_targets <- function(class = "value_filter_block",
                         session = shiny::getDefaultReactiveDomain()) {

  stopifnot(is.character(class), length(class) == 1L, !is.null(session))

  f <- session$userData$blockr_ctrl_targets

  if (!is.function(f)) {
    return(character())
  }

  f(class)
}

#' Control-channel bridge extension
#'
#' A dock extension with no UI that installs the cross-block control channel
#' ([install_ctrl_send()]) on the board, so that blocks can call
#' [ctrl_send()] / [ctrl_clear()] and senders can offer a target picker
#' ([ctrl_targets()]).
#'
#' A block server cannot open the channel itself because it never sees the
#' board's `update` reactive. A dock extension does, and a board carries it.
#'
#' @section Wiring:
#' Add it to the board's `extensions` and to no grid, since it renders an empty
#' div:
#'
#' ```r
#' new_dock_board(
#'   extensions = list(
#'     dag = blockr.dag::new_dag_extension(),
#'     ctrl_bridge = blockr.viz::new_ctrl_bridge_extension()
#'   ),
#'   grids = list(Main = dock_grid(ext("dag")))
#' )
#' ```
#'
#' A board needs exactly one bridge. The registry that lets [ctrl_clear()]
#' clear only its own claim lives inside the closure `install_ctrl_send()`
#' builds, so two bridges give two senders separate views of who owns what.
#'
#' @section Saved boards:
#' Dock rebuilds an extension from its constructor name and package, so the
#' bridge is an exported constructor. A board saved by an app without the
#' bridge comes back without a channel and needs one re-save.
#'
#' @param ... Forwarded to `blockr.dock::new_dock_extension()`. On restore, dock
#'   calls this constructor with the `ctor`/`pkg` it recorded, so the dots must
#'   be here for a saved board to come back.
#'
#' @return A `dock_extension` (see `blockr.dock::new_dock_extension()`).
#'
#' @examples
#' if (interactive()) {
#'   new_ctrl_bridge_extension()
#' }
#'
#' @export
new_ctrl_bridge_extension <- function(...) {

  if (!requireNamespace("blockr.dock", quietly = TRUE)) {
    stop(
      "`new_ctrl_bridge_extension()` requires blockr.dock. Install it, or ",
      "call `install_ctrl_send()` from a board callback instead.",
      call. = FALSE
    )
  }

  blockr.dock::new_dock_extension(
    server = function(id, board, update, ...) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          install_ctrl_send(update, board = board)
          list(state = list())
        }
      )
    },
    ui = function(ns, ...) htmltools::div(),
    name = "Control bridge",
    class = "ctrl_bridge_extension",
    ...
  )
}
