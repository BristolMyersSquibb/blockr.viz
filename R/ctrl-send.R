#' Cross-block external control
#'
#' A pair of helpers that let one block set the externally controllable
#' arguments (see the `external_ctrl` argument to [blockr.core::new_block()])
#' of another block on the same board, through the board's regular update
#' channel. Typical use: a drill-down chart feeds a function block whose
#' function calls `ctrl_send()` to push the clicked patient into a
#' patient-profile block -- no data link between sender and target, so lazy
#' evaluation never drags the sender's upstream into the target's closure.
#'
#' `install_ctrl_send()` wires the channel up. It must be called once per
#' session by any board component that holds the board's `update` reactive:
#' a board callback, a plugin, or a dock extension server. It stashes a send
#' closure in `session$userData`, where `ctrl_send()` -- called from block
#' code, e.g. inside a function block's `fn` -- picks it up.
#'
#' Updates pushed this way go through the board's full validation:
#' arguments that are not externally controllable on the target block are
#' rejected with a user-facing notification (see
#' `blockr.core:::validate_board_update`).
#'
#' @section Semantics under lazy evaluation:
#' `ctrl_send()` writes state; it never reads another block's result, so it
#' adds nothing to any evaluation closure. A function block calling it is
#' re-evaluated (and thus sends) only while it is on screen, which is
#' exactly when its upstream chart can be clicked. Repeated sends of an
#' unchanged value are no-ops (`apply_block_mod_delta()` compares with
#' `identical()`).
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
#'   was sent, `FALSE` otherwise (not the owner, or no channel installed).
#'   `install_ctrl_send()` returns the send closure, invisibly.
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
#' The undo side of `ctrl_send()`, with ownership semantics: it resets the
#' target's controllable arguments **only if the last `ctrl_send()` to that
#' target came from the same block**. This is what lets a data-driven sender
#' propagate an un-drill (its input reverts to the no-claim shape, so it
#' calls `ctrl_clear()`) without ever clobbering state it does not own: a
#' sender that merely re-evaluates undrilled -- at startup, on a board
#' restore, or while a *different* sender's claim (or a manual edit riding
#' on one) is active on the target -- finds it owns nothing and no-ops.
#' Named values passed via `...` are what "cleared" means for the target
#' (e.g. `state = list(columns = list())` empties a value filter).
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

# The calling block's identity: the module namespace of the session the
# helper is invoked from (ctrl_send()/ctrl_clear() run inside the block's
# own server / expr evaluation). Used only to scope clears to their author.
ctrl_author <- function(session) {
  ns <- tryCatch(session$ns(""), error = function(e) NULL)
  if (is.null(ns) || !nzchar(ns)) "(root)" else ns
}

#' @param update The board update reactive, as handed to board callbacks,
#'   plugins and dock extension servers.
#' @param board The board reactive values, as handed to the same components
#'   (`board$board` is the current board). Optional: without it the channel
#'   still sends, but [ctrl_targets()] finds no candidates and senders fall
#'   back to typing a block id.
#'
#' @rdname ctrl_send
#' @export
install_ctrl_send <- function(update, board = NULL,
                              session = shiny::getDefaultReactiveDomain()) {

  stopifnot(is.function(update), !is.null(session))

  # Last-author registry, one entry per target block id. `ctrl_send()`
  # claims authorship; `ctrl_clear()` only acts when the caller still holds
  # it (and releases it when it does).
  authors <- new.env(parent = emptyenv())

  send <- function(target, args, author = NULL) {
    if (!is.null(author)) assign(target, author, envir = authors)
    update(
      list(blocks = list(mod = stats::setNames(list(args), target)))
    )
  }

  clear <- function(target, args, author = NULL) {
    owner <- if (exists(target, envir = authors)) get(target, envir = authors)
    if (is.null(author) || !identical(owner, author)) {
      return(FALSE)
    }
    rm(list = target, envir = authors)
    update(
      list(blocks = list(mod = stats::setNames(list(args), target)))
    )
    TRUE
  }

  # Candidate targets, for senders that offer a picker rather than a text
  # field. Reads `board$board` reactively, so a picker re-renders as blocks
  # come and go.
  targets <- function(class = "value_filter_block") {

    if (is.null(board)) {
      return(character())
    }

    blks <- blockr.core::board_blocks(board$board)

    if (!length(blks)) {
      return(character())
    }

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

#' Controllable blocks on the board
#'
#' The block ids a sender may point at: every block on the board of the given
#' class. Populates the target picker of the drill senders (the table, chart
#' and tile blocks' external-control option, and
#' `blockr.extra::new_ctrl_filter_block()`), and is available to a hand-rolled
#' sender (a function block calling [ctrl_send()]) that wants one too.
#'
#' Reads the board reactively, so a picker built on it re-renders as blocks are
#' added and removed. Returns `character()` when no channel is installed, or
#' when [install_ctrl_send()] was called without `board` -- in which case a
#' sender should fall back to a plain block-id field rather than offering an
#' empty picker.
#'
#' @param class Character(1). Block class to look for. The default is the value
#'   filter block, which is the one target shape [ctrl_send()] senders in this
#'   package push (a `state` of filter conditions); it serves both plain data
#'   frames and `dm`s.
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

#' Read filter conditions off a drilled annotated data frame
#'
#' Turns the drilled subset of an annotated (ARD-shaped) data frame into the
#' `columns` payload a value filter takes, so a sender can push it with
#' [ctrl_send()]. This is the claim logic behind the drill senders, exported
#' so a function block prototyping a bespoke sender can call it instead of
#' carrying its own copy.
#'
#' @section Two claim modes:
#' **Annotated (`columns = NULL`, the default) -- table and summary drills.**
#' The drilled data frame is read on its ARD identity columns: `.variable` is
#' the source column *name*, `.variable_level` the raw source *value* on that
#' row, and each `.group<k>` / `.group<k>_level` pair is an enclosing grouping
#' dimension. Dimensions come back outermost first (`.group1`, `.group2`, ...),
#' with the `.variable` leaf last.
#'
#' **Source columns (`columns` given) -- chart and tile drills.** A chart does
#' not carry the ARD identity: it coerces its input with `as_plain_df()`, which
#' *drops* the `.variable` columns, and its drilled output is a plain row subset
#' of the source data. There is nothing in that subset saying which column was
#' drilled, so the caller names the candidates -- `columns = "SEX"`, or several
#' if the chart's drill column varies -- and each is claimed from the subset.
#'
#' @section What counts as a claim:
#' Either way, a dimension becomes a filter condition only when the subset
#' resolves it to **exactly one** value: one value is a decision, many are not.
#'
#' That single rule is what makes an un-drill propagate. Clicking one bar (or
#' one level row) leaves `SEX` single-valued -- a claim. An *undrilled* chart or
#' table passes all its rows through, so `SEX` still holds both `F` and `M` --
#' no claim, and the caller's cue to [ctrl_clear()]. A brush across several bars
#' is likewise deliberately *not* a claim, and neither is a header row in a
#' nested table whose leaf stays multi-valued (its outer group is still claimed).
#'
#' @param data A data frame, typically the drilled output of a table, chart or
#'   tile block. Anything that resolves no dimension yields an empty list rather
#'   than an error.
#' @param table Character(1). Name of the table in the target's `dm` that the
#'   conditions apply to. Empty (`""`) for a value filter fed a plain data
#'   frame: its conditions name no table.
#' @param mode Character(1). Filter mode for each condition, e.g. `"multi"`.
#' @param columns Character vector of *source* column names to claim, for
#'   drilled output that carries no ARD identity (charts, tiles). `NULL` (the
#'   default) reads the ARD identity columns instead.
#'
#' @return A list of `list(name=, table=, mode=, values=)` entries, one per
#'   claimed dimension. Empty when nothing is claimed.
#'
#' @examples
#' # Annotated mode: a drilled summary table.
#' drilled <- data.frame(
#'   .variable = "SEX", .variable_level = "F", n = 143
#' )
#' drill_claim_columns(drilled, table = "adsl")
#'
#' # Source-column mode: a chart drilled to one bar.
#' drill_claim_columns(
#'   data.frame(USUBJID = c("01-001", "01-002"), SEX = c("F", "F")),
#'   table = "adsl",
#'   columns = "SEX"
#' )
#'
#' @export
drill_claim_columns <- function(data, table, mode = "multi", columns = NULL) {

  stopifnot(
    is.character(table), length(table) == 1L,
    is.character(mode), length(mode) == 1L,
    is.null(columns) || is.character(columns)
  )

  if (!is.data.frame(data)) {
    return(list())
  }

  nms <- names(data)

  # A dm-backed value filter needs to know which table a condition applies to;
  # one filtering a plain data frame has no tables to name, and its conditions
  # carry no `table` at all. An empty `table` means the latter.
  cond <- function(name, values) {
    if (nzchar(table)) {
      list(name = name, table = table, mode = mode, values = values)
    } else {
      list(name = name, mode = mode, values = values)
    }
  }

  # Source-column mode: no ARD identity to read (chart / tile drills), so the
  # caller names the columns a click may claim.
  if (length(columns)) {
    claims <- list()

    for (col in columns) {
      if (!col %in% nms) {
        next
      }
      value <- single_value(as.character(data[[col]]))
      if (is.null(value)) {
        next
      }
      claims[[length(claims) + 1L]] <- cond(col, value)
    }

    return(claims)
  }

  if (!all(c(".variable", ".variable_level") %in% nms)) {
    return(list())
  }

  var <- as.character(data$.variable)
  lvl <- as.character(data$.variable_level)
  keep <- !is.na(var) & !is.na(lvl) & nzchar(lvl)

  if (!any(keep)) {
    return(list())
  }

  cols <- list()

  # Enclosing group dimensions, outermost first.
  for (g in group_level_cols(nms)) {
    gname <- substr(g, 1L, nchar(g) - 6L)
    if (!gname %in% nms) {
      next
    }
    name <- single_value(as.character(data[[gname]])[keep])
    value <- single_value(as.character(data[[g]])[keep])
    if (is.null(name) || is.null(value)) {
      next
    }
    cols[[length(cols) + 1L]] <- cond(name, value)
  }

  # The `.variable` leaf.
  name <- single_value(var[keep])
  value <- single_value(lvl[keep])

  if (!is.null(name) && !is.null(value)) {
    cols[[length(cols) + 1L]] <- cond(name, value)
  }

  cols
}

# `.group<k>_level` columns, ordered by k.
group_level_cols <- function(nms) {
  gl <- nms[startsWith(nms, ".group") & endsWith(nms, "_level")]
  gl[order(as.integer(substr(gl, 7L, nchar(gl) - 6L)))]
}

# The single distinct value of `x`, or NULL if it does not resolve to one.
single_value <- function(x) {
  u <- unique(x[!is.na(x) & nzchar(x)])
  if (length(u) == 1L) u else NULL
}

#' Receipt for a cross-block control send
#'
#' Builds the status card a sender block returns as its output, so a
#' function block driving another block via [ctrl_send()] shows a
#' readable summary instead of a debug data frame. Function block outputs
#' render HTML when the result is html-renderable, so returning this as the
#' last expression of the `fn` is all it takes.
#'
#' @section It never reads the target:
#' `ctrl_receipt()` is a pure function of its arguments: it formats `cols`,
#' which the caller has just computed, and knows nothing else. It does not
#' read the target block's state, the board, or any reactive -- there is no
#' back-edge from target to sender, and there must not be one: a function
#' block's `fn` re-runs as a whole, so a reactive read of the target would
#' re-trigger the [ctrl_send()] in the same body, which re-triggers any
#' sibling sender, forever.
#'
#' That is why the card is written in the past tense ("Sent to Cohort"). It
#' reports what this block pushed, which stays true whatever happens to the
#' target afterwards -- including a later sender replacing this claim
#' (`ctrl_send()` is last-write-wins). What the target currently *holds* is
#' the target's own business to display.
#'
#' @param cols Columns pushed to the target, in the shape a value filter
#'   takes: a list of `list(name=, table=, mode=, values=)` entries. Empty
#'   (or `NULL`) renders the idle state.
#' @param target Character(1). Human-readable name of the block written to,
#'   e.g. `"Cohort"`. Shown in the heading.
#' @param hint Character(1) or `NULL`. Secondary line: how to undo (active
#'   state) or how to start (idle state).
#'
#' @return An [htmltools::tag()], carrying its own stylesheet dependency.
#'
#' @examples
#' # Inside a sender function block's fn (always qualify: fns are deparsed):
#' # function(data) {
#' #   cols <- list(list(name = "SEX", table = "adsl",
#' #                     mode = "multi", values = "F"))
#' #   blockr.viz::ctrl_send("cohort_filter", state = list(columns = cols))
#' #   blockr.viz::ctrl_receipt(cols, "Cohort")
#' # }
#'
#' @export
ctrl_receipt <- function(cols = list(), target = NULL, hint = NULL) {

  stopifnot(
    is.null(cols) || is.list(cols),
    is.null(target) || (is.character(target) && length(target) == 1L),
    is.null(hint) || (is.character(hint) && length(hint) == 1L)
  )

  if (!length(cols)) {
    return(
      htmltools::div(
        class = "ctrl-receipt ctrl-receipt--idle",
        htmltools::div(class = "ctrl-receipt-head", "Nothing sent"),
        receipt_hint(hint %||% "Click a level row in the summary."),
        ctrl_receipt_dep()
      )
    )
  }

  chips <- lapply(cols, function(col) {
    htmltools::div(
      class = "ctrl-receipt-chip",
      htmltools::span(class = "ctrl-receipt-name", as.character(col$name)),
      htmltools::span(class = "ctrl-receipt-op", "="),
      htmltools::span(
        class = "ctrl-receipt-value",
        paste(as.character(col$values), collapse = ", ")
      )
    )
  })

  htmltools::div(
    class = "ctrl-receipt",
    htmltools::div(
      class = "ctrl-receipt-head",
      if (is.null(target)) {
        "Sent"
      } else {
        htmltools::tagList(
          "Sent to ",
          htmltools::span(class = "ctrl-receipt-target", target)
        )
      }
    ),
    htmltools::div(class = "ctrl-receipt-chips", chips),
    receipt_hint(hint),
    ctrl_receipt_dep()
  )
}

receipt_hint <- function(hint) {
  if (is.null(hint)) {
    return(NULL)
  }
  htmltools::div(class = "ctrl-receipt-hint", hint)
}

ctrl_receipt_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-ctrl-receipt",
    version = as.character(utils::packageVersion("blockr.viz")),
    src = system.file("css", package = "blockr.viz"),
    stylesheet = "ctrl-receipt.css"
  )
})

#' Control-channel bridge extension
#'
#' A dock extension with no UI whose only job is to install the cross-block
#' control channel ([install_ctrl_send()]) on the board, so that blocks can
#' call [ctrl_send()] / [ctrl_clear()] and senders can offer a target picker
#' ([ctrl_targets()]).
#'
#' A block server only ever receives `(id, data)` -- it never sees the board's
#' `update` reactive, so it cannot open the channel for itself. Board
#' callbacks, plugins and dock extensions do see it; an extension is the one of
#' those that a board carries with it, which is what makes this the piece to
#' add to an app.
#'
#' @section Wiring:
#' Add it to the board's `extensions`, and to **no** grid -- it renders an
#' empty div, and putting it on screen would just add a blank panel:
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
#' One instance serves every sender on the board, and there must only be one:
#' the last-author registry that makes [ctrl_clear()] clear only its own claim
#' lives inside the closure `install_ctrl_send()` builds, so two bridges would
#' give two senders two disjoint views of who owns what.
#'
#' @section Saved boards:
#' Dock serializes an extension by constructor name and package and rebuilds it
#' on restore, which is why this is an exported constructor rather than the
#' handful of lines it wraps: a closure defined in an `app.R` has no package to
#' record, and would not survive a board round-trip. Note that boards saved
#' *before* the bridge was added to the app carry their old extension list and
#' come back without a channel -- they need one re-save.
#'
#' @return A `dock_extension` (see `blockr.dock::new_dock_extension()`).
#'
#' @examples
#' if (interactive()) {
#'   new_ctrl_bridge_extension()
#' }
#'
#' @export
new_ctrl_bridge_extension <- function() {

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
    class = "ctrl_bridge_extension"
  )
}

# ---------------------------------------------------------------------------
# Drill sender glue -- the shared block-side wiring behind the table / chart /
# tile blocks' "Send to filter (beta)" gear section. One code path for all
# three: each block server supplies its OWN drill-filter state, and these
# helpers turn it into a claim (dd_ctrl_claims) and keep the target block in
# sync (dd_ctrl_sender). Deliberately NOT exported: the public surface is the
# block argument (`ctrl_target` / `ctrl_table`), not the plumbing.
# ---------------------------------------------------------------------------

#' Claims from a block's own drill-filter state.
#'
#' `filters` is a named list, column -> drilled value(s), taken from the
#' block's filter reactives (the state its drill click wrote). The input is
#' subset on those pairs and the claim is read off the subset with
#' [drill_claim_columns()] -- so the one-value-per-dimension rule (and the
#' un-drill propagation it buys) is the same one the standalone
#' `ctrl_filter_block` applies downstream.
#'
#' Which claim mode applies falls out of the filter columns themselves: a
#' structured table drills on its dot-prefixed identity columns
#' (`.variable`, `.group<k>`, ...), so all-dot filters read the ARD identity
#' off the subset (`columns = NULL`); a chart / tile / flat table drills on a
#' real source column, which IS the claim column. The block never asks the
#' user which column was drilled -- it knows.
#' @noRd
dd_ctrl_claims <- function(data, table, filters) {

  if (!is.data.frame(data) || !length(filters)) {
    return(list())
  }

  if (!all(names(filters) %in% names(data))) {
    return(list())
  }

  keep <- rep(TRUE, nrow(data))
  for (col in names(filters)) {
    keep <- keep & as.character(data[[col]]) %in% as.character(filters[[col]])
  }

  cols <- names(filters)[!startsWith(names(filters), ".")]

  drill_claim_columns(
    data[keep, , drop = FALSE],
    table = table %||% "",
    columns = if (length(cols)) cols
  )
}

#' Keep a target value filter in sync with a block's claim.
#'
#' The one observer shared by the three drill senders: a claim sends, no
#' claim clears -- and [ctrl_clear()] only acts when this block still owns
#' the target's last claim, so an un-drill never clobbers a sibling sender's
#' cohort. Re-aiming (or un-setting) the target releases the OLD target's
#' claim the same ownership-scoped way, so a filter is never left stuck on a
#' sender that no longer points at it.
#' @noRd
dd_ctrl_sender <- function(r_target, r_claims,
                           session = shiny::getDefaultReactiveDomain()) {

  last_target <- ""

  shiny::observe({
    tgt <- trimws(r_target() %||% "")
    claims <- r_claims()

    if (nzchar(last_target) && !identical(last_target, tgt)) {
      ctrl_clear(last_target, state = list(columns = list()),
                 session = session)
    }
    last_target <<- tgt

    if (!nzchar(tgt)) {
      return()
    }

    payload <- list(columns = claims)

    if (length(claims)) {
      ctrl_send(tgt, state = payload, session = session)
    } else {
      ctrl_clear(tgt, state = payload, session = session)
    }
  })
}

#' The board's candidate targets, as a reactiveVal with an identical() skip.
#'
#' [ctrl_targets()] reads the board reactively, and EVERY board update --
#' including this block's own [ctrl_send()] -- invalidates it. The skip means
#' anything rendering off this reactiveVal (the gear's target picker, the
#' table's data-attributes) re-renders only when the candidate SET actually
#' changes: blocks added / removed / renamed, not every click.
#' @noRd
dd_ctrl_choices <- function(session = shiny::getDefaultReactiveDomain()) {

  rv <- shiny::reactiveVal(character())

  shiny::observe({
    choices <- ctrl_targets("value_filter_block", session = session)
    if (!identical(choices, rv())) {
      rv(choices)
    }
  })

  rv
}

#' Candidate targets for the JS gear: `[{value: blockId, label: blockName}]`.
#' @noRd
dd_ctrl_choices_json <- function(choices) {
  if (!length(choices)) {
    return("[]")
  }
  as.character(jsonlite::toJSON(
    data.frame(value = unname(choices), label = names(choices),
               stringsAsFactors = FALSE)
  ))
}

#' The same, as a plain list for hosts whose config travels as one R list
#' (the chart's `drilldown-data` message, the tile's config JSON).
#' @noRd
dd_ctrl_choices_list <- function(choices) {
  if (!length(choices)) {
    return(list())
  }
  unname(Map(
    function(value, label) list(value = value, label = label),
    unname(choices), names(choices)
  ))
}
