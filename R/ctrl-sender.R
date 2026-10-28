# The drill sender: the observer that keeps a target filter block in sync
# with a block's claim (ctrl_sender()), the latch that keeps a restored claim
# from being pushed again (ctrl_pristine()), and the target picker's choices
# (ctrl_choices()). The kit wires them for every block (viz_drill_sender(),
# R/kit-server.R); the claims are built in R/ctrl-claims.R.
#
# ctrl_sender() is exported as plumbing (`@keywords internal`) because drill
# senders also live outside this package (blockr.sandbox's composer table).
# They need the same send guard, which stops a send -> board update ->
# re-evaluation -> send loop.
#
# Kept for compatibility, at the end of this file: dd_ctrl_claims(),
# dd_ctrl_sender(), dd_ctrl_subject() and dd_col_claims(), the names
# blockr.viz 0.3.0 exported.

# Has the user drilled yet? A latch: TRUE until the drill selection first
# differs from the constructor's, FALSE afterwards. A live comparison would
# read a drill cleared back to the constructor's selection as pristine again
# and swallow the clear.
#
# It is a plain closure that flips when the sender calls it. A `reactiveVal`
# set from a separate observer would race the sender's observer, which
# invalidates on the same drill in an undefined order.
#
# `r_state` must report every piece of drill state the block has and `initial`
# the constructor's value for each. State the closure cannot see leaves the
# latch pristine and the sender never pushes. Add a drill dimension to a
# block, add it here.
#
# `r_state`: function returning the block's current drill state, as a list.
# `initial`: the same shape, as the constructor was given it.
#' @noRd
ctrl_pristine <- function(r_state, initial) {

  touched <- FALSE

  function() {

    cur <- r_state()

    if (!touched && !identical(cur, initial)) {
      touched <<- TRUE
    }

    !touched
  }
}

#' Keep a target value filter in sync with a block's claim.
#'
#' The observer shared by the three drill senders: a claim sends, no claim
#' clears. [ctrl_clear()] only acts when this block still owns the target's
#' last claim, so an un-drill never clobbers a sibling sender's cohort.
#' Re-aiming or un-setting the target releases the old target's claim the same
#' way.
#'
#' A send is a board update, which re-evaluates every block, including this
#' one, and `r_claims` invalidates on that re-evaluation. The observer
#' therefore sends only a payload that differs from the last one. `r_pristine`
#' suppresses the push of a restored claim: the target comes back from the
#' same saved board already holding it. The claim is recorded as sent, so the
#' first real drill diffs against it.
#'
#' @param r_target Reactive returning the target block id (`""` for none).
#' @param r_claims Reactive returning the claim list (`NULL` = hold, see
#'   [ctrl_claims()]).
#' @param r_pristine Optional latch from `ctrl_pristine()`; a sender with no
#'   restorable drill state may omit it.
#' @param session Shiny session.
#' @param r_nonce Optional reactive returning a click counter minted in the
#'   browser. It is part of the send-once key and never part of the payload.
#'   It lets a re-click of the same mark in transient-drill mode send again,
#'   since the claim is identical, while a board re-evaluation, which does not
#'   change the counter, does not.
#' @return The observer, invisibly.
#' @keywords internal
#' @export
ctrl_sender <- function(r_target, r_claims, r_pristine = NULL,
                           session = shiny::getDefaultReactiveDomain(),
                           r_nonce = NULL) {

  last_target <- ""
  last_key <- NULL

  shiny::observe({
    tgt <- ctrl_resolve_target(r_target(), session)
    claims <- r_claims()

    if (nzchar(last_target) && !identical(last_target, tgt)) {
      ctrl_clear(last_target, state = list(columns = list()),
                 session = session)
      last_key <<- NULL
    }
    last_target <<- tgt

    if (!nzchar(tgt)) {
      return()
    }

    # Read the latch on every pass: it carries this observer's dependency on
    # the drill selection.
    pristine <- is.function(r_pristine) && isTRUE(r_pristine())
    nonce <- if (is.function(r_nonce)) r_nonce() else NULL

    # No claims: hold. `last_key` stays, so coming back on screen re-reads the
    # same claim and the skip below swallows the re-send.
    if (is.null(claims)) {
      return()
    }

    payload <- list(columns = claims)

    if (identical(list(payload, nonce), last_key)) {
      return()
    }

    # One click, one send. A claim on subject ids changes under the same click
    # when anything upstream changes, and each re-send is a board-wide update.
    if (!is.null(nonce) && !is.null(last_key) &&
          identical(nonce, last_key[[2L]])) {
      return()
    }
    last_key <<- list(payload, nonce)

    if (pristine) {
      return()
    }

    if (length(claims)) {
      ctrl_send(tgt, state = payload, session = session)
    } else {
      ctrl_clear(tgt, state = payload, session = session)
    }
  })
}

# The board's candidate targets as a reactiveVal with an identical() skip, so
# the gear's picker re-renders only when the candidate set changes, not on
# every board update. It starts at the current set, so the block's first
# payload already carries it and is not sent a second time.
#' @noRd
ctrl_choices <- function(session = shiny::getDefaultReactiveDomain()) {

  now <- function() {
    c(
      ctrl_auto_choice(session),
      ctrl_targets("value_filter_block", session = session)
    )
  }

  rv <- shiny::reactiveVal(shiny::isolate(now()))

  shiny::observe({
    choices <- now()
    if (!identical(choices, rv())) {
      rv(choices)
    }
  })

  rv
}

# The "auto" entry at the head of the picker, labelled with the block the
# board currently resolves it to.
#' @noRd
ctrl_auto_choice <- function(session = shiny::getDefaultReactiveDomain()) {

  id <- ctrl_auto_target(session)

  if (!nzchar(id)) {
    return(stats::setNames("auto", "Automatic (no filter block found)"))
  }

  cands <- ctrl_targets("value_filter_block", session = session)
  nme <- names(which(cands == id))
  lab <- if (length(nme) == 1L && nzchar(nme)) nme else id

  stats::setNames("auto", paste0("Automatic (", lab, ")"))
}

# Candidate targets for the gear, `[{value: blockId, label: blockName}]`.
#' @noRd
ctrl_choices_list <- function(choices) {
  if (!length(choices)) {
    return(list())
  }
  unname(Map(
    function(value, label) list(value = value, label = label),
    unname(choices), names(choices)
  ))
}

# Names blockr.viz 0.3.0 exported, kept for compatibility: blockr.sandbox's
# composer table and other callers still use them. New code calls the names
# without `dd_`.
#' @rdname ctrl_claims
#' @export
dd_ctrl_claims <- function(...) ctrl_claims(...)

#' @rdname ctrl_sender
#' @export
dd_ctrl_sender <- function(...) ctrl_sender(...)

#' @rdname ctrl_subject
#' @export
dd_ctrl_subject <- function(...) ctrl_subject(...)

#' The column half of a structured click, as claims
#'
#' The browser sends a header click's source column and values
#' (`TRT01P = "Placebo"`), so this reshapes them into conditions; a pooled
#' column sends its members. Exported in 0.3.0 and kept for compatibility.
#'
#' @param col_keys A list of `list(column=, values=)`, as the click sent it.
#' @param table Name of the table the claim applies to, `""` for none.
#' @return A list of filter conditions, `list()` when there is no column half.
#' @keywords internal
#' @export
dd_col_claims <- function(col_keys, table = "") {
  out <- lapply(col_keys, function(k) {
    nme <- as.character(k[["column"]] %||% "")[1L]
    vals <- as.character(unlist(k[["values"]], use.names = FALSE))
    if (is.na(nme) || !nzchar(nme) || !length(vals)) {
      return(NULL)
    }
    claim_cond(nme, vals, table)
  })
  Filter(Negate(is.null), out)
}
