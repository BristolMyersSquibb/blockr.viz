#' Cross-block external control
#'
#' Helpers that let one block set the externally controllable arguments (see
#' the `external_ctrl` argument to [blockr.core::new_block()]) of another block
#' on the same board, through the board's regular update channel. Sender and
#' target share no data link.
#'
#' `install_ctrl_send()` wires the channel up. Call it once per session from
#' any board component that holds the board's `update` reactive. It stores a
#' send closure in `session$userData`, where `ctrl_send()` finds it. Updates go
#' through the board's full validation. `ctrl_send()` writes state and never
#' reads another block's result. Repeated sends of an unchanged value are
#' no-ops.
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

  ctrl_check(target, session)
  args <- ctrl_named_args(list(...), "ctrl_send", "named values")

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
#' block. The values passed via `...` define "cleared" for the target, e.g.
#' `state = list(columns = list())` empties a value filter.
#'
#' @rdname ctrl_send
#' @export
ctrl_clear <- function(target, ..., session = shiny::getDefaultReactiveDomain()) {

  ctrl_check(target, session)
  args <- ctrl_named_args(list(...), "ctrl_clear", "named reset values")

  clear <- session$userData$blockr_ctrl_clear

  if (!is.function(clear)) {
    return(invisible(FALSE))
  }

  invisible(clear(target, args, author = ctrl_author(session)))
}

ctrl_check <- function(target, session) {
  stopifnot(
    is.character(target), length(target) == 1L, nzchar(target),
    !is.null(session)
  )
}

ctrl_named_args <- function(args, fn, what) {
  if (length(args) == 0L || is.null(names(args)) || !all(nzchar(names(args)))) {
    stop("`", fn, "()` expects ", what, ", one per controllable argument.",
         call. = FALSE)
  }
  args
}

# The calling block's identity: the module namespace of its session.
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

  # Last author per target block id: `ctrl_clear()` acts only for the current
  # holder and releases it.
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
#' one, the one value filter block. Two or more candidates resolve to nothing.
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
dd_ctrl_resolve <- function(target, session) {
  target <- trimws(target %||% "")
  if (identical(target, "auto")) ctrl_auto_target(session) else target
}

#' The board's subject column
#'
#' The column a drill claims when the board says what a subject is: the
#' `subject` field of the "study_roles" board option, or `USUBJID` when the
#' option exists and the field is empty. A board without the option has no
#' subjects and its senders claim columns.
#'
#' @param session Shiny session (defaults to the current reactive domain).
#' @return The column name, or `NULL` when the board declares no subjects.
#' @keywords internal
#' @export
dd_ctrl_subject <- function(session = shiny::getDefaultReactiveDomain()) {

  if (is.null(session)) {
    return(NULL)
  }

  val <- tryCatch(
    blockr.core::get_board_option_or_null("study_roles", session),
    error = function(e) NULL
  )

  if (is.null(val)) {
    return(NULL)
  }

  col <- trimws(as.character(unlist(val$subject, use.names = FALSE)))
  col <- col[!is.na(col) & nzchar(col)]

  if (length(col) == 1L) col else "USUBJID"
}

#' Controllable blocks on the board
#'
#' The block ids a sender may point at: every block on the board of the given
#' class. Returns `character()` when no channel is installed, or when
#' [install_ctrl_send()] was called without `board`.
#'
#' @param class Character(1). Block class to look for. The default is the value
#'   filter block.
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
#' `columns` payload a value filter takes, ready for [ctrl_send()].
#'
#' With `columns = NULL` the data frame is read on its ARD identity columns:
#' `.variable` is the source column name, `.variable_level` the source value on
#' that row, and each `.group<k>` / `.group<k>_level` pair is an enclosing
#' grouping dimension. Dimensions come back outermost first, with the
#' `.variable` leaf last. A `.filter` column (see [drill_source()]) counts as a
#' leaf for reading its enclosing groups. Chart and tile drills carry no ARD
#' identity, so the caller names the candidate `columns` and each is claimed
#' from the subset.
#'
#' A dimension becomes a filter condition only when the subset resolves it to
#' exactly one value. An undrilled chart passes all its rows through and
#' nothing is claimed, which is the caller's cue to [ctrl_clear()].
#'
#' @param data A data frame, typically the drilled output of a table, chart or
#'   tile block. Anything that resolves no dimension yields an empty list.
#' @param table Character(1). Name of the table in the target's `dm` that the
#'   conditions apply to. `""` for a value filter fed a plain data frame.
#' @param mode Character(1). Filter mode for each condition, e.g. `"multi"`.
#' @param columns Character vector of source column names to claim, for
#'   drilled output that carries no ARD identity. `NULL` reads the ARD identity
#'   columns instead.
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

  # Appends the claim for `name` / `value` when both resolved to one value.
  add <- function(claims, name, value) {
    if (is.null(name) || is.null(value)) {
      return(claims)
    }
    c(claims, list(claim_cond(name, value, table, mode)))
  }

  if (length(columns)) {
    claims <- list()
    for (col in columns[columns %in% nms]) {
      claims <- add(claims, col, single_value(as.character(data[[col]])))
    }
    return(claims)
  }

  has_leaf <- all(c(".variable", ".variable_level") %in% nms)
  has_fil <- ".filter" %in% nms
  if (!has_leaf && !has_fil) {
    return(list())
  }

  blank <- character(nrow(data))
  var <- if (has_leaf) as.character(data$.variable) else blank
  lvl <- if (has_leaf) as.character(data$.variable_level) else blank
  fil <- if (has_fil) as.character(data$.filter) else blank

  # A mixed table fills the leaf columns on some rows and `.filter` on others,
  # so a row carries a claim if either is usable.
  leaf_ok <- has_leaf & !is.na(var) & !is.na(lvl) & nzchar(lvl)
  fil_ok <- has_fil & !is.na(fil) & nzchar(fil)
  keep <- leaf_ok | fil_ok

  if (!any(keep)) {
    return(list())
  }

  cols <- list()

  for (g in group_level_cols(nms)) {
    gname <- substr(g, 1L, nchar(g) - 6L)
    if (gname %in% nms) {
      cols <- add(cols, single_value(as.character(data[[gname]])[keep]),
                  single_value(as.character(data[[g]])[keep]))
    }
  }

  if (any(leaf_ok)) {
    cols <- add(cols, single_value(var[leaf_ok]), single_value(lvl[leaf_ok]))
  }

  cols
}

# One filter condition. An empty `table` (a value filter on a plain data
# frame) leaves the `table` field out.
claim_cond <- function(name, values, table = "", mode = "multi") {
  if (nzchar(table %||% "")) {
    list(name = name, table = table, mode = mode, values = values)
  } else {
    list(name = name, mode = mode, values = values)
  }
}

# `.group<k>_level` columns, ordered by k.
group_level_cols <- function(nms) {
  gl <- nms[startsWith(nms, ".group") & endsWith(nms, "_level")]
  gl[order(as.integer(substr(gl, 7L, nchar(gl) - 6L)))]
}

# The value filter's tokens for missing and empty values (blockr.dm
# VALUE_FILTER_NA / VALUE_FILTER_EMPTY). A claim names a missing value with
# these, never with NA or the table's "(Missing)" level.
CTRL_NA <- "<NA>"
CTRL_EMPTY <- "<empty>"

# The elements of `x` selected by a claim's values, tokens included.
ctrl_claim_hit <- function(x, values) {
  values <- as.character(unlist(values))
  x <- as.character(x)
  hit <- x %in% setdiff(values, c(CTRL_NA, CTRL_EMPTY))
  if (CTRL_NA %in% values) hit <- hit | is.na(x)
  if (CTRL_EMPTY %in% values) hit <- hit | (!is.na(x) & x == "")
  hit
}

# One "multi" claim per named column: the values the drilled rows hold,
# missing and empty ones as tokens.
drilled_value_claims <- function(rows, table, columns) {
  claims <- list()
  for (col in intersect(columns, names(rows))) {
    x <- as.character(rows[[col]])
    v <- unique(x[!is.na(x) & nzchar(x)])
    v <- c(v, if (anyNA(x)) CTRL_NA, if (any(!is.na(x) & x == "")) CTRL_EMPTY)
    if (length(v)) {
      claims[[length(claims) + 1L]] <- claim_cond(col, v, table)
    }
  }
  claims
}

# The single distinct value of `x`, or NULL if it does not resolve to one.
single_value <- function(x) {
  u <- unique(x[!is.na(x) & nzchar(x)])
  if (length(u) == 1L) u else NULL
}

#' Receipt for a cross-block control send
#'
#' Builds the status card a sender block returns as its output, so a function
#' block driving another block via [ctrl_send()] shows a readable summary.
#' Return it as the last expression of the `fn`.
#'
#' `ctrl_receipt()` is a pure function of its arguments. It reads no reactive,
#' since a read of the target would re-trigger the [ctrl_send()] in the same
#' function body and loop.
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
    version = js_version("ctrl-receipt.css", "css"),
    src = system.file("css", package = "blockr.viz"),
    stylesheet = "ctrl-receipt.css"
  )
})

#' Control-channel bridge extension
#'
#' A dock extension with no UI that installs the cross-block control channel
#' ([install_ctrl_send()]) on the board, so that blocks can call
#' [ctrl_send()] / [ctrl_clear()] and senders can offer a target picker
#' ([ctrl_targets()]). Add it to the board's `extensions` and to no grid. A
#' board needs exactly one bridge, since the registry behind [ctrl_clear()]
#' lives in the closure `install_ctrl_send()` builds.
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

# ---------------------------------------------------------------------------
# Drill sender glue: the block-side wiring behind the "Send to filter" gear
# section of the table, chart and tile blocks. dd_ctrl_claims() turns a
# block's drill state into a claim and dd_ctrl_sender() keeps the target block
# in sync. Both are exported because drill senders also live outside this
# package.
# ---------------------------------------------------------------------------

#' Claims from a block's own drill-filter state.
#'
#' The input is subset on the `filters` pairs and the claim is read off the
#' subset, so the one-value-per-dimension rule of [drill_claim_columns()]
#' applies. All-dot filter columns (a structured table's `.variable`,
#' `.group<k>`, ...) read the ARD identity off the subset. Any other column is
#' a real source column and is the claim column.
#'
#' The sender reads `list()` as "clear the target" and `NULL` as "keep what the
#' target has". `list()` is returned only when the user has no drill. `NULL` is
#' returned when the block cannot resolve its selection right now: there is no
#' data frame (the block is off screen), the frame lacks a filtered column (it
#' is being rebuilt), or nothing in the subset resolves to a claim.
#'
#' @param data The block's (annotated) input, as a data frame.
#' @param table Name of the table the claim applies to, `""` when the target
#'   filters a plain data frame (or when the target resolves the table itself).
#' @param filters Named list, column -> drilled value(s) from the block's
#'   filter reactives; the columns AND together. An `NA` value (or a table's
#'   "(Missing)" level) takes the rows where the column is missing or empty,
#'   and the claim names them with the tokens `"<NA>"` / `"<empty>"`. A value
#'   on a column carrying a group definition (see [expand_groups()]) is a group
#'   name, matched against the group's members, and the claim is the set of
#'   source values the group holds.
#' @param subject The board's subject column ([dd_ctrl_subject()]), or `NULL`.
#'   When given, the claim becomes the subjects behind the click, with the
#'   column clause kept as its `label`. A structured table resolves them
#'   through its stamped `source_data` ([drill_source()]). A frame that carries
#'   the subject column neither way keeps the column claim.
#' @return A list of filter conditions, `list()` when the user has no drill,
#'   `NULL` when the claim cannot be resolved right now (hold).
#' @keywords internal
#' @export
dd_ctrl_claims <- function(data, table, filters, subject = NULL) {

  if (!is.data.frame(data)) {
    return(NULL)
  }

  if (!length(filters)) {
    return(list())
  }

  if (!all(names(filters) %in% names(data))) {
    return(NULL)
  }

  table <- table %||% ""
  keep <- dd_claim_rows(data, filters)
  cols <- names(filters)[!startsWith(names(filters), ".")]
  claim_col <- dd_claim_source_cols(data, cols)

  out <- dd_claim_conditions(data, keep, filters, cols, claim_col, table)

  # A structured table's "(Missing)" level names the missing value.
  out <- lapply(out, function(cl) {
    v <- as.character(unlist(cl$values))
    if (MISSING_LEVEL %in% v) {
      cl$values <- unique(replace(v, v == MISSING_LEVEL, CTRL_NA))
    }
    cl
  })

  # Subjects resolve even where no column claim does.
  if (length(subject)) {
    ids <- dd_claim_subjects(data, keep, out, subject)
    if (length(ids)) {
      return(list(list(name = subject, mode = "multi", values = ids,
                       label = dd_claim_label(out))))
    }
  }

  # The user has a selection, so an empty read means this block failed to name
  # it, which is a hold.
  if (!length(out)) {
    return(NULL)
  }

  out
}

# The rows the filters select. A click on a group of an overlap definition
# names the group, while an unexpanded group column holds raw levels, so match
# the group's members. An NA filter value takes the rows where the column is
# missing or empty.
dd_claim_rows <- function(data, filters) {
  keep <- rep(TRUE, nrow(data))
  for (col in names(filters)) {
    x <- as.character(data[[col]])
    vals <- as.character(unlist(filters[[col]]))
    vals[vals %in% MISSING_LEVEL] <- NA_character_
    hit <- x %in% group_column_values(data[[col]], vals[!is.na(vals)])
    if (anyNA(vals)) {
      hit <- hit | is.na(x) | x == ""
    }
    keep <- keep & hit
  }
  keep
}

# The column each of `cols` claims under. A picker-made copy (`Color` <-
# `RACE`) carries its origin in `blockr_source`, and the claim names the
# original column, which the target filter sees. A copy whose source is gone
# claims under its own name.
dd_claim_source_cols <- function(data, cols) {
  vapply(cols, function(col) {
    src <- attr(data[[col]], "blockr_source", exact = TRUE)
    if (is.character(src) && length(src) == 1L && nzchar(src) &&
          src %in% names(data)) src else col
  }, character(1L), USE.NAMES = FALSE)
}

# The column conditions of a click, in the order of the claim columns. Plain
# columns claim the set of values the clicked rows hold (a click can cover
# several, e.g. the patients under a bar segment). With no named column the
# ARD identity columns speak. With only group columns, only they claim.
dd_claim_conditions <- function(data, keep, filters, cols, claim_col, table) {

  grouped <- vapply(cols, function(col) has_group_def(data[[col]]), logical(1L))
  group_claims <- dd_group_claims(data, keep, filters, cols[grouped],
                                  claim_col[grouped])

  plain_cols <- setdiff(unique(claim_col[!grouped]), names(group_claims))
  rows <- data[keep, , drop = FALSE]
  out <- if (length(plain_cols)) {
    drilled_value_claims(rows, table, plain_cols)
  } else if (!length(cols)) {
    drill_claim_columns(rows, table = table)
  } else {
    list()
  }

  if (length(group_claims)) {
    out <- c(out, unname(Map(function(name, values) {
      claim_cond(name, values, table)
    }, names(group_claims), unname(group_claims))))
    ord <- match(vapply(out, function(e) as.character(e$name), character(1L)),
                 unique(claim_col))
    out <- out[order(ord)]
  }

  out
}

# Claims for the drilled columns that carry a group definition, as a list
# named by claim column. A group can pool several raw levels, so its claim is
# a set: the source values of the clicked rows, or the group's members when
# no source column exists to claim.
dd_group_claims <- function(data, keep, filters, cols, claim_col) {
  out <- list()
  for (i in seq_along(cols)) {
    col <- cols[[i]]
    to <- claim_col[[i]]
    vals <- if (identical(to, col)) {
      v <- as.character(unlist(filters[[col]]))
      group_column_values(data[[col]], v[!is.na(v)], origin = TRUE)
    } else {
      v <- unique(as.character(data[[to]][keep]))
      v <- v[!is.na(v) & nzchar(v)]
      m <- unlist(group_def_origin(data[[col]])$groups, use.names = FALSE)
      v[order(match(v, m), v)]
    }
    if (length(vals)) {
      out[[to]] <- vals
    }
  }
  out
}

# The subjects behind a click: off the clicked rows when the frame carries the
# subject column, else off the stamped `source_data` narrowed by the column
# claims (a structured table's rows are display rows). NULL when neither
# frame knows the subject.
dd_claim_subjects <- function(data, keep, claims, subject) {

  ids <- if (subject %in% names(data)) {
    data[[subject]][keep]
  } else {
    src <- attr(data, "source_data", exact = TRUE)
    names_ok <- vapply(claims, function(cl) cl$name %in% names(src),
                       logical(1L))
    if (!is.data.frame(src) || !subject %in% names(src) ||
          !length(claims) || !all(names_ok)) {
      return(NULL)
    }
    hit <- rep(TRUE, nrow(src))
    for (cl in claims) {
      hit <- hit & ctrl_claim_hit(src[[cl$name]], cl$values)
    }
    src[[subject]][hit]
  }

  ids <- unique(as.character(ids))
  ids[!is.na(ids) & nzchar(ids)]
}

# What the user clicked, in the words a filter trail uses:
# "AEBODSYS = Nausea; TRT = Placebo".
dd_claim_label <- function(claims) {
  parts <- vapply(claims, function(cl) {
    vals <- as.character(unlist(cl$values))
    vals[vals == CTRL_NA] <- "(missing)"
    vals[vals == CTRL_EMPTY] <- "(empty)"
    if (length(vals) > 6L) {
      vals <- c(vals[1:6], "…")
    }
    paste0(cl$name, " = ", paste(vals, collapse = ", "))
  }, character(1L))
  paste(parts, collapse = "; ")
}

# Has the user drilled yet? A latch: TRUE until the drill selection first
# differs from the constructor's, FALSE afterwards, so a drill cleared back to
# the constructor's selection does not read as pristine and swallow the clear.
# `r_state` returns the block's current drill state as a list and must report
# every piece of it; `initial` is the same shape as the constructor was given
# it.
dd_ctrl_pristine <- function(r_state, initial) {

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
#' last claim. Re-aiming or un-setting the target releases the old target's
#' claim the same way.
#'
#' A send is a board update, which re-evaluates this block, so the observer
#' sends only a payload that differs from the last one. `r_pristine`
#' suppresses the push of a restored claim, since the target comes back from
#' the same saved board already holding it.
#'
#' @param r_target Reactive returning the target block id (`""` for none).
#' @param r_claims Reactive returning the claim list (`NULL` = hold, see
#'   [dd_ctrl_claims()]).
#' @param r_pristine Optional latch from `dd_ctrl_pristine()`; a sender with no
#'   restorable drill state may omit it.
#' @param session Shiny session.
#' @param r_nonce Optional reactive returning a click counter minted in the
#'   browser. It is part of the send-once key, not of the payload. A re-click
#'   of the same mark sends again, a board re-evaluation does not.
#' @return The observer, invisibly.
#' @keywords internal
#' @export
dd_ctrl_sender <- function(r_target, r_claims, r_pristine = NULL,
                           session = shiny::getDefaultReactiveDomain(),
                           r_nonce = NULL) {

  last_target <- ""
  last_key <- NULL

  shiny::observe({
    tgt <- dd_ctrl_resolve(r_target(), session)
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

    # Hold. `last_key` stays, so coming back on screen re-reads the same claim
    # and the skip below swallows the re-send.
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
# the gear's picker re-renders only when the candidate set changes.
dd_ctrl_choices <- function(session = shiny::getDefaultReactiveDomain()) {

  rv <- shiny::reactiveVal(character())

  shiny::observe({
    choices <- c(
      dd_ctrl_auto_choice(session),
      ctrl_targets("value_filter_block", session = session)
    )
    if (!identical(choices, rv())) {
      rv(choices)
    }
  })

  rv
}

# The "auto" entry at the head of the picker, labelled with the block the
# board currently resolves it to.
dd_ctrl_auto_choice <- function(session = shiny::getDefaultReactiveDomain()) {

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
dd_ctrl_choices_list <- function(choices) {
  if (!length(choices)) {
    return(list())
  }
  unname(Map(
    function(value, label) list(value = value, label = label),
    unname(choices), names(choices)
  ))
}

#' The column half of a structured click, as claims.
#'
#' The browser sends the source column and its value off the column header
#' (`TRT01P = "Placebo"`). A pooled column ("All Active") sends its members.
#'
#' @param col_keys A list of `list(column=, values=)`, as the click sent it.
#' @param table Name of the table the claim applies to, `""` for none.
#' @return A list of filter conditions, `list()` when there is no column half.
#' @keywords internal
#' @export
dd_col_claims <- function(col_keys, table = "") {

  out <- list()

  for (k in col_keys) {

    nme <- as.character(k[["column"]] %||% "")[1L]
    vals <- as.character(unlist(k[["values"]], use.names = FALSE))

    if (!is.na(nme) && nzchar(nme) && length(vals)) {
      out[[length(out) + 1L]] <- claim_cond(nme, vals, table)
    }
  }

  out
}
