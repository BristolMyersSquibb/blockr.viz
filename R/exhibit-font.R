# The smallest type an exported table is allowed to shrink to before it is
# broken over more than one slide.
#
# One number governs both axes. The pptx paginator steps the font down to
# reach it while the columns are still squeezed (width) and again while the
# rows still overflow (height), and only past it does it deal columns
# sideways or carry rows onto a second slide. Lowering it is therefore the
# whole of "fit this table on one slide": there is nothing else to toggle.
#
# Below this nothing is legible on a projector, whatever a board asks for.
MIN_FONT_FLOOR <- 5

# First hit wins:
#
#   1. what the caller passed
#   2. the board option, when a board is running and carries one
#   3. `getOption("blockr.viz.ft_min_font_size")`
#   4. 11pt
#
# The board option sits UNDER the argument and OVER the R option on purpose.
# It is read here, at the bottom of the stack, rather than threaded in by
# each caller: that way a deck built by blockr.outline, a table block's own
# PowerPoint download and a summarize block's all read the same value with no
# plumbing between the packages, and cannot disagree about how small a table
# may go. A script outside Shiny finds no session and falls through.
exhibit_min_font_size <- function(x = NULL) {

  for (v in list(x, board_min_font_size(),
                 getOption("blockr.viz.ft_min_font_size"))) {
    n <- suppressWarnings(as.numeric(v))
    if (length(n) == 1L && is.finite(n)) {
      # Whole points, because the ladders step by one and a fractional floor
      # would leave a step they can never land on.
      return(max(MIN_FONT_FLOOR, floor(n)))
    }
  }

  11
}

# The board's value, or NULL for every way there might not be one: no session
# (a script, a test), no board options at all, or a board whose options do not
# include this one. Never an error -- an export is not the place to discover
# that a board is configured differently than expected.
board_min_font_size <- function() {
  tryCatch(
    blockr.core::get_board_option_or_null(
      "exhibit_min_font_size", blockr.core::get_session()
    ),
    error = function(e) NULL
  )
}

#' Smallest exhibit font board option
#'
#' Board option that adds "Smallest table font" to the board sidebar: the
#' size an exported table shrinks to before it is split over several slides.
#'
#' A table carried over two slides is read by flipping back and forth, and a
#' table dealt left-half then right-half is worse. Both are avoidable most of
#' the time by setting the table a point or two smaller, so the paginator
#' shrinks before it splits -- but how small is too small is a house style
#' question, not something a package can answer. This is where a board
#' answers it.
#'
#' It governs both axes, so it is the whole setting: there is no separate
#' "fit on one slide" switch, because that is what the ladder already tries
#' to do. Set it to the base size (13pt) to forbid shrinking altogether, or
#' down to 8pt for a deck that must not split its tables. When even the
#' smallest allowed size does not fit -- a two hundred row listing never
#' will -- the table is still split, and the download says so rather than
#' shrinking past what anyone can read.
#'
#' One value serves every route out of the app: the deck built by
#' blockr.outline's slide builder and the PowerPoint download on a table,
#' summarize table or chart block all read it, so a slide and the block it
#' came from stay the same table.
#'
#' @param value Default floor in points.
#' @param category Settings sidebar category.
#' @param ... Forwarded to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#' @seealso [write_exhibit_pptx()], [pptx_add_exhibit()]
#' @examplesIf interactive()
#' new_exhibit_font_option()
#' @export
new_exhibit_font_option <- function(value = 11, category = "Table options",
                                    ...) {
  blockr.core::new_board_option(
    id = "exhibit_min_font_size",
    default = value,
    ui = function(id) {
      shiny::tagList(
        shiny::selectInput(
          shiny::NS(id, "exhibit_min_font_size"),
          "Smallest table font",
          choices = c(
            "13 pt (never shrink)" = "13",
            "12 pt" = "12",
            "11 pt" = "11",
            "10 pt" = "10",
            "9 pt" = "9",
            "8 pt" = "8",
            "7 pt" = "7"
          ),
          selected = as.character(value)
        ),
        shiny::helpText(
          paste(
            "Exported tables shrink to this size to stay on one slide.",
            "Only what still does not fit is split."
          )
        )
      )
    },
    server = function(..., session) {
      shiny::observeEvent(
        blockr.core::get_board_option_or_null("exhibit_min_font_size", session),
        {
          val <- blockr.core::get_board_option_value(
            "exhibit_min_font_size", session
          )
          shiny::updateSelectInput(
            session, "exhibit_min_font_size", selected = as.character(val)
          )
        }
      )
    },
    category = category,
    ...
  )
}

# A board with a table on it gets the setting without the app naming it, the
# way blockr.io's read and write blocks contribute the data directory. The
# ask this answers came from a reader of a deck, not from the person who
# assembled the app, so an option only an app author can add is an option
# nobody sets. Duplicates across blocks are dropped by
# `combine_board_options()`.
#
# The chart block does not contribute it: a picture is one slide by
# definition, and the floor has nothing to say about it.

#' @exportS3Method blockr.core::board_options
board_options.table_block <- function(x, ...) {
  blockr.core::combine_board_options(new_exhibit_font_option(), NextMethod())
}

#' @exportS3Method blockr.core::board_options
board_options.summarize_table_block <- function(x, ...) {
  blockr.core::combine_board_options(new_exhibit_font_option(), NextMethod())
}

# --- what a split table reports ----------------------------------------------

# A table that could not be kept on one slide, said out loud in a form a
# caller can catch.
#
# Signalled as a classed message rather than returned, because the paginator
# is called for its slides and every route to it (three block downloads, the
# deck builder) would otherwise have to unpack a second value. Unhandled it
# prints to the log, which is what the column deal already did; a download
# handler catches `blockr_exhibit_split` and turns the same information into
# a notification, since a warning in a deployed app's log is a warning nobody
# reads.
#
# `fit_size` is the size at which it WOULD have been one slide, or NULL when
# no legible size does. That number is the actionable half of the message:
# it is exactly what to set the board option to.
exhibit_split_note <- function(what, pages = 1L, sets = 1L, size = NULL,
                               floor = NULL, fit_size = NULL) {

  if (pages <= 1L && sets <= 1L) {
    return(invisible(NULL))
  }

  # An untitled exhibit is reported as "a table" rather than as '': the
  # caller reading the condition gets NULL and can say it its own way.
  named <- is.character(what) && length(what) == 1L && nzchar(what)
  what <- if (named) what
  subject <- if (named) paste0("'", what, "'") else "A table"

  txt <- paste0(
    subject, " did not fit one slide",
    if (!is.null(size)) paste0(" at ", size, "pt"), ": ",
    if (pages > 1L) paste0("carried over ", pages, " slides"),
    if (pages > 1L && sets > 1L) ", its ",
    if (sets > 1L) {
      paste0(if (pages <= 1L) "its ", "columns dealt over ", sets,
             " sets of slides")
    },
    "."
  )

  if (!is.null(fit_size)) {
    txt <- paste0(txt, " It fits on one slide at about ", fit_size,
                  "pt (smallest table font is ", floor, "pt).")
  } else if (!is.null(floor)) {
    txt <- paste0(txt, " No size down to ", floor,
                  "pt keeps it on one slide.")
  }

  message(
    structure(
      class = c("blockr_exhibit_split", "message", "condition"),
      list(
        message = paste0(txt, "\n"),
        call = NULL,
        what = what,
        pages = pages,
        sets = sets,
        size = size,
        floor = floor,
        fit_size = fit_size
      )
    )
  )
}
