# The hard floor in points: no exhibit font goes below it, whatever a board
# asks for.
MIN_FONT_FLOOR <- 5

#' The smallest font size any exhibit goes down to
#'
#' For exporters in other blockr packages; not part of the user API.
#' @return A number of points.
#' @keywords internal
#' @export
exhibit_font_floor <- function() MIN_FONT_FLOOR

# The smallest type an exported table shrinks to before the pptx paginator
# splits it. One number governs both axes. First hit wins:
#
#   1. what the caller passed
#   2. the board option, when a board is running and carries one
#   3. `getOption("blockr.viz.ft_min_font_size")`
#   4. 11pt
#
# The board option is read here so every route (decks, block downloads) reads
# the same value. A script outside Shiny finds no session and falls through.
#' The smallest font size an exhibit may use
#'
#' For exporters in other blockr packages; not part of the user API.
#' @keywords internal
#' @export
exhibit_min_font_size <- function(x = NULL) {

  for (v in list(x, board_min_font_size(),
                 getOption("blockr.viz.ft_min_font_size"))) {
    n <- suppressWarnings(as.numeric(v))
    if (length(n) == 1L && is.finite(n)) {
      # Whole points, because the ladders step by one.
      return(max(MIN_FONT_FLOOR, floor(n)))
    }
  }

  11
}

# The board's value, or NULL when there is none: no session, no board
# options, or a board without this one. Never an error.
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
#' The paginator shrinks a table to this size on both axes before it carries
#' rows onto another slide or deals columns over several. Set it to the base
#' size (13pt) to forbid shrinking, or down to 8pt for a deck that must not
#' split its tables. When even the smallest allowed size does not fit, the
#' table is split and the download says so.
#'
#' One value serves every route out of the app: the deck built by
#' blockr.outline's slide builder and the PowerPoint download on a table,
#' summarize table or chart block.
#'
#' @param value Default floor in points.
#' @param category Settings sidebar category.
#' @param ... Forwarded to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#' @seealso [write_exhibit_pptx()], [pptx_add_exhibit()]
#' @examples
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
            "7 pt" = "7",
            # The last step the ladder takes.
            "6 pt" = "6"
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

# A board with a table on it gets the setting without the app naming it.
# Duplicates across blocks are dropped by `combine_board_options()`. The chart
# block does not contribute it: a picture is one slide.

#' @exportS3Method blockr.core::board_options
board_options.table_block <- function(x, ...) {
  blockr.core::combine_board_options(new_exhibit_font_option(),
                                     new_download_footer_option(),
                                     NextMethod())
}

#' @exportS3Method blockr.core::board_options
board_options.summarize_table_block <- function(x, ...) {
  blockr.core::combine_board_options(new_exhibit_font_option(),
                                     new_download_footer_option(),
                                     NextMethod())
}

# --- what a split table reports ----------------------------------------------

# A table that could not be kept on one slide, signalled as a classed message
# (`blockr_exhibit_split`) so a download handler can catch it and turn it into
# a notification. `fit_size` is the size at which it would have been one
# slide, or NULL when no legible size does.
#' Tell the reader that an exhibit was split
#'
#' For exporters in other blockr packages; not part of the user API.
#' @keywords internal
#' @export
exhibit_split_note <- function(what, pages = 1L, sets = 1L, size = NULL,
                               floor = NULL, fit_size = NULL) {

  if (pages <= 1L && sets <= 1L) {
    return(invisible(NULL))
  }

  # An untitled exhibit is reported as "A table"; the condition's `what` is
  # NULL then.
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
