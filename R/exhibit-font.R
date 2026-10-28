# The hard floor in points: no exhibit font goes below it.
MIN_FONT_FLOOR <- 5

#' The smallest font size any exhibit goes down to
#'
#' For exporters in other blockr packages; not part of the user API.
#' @return A number of points.
#' @keywords internal
#' @export
exhibit_font_floor <- function() MIN_FONT_FLOOR

# The smallest type an exported table shrinks to before the pptx paginator
# splits it, on both axes. First hit wins: the caller's value, the board
# option (when a board is running and carries one),
# `getOption("blockr.viz.ft_min_font_size")`, then 11pt.
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
# options, or a board without this one.
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
#' size (13pt) to forbid shrinking. When even the smallest allowed size does
#' not fit, the table is split and the download says so. One value serves the
#' decks built by blockr.outline and the PowerPoint download of every block.
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
  new_input_option(
    "exhibit_min_font_size", value,
    input = function(inputId) {
      shiny::selectInput(
        inputId, "Smallest table font",
        choices = c("13 pt (never shrink)" = "13", "12 pt" = "12",
                    "11 pt" = "11", "10 pt" = "10", "9 pt" = "9",
                    "8 pt" = "8", "7 pt" = "7", "6 pt" = "6"),
        selected = as.character(value)
      )
    },
    update = function(session, id, val) {
      shiny::updateSelectInput(session, id, selected = as.character(val))
    },
    help = paste("Exported tables shrink to this size to stay on one slide.",
                 "Only what still does not fit is split."),
    category = category, ...
  )
}

# A board with a table on it gets the setting. Duplicates across blocks are
# dropped by `combine_board_options()`. The chart block does not contribute it.

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
# (`blockr_exhibit_split`) that a download handler can turn into a
# notification. `fit_size` is the size at which it would fit one slide, or NULL
# when no legible size does.
#' Tell the reader that an exhibit was split
#'
#' For exporters in other blockr packages; not part of the user API.
#' @keywords internal
#' @export
exhibit_split_note <- function(what, pages = 1L, sets = 1L, size = NULL,
                               floor = NULL, fit_size = NULL) {
  if (pages <= 1L && sets <= 1L) return(invisible(NULL))

  # An untitled exhibit is reported as "A table" and `what` is NULL.
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
      list(message = paste0(txt, "\n"), call = NULL, what = what,
           pages = pages, sets = sets, size = size, floor = floor,
           fit_size = fit_size)
    )
  )
}
