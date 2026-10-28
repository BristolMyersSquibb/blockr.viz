#' Options That Tune an Exported Table
#'
#' Everything the table exporters read from `options()`. An app sets them once
#' at startup; none is required. The values below are the defaults. A board
#' option for the same thing wins.
#'
#' @section Fitting a table to a slide:
#' The exporter shrinks a table before it breaks it, and carries it downward
#' before it deals its columns sideways.
#'
#' \describe{
#'   \item{`blockr.viz.ft_min_font_size` (11)}{How small the type may go
#'     before a table is split. One floor, both axes. The board option
#'     `exhibit_min_font_size` ("Smallest table font",
#'     [new_exhibit_font_option()]) overrides it, and 5pt is a hard floor
#'     under both.}
#'   \item{`blockr.viz.ft_font_size` (13)}{The size a table starts at, before
#'     any shrinking.}
#'   \item{`blockr.viz.ft_footer_gap` (0.05)}{Inches left above whatever the
#'     layout puts at the foot of the slide (footer, date, slide number).
#'     `0` lets a page run right up to it.}
#'   \item{`blockr.viz.ft_header_break_tol` (0, off)}{Set to a fraction (say
#'     0.6) to deal the columns over two sets of slides when a header word is
#'     cut past that much of itself. Off, the columns come apart only when a
#'     data cell no longer fits its own longest word.}
#'   \item{`blockr.viz.ft_fit_width` (unset)}{Total table width in inches. A
#'     deck sets it from the reference template; unset means natural widths,
#'     which suits html and pdf.}
#' }
#'
#' @section How the width is shared out:
#' \describe{
#'   \item{`blockr.viz.ft_stub_share` (0.3)}{The most the row stub claims
#'     while the data columns are still short of their full headers. Raise it
#'     to keep long row labels on one line, lower it to feed the columns.}
#'   \item{`blockr.viz.ft_width_slack` (1.04)}{Margin for the measuring font
#'     differing from the rendering font. Raise it if a deck wraps where the
#'     export predicted it would not.}
#'   \item{`blockr.viz.ft_col_widths` ("measured")}{`"even"` is the
#'     positional rule: the stub takes a fixed width, the data columns share
#'     what is left equally, whatever their text needs.}
#' }
#'
#' @section Typography and appearance:
#' \describe{
#'   \item{`blockr.viz.ft_font` ("Inter")}{The table's typeface. A deck sets
#'     it from the reference template's font scheme; an app naming a face
#'     wins.}
#'   \item{`blockr.viz.ft_header_bg` (unset)}{Header band colors. blockr.viz
#'     ships no palette of its own.}
#'   \item{`blockr.viz.ft_emphasis_colors` (unset)}{The ramp for emphasised
#'     columns.}
#'   \item{`blockr.viz.pptx_template` (unset)}{Reference deck for a block's
#'     own PowerPoint download, when the app has not set
#'     `blockr.outline.template`.}
#'   \item{`blockr.viz.paint_res` (300)}{Dots per inch for a summarize table,
#'     which reaches a slide as a picture because a PowerPoint cell cannot
#'     hold a glyph.}
#'   \item{`blockr.viz.gg_slide_fill` (1)}{How far a chart grows into the
#'     slide it is placed on.}
#' }
#'
#' @section Not options:
#' `static_table(cell_padding = )` (`NULL`, the flextable default of 5pt,
#' tightened to 2 when a table cannot fit any other way) is a function
#' argument. The stub's minimum width is a 1.2in constant in the width
#' allocator.
#'
#' @name exhibit-options
#' @seealso [static_table()], [write_exhibit_pptx()],
#'   [new_exhibit_font_option()]
#' @examples
#' # A house style, set once at app startup.
#' old <- options(
#'   blockr.viz.ft_font_size = 12,
#'   blockr.viz.ft_min_font_size = 9,
#'   blockr.viz.ft_stub_share = 0.35
#' )
#' options(old)
NULL
