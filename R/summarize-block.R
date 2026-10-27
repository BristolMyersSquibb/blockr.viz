#' Summarize table block (superseded)
#'
#' Superseded by [new_table_block()] with `layout = "groups"`, which takes the
#' same summaries. This constructor translates its arguments and returns a
#' table block, so code and boards that use it keep working.
#'
#' @param ... Arguments of the former summarize table block (`by`,
#'   `summaries`, `id_var`, `group`, `value`, `func`, ...), translated to
#'   [new_table_block()]'s.
#'
#' @return A blockr transform block of class `table_block`.
#' @examplesIf interactive()
#' new_summarize_table_block(
#'   by = "Species",
#'   summaries = list(list(type = "simple", func = "count", show = "bar"))
#' )
#' @export
new_summarize_table_block <- function(...) {
  args <- table_legacy_args(list(...), from = "summarize")
  # A summarize table is the groups layout, also when nothing old was set.
  args$layout <- "groups"
  # A restore hands over the saved ctor; the result is a table block and
  # saves as one.
  args$ctor <- "new_table_block"
  args$ctor_pkg <- "blockr.viz"
  do.call(new_table_block, args)
}


#' Argument specs of the groups layout's presentation settings, picked by
#' table_arguments().
#' @noRd
summarize_arguments <- function() {
  blockr.core::new_arg_specs(
    facet_layout = new_arg_spec(
      paste0(
        "Column order of faceted summaries: by_summary = each ",
        "summary's level copies adjacent on one shared scale (comparison ",
        "reading, default); by_level = pooled columns and fields lead, ",
        "then one spanning column group per facet level (the Table-1 / ",
        "CSR reading)."
      ),
      example = "by_summary",
      type = arg_enum(c("by_summary", "by_level"))
    ),
    sort_by = new_arg_spec(
      paste0(
        "Ordering: value (the first column's value), data (the data's ",
        "own order -- factor levels, else first appearance in the rows: ",
        "use it for visits and dose groups, which read wrong ",
        "alphabetically), alpha (alphabetical), a summary column name, or ",
        "a facet level name. \"label\" is read as \"alpha\"."
      ),
      example = "value",
      type = arg_string()
    ),
    sort_dir = new_arg_spec(
      "Sort direction.",
      example = "desc",
      type = arg_enum(c("desc", "asc"))
    ),
    top_n = new_arg_spec(
      paste0(
        "Optional cap on the number of rows, with a visible fold row ",
        "for what falls below the cut. Leave unset for the default ",
        "behaviour: every row rendered, scrolling with the panel. Set it ",
        "only for report exhibits, where there is no scrollbar."
      ),
      example = 10L,
      type = arg_integer()
    ),
    axis = new_arg_spec(
      paste0(
        "Print each glyph column's domain as a tick strip under its header ",
        "(default TRUE) -- the scale named once at the top of the column ",
        "instead of a track repeated on every row. Applies to every mark: ",
        "value domain for bars, boxes and dot ranges, x domain (dates as ",
        "dates) for swimlanes and sparklines. FALSE drops them all."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    value_labels = new_arg_spec(
      paste0(
        "Print each mark's value beside it (default TRUE): a bar's value, a ",
        "box's or dot range's centre, a dumbbell's change, a sparkline's ",
        "last value; a colour-split column prints one number per level. ",
        "FALSE drops them all. Use for \"show the numbers\" / \"hide the ",
        "numbers\"."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    bar_width = new_arg_spec(
      paste0(
        "Length of the marks that carry a value label (bars, boxes, dot ",
        "ranges, sparklines): fit (default) fills the panel up to 320px; ",
        "narrow, medium and wide fix them at 90, 150 and 240px."
      ),
      example = "fit",
      type = arg_enum(c("narrow", "medium", "wide", "fit"))
    )
  )
}
