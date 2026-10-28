# The table's prepare step: the block's `script` run on the input before any
# layout builds its table, so a layout switch keeps it. The script and its
# reactives are the kit's (viz_script_sources()); the table adds how it reads
# its input and how a layout's filter goes around the script.
#
# Returns viz_script_sources()'s list plus `wrap_after(ex)` (the script
# applied to the expression's input) and `wrap_drill(filters, members)` (the
# groups click filter around the script).
table_prepare <- function(data, state) {
  # The script reads a data frame; a finished table is converted first and
  # keeps its annotation columns, which the records layout draws.
  raw <- shiny::reactive({
    d <- data()
    if (is.data.frame(d)) d else as_annotated_df(d)
  })
  prep <- viz_script_sources(raw, state)
  prep$wrap_after <- function(ex) {
    sx <- prep$compiled()
    if (is.null(sx)) ex else dd_splice_slot(ex, sx)
  }
  # A clicked column only the script makes is filtered after it.
  prep$wrap_drill <- function(filters, members = NULL) {
    sx <- prep$compiled()
    src <- if (!is.null(sx) && length(filters)) {
      tryCatch(names(raw()), error = function(e) NULL)
    }
    table_drill_wrap(sx, filters, members, src)
  }
  prep
}
