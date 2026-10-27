# The variables layout of the table block on the kit: one row per statistic
# or level of each variable, one column per level of `by` (a Table 1). The
# variables are the summaries' columns, shared with the groups layout, so a
# switch between the two transposes the table. summary_table() builds the
# frame; the structured table draws it (table_frame_part(),
# R/table-records.R), and a click filters the built table.

#' The variables layout on the kit
#' @param kit The kit (viz_block_server()).
#' @param src table_sources()'s list.
#' @return A part, as R/table-server.R describes it.
#' @noRd
table_variables_part <- function(kit, src) {
  st <- kit$state
  shown <- shiny::reactive({
    d <- src$rows()
    if (!is.data.frame(d)) {
      return(d)
    }
    table_model_variables(src$values(), d)
  })
  # The click filters the built table: the code that builds it takes the
  # place of the input in the filter.
  built_from <- function(ex) {
    d <- tryCatch(src$rows(), error = function(e) NULL)
    built <- table_variables_call(table_variables_cfg(table_state_values(st), d))
    if (is.null(built)) {
      return(ex)
    }
    built[[2L]] <- quote(.(data))
    table_sub_data(ex, built)
  }
  table_frame_part(kit, src, shown, layout = "variables", filter = built_from)
}
