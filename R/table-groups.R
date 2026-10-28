#' The groups layout on the kit
#' @return A part, as R/table-server.R describes it, plus `failed()`.
#' @noRd
table_groups_part <- function(kit, src) {
  st <- kit$state
  shown <- shiny::reactive(src$rows())
  model <- shiny::reactive({
    d <- shown()
    shiny::req(is.data.frame(d))
    table_model_groups(src$values(), d, scale_map = src$scale_map(),
                       subject = src$subject())
  })
  # A click on a pooled group of an overlap definition filters on the
  # group's members (dd_group_filters_members()).
  members <- dd_group_filters_members(
    function() table_selection_groups(st$selection()), shown
  )
  list(
    shown = shown,
    view = shiny::reactive(table_groups_payload(
      tryCatch(model(), error = function(e) NULL), src$values(),
      tryCatch(shown(), error = function(e) NULL), src
    )),
    failed = shiny::reactive(as.list(model()$prep$failed)),
    expr = function(sel) {
      f <- table_selection_groups(sel)
      src$prep$wrap_drill(f, members(f))
    },
    claims = function(sel) {
      d <- tryCatch(shown(), error = function(e) NULL)
      dd_ctrl_claims(d, st$drill_table(), table_selection_groups(sel),
                     subject = src$subject())
    },
    receipt = function(sel) {
      p <- summarize_drill_phrase(table_selection_groups(sel))
      if (!is.null(p)) paste0("Filtered: ", p)
    },
    exhibit = function(tt) {
      table_groups_exhibit(table_state_values(st), shown(), tt, src,
                           table_picture(kit$capture()))
    },
    title_args = function(v) table_groups_title_args(kit$spec, v)
  )
}

# The view of a prepared groups table: the TableModel, or what to say
# instead, and the legend and the note under it.
table_groups_payload <- function(m, v, d, src) {
  if (is.null(m)) {
    return(list(empty = list(kind = "no_data", message = "No data")))
  }
  if (!is.null(m$prep$err)) {
    return(list(empty = list(kind = "message", message = m$prep$err)))
  }
  model <- table_groups_view(m, axis = isTRUE(v$column_axis))
  if (identical(v$look, "cards")) {
    model$cards <- cards_payload(table_cards_groups(
      m$cells, m$prep, data = d, scale_map = src$scale_map()
    ))
  }
  derived <- list(legend = summarize_legend_spec(m$prep),
                  note = m$prep$note)
  list(model = model, derived = derived[!vapply(derived, is.null,
                                                logical(1L))])
}

# The settings a groups title can name: the spec's slots plus the first
# summary's roles (summarize_title_args()). `by` is the innermost column.
table_groups_title_args <- function(spec, v) {
  a <- viz_slot_values(spec, v)
  g <- summarize_title_args(list(
    by = v$by, id_var = v$id, sort_by = v$sort_by, top_n = v$top_n,
    drill = v$drill, summaries = v$summaries
  ))
  a[names(g)] <- g
  a
}

# The downloads' exhibit: values for the spreadsheet, the table with its marks
# (or cards) for the page and deck, and the browser's picture when it drew one.
table_groups_exhibit <- function(v, d, tt, src, picture) {
  shiny::req(is.data.frame(d))
  e <- static_summarize_table(
    d, id_var = v$id, summaries = v$summaries, by = v$by, look = v$look,
    facet_layout = v$facet_layout, sort_by = v$sort_by,
    sort_dir = table_sort_dir(v), top_n = v$top_n, axis = v$column_axis,
    pct_of = v$percent_of, missing = v$missing, format = v$format,
    unit = v$unit, value_labels = v$value_labels, sortable = v$sortable,
    bar_width = v$bar_width, title = v$title, subtitle = v$subtitle,
    caption = v$caption, .title_args = script_title_args(
      table_groups_title_args(viz_block_spec("table"), v),
      src$prep$specs(), v$script_values
    ),
    scale_map = src$scale_map(), subject = src$subject()
  )
  new_viz_exhibit(
    summarize_export_df(e$prep), picture = picture,
    render = if (!is.null(e$cards)) cards_exhibit(e) else e,
    title = tt$title %||% e$title, subtitle = tt$subtitle %||% e$subtitle,
    caption = tt$caption %||% e$caption,
    meta = list(dataset = dl_dataset(d), source = d)
  )
}
