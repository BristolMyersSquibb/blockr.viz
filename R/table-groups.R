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
      p <- table_drill_phrase(table_selection_groups(sel))
      if (!is.null(p)) paste0("Filtered: ", p)
    },
    exhibit = function(tt) {
      table_groups_exhibit(model(), src$values(), shown(), tt, src,
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
  legend <- groups_legend_spec(m$prep)
  list(model = model, derived = if (!is.null(legend)) list(legend = legend))
}

# The settings a groups title can name: the spec's slots plus the first
# summary's roles (groups_title_roles()).
table_groups_title_args <- function(spec, v) {
  a <- viz_slot_values(spec, v)
  g <- groups_title_roles(v)
  a[names(g)] <- g
  a
}

# The downloads' exhibit, from the model the screen draws: values for the
# spreadsheet, the table with its marks (or cards) for the page and deck, and
# the browser's picture when it drew one.
table_groups_exhibit <- function(m, v, d, tt, src, picture) {
  shiny::req(is.data.frame(d), !is.null(m))
  e <- groups_exhibit(m, d, v, tt$title, tt$subtitle, tt$caption,
                      scale_map = src$scale_map())
  new_viz_exhibit(
    summarize_export_df(e$prep), picture = picture,
    render = if (!is.null(e$cards)) cards_exhibit(e) else e,
    title = tt$title, subtitle = tt$subtitle, caption = tt$caption,
    meta = list(dataset = dl_dataset(d), source = d)
  )
}
