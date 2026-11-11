# What the chart sends the browser: one payload with the state as the spec
# writes it (viz_wire()), the rows the current mapping needs, a description
# of every column of the input (the gear picks from all of them), and
# `derived`: what R computes for the drawing and never takes back. The pieces
# are cached on exactly their inputs, so a setting that changes only the
# drawing reuses them.

chart_view <- function(kit, src) {
  spec <- kit$spec
  revs <- kit$revs
  rows <- viz_rows()
  wire_cache <- last_value_cache()
  band_cache <- last_value_cache()
  smoother_cache <- last_value_cache()
  meta_cache <- last_value_cache()
  cells_cache <- last_value_cache()
  levels_cache <- last_value_cache()
  board_theme <- board_echart_theme()
  scale_map <- dd_board_scale_map()

  band <- shiny::reactive({
    v <- src$values()
    d <- chart_grid_data(v, src$plain())
    a <- chart_band_args(v, d)
    if (is.null(a)) {
      return(NULL)
    }
    band_cache(list(d[chart_args_cols(a, d)], a, v[c("ref_min", "ref_max")]),
               function() chart_band(v, d, a))
  })
  smoother <- shiny::reactive({
    v <- src$values()
    d <- chart_grid_data(v, src$plain())
    a <- chart_smoother_args(v, d)
    if (is.null(a)) {
      return(NULL)
    }
    smoother_cache(list(d[chart_args_cols(a, d)], a),
                   function() chart_smoother(v, d, a))
  })
  titles <- shiny::reactive({
    d <- src$plain()
    shiny::req(is.data.frame(d))
    inp <- tryCatch(src$input(), error = function(e) NULL)
    chart_titles(spec, src$values(), d,
                 input_display_attrs(if (is.list(inp)) inp$df else NULL),
                 src$prep$specs())
  })
  # One row per mark for the types chart_cells() serves, the mapped columns'
  # rows for the rest.
  shipped <- shiny::reactive({
    d <- src$plain()
    shiny::req(is.data.frame(d))
    v <- src$values()
    a <- chart_cells_args(v, d)
    if (!is.null(a)) {
      return(cells_cache(list(d[chart_cells_cols(a)], a),
                         function() chart_cells(d, a)))
    }
    cols <- chart_needed_cols(v, d)
    d[, cols, drop = FALSE]
  })
  # The values the gear offers for a highlight column the cells do not hold.
  hl_levels <- shiny::reactive({
    d <- src$plain()
    a <- chart_cells_args(src$values(), d)
    col <- a$hl_col
    if (!length(col) || col %in% c(a$facets, a$category, a$color)) {
      return(NULL)
    }
    levels_cache(d[[col]], function() {
      stats::setNames(list(I(chart_levels(d[[col]]))), col)
    })
  })
  scales <- shiny::reactive({
    v <- src$values()
    shiny_json(dd_scales_config(
      scale_map(), v$type, color = v$color,
      group = chart_category(v), data = src$plain()
    ))
  })

  payload <- function() {
    d <- src$plain()
    shiny::req(is.data.frame(d))
    v <- kit$values()
    df <- shipped()
    # The columns the payload describes are the whole input's, below.
    w <- wire_cache(df, function() {
      if (is_chart_cells(df)) {
        list(data = chart_cells_json(df))
      } else {
        viz_wire_df(df, meta = FALSE)
      }
    })
    viz_payload(spec, v, rows(w$data, w$columns), revs, derived = list(
      titles = titles(),
      band = band(),
      smoother = smoother(),
      hist = src$hist()$edges,
      levels = hl_levels(),
      scales = scales(),
      palette = as.list(dd_palette()),
      theme = board_theme()
    ), columns = meta_cache(d, function() viz_wire_columns(d)))
  }
  list(payload = payload, titles = titles)
}

# The columns the payload carries: the mapped ones, and those a reader can
# switch a role to from the sentence, so the switch needs no new payload.
chart_needed_cols <- function(v, d) {
  # An aggregated chart draws its category and the column it aggregates; a
  # count aggregates none.
  axes <- if ((v$type %||% "bar") %in% CHART_AGGREGATED_TYPES) {
    c(chart_category(v), chart_value_col(v))
  } else if (identical(v$type, "histogram")) {
    c(v$x, if (length(v$x)) chart_bin_col(v$x))
  } else {
    c(v$x, v$y)
  }
  needed <- c(
    axes, v$color, v$facet, v$facet_row, v$series, v$xend, v$open, v$label, v$tooltip,
    v$drill, v$ymin, v$ymax, v$linetype, names(v$highlight), v$id,
    v$ref_min, v$ref_max,
    if (length(v$sort_by) && !v$sort_by %in% c("value", "data", "alpha",
                                               "onset")) v$sort_by,
    if ("AVISIT" %in% c(v$x, chart_category(v))) "AVISITN"
  )
  if (length(v$subtitle) && grepl("@", v$subtitle, fixed = TRUE)) {
    needed <- c(needed, names(column_kinds(d)))
  }
  needed <- c(needed, unlist(
    template_slot_choices(list(v$title, v$subtitle, v$caption)),
    use.names = FALSE
  ))
  # Sorted, so swapping roles keeps the rows the browser holds.
  needed <- sort(unique(as.character(unlist(needed))))
  needed[nzchar(needed) & needed %in% names(d)]
}

# The chart text (viz_titles()): slots name the spec's arguments, the
# prepare script's declared values and the highlight. A script value takes
# the word of a setting with the same name (script_title_args()).
chart_titles <- function(spec, v, d, auto, specs) {
  a <- script_title_args(viz_slot_values(spec, v), specs, v$script_values)
  # A split the chart does not draw (its column absent or NA in every row)
  # leaves the text too: "[, split by {label(@facet)}]" drops.
  for (k in intersect(c("color", "facet", "facet_row", "series"),
                     names(a))) {
    a[k] <- list(chart_split_in(d, a[[k]]))
  }
  attr(d, "blockr_highlight") <- chart_highlight_text(v$highlight)
  viz_titles(v, d, auto, a)
}

# What the downloads write (export-registry.R): the numbers the chart shows,
# one row per mark for the aggregated types and the rows for the rest; the
# browser's picture, when it has sent one; the resolved text. Read when a
# format is clicked. The chart's data downloads write the data view's tables
# (chart-data-view.R, data-download.R), not this.
chart_exhibit <- function(kit, src, view) {
  function() {
    v <- src$values()
    data <- chart_xlsx_frame(
      src$plain(), chart_type = v$type, group = chart_category(v),
      color = v$color, facet = c(v[["facet_row"]], v[["facet"]]),
      func = v$func,
      value = chart_value_col(v), na_group = v$missing
    )
    ti <- view$titles()
    text <- function(band) if (!is.null(band)) band$text
    # The footer's {dataset} and {filters} are the input's: the numbers the
    # chart shows carry neither.
    input <- tryCatch(kit$data(), error = function(e) NULL)
    new_viz_exhibit(
      data, picture = kit$capture(),
      title = text(ti$title), subtitle = text(ti$subtitle),
      caption = text(ti$caption), kind = "chart",
      meta = list(dataset = if (is.data.frame(input)) dl_dataset(input),
                  source = input)
    )
  }
}

# What the download menu asks its formats about (viz_downloads()): a chart
# with the picture it has, and none of its numbers.
chart_exhibit_probe <- function(kit, src) {
  function() {
    shiny::req(is.data.frame(src$plain()))
    new_viz_exhibit(data.frame(), picture = kit$capture(), kind = "chart")
  }
}

# The aggregated column of an aggregated chart: `value`, NULL for a count.
# A boxplot or point range always reads it.
chart_value_col <- function(v) {
  type <- v$type %||% "bar"
  if (!type %in% CHART_AGGREGATED_TYPES ||
        (!type %in% CHART_DISTRIBUTION_TYPES &&
           identical(v$func %||% "count", "count"))) {
    return(NULL)
  }
  v$value
}

# What the Excel download holds: one row per mark for the aggregated types
# (the table block's engine), the rows themselves for the rest.
chart_xlsx_frame <- function(data, chart_type, group = NULL, color = NULL,
                             facet = NULL, func = "count", value = NULL,
                             na_group = "level") {
  if (!is.data.frame(data)) {
    return(NULL)
  }
  if (!isTRUE(chart_type %in% c("bar", "waterfall", "pie", "treemap")) ||
        !length(intersect(group, names(data)))) {
    return(data)
  }
  keys <- intersect(c(chart_facets(list(facet = facet), data), group,
                      chart_split_in(data, color)), names(data))
  table_aggregate(data, unique(keys), chart_summaries(func, value, data),
                  na_group = na_group %||% "level")$data
}
