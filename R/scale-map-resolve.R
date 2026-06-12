# Drilldown-chart glue for the board scale map (convention: option id
# "scale_map", see blockr.design/open/blockr.theme). The contract code —
# value shape, resolver, hash assignment — lives in blockr.theme, consumed
# here behind a Suggests guard: without blockr.theme installed the chart
# keeps its standard palette cycling. Reading the option value needs only
# blockr.core.

# Reactive over the board's "scale_map" option; NULL when the board has none.
dd_board_scale_map <- function() {
  shiny::reactive({
    val <- blockr.core::get_board_option_or_null(
      "scale_map", blockr.core::get_session()
    )
    if (is.null(val) || !length(val)) NULL else val
  })
}

# Which drilldown role drives coloring, per chart type (pinned against the
# JS render paths in inst/js/drilldown-chart.js): stacked bar, radar and the
# individual/timeline families color by the `color` role; pie and treemap
# color their `group` slices; boxplot draws single-color boxes (no colored
# role). The x-axis/category role never auto-colors — coloring by a variable
# means mapping it to the colored role, like ggplot.
dd_colored_var <- function(chart_type, color, group) {
  role <- switch(
    chart_type %||% "bar",
    bar = ,
    scatter = ,
    line = ,
    radar = ,
    gantt = "color",
    pie = ,
    treemap = "group",
    NULL
  )

  var <- switch(role %||% "none", color = color, group = group, NULL)

  if (is.null(var) || !nzchar(var)) NULL else var
}

# Levels for resolution: factor levels when available (the data-level order
# contract), observed values otherwise.
dd_levels <- function(col) {
  if (is.factor(col)) {
    levels(col)
  } else {
    lv <- unique(as.character(col))
    lv[!is.na(lv)]
  }
}

# Mirrors the JS BLOCKR_PALETTE (inst/js/drilldown-chart.js) — the pool the
# chart cycles when no scale applies, so hash assignment draws from the same
# colors.
DD_BLOCKR_PALETTE <- c(
  "#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#CC79A7"
)

# Build the `scales` entry of the drilldown config payload, or NULL when no
# map / no colored role / no binding / no blockr.theme installed. Named
# vectors are converted to lists so names survive Shiny's JSON encoding
# (jsonlite drops names on atomic vectors).
dd_scales_config <- function(map, chart_type, color, group, data) {
  var <- dd_colored_var(chart_type, color, group)

  if (is.null(map) || is.null(var) || !is.data.frame(data) ||
        !var %in% names(data)) {
    return(NULL)
  }

  if (!requireNamespace("blockr.theme", quietly = TRUE)) {
    return(NULL)
  }

  res <- blockr.theme::resolve_scales(
    map, var,
    levels = dd_levels(data[[var]]),
    palette = DD_BLOCKR_PALETTE
  )

  if (is.null(res)) {
    return(NULL)
  }

  c(
    list(var = var),
    lapply(res, as.list)
  )
}
