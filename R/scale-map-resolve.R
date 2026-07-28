# Drilldown-chart glue for the board scale map (convention: option id
# "scale_map", see blockr.design/open/blockr.theme). The contract code --
# value shape, resolver, hash assignment -- lives in blockr.theme, consumed
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
# JS render paths in inst/js/chart.js): stacked bar, radar, the distribution
# marks (boxplot/pointrange/band) and the individual/timeline families color
# by the `color` role (a distribution mark splits each group into one colored
# slot per level); pie and treemap color their `group` slices. The
# x-axis/category role never auto-colors -- coloring by a variable means
# mapping it to the colored role, like ggplot.
#
# A type missing from this switch resolves to no variable, so R sends no
# `scales` at all and the renderer silently falls back to palette cycling --
# the board's fixed colors are ignored with nothing to show for it. Any new
# chart type has to be added here.
dd_colored_var <- function(chart_type, color, group) {
  role <- switch(
    chart_type %||% "bar",
    bar = ,
    scatter = ,
    line = ,
    radar = ,
    boxplot = ,
    pointrange = ,
    band = ,
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

# The ONE scale-resolution seam for this package's renderers: resolve `var`'s
# binding for the actual data `column` via blockr.theme's canonical
# provenance-aware resolver -- a column copied by the picker block (SEX
# picked into "color") carries its origin in `blockr_source` and inherits
# the source's binding, so the fixed SEX colors survive the rename. Callers
# reach this only behind has_blockr_theme(), which gates on the version that
# introduced the resolver -- no renderer here decides for itself whether
# provenance applies, it only decides whether blockr.theme can answer at all.
dd_resolve_scales <- function(map, var, column) {
  if (is.null(map) || is.null(var)) {
    return(NULL)
  }
  blockr.theme::resolve_scales_col(map, var, column, palette = dd_palette())
}


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

  if (!has_blockr_theme()) {
    return(NULL)
  }

  # Resolve through provenance (picker copies), but emit the CHART's column
  # name as `var`: the JS side matches `scales.var` against the mapped column.
  res <- dd_resolve_scales(map, var, data[[var]])

  if (is.null(res)) {
    return(NULL)
  }

  c(
    list(var = var),
    lapply(res, as.list)
  )
}

# Resolve `col`'s scale-map colors to a PER-ROW hex vector (one entry per row
# of `data`, NA where the row's value is unmapped) for the CATEGORICAL row
# swatch (e.g. SEX: F = teal, M = orange) -- the same colors the chart uses, via
# the same resolver. This is orthogonal to the numeric `cell_color` heatmap
# (sequential / diverging over the body cells): the swatch rides the categorical
# column, the heatmap owns the numeric body, so both coexist. Returns NULL when
# there is no map, `col` is not bound in it (resolve_scales returns NULL), or
# blockr.theme is not installed.
dd_row_hex <- function(map, col, data) {
  if (is.null(map) || is.null(col) || !is.data.frame(data) ||
        !col %in% names(data)) {
    return(NULL)
  }

  if (!has_blockr_theme()) {
    return(NULL)
  }

  res <- dd_resolve_scales(map, col, data[[col]])

  pal <- res$color
  if (is.null(pal)) {
    return(NULL)
  }

  unname(pal[as.character(data[[col]])])
}

# Identity color for an EXPLICIT "Color by" pick: dd_row_hex (the scale map)
# first, falling back to deterministic palette cycling when the column has no
# binding (or the board has no map) -- the same fallback the chart applies,
# so "Color by" works on any categorical column and STILL matches the chart's
# colors. Level order mirrors the chart's _orderLevels for unbound columns
# (factor levels, else alphabetical -- JS sorts by localeCompare), keeping the
# level -> color assignment identical across blocks. NOT used for the table's
# smart-default stub tint, which stays map-bound-only (an unbound, usually
# unique, rowname column would rainbow every table by default).
dd_ident_hex <- function(map, col, data) {
  hx <- dd_row_hex(map, col, data)
  if (!is.null(hx)) {
    return(hx)
  }
  if (is.null(col) || !is.data.frame(data) || !col %in% names(data)) {
    return(NULL)
  }
  x <- data[[col]]
  lv <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  lv <- lv[!is.na(lv)]
  if (!length(lv)) {
    return(NULL)
  }
  pal <- stats::setNames(rep_len(dd_palette(), length(lv)), lv)
  unname(pal[as.character(x)])
}
