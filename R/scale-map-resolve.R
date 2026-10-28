# Glue for the board scale map (board option "scale_map"). The value shape,
# resolver and hash assignment live in blockr.theme, used behind a Suggests
# guard: without it the chart keeps its palette cycling.

# Reactive over the board's "scale_map" option; NULL when the board has none.
dd_board_scale_map <- function() {
  shiny::reactive({
    val <- blockr.core::get_board_option_or_null(
      "scale_map", blockr.core::get_session()
    )
    if (is.null(val) || !length(val)) NULL else val
  })
}

# The role that drives coloring, per chart type: `color` for most types, the
# `group` slices for pie and treemap. The category axis never colors. A type
# missing from this list gets no `scales` and falls back to palette cycling,
# so a new chart type has to be added here.
dd_colored_var <- function(chart_type, color, group) {
  type <- chart_type %||% "bar"
  var <- if (type %in% c("pie", "treemap")) {
    group
  } else if (type %in% c("bar", "scatter", "line", "radar", "boxplot",
                         "pointrange", "band", "gantt")) {
    color
  }
  if (is.null(var) || !nzchar(var)) NULL else var
}

# Factor levels, else the observed values.
dd_levels <- function(col) {
  if (is.factor(col)) {
    levels(col)
  } else {
    lv <- unique(as.character(col))
    lv[!is.na(lv)]
  }
}

# Resolve `var`'s binding for the data `column` with blockr.theme's
# provenance-aware resolver: a column copied by the picker block carries its
# origin in `blockr_source` and inherits the source's binding. Callers go
# through has_blockr_theme() first.
#
# A column carrying a group definition (expand_groups()) has pools among its
# levels ("All Xanomeline"), which no map pins. A pool takes the colour of a
# member (see group_pool_colour_levels()) unless the map pins the pool name,
# and the level order is the group order.
dd_resolve_scales <- function(map, var, column) {
  if (is.null(map) || is.null(var)) {
    return(NULL)
  }
  reps <- group_pool_colour_levels(column)
  if (!length(reps)) {
    return(
      blockr.theme::resolve_scales_col(map, var, column, palette = dd_palette())
    )
  }
  dd_resolve_group_scales(map, var, column, reps)
}

dd_resolve_group_scales <- function(map, var, column, reps) {
  lv <- dd_levels(column)
  reps <- reps[names(reps) %in% lv]
  src <- attr(column, "blockr_source", exact = TRUE)

  # Resolve the pools' stand-in members alongside the column's own levels;
  # each level is answered on its own.
  ext <- unique(c(lv, unname(reps)))
  attr(ext, "blockr_source") <- src
  res <- blockr.theme::resolve_scales_col(map, var, ext, palette = dd_palette())
  if (is.null(res)) {
    return(NULL)
  }

  # The binding used, to leave a pinned pool name alone.
  m <- blockr.theme::as_scale_map(map)
  bind <- if (var %in% names(m)) var else src
  binding <- if (is.character(bind) && length(bind) == 1L) m[[bind]]

  for (ch in setdiff(names(res), "order")) {
    vals <- res[[ch]]
    fixed <- names(binding[[ch]])
    for (g in names(reps)) {
      if (g %in% fixed || !reps[[g]] %in% names(vals)) {
        next
      }
      vals[[g]] <- vals[[reps[[g]]]]
    }
    res[[ch]] <- vals[names(vals) %in% lv]
  }
  res$order <- lv
  res
}

# The `scales` entry of the payload, or NULL without a map, a colored role, a
# binding or blockr.theme. Named vectors become lists so the names survive
# JSON.
dd_scales_config <- function(map, chart_type, color, group, data) {
  var <- dd_colored_var(chart_type, color, group)

  if (is.null(map) || is.null(var) || !is.data.frame(data) ||
        !var %in% names(data)) {
    return(NULL)
  }

  if (!has_blockr_theme()) {
    return(NULL)
  }

  # `var` is the chart's column name: the JS matches it against the mapping.
  res <- dd_resolve_scales(map, var, data[[var]])
  if (is.null(res)) {
    return(NULL)
  }
  c(list(var = var), lapply(res, as.list))
}

# `col`'s scale-map colors as a per-row hex vector (NA where the value is
# unmapped), for the table's categorical row swatch: the chart's colors, via
# the same resolver. NULL without a map, a binding for `col`, or blockr.theme.
dd_row_hex <- function(map, col, data) {
  if (is.null(map) || is.null(col) || !is.data.frame(data) ||
        !col %in% names(data)) {
    return(NULL)
  }

  if (!has_blockr_theme()) {
    return(NULL)
  }

  pal <- dd_resolve_scales(map, col, data[[col]])$color
  if (is.null(pal)) {
    return(NULL)
  }
  unname(pal[as.character(data[[col]])])
}

# Identity color for an explicit "Color by" pick: dd_row_hex() first, else
# palette cycling over the levels in the chart's order (factor levels, else
# alphabetical), so the colors match the chart's. The table's default stub
# tint does not use it: it stays map-bound only.
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
  pal <- stats::setNames(dd_level_palette(length(lv)), lv)
  unname(pal[as.character(x)])
}
