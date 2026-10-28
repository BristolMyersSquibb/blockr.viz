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

# Which role drives coloring, per chart type (pinned against the JS render
# paths in inst/js/chart/): stacked bar, radar, the distribution marks
# (boxplot, pointrange, band) and the individual and timeline families color by
# the `color` role (a distribution mark splits each group into one colored slot
# per level); pie and treemap color their `group` slices. The x-axis or
# category role never auto-colors: coloring by a variable means mapping it to
# the colored role, as in ggplot.
#
# A type missing from this list resolves to no variable, so R sends no `scales`
# at all and the renderer falls back to palette cycling: the board's fixed
# colors are ignored with nothing to show for it. A new chart type has to be
# added here.
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

# The one scale-resolution seam for this package's renderers: resolve `var`'s
# binding for the data `column` with blockr.theme's provenance-aware resolver.
# A column copied by the picker block (SEX picked into "color") carries its
# origin in `blockr_source` and inherits the source's binding, so the fixed SEX
# colors survive the rename. Callers go through has_blockr_theme() first, which
# gates on the version that introduced the resolver; no renderer here decides
# for itself whether provenance applies.
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
    res <- blockr.theme::resolve_scales_col(map, var, column,
                                            palette = dd_palette())
    return(dd_dodge_scales(res, map, var, column))
  }
  dd_resolve_group_scales(map, var, column, reps)
}

# blockr.theme gives a level the map does not pin a colour by a hash of its
# name, so two levels can draw the same colour (Placebo and Xanomeline High
# Dose did). Within one view, a hashed level whose colour an earlier level
# already holds takes the first palette colour no level holds. Pinned colours
# never move, and with more levels than colours the hash stands. `skip`:
# levels that take part in neither (a pool, which borrows a member's colour).
# A channel's entries split into pins (named) and pool (unnamed). blockr.theme
# allows both in one vector, e.g. c("01-701-1015" = "#d55e00", pool), where the
# pool's names are "". An all-named or all-unnamed vector reads as before.
dd_spec_pinned <- function(spec) {
  nm <- names(spec)
  if (is.null(nm)) {
    return(rep(FALSE, length(spec)))
  }
  !is.na(nm) & nzchar(nm)
}

dd_spec_fixed <- function(spec) {
  as.character(names(spec)[dd_spec_pinned(spec)])
}

dd_spec_pool <- function(spec) {
  pool <- unname(spec[!dd_spec_pinned(spec)])
  if (length(pool)) pool
}

dd_dodge_scales <- function(res, map, var, column, skip = character()) {
  vals <- res$color
  if (is.null(vals)) {
    return(res)
  }
  m <- blockr.theme::as_scale_map(map)
  src <- attr(column, "blockr_source", exact = TRUE)
  bind <- if (var %in% names(m)) var else src
  spec <- if (is.character(bind) && length(bind) == 1L) m[[bind]]$color
  fixed <- dd_spec_fixed(spec)
  pool <- dd_spec_pool(spec) %||% attr(m, "palette", exact = TRUE) %||% dd_palette()
  if (sum(!names(vals) %in% skip) > length(pool)) {
    return(res)
  }
  used <- unname(vals[names(vals) %in% fixed])
  for (lv in names(vals)) {
    if (lv %in% c(fixed, skip)) next
    if (vals[[lv]] %in% used) {
      free <- setdiff(pool, used)
      if (length(free)) vals[[lv]] <- free[[1L]]
    }
    used <- c(used, vals[[lv]])
  }
  res$color <- vals
  res
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
  # Dodge among the raw levels; a pool then borrows its member's colour.
  res <- dd_dodge_scales(res, map, var, ext, skip = names(reps))

  # The binding used, to leave a pinned pool name alone.
  m <- blockr.theme::as_scale_map(map)
  bind <- if (var %in% names(m)) var else src
  binding <- if (is.character(bind) && length(bind) == 1L) m[[bind]]

  for (ch in setdiff(names(res), "order")) {
    vals <- res[[ch]]
    fixed <- dd_spec_fixed(binding[[ch]])
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

  # Resolve through provenance (picker copies), but emit the chart's column name
  # as `var`: the JS matches `scales.var` against the mapped column.
  res <- dd_resolve_scales(map, var, data[[var]])
  if (is.null(res)) {
    return(NULL)
  }
  c(list(var = var), lapply(res, as.list))
}

# `col`'s scale-map colors as a per-row hex vector (one entry per row of
# `data`, NA where the row's value is unmapped), for the categorical row swatch
# (e.g. SEX: F = teal, M = orange): the chart's colors, via the same resolver.
# This is separate from the numeric `cell_color` heatmap (sequential or
# diverging over the body cells): the swatch rides the categorical column and
# the heatmap owns the numeric body, so both coexist. NULL without a map, a
# binding for `col` in it, or blockr.theme.
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

# Identity color for an explicit "Color by" pick: dd_row_hex() (the scale map)
# first, falling back to deterministic palette cycling when the column has no
# binding or the board has no map. That is the fallback the chart applies, so
# "Color by" works on any categorical column and still matches the chart's
# colors. Level order mirrors the chart's for unbound columns (factor levels,
# else alphabetical; JS sorts by localeCompare), so the level to color
# assignment is identical across blocks. Not used for the table's
# smart-default stub tint, which stays map-bound only: an unbound, usually
# unique, rowname column would rainbow every table by default.
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
