# Vendored scale-map resolver (consumer side of the cross-package
# convention, see blockr.design/open/cdex-attribute-map). The "scale_map"
# board option carries per-variable discrete scales: each binding is a named
# list of channels (color/shape/linetype); a *named* vector fixes values per
# level, an *unnamed* vector is a pool assigned by a stable hash of the level
# name. Writers (constructors, catalog, sidebar editor) live in
# blockr.pharma; consumers vendor this resolver so no dependency is needed.
# The hash line is pinned by the convention — do not change it without
# updating the convention and every copy (the agreement fixture in
# tests/testthat/test-scale-map-resolve.R drifts loudly otherwise).

# Reactive over the board's "scale_map" option; NULL when the board has none.
dd_board_scale_map <- function() {
  shiny::reactive({
    val <- blockr.core::get_board_option_or_null(
      "scale_map", blockr.core::get_session()
    )
    if (is.null(val) || !length(val)) NULL else val
  })
}

# The pinned hash assignment of the convention.
dd_scale_hash_pick <- function(level, pool) {
  idx <- strtoi(substr(rlang::hash(level), 1L, 7L), 16L) %% length(pool)
  pool[[idx + 1L]]
}

# Normalize a channel spec that may arrive as a list after JSON deser.
dd_channel <- function(spec) {
  if (is.null(spec) || !length(spec)) {
    return(NULL)
  }
  if (is.list(spec)) {
    nms <- names(spec)
    spec <- unlist(spec, use.names = FALSE)
    names(spec) <- nms
  }
  spec
}

# Resolve the binding for `var` against the levels actually shown. Returns
# list(color = <named chr>, shape = <named int>, linetype = <named chr>,
# order = <chr>) with unresolvable channels absent, or NULL when the map has
# no binding for `var`.
dd_resolve_scales <- function(map, var, levels, palette = NULL) {
  if (is.null(map) || is.null(var) || !length(levels) ||
        !var %in% names(map)) {
    return(NULL)
  }

  levels <- unique(as.character(levels))
  binding <- map[[var]]

  resolve_channel <- function(channel, fallback_pool = NULL) {
    spec <- dd_channel(binding[[channel]])
    fixed <- if (!is.null(spec) && !is.null(names(spec))) spec
    pool <- if (!is.null(spec) && is.null(names(spec))) spec
    pool <- pool %||% fallback_pool

    vals <- lapply(levels, function(lv) {
      if (!is.null(fixed) && lv %in% names(fixed)) {
        fixed[[lv]]
      } else if (!is.null(pool) && length(pool)) {
        dd_scale_hash_pick(lv, pool)
      } else {
        NULL
      }
    })

    keep <- !vapply(vals, is.null, logical(1L))
    if (!any(keep)) {
      return(NULL)
    }

    out <- unlist(vals[keep])
    names(out) <- levels[keep]
    out
  }

  fixed_names <- unique(unlist(lapply(
    binding[c("color", "shape", "linetype")],
    function(spec) names(dd_channel(spec))
  )))

  res <- Filter(
    Negate(is.null),
    list(
      color = resolve_channel("color", fallback_pool = palette),
      shape = resolve_channel("shape"),
      linetype = resolve_channel("linetype")
    )
  )

  res$order <- c(intersect(fixed_names, levels), setdiff(levels, fixed_names))

  res
}

# Which drilldown role drives coloring, per chart type (pinned against the
# JS render paths in inst/js/drilldown-chart.js): stacked bar and the
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
# map / no colored role / no binding. Named vectors are converted to lists so
# names survive Shiny's JSON encoding (jsonlite drops names on atomic
# vectors).
dd_scales_config <- function(map, chart_type, color, group, data) {
  var <- dd_colored_var(chart_type, color, group)

  if (is.null(map) || is.null(var) || !is.data.frame(data) ||
        !var %in% names(data)) {
    return(NULL)
  }

  res <- dd_resolve_scales(
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
