#' Emit a Chart Block's State as Plain ggplot2 Code
#'
#' The expression-emitting sibling of [static_chart()]. Where `static_chart()`
#' *interprets* chart-block state at render time (and therefore ships with
#' every rendered document), `chart_expr()` *compiles* the same state into a
#' plain dplyr + ggplot2 pipeline -- the code an analyst would have written by
#' hand. A document carrying that code reproduces the chart with no blockr
#' dependency at all.
#'
#' The emitted program is a single expression of the shape
#'
#' ```r
#' data |>
#'   count(SEX, ARM, name = "n") |>
#'   ggplot(aes(x = n, y = reorder(SEX, n, sum), fill = ARM)) +
#'   geom_col(width = 0.6) +
#'   scale_fill_manual(values = c(...)) +
#'   labs(title = "Count by SEX and ARM", x = "Count", y = NULL) +
#'   theme_minimal(base_size = 11) +
#'   theme(...)
#' ```
#'
#' Two compilation modes exist, keyed on `data`:
#'
#' * **With `data`** (a snapshot of the block's result), data-dependent
#'   choices are *baked in as literals*: level colors resolve through the
#'   board scale map / house palette into a named `c(level = "#hex")` vector,
#'   `{token}` title templates resolve to their text, count labels become a
#'   named label vector, and the line chart's marker cutoff is decided. This
#'   is the reproducible-report mode: the document freezes what you saw.
#' * **Without `data`**, the emitted code *recomputes* those choices at run
#'   time (label functions over the data variable, unnamed palette vector,
#'   templates left verbatim). The code stays correct under new data but is
#'   slightly less pretty.
#'
#' Argument names mirror [new_chart_block()] / [static_chart()] one to one.
#' Supported types are the [static_chart()] set: `"bar"` (stacked / grouped /
#' percent, `func = "identity"` for precomputed heights), `"boxplot"`,
#' `"scatter"` and `"line"`. An uncovered type -- or covered-but-undrawable
#' state (no group on a bar, say) -- returns `NULL`, so callers can fall back
#' to the `static_chart()` call form.
#'
#' @param var The variable name (string) holding the chart's data in the
#'   emitted code.
#' @inheritParams static_chart
#' @param data Optional data snapshot (the block's result). When supplied,
#'   data-dependent decisions are baked into the emitted code as literals;
#'   when `NULL`, the code recomputes them at run time.
#' @param qualify Prefix every non-base function with its namespace
#'   (`ggplot2::`, `dplyr::`, `stats::`)? Qualified code evaluates anywhere
#'   without `library()` calls -- the form a generated document wants.
#'   Unqualified code (the default) is the form a human wants to read, and
#'   assumes `library(ggplot2)` + `library(dplyr)`.
#'
#' @return A quoted expression (a single call), or `NULL` when the state
#'   cannot be compiled. Use [chart_code()] for a formatted character
#'   rendering.
#'
#' @examples
#' ex <- chart_expr("aes_data", chart_type = "bar", group = "Species")
#' cat(chart_code(ex))
#'
#' @export
chart_expr <- function(var = "data",
                       chart_type = "bar",
                       group = NULL,
                       color = NULL,
                       facet = NULL,
                       value = ".count",
                       func = "count",
                       x = NULL,
                       y = NULL,
                       series = NULL,
                       bar_mode = "stacked",
                       orientation = "horizontal",
                       sort_by = "value",
                       sort_dir = NULL,
                       count_on = "off",
                       count_col = NULL,
                       facet_scales = "fixed",
                       box_points = "none",
                       smoother = "none",
                       identity_line = FALSE,
                       lo = NULL,
                       hi = NULL,
                       step = NULL,
                       vlines = NULL,
                       hlines = NULL,
                       line_width_mult = 1,
                       dot_size_mult = 1,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       data = NULL,
                       scale_map = NULL,
                       qualify = FALSE,
                       ...) {

  chart_type <- (chart_type %||% "bar")[1L]

  col <- function(v) ce_col(v, data)

  group <- col(group)
  color <- col(color)
  facet <- col(facet)
  x <- col(x)
  y <- col(y)
  series <- col(series)
  count_col <- col(count_col)
  lo <- col(lo)
  hi <- col(hi)
  value_col <- if (!identical(value %||% ".count", ".count")) col(value)

  st <- list(
    var = as.name(var), data = data, scale_map = scale_map,
    group = group, color = color, facet = facet,
    value_col = value_col, func = func %||% "count",
    x = x, y = y, series = series,
    bar_mode = bar_mode %||% "stacked",
    orientation = orientation %||% "horizontal",
    sort_by = sort_by %||% "value", sort_dir = sort_dir,
    count_on = count_on %||% "off", count_col = count_col,
    box_points = box_points %||% "none", smoother = smoother %||% "none",
    identity_line = isTRUE(identity_line),
    lo = lo, hi = hi, step = step,
    vlines = vlines, hlines = hlines,
    line_width_mult = line_width_mult %||% 1,
    dot_size_mult = dot_size_mult %||% 1
  )

  fam <- switch(
    chart_type,
    bar = ce_bar(st),
    boxplot = ce_boxplot(st),
    scatter = ce_scatter(st),
    line = ce_line(st),
    NULL
  )

  if (is.null(fam)) {
    return(NULL)
  }

  # Head: thread the data through the transform stages into ggplot(). Built
  # NESTED -- ggplot(count(var, ...), aes(...)) -- because the native pipe
  # is parser sugar: a constructed `|>` call cannot be evaluated.
  # chart_code() renders the nested chain back in pipe form for display.
  inner <- Reduce(
    function(lhs, stage) {
      as.call(c(list(stage[[1L]], lhs), as.list(stage)[-1L]))
    },
    fam$stages,
    init = st$var
  )
  head <- as.call(c(
    list(call("::", as.name("ggplot2"), as.name("ggplot"))),
    list(inner, fam$aes)
  ))

  parts <- fam$layers

  # Color / fill scale: house palette, or the board scale map resolved to
  # named literals when a data snapshot is at hand.
  if (!is.null(fam$scale_aes)) {
    parts <- c(parts, list(ce_scale(fam$scale_aes, fam$scale_col, st)))
  }

  # Facets, shared across families.
  if (!is.null(st$facet)) {
    parts <- c(parts, list(ce_facet(st, facet_scales)))
  }

  # One labs() with axis labels and the title band.
  parts <- c(parts, list(ce_labs(fam$axis_labs, st, chart_type,
                                 title, subtitle, caption)))

  parts <- c(parts, ce_theme())

  out <- Reduce(function(a, b) call("+", a, b), parts, init = head)

  if (isTRUE(qualify)) out else ce_unqualify(out)
}

#' @param expr An expression as returned by `chart_expr()`.
#' @param width Target line width for the formatted code.
#'
#' @return `chart_code()`: the expression formatted as a single string, one
#'   pipeline stage / ggplot layer per line.
#'
#' @rdname chart_expr
#' @export
chart_code <- function(expr, width = 76L) {

  if (is.null(expr)) {
    return(NULL)
  }

  is_plus <- function(e) {
    is.call(e) && length(e) == 3L && identical(e[[1L]], as.name("+"))
  }

  # Flatten the left-leaning `+` chain; its head is the ggplot(...) call.
  parts <- list()
  while (is_plus(expr)) {
    parts <- c(list(expr[[3L]]), parts)
    expr <- expr[[2L]]
  }
  head <- expr

  # The head threads the data through nested transform stages --
  # ggplot(count(var, ...), aes(...)). Unwrap the first-argument chain down
  # to the data symbol and render it back in pipe form.
  stages <- list()
  root <- NULL
  if (is.call(head) && length(head) >= 2L) {
    cur <- head[[2L]]
    while (is.call(cur) && length(cur) >= 2L) {
      stages <- c(list(cur), stages)
      cur <- cur[[2L]]
    }
    if (is.name(cur) && length(stages)) {
      root <- cur
    }
  }

  # Deparse one part; continuation lines pick up the part's indent. (Guard
  # the single-line case: paste0() recycles zero-length input to "".)
  dp <- function(e, indent) {
    txt <- deparse(e, width.cutoff = max(20L, width - indent))
    if (length(txt) > 1L) {
      txt <- c(txt[1L], paste0(strrep(" ", indent + 2L), trimws(txt[-1L])))
    }
    paste(txt, collapse = "\n")
  }

  drop_data_arg <- function(e) {
    as.call(c(list(e[[1L]]), as.list(e)[-(1:2)]))
  }

  lines <- character()

  if (!is.null(root)) {
    lines <- c(lines, paste0(deparse(root), " |>"))
    for (s in stages) {
      lines <- c(lines, paste0("  ", dp(drop_data_arg(s), 2L), " |>"))
    }
    lines <- c(lines, paste0(
      "  ", dp(drop_data_arg(head), 2L), if (length(parts)) " +"
    ))
  } else {
    lines <- c(lines, paste0(dp(head, 0L), if (length(parts)) " +"))
  }

  for (i in seq_along(parts)) {
    lines <- c(lines, paste0(
      "  ", dp(parts[[i]], 2L), if (i < length(parts)) " +"
    ))
  }

  paste(lines, collapse = "\n")
}

# -- shared pieces -----------------------------------------------------------

# A state column reference: "" and NA become NULL; with a data snapshot,
# columns the data lost degrade to NULL as well (same contract as gg_col).
ce_col <- function(v, data) {
  v <- as.character(v %||% character())[1L]
  if (is.na(v) || !nzchar(v)) {
    return(NULL)
  }
  if (!is.null(data) && !v %in% names(data)) {
    return(NULL)
  }
  v
}

# Build a call from a "pkg::fn" head; emission is always qualified and
# ce_unqualify() strips the prefixes off at the end when unqualified code is
# asked for -- one canonical form, one walker.
ce_call <- function(fn, ...) {
  parts <- strsplit(fn, "::", fixed = TRUE)[[1L]]
  head <- if (length(parts) == 2L) {
    call("::", as.name(parts[[1L]]), as.name(parts[[2L]]))
  } else {
    as.name(fn)
  }
  args <- list(...)
  as.call(c(list(head), args[!vapply(args, is.null, logical(1L))]))
}

# A literal (possibly named) vector as a c(...) call; scalars stay bare.
ce_vec <- function(v) {
  if (length(v) == 1L && is.null(names(v))) {
    return(v)
  }
  as.call(c(list(as.name("c")), as.list(v)))
}

# `$` access on the data variable: var$col.
ce_dollar <- function(var, col) {
  call("$", var, as.name(col))
}

ce_unqualify <- function(e) {
  if (is.call(e)) {
    if (identical(e[[1L]], as.name("::"))) {
      return(e[[3L]])
    }
    for (i in seq_along(e)) {
      if (!is.null(e[[i]])) {
        e[i] <- list(ce_unqualify(e[[i]]))
      }
    }
  }
  e
}

# aes(...) with NULL slots dropped.
ce_aes <- function(...) {
  ce_call("ggplot2::aes", ...)
}

# The category-axis factor expression: how the emitted code orders levels.
# `metric` is the aggregated metric symbol for "value" sorts (reorder by the
# level's total), NULL where the raw values sort (boxplot medians).
#
# ggplot draws the FIRST level of a discrete y axis at the BOTTOM, so
# "largest first" on a horizontal chart is *ascending* level order -- which
# is exactly what stats::reorder() emits; the vertical chart wants the
# negated key. Direction overrides negate the key.
ce_cat_axis <- function(g, st, metric, fun, horiz, extra = NULL) {

  gs <- as.name(g)

  if (identical(st$sort_by, "alpha")) {
    # Alphabetical, first name on top of a horizontal chart = descending
    # levels. sort_dir flips.
    desc <- if (horiz) !identical(st$sort_dir, "desc") else
      identical(st$sort_dir, "desc")
    lv <- ce_call("base::sort", ce_call("base::unique", gs),
                  decreasing = if (desc) TRUE else NULL)
    return(ce_call("base::factor", gs, levels = lv))
  }

  # "value" (and column sorts, which degrade to "value" here): largest
  # first. Ascending levels put the largest on top of a horizontal chart.
  key <- metric %||% gs
  flip <- if (horiz) identical(st$sort_dir, "asc") else
    !identical(st$sort_dir, "asc")
  if (flip) {
    key <- call("-", key)
  }

  # `fun` may itself be qualified ("stats::median"); build it the same way
  # ce_call() builds heads so ce_unqualify() can strip it later.
  fun_parts <- strsplit(fun, "::", fixed = TRUE)[[1L]]
  fun_expr <- if (length(fun_parts) == 2L) {
    call("::", as.name(fun_parts[[1L]]), as.name(fun_parts[[2L]]))
  } else {
    as.name(fun)
  }

  do.call(ce_call, c(list("stats::reorder", gs, key, fun_expr), extra),
          quote = TRUE)
}

# Level -> hex for the emitted scale: the same resolution static_chart
# performs (board scale map through blockr.theme, else the house palette
# cycled over the levels), frozen into a named literal when a data snapshot
# is present; the bare palette pool otherwise.
ce_scale <- function(aes, col, st) {

  values <- if (!is.null(st$data)) {
    map <- st$scale_map %||% tryCatch(
      blockr.core::get_board_option_or_null(
        "scale_map", blockr.core::get_session()
      ),
      error = function(e) NULL
    )
    gg_level_colors(map, col, st$data)
  } else {
    # Unnamed values assign over the levels in order -- palette cycling,
    # as long as there are enough. dd_palette() is the full house pool.
    dd_palette()
  }

  ce_call(
    if (identical(aes, "fill")) {
      "ggplot2::scale_fill_manual"
    } else {
      "ggplot2::scale_color_manual"
    },
    values = ce_vec(values)
  )
}

ce_facet <- function(st, facet_scales) {

  scales <- match.arg(
    as.character(facet_scales %||% "fixed")[1L],
    c("fixed", "free_y", "free_x", "free")
  )

  labeller <- if (st$count_on %in% c("facet", "both") && !is.null(st$data)) {
    ce_call(
      "ggplot2::as_labeller",
      ce_vec(gg_count_labels(st$data, st$facet, st$count_col, st$func))
    )
  }

  ce_call(
    "ggplot2::facet_wrap",
    call("~", as.name(st$facet)),
    scales = if (!identical(scales, "fixed")) scales,
    labeller = labeller
  )
}

# Count labels for the category axis: baked to a named vector with a data
# snapshot, a label function over the data variable without.
ce_axis_labels <- function(st) {

  if (!st$count_on %in% c("axis", "both") || is.null(st$group)) {
    return(NULL)
  }

  if (!is.null(st$data)) {
    return(ce_vec(gg_count_labels(st$data, st$group, st$count_col, st$func)))
  }

  n_expr <- if (is.null(st$count_col)) {
    ce_call("base::table", ce_dollar(st$var, st$group))
  } else {
    ce_call(
      "base::tapply",
      ce_dollar(st$var, st$count_col),
      ce_dollar(st$var, st$group),
      quote(function(v) length(unique(v[!is.na(v)])))
    )
  }

  bquote(
    function(lv) paste0(lv, " (", .(n)[lv], ")"),
    list(n = n_expr)
  )
}

# One labs() call: axis labels (from the family) plus the title band.
# Templates resolve against a data snapshot when present; the automatic tier
# composes from state alone.
ce_labs <- function(axis_labs, st, chart_type, title, subtitle, caption) {

  resolve <- function(txt, auto = "") {
    if (is.null(txt)) {
      txt <- auto
    }
    txt <- as.character(txt)[1L]
    if (is.na(txt) || !nzchar(txt)) {
      return(NULL)
    }
    if (grepl("{", txt, fixed = TRUE) && !is.null(st$data)) {
      txt <- resolve_title_template(txt, st$data)
    }
    if (nzchar(txt)) txt else NULL
  }

  auto <- gg_auto_title(
    chart_type, st$group, st$color, st$value_col, st$func, st$x, st$y
  )

  band <- list(
    title = resolve(title, auto),
    subtitle = resolve(subtitle),
    caption = resolve(caption)
  )
  band <- band[!vapply(band, is.null, logical(1L))]

  # axis_labs keeps literal NULLs on purpose: `labs(y = NULL)` REMOVES an
  # axis title, which is different from not mentioning the axis.
  as.call(c(
    list(call("::", as.name("ggplot2"), as.name("labs"))),
    band,
    axis_labs
  ))
}

# Print typography, the compact subset of gg_theme() a human would write.
ce_theme <- function() {
  list(
    ce_call("ggplot2::theme_minimal", base_size = 11),
    ce_call(
      "ggplot2::theme",
      plot.title = ce_call("ggplot2::element_text", face = "bold", size = 13),
      legend.position = "bottom",
      legend.title = ce_call("ggplot2::element_blank"),
      panel.grid.minor = ce_call("ggplot2::element_blank")
    )
  )
}

# -- families ----------------------------------------------------------------

# Aggregation as the dplyr an analyst writes: count() for counts,
# summarise(.by =) for metric functions, distinct() for identity heights.
# Returns NULL when the state cannot aggregate (metric func, no value), the
# stages plus the metric symbol otherwise.
ce_agg <- function(st) {

  cells <- unique(c(st$facet, st$group, st$color))
  syms <- lapply(cells, as.name)

  if (identical(st$func, "identity")) {
    if (is.null(st$value_col)) {
      return(NULL)
    }
    stage <- as.call(c(
      list(ce_call("dplyr::distinct")[[1L]]),
      syms,
      list(.keep_all = TRUE)
    ))
    return(list(stages = list(stage), metric = as.name(st$value_col)))
  }

  if (identical(st$func, "count") || is.null(st$func)) {
    stage <- as.call(c(
      list(ce_call("dplyr::count")[[1L]]),
      syms,
      list(name = "n")
    ))
    return(list(stages = list(stage), metric = as.name("n")))
  }

  if (is.null(st$value_col)) {
    return(NULL)
  }

  by <- if (length(syms) == 1L) {
    syms[[1L]]
  } else {
    as.call(c(list(as.name("c")), syms))
  }

  if (identical(st$func, "count_distinct")) {
    metric <- "n"
    agg <- ce_call("dplyr::n_distinct", as.name(st$value_col))
  } else {
    # mean / sum / min / max are base, median lives in stats.
    metric <- paste0(st$func, "_", st$value_col)
    fn <- if (identical(st$func, "median")) "stats::median" else st$func
    agg <- ce_call(fn, as.name(st$value_col), na.rm = TRUE)
  }

  stage <- as.call(c(
    list(ce_call("dplyr::summarise")[[1L]]),
    stats::setNames(list(agg), metric),
    list(.by = by)
  ))

  list(stages = list(stage), metric = as.name(metric))
}

ce_bar <- function(st) {

  if (is.null(st$group)) {
    return(NULL)
  }

  agg <- ce_agg(st)

  if (is.null(agg)) {
    return(NULL)
  }

  horiz <- !identical(st$orientation, "vertical")
  cat_axis <- ce_cat_axis(st$group, st, agg$metric, "sum", horiz)

  aes <- if (horiz) {
    ce_aes(x = agg$metric, y = cat_axis,
           fill = if (!is.null(st$color)) as.name(st$color))
  } else {
    ce_aes(x = cat_axis, y = agg$metric,
           fill = if (!is.null(st$color)) as.name(st$color))
  }

  position <- switch(
    st$bar_mode,
    grouped = ce_call("ggplot2::position_dodge2", preserve = "single"),
    percent = "fill",
    NULL
  )

  col_args <- list(position = position, width = 0.6)
  if (is.null(st$color)) {
    col_args$fill <- dd_palette(1L)
  }

  layers <- list(do.call(ce_call, c(list("ggplot2::geom_col"), col_args), quote = TRUE))

  pct <- identical(st$bar_mode, "percent") && !is.null(st$color)
  pct_labels <- quote(function(v) paste0(round(100 * v), "%"))

  cat_labels <- ce_axis_labels(st)

  if (horiz) {
    if (!is.null(cat_labels)) {
      layers <- c(layers, list(
        ce_call("ggplot2::scale_y_discrete", labels = cat_labels)
      ))
    }
    if (pct) {
      layers <- c(layers, list(
        ce_call("ggplot2::scale_x_continuous", labels = pct_labels)
      ))
    }
  } else {
    if (!is.null(cat_labels)) {
      layers <- c(layers, list(
        ce_call("ggplot2::scale_x_discrete", labels = cat_labels)
      ))
    }
    if (pct) {
      layers <- c(layers, list(
        ce_call("ggplot2::scale_y_continuous", labels = pct_labels)
      ))
    }
  }

  val_lab <- if (pct) "Share" else gg_value_label(st$value_col, st$func)
  axis_labs <- if (horiz) {
    list(x = val_lab, y = NULL)
  } else {
    list(x = NULL, y = val_lab)
  }

  list(
    stages = agg$stages,
    aes = aes,
    layers = layers,
    scale_aes = if (!is.null(st$color)) "fill",
    scale_col = st$color,
    axis_labs = axis_labs
  )
}

ce_boxplot <- function(st) {

  if (is.null(st$group) || is.null(st$value_col)) {
    return(NULL)
  }

  if (!is.null(st$data) && !is.numeric(st$data[[st$value_col]])) {
    return(NULL)
  }

  v <- as.name(st$value_col)

  cat_axis <- ce_cat_axis(
    st$group, st, v, "stats::median", horiz = TRUE,
    extra = list(na.rm = TRUE)
  )

  aes <- ce_aes(x = v, y = cat_axis,
                fill = if (!is.null(st$color)) as.name(st$color))

  box_args <- list(
    outlier.shape = if (identical(st$box_points, "outliers")) 19 else NA,
    outlier.size = if (identical(st$box_points, "outliers")) 1,
    alpha = 0.85
  )
  if (is.null(st$color)) {
    box_args$fill <- dd_palette(1L)
  }

  layers <- list(do.call(ce_call, c(list("ggplot2::geom_boxplot"), box_args), quote = TRUE))

  if (identical(st$box_points, "all")) {
    pos <- if (!is.null(st$color)) {
      ce_call("ggplot2::position_jitterdodge", jitter.height = 0.15)
    } else {
      ce_call("ggplot2::position_jitter", height = 0.15)
    }
    layers <- c(layers, list(ce_call(
      "ggplot2::geom_point",
      position = pos, size = 0.7, alpha = 0.45, color = "#4b5563"
    )))
  }

  cat_labels <- ce_axis_labels(st)
  if (!is.null(cat_labels)) {
    layers <- c(layers, list(
      ce_call("ggplot2::scale_y_discrete", labels = cat_labels)
    ))
  }

  list(
    stages = list(),
    aes = aes,
    layers = layers,
    scale_aes = if (!is.null(st$color)) "fill",
    scale_col = st$color,
    axis_labs = list(x = st$value_col, y = NULL)
  )
}

ce_scatter <- function(st) {

  if (is.null(st$x) || is.null(st$y)) {
    return(NULL)
  }

  aes <- ce_aes(x = as.name(st$x), y = as.name(st$y),
                color = if (!is.null(st$color)) as.name(st$color))

  size <- 1.8 * st$dot_size_mult

  pt_args <- list(size = size, alpha = 0.8)
  if (is.null(st$color)) {
    pt_args$color <- dd_palette(1L)
  }

  layers <- list(do.call(ce_call, c(list("ggplot2::geom_point"), pt_args), quote = TRUE))

  if (st$identity_line) {
    rng <- ce_call(
      "base::range",
      ce_call("base::c", ce_dollar(st$var, st$x), ce_dollar(st$var, st$y)),
      na.rm = TRUE
    )
    layers <- c(layers, list(
      ce_call("ggplot2::geom_abline", slope = 1, intercept = 0,
              linetype = "dashed", color = "#6b7280"),
      ce_call("ggplot2::expand_limits", x = rng, y = rng)
    ))
  }

  if (st$smoother %in% c("lm", "loess")) {
    layers <- c(layers, list(ce_call(
      "ggplot2::geom_smooth",
      method = st$smoother, se = FALSE,
      # As a bare call, not a formula object: a formula would drag its
      # construction environment into the emitted AST.
      formula = call("~", as.name("y"), as.name("x")),
      linewidth = 0.7, color = "#374151"
    )))
  }

  layers <- c(layers, ce_helper_lines(st))

  list(
    stages = list(),
    aes = aes,
    layers = layers,
    scale_aes = if (!is.null(st$color)) "color",
    scale_col = st$color,
    axis_labs = list(x = st$x, y = st$y)
  )
}

ce_line <- function(st) {

  if (is.null(st$x) || is.null(st$y)) {
    return(NULL)
  }

  grp <- unique(c(st$series, st$color))

  group_expr <- if (length(grp) == 2L) {
    ce_call("base::interaction", as.name(grp[[1L]]), as.name(grp[[2L]]))
  } else if (length(grp) == 1L) {
    as.name(grp[[1L]])
  }

  aes <- ce_aes(
    x = as.name(st$x), y = as.name(st$y),
    color = if (!is.null(st$color)) as.name(st$color),
    group = group_expr
  )

  layers <- list()

  if (!is.null(st$lo) && !is.null(st$hi)) {
    layers <- c(layers, list(ce_call(
      "ggplot2::geom_ribbon",
      ce_aes(ymin = as.name(st$lo), ymax = as.name(st$hi)),
      alpha = 0.15, fill = dd_palette(1L)
    )))
  }

  lw <- 0.7 * st$line_width_mult

  direction <- switch(
    st$step %||% "",
    start = "vh",
    middle = "mid",
    end = "hv",
    NULL
  )

  layers <- c(layers, list(
    if (is.null(direction)) {
      ce_call("ggplot2::geom_line", linewidth = lw)
    } else {
      ce_call("ggplot2::geom_step", linewidth = lw, direction = direction)
    }
  ))

  # Marker cutoff: decided from the snapshot when present (the canvas'
  # 50-series budget), included otherwise -- few-series lines are the norm.
  markers <- is.null(st$data) || !length(grp) ||
    nrow(unique(st$data[grp])) <= 50L

  if (markers) {
    layers <- c(layers, list(ce_call(
      "ggplot2::geom_point", size = 1.4 * st$dot_size_mult
    )))
  }

  layers <- c(layers, ce_helper_lines(st))

  list(
    stages = list(),
    aes = aes,
    layers = layers,
    scale_aes = if (!is.null(st$color)) "color",
    scale_col = st$color,
    axis_labs = list(x = st$x, y = st$y)
  )
}

ce_helper_lines <- function(st) {

  vlines <- suppressWarnings(as.numeric(unlist(st$vlines %||% list())))
  hlines <- suppressWarnings(as.numeric(unlist(st$hlines %||% list())))
  vlines <- vlines[is.finite(vlines)]
  hlines <- hlines[is.finite(hlines)]

  c(
    if (length(vlines)) {
      list(ce_call(
        "ggplot2::geom_vline", xintercept = ce_vec(vlines),
        linetype = "dashed", color = "#9ca3af"
      ))
    },
    if (length(hlines)) {
      list(ce_call(
        "ggplot2::geom_hline", yintercept = ce_vec(hlines),
        linetype = "dashed", color = "#9ca3af"
      ))
    }
  )
}
