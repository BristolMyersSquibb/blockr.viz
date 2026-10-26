# Public arguments of new_chart_block() and the chart's internal names.
#
# The constructor, the saved state and the `{@arg}` slots speak the public
# names. The server and the JavaScript (inst/js/chart/) keep the names they
# had before blockr.viz 0.3.0: `chart_type`, `group`/`value`/`orientation`,
# `bar_mode`, `tt_fields`, `value_lines`, `lo`/`hi`, `band_id`, `count_col`,
# `count_on`, `na_group`, `ctrl_target`/`ctrl_table`, `values` and the four
# `filter_*` fields. This file is the only place the two meet.
#
# The one translation that is not a rename is the roles. Publicly `x` and `y`
# are the screen axes for every type, as in ggplot2: a horizontal bar is
# `y = "category", x = "amount"`. Internally an aggregated chart has a
# category column (`group`), an aggregated column (`value`) and an
# `orientation`.

# Types whose category and value columns are drawn on screen axes, and which
# can lie either way. Waterfall is vertical only, pie and treemap have no
# axes: for those `x` is the category and `y` the value.
CHART_ORIENTED_TYPES <- c("bar", "boxplot", "pointrange")
CHART_AGGREGATED_TYPES <- c("bar", "waterfall", "pie", "treemap",
                            "boxplot", "pointrange")
CHART_DISTRIBUTION_TYPES <- c("boxplot", "pointrange")
CHART_TYPES <- c("bar", "histogram", "waterfall", "pie", "treemap",
                 "boxplot", "pointrange", "scatter", "line", "area", "band",
                 "gantt")

# Where the colour legend goes (`legend`): under the plot, above it, the
# names on the marks, or none.
CHART_LEGENDS <- c("bottom", "top", "marks", "none")

# position (public) <-> bar_mode (internal)
CHART_POSITIONS <- c(stack = "stacked", dodge = "grouped", fill = "percent")

# pct_of values: the category role is `group` internally
CHART_PCT_OF <- c(facet = "facet", category = "group", color = "color")

chart_position_internal <- function(position) {
  unname(CHART_POSITIONS[position %||% "stack"]) %|NA|% "stacked"
}

chart_position_public <- function(bar_mode) {
  names(CHART_POSITIONS)[match(bar_mode %||% "stacked", CHART_POSITIONS)] %|NA|%
    "stack"
}

chart_pct_of_internal <- function(pct_of) {
  out <- unname(CHART_PCT_OF[pct_of %||% "facet"])
  out[!is.na(out)] %||% "facet"
}

# Unknown values pass through, so the constructor can refuse them.
chart_pct_of_public <- function(pct_of) {
  pct_of <- pct_of %||% "facet"
  out <- names(CHART_PCT_OF)[match(pct_of, CHART_PCT_OF)]
  ifelse(is.na(out), pct_of, out)
}

`%|NA|%` <- function(x, y) {
  if (length(x) != 1L || is.na(x)) y else x
}

# Which way an oriented chart lies when `orientation` is not given. The
# column `func` aggregates sits on the value axis, the one it groups by on
# the category axis.
#
# - Only one of `x`/`y` set: with a row count (bar, `func = "count"`) that
#   column is the category; otherwise (a sum, a boxplot) it is the value.
# - Both set: a numeric column beside a non-numeric one is the value. Any
#   other pair is vertical, the category on `x`, as in ggplot2. A pair that
#   reads the other way (a distinct count of a character id by a category)
#   needs an explicit `orientation`.
# - Neither set: the type's default, as the browser draws it (bar
#   horizontal, boxplot and pointrange vertical).
#
# `is_num(col)` answers TRUE / FALSE, or NA when the data is not there yet.
chart_infer_orientation <- function(type, x, y, func, is_num) {
  has_x <- length(x) > 0L
  has_y <- length(y) > 0L
  count <- identical(type, "bar") && identical(func %||% "count", "count")
  if (has_x && !has_y) {
    return(if (count) "vertical" else "horizontal")
  }
  if (has_y && !has_x) {
    return(if (count) "horizontal" else "vertical")
  }
  if (has_x && has_y) {
    if (isTRUE(is_num(x)) && isFALSE(is_num(y))) {
      return("horizontal")
    }
    return("vertical")
  }
  if (type %in% CHART_DISTRIBUTION_TYPES) "vertical" else "horizontal"
}

# The column a histogram's bins are written to: `x` with a suffix. It carries
# x's label, so a click on a bin reads "hwy = 20-25" in the footer.
chart_bin_col <- function(x) {
  if (length(x)) paste0(x, "_bin") else NULL
}

# About `bins` bins over the range of `v`, at readable breaks (pretty()), as
# an ordered factor labelled "lo-hi". Bins are closed on the left, the last
# one on both sides. `edges` maps each label to its ends, for the filter a
# click on a bin emits.
chart_bins <- function(v, bins = 30) {
  v <- suppressWarnings(as.numeric(v))
  fin <- v[is.finite(v)]
  if (!length(fin)) {
    return(list(bin = factor(rep(NA_character_, length(v))),
                edges = data.frame(label = character(), lo = numeric(),
                                   hi = numeric())))
  }
  br <- pretty(range(fin), n = max(1L, as.integer(bins)))
  if (length(br) < 2L) br <- c(br[[1L]], br[[1L]] + 1)
  fmt <- format(br, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
  labels <- paste0(utils::head(fmt, -1L), "\u2013", utils::tail(fmt, -1L))
  bin <- cut(v, br, labels = labels, right = FALSE, include.lowest = TRUE)
  list(
    bin = factor(bin, levels = labels),
    edges = data.frame(label = labels, lo = utils::head(br, -1L),
                       hi = utils::tail(br, -1L), last = c(rep(FALSE,
                       length(labels) - 1L), TRUE))
  )
}

# The condition a click on bins means: x inside any of them.
chart_bin_cond <- function(x, labels, edges) {
  if (!is.data.frame(edges)) {
    return(NULL)
  }
  e <- edges[edges$label %in% labels, , drop = FALSE]
  if (!nrow(e)) {
    return(NULL)
  }
  parts <- lapply(seq_len(nrow(e)), function(i) {
    if (isTRUE(e$last[[i]])) {
      bquote(.data[[.(x)]] >= .(e$lo[[i]]) & .data[[.(x)]] <= .(e$hi[[i]]))
    } else {
      bquote(.data[[.(x)]] >= .(e$lo[[i]]) & .data[[.(x)]] < .(e$hi[[i]]))
    }
  })
  Reduce(function(a, b) bquote((.(a)) | (.(b))), parts)
}

# Public roles -> the internal ones the server and the browser read.
chart_roles_internal <- function(type, x, y, func, orientation,
                                 is_num = function(col) NA) {
  if (identical(type, "histogram")) {
    return(list(group = chart_bin_col(x), value = ".count", x = x, y = NULL,
                orientation = "vertical"))
  }
  if (!type %in% CHART_AGGREGATED_TYPES) {
    return(list(group = NULL, value = ".count", x = x, y = y,
                orientation = orientation))
  }
  if (type %in% CHART_ORIENTED_TYPES) {
    orient <- orientation %||%
      chart_infer_orientation(type, x, y, func, is_num)
  } else {
    orient <- NULL
  }
  if (identical(orient, "horizontal")) {
    group <- y
    value <- x
  } else {
    group <- x
    value <- y
  }
  if (identical(func %||% "count", "count") &&
        !type %in% CHART_DISTRIBUTION_TYPES) {
    value <- ".count"
  }
  list(group = group, value = value, x = NULL, y = NULL,
       orientation = orient)
}

# The gear's config (internal names, every role as the browser holds it) ->
# public `x`, `y`, `orientation`. `orientation` comes back only when the
# inference would get it wrong, so a chart that reads the usual way saves
# without one. `prev` is the public state before the edit: a type switch to
# another family whose roles the gear left empty keeps the columns.
chart_roles_public <- function(type, group, value, x, y, orientation, func,
                               prev, is_num = function(col) NA) {
  blank <- function(v) {
    v <- as.character(unlist(v))
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) v[[1L]] else NULL
  }
  group <- blank(group)
  value <- blank(value)
  if (identical(value, ".count")) value <- NULL
  x <- blank(x)
  y <- blank(y)

  if (identical(type, "histogram")) {
    return(list(x = x %||% prev$x, y = NULL, orientation = NULL))
  }
  if (!type %in% CHART_AGGREGATED_TYPES) {
    if (is.null(x) && is.null(y)) {
      x <- prev$x
      y <- prev$y
    }
    return(list(x = x, y = y, orientation = prev$orientation))
  }
  if (is.null(group) && is.null(value)) {
    return(list(x = prev$x, y = prev$y, orientation = prev$orientation))
  }
  if (!type %in% CHART_ORIENTED_TYPES) {
    return(list(x = group, y = value, orientation = NULL))
  }
  orient <- blank(orientation) %||%
    chart_infer_orientation(type, group, value, func, is_num)
  if (identical(orient, "horizontal")) {
    out <- list(x = value, y = group)
  } else {
    out <- list(x = group, y = value)
  }
  inferred <- chart_infer_orientation(type, out$x, out$y, func, is_num)
  out["orientation"] <- list(if (identical(inferred, orient)) NULL else orient)
  out
}

# x_ranges: a named list of c(from, to), as the caller writes it, or the
# list of {name, from, to} the browser sends back. NULL when empty. Dates
# travel as ISO strings, numbers as numbers.
chart_ranges_state <- function(x_ranges) {
  if (!is.list(x_ranges) || !length(x_ranges)) {
    return(NULL)
  }
  nms <- names(x_ranges) %||% rep("", length(x_ranges))
  out <- list()
  for (i in seq_along(x_ranges)) {
    r <- x_ranges[[i]]
    if (is.list(r) && !is.null(r$from)) {
      v <- list(r$from, r$to)
      nm <- as.character(r$name %||% "")
    } else {
      if (length(r) != 2L) next
      v <- if (inherits(r, c("Date", "POSIXt"))) as.list(format(r)) else as.list(r)
      nm <- nms[[i]]
    }
    if (length(v) != 2L || any(vapply(v, is.null, logical(1L)))) next
    key <- if (nzchar(nm)) nm else paste0("range", i)
    out[[key]] <- unlist(v)
  }
  if (length(out)) out else NULL
}

# What the browser reads: [{name, from, to}].
chart_ranges_json <- function(x_ranges) {
  if (!length(x_ranges)) {
    return(NULL)
  }
  unname(Map(function(nm, v) {
    list(name = if (grepl("^range[0-9]+$", nm)) "" else nm,
         from = v[[1L]], to = v[[2L]])
  }, names(x_ranges), x_ranges))
}

# selection (public) <-> filter_type / filters / filter_range / filter_point
# (internal). NULL when nothing is selected, so a chart without a click saves
# no selection.
chart_selection <- function(type = "categorical", filters = NULL,
                            range = NULL, point = NULL) {
  type <- type %||% "categorical"
  if (identical(type, "categorical") && is.null(filters) &&
        is.null(range) && is.null(point)) {
    return(NULL)
  }
  list(type = type, filters = filters, range = range, point = point)
}

chart_selection_state <- function(selection) {
  if (!is.list(selection) || !length(selection)) {
    return(NULL)
  }
  chart_selection(
    type = as.character(unlist(selection$type))[1L] %|NA|% "categorical",
    filters = chart_filters_state(selection$filters),
    range = null_state(selection$range),
    point = null_state(selection$point)
  )
}

# Old argument names, as boards saved before 0.3.0 carry them and as code
# written against those versions passes them. Returns the arguments under
# their current names. Used by blockr_deser.chart_block() (silently) and by
# the constructor (with a warning).
#
# Saved before 0.3.0, an aggregated chart had `group`/`value` and an
# `orientation` that was often unset (the browser then drew a bar horizontal,
# a boxplot vertical). The result carries the orientation explicitly where a
# distinct count would read it the other way.
CHART_LEGACY_ARGS <- c(
  "chart_type", "group", "value", "bar_mode", "tt_fields",
  "line_width_mult", "dot_size_mult", "value_lines", "lo", "hi",
  "band_id", "count_col", "count_on", "na_group", "ctrl_target",
  "ctrl_table", "values", "filter_type", "filters", "filter_range",
  "filter_point", "filter_column", "filter_values", "baseline",
  "func_toggle", "metric", "agg_fn", "direct_labels"
)

chart_legacy_args <- function(args) {
  if (!any(CHART_LEGACY_ARGS %in% names(args))) {
    return(args)
  }
  take <- function(nm) {
    v <- args[[nm]]
    args[[nm]] <<- NULL
    v
  }
  # 1:1 renames, current name wins when both are given
  renames <- c(chart_type = "type", tt_fields = "tooltip",
               line_width_mult = "line_width", dot_size_mult = "point_size",
               value_lines = "y_lines", lo = "ymin", hi = "ymax",
               count_on = "show_n",
               na_group = "missing", ctrl_target = "drill_target",
               ctrl_table = "drill_table", values = "script_values",
               agg_fn = "func")
  for (o in names(renames)) {
    if (o %in% names(args)) {
      v <- take(o)
      if (is.null(args[[renames[[o]]]])) args[renames[[o]]] <- list(v)
    }
  }
  if ("metric" %in% names(args)) {
    v <- take("metric")
    if (is.null(args$value)) args["value"] <- list(v)
  }
  # `direct_labels = TRUE` (0.3.0 before `legend`): the names on the marks.
  if ("direct_labels" %in% names(args)) {
    v <- take("direct_labels")
    if (is.null(args$legend) && isTRUE(bool_state(v))) args$legend <- "marks"
  }

  type <- as.character(unlist(args$type %||% "bar"))[1L]
  baseline <- take("baseline")
  if (identical(type, "bar") && identical(baseline, "cumulative")) {
    type <- "waterfall"
  }
  if (identical(type, "radar")) {
    warning("new_chart_block(): `type = \"radar\"` is no longer offered; ",
            "the chart is drawn as a bar chart.", call. = FALSE)
    type <- "bar"
  }
  args$type <- type

  if ("bar_mode" %in% names(args)) {
    v <- take("bar_mode")
    if (is.null(args$position)) args$position <- chart_position_public(v)
  }

  # roles
  group <- take("group")
  value <- take("value")
  if (type %in% CHART_AGGREGATED_TYPES) {
    func <- args$func %||% "count"
    value <- chr_state(value)
    if (identical(value, ".count")) value <- NULL
    group <- chr_state(group)
    orient <- chr_state(args$orientation)
    if (type %in% CHART_ORIENTED_TYPES) {
      orient <- orient %||%
        (if (type %in% CHART_DISTRIBUTION_TYPES) "vertical" else "horizontal")
      if (identical(orient, "horizontal")) {
        args$x <- value
        args$y <- group
      } else {
        args$x <- group
        args$y <- value
      }
      inferred <- chart_infer_orientation(type, args$x, args$y, func,
                                          function(col) NA)
      # A distinct count reads a character column: without data the
      # inference cannot tell which side is the category, so keep the
      # orientation the board was drawn with.
      keep <- !identical(inferred, orient) ||
        func %in% c("count_distinct", "pct_distinct")
      args["orientation"] <- list(if (keep) orient else NULL)
    } else {
      args$x <- group
      args$y <- value
      args["orientation"] <- list(NULL)
    }
  }


  # one id column
  band_id <- chr_state(take("band_id"))
  count_col <- chr_state(take("count_col"))
  if (is.null(args$id)) args["id"] <- list(band_id %||% count_col)

  # pct_of values
  if (!is.null(args$pct_of)) {
    args$pct_of <- chart_pct_of_public(as.character(unlist(args$pct_of)))
  }

  # selection
  ft <- take("filter_type")
  fl <- take("filters")
  fr <- take("filter_range")
  fp <- take("filter_point")
  fc <- take("filter_column")
  fv <- take("filter_values")
  if (is.null(args$selection)) {
    fl <- chart_filters_state(fl) %||% chart_legacy_filters(fc, fv)
    args["selection"] <- list(chart_selection(
      ft, fl, null_state(fr), null_state(fp)
    ))
  }

  take("func_toggle")
  args
}

# Something that reads and writes like a reactiveVal (`f()`, `f(v)`) but is
# backed by other state. The server's internal names are these, over the
# public reactiveVals, so the code that reads and writes them is unchanged.
chart_facade <- function(get, set) {
  force(get)
  force(set)
  function(value) {
    if (base::missing(value)) get() else invisible(set(value))
  }
}

# `{@arg}` slots in a title template name the PUBLIC arguments
# (`{@type}`, `{label(@y)}`); the resolver and the gear know the internal
# ones. Rewrite the names just before resolving; the stored template keeps
# what the author wrote. A saved template that already names an internal
# argument ("{@group}") resolves as it did.
CHART_SLOT_INTERNAL <- c(type = "chart_type", position = "bar_mode",
                         tooltip = "tt_fields", y_lines = "value_lines",
                         show_n = "count_on", missing = "na_group",
                         line_width = "line_width_mult",
                         point_size = "dot_size_mult")

# The internal name behind each public slot name, for the current roles: on
# an aggregated chart `x` and `y` are `group` and `value`, one way or the
# other.
chart_slot_alias <- function(type, roles) {
  out <- CHART_SLOT_INTERNAL
  if (type %in% CHART_AGGREGATED_TYPES) {
    if (identical(roles$orientation, "horizontal")) {
      out <- c(out, x = "value", y = "group")
    } else {
      out <- c(out, x = "group", y = "value")
    }
  }
  out
}

chart_template_internal <- function(template, alias) {
  if (!is.character(template) || !length(template) || is.na(template[[1L]]) ||
        !grepl("@", template[[1L]], fixed = TRUE)) {
    return(template)
  }
  out <- template[[1L]]
  for (nm in names(alias)) {
    out <- gsub(paste0("@(\\s*)", nm, "\\b(?!\\.)"),
                paste0("@\\1", alias[[nm]]), out, perl = TRUE)
  }
  out
}
