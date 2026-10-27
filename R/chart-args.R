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

# position (public) <-> bar_mode (internal)
CHART_POSITIONS <- c(stack = "stacked", dodge = "grouped", fill = "percent")

# pct_of values: the category role is `group` internally; "total" is no
# role, so the whole chart is one denominator.
CHART_PCT_OF <- c(facet = "facet", category = "group", color = "color",
                  total = "total")

chart_position_public <- function(bar_mode) {
  names(CHART_POSITIONS)[match(bar_mode %||% "stacked", CHART_POSITIONS)] %|NA|%
    "stack"
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

# highlight: one column and the values of it that stand out, as
# `list(country = "Switzerland")`. A named character vector is the same
# thing. NULL when there is no column or no value. Saved boards and MCP
# write it as given; the browser sends the column and the values apart,
# see chart_highlight_msg().
chart_highlight_state <- function(highlight) {
  if (is.null(highlight) || !length(highlight)) {
    return(NULL)
  }
  if (is.atomic(highlight)) {
    highlight <- as.list(highlight)
    if (length(unique(names(highlight))) == 1L) {
      highlight <- stats::setNames(list(unlist(highlight)), names(highlight)[[1L]])
    }
  }
  nms <- names(highlight)
  if (!is.list(highlight) || is.null(nms) || !all(nzchar(nms))) {
    stop("`highlight` names a column and its values, e.g. ",
         "`list(country = \"Switzerland\")`.", call. = FALSE)
  }
  if (length(highlight) > 1L) {
    stop("`highlight` names one column. Got ",
         paste0("`", nms, "`", collapse = ", "), ".", call. = FALSE)
  }
  vals <- as.character(unlist(highlight[[1L]], use.names = FALSE))
  vals <- unique(vals[!is.na(vals) & nzchar(vals)])
  if (!length(vals)) {
    return(NULL)
  }
  stats::setNames(list(vals), nms[[1L]])
}

# How a title's `{highlight}` prints the values: "Switzerland",
# "Switzerland and Germany", "Switzerland, Germany and Japan". "" for none.
chart_highlight_text <- function(highlight) {
  hl <- tryCatch(chart_highlight_state(highlight), error = function(e) NULL)
  if (is.null(hl)) {
    return("")
  }
  v <- hl[[1L]]
  if (length(v) == 1L) {
    return(v)
  }
  paste(paste(utils::head(v, -1L), collapse = ", "), "and", utils::tail(v, 1L))
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

# Arguments older charts had and that have no current meaning. A restore
# warns and drops them; any other unknown name is a block attribute.
CHART_RETIRED_ARGS <- c(
  "expose", "vlines", "hlines", "ref_x", "ref_y", "step", "smooth",
  "metrics", "group_by", "color_by", "facet_by", "x_col", "y_col",
  "series_by", "x_end_col", "lo_col", "hi_col"
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
