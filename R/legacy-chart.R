# LEGACY: the chart's arguments from blockr.viz < 0.3.0 that a rename cannot
# say (roles, the selection, `baseline`, `radar`, the id columns). Plain
# renames and value maps are rows of LEGACY_NAMES (R/legacy.R), applied first.

`%|NA|%` <- function(x, y) {
  if (length(x) != 1L || is.na(x)) y else x
}


# Arguments older charts had and that have no current meaning. A restore warns
# and drops them; any other unknown name is a block attribute.
LEGACY_CHART_RETIRED <- c(
  "vlines", "hlines", "ref_x", "ref_y", "step", "smooth",
  "metrics", "group_by", "color_by", "facet_by", "x_col", "y_col",
  "series_by", "x_end_col", "lo_col", "hi_col"
)

# Returns the arguments under their current names. An aggregated chart saved
# before 0.3.0 often had no `orientation` (the browser drew a bar horizontal,
# a boxplot vertical); the result states it where a distinct count would read
# it the other way.
# `old`: the arguments carried an old chart name (before the renames).
legacy_chart_roles <- function(args, old) {
  if (!old) {
    return(args)
  }
  take <- function(nm) {
    v <- args[[nm]]
    args[[nm]] <<- NULL
    v
  }
  # The current name wins when both are given.
  move <- function(old, new, f = identity) {
    if (old %in% names(args)) {
      v <- take(old)
      if (is.null(args[[new]])) args[new] <<- list(f(v))
    }
  }
  move("metric", "value")
  # `direct_labels = TRUE` (0.3.0 before `legend`): the names on the marks.
  if ("direct_labels" %in% names(args)) {
    v <- take("direct_labels")
    if (is.null(args[["legend"]]) && isTRUE(bool_state(v))) {
      args$legend <- "marks"
    }
  }

  type <- as.character(unlist(args[["type"]] %||% "bar"))[1L]
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

  # Roles.
  group <- take("group")
  value <- take("value")
  if (type %in% CHART_AGGREGATED_TYPES) {
    func <- args[["func"]] %||% "count"
    value <- chr_state(value)
    if (identical(value, ".count")) value <- NULL
    group <- chr_state(group)
    orient <- chr_state(args[["orientation"]])
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
      inferred <- chart_infer_lie(
        list(type = type, x = args[["x"]], y = args[["y"]], func = func), NULL
      )
      # A distinct count reads a character column: without data the inference
      # cannot tell which side is the category, so keep the orientation the
      # board was drawn with.
      keep <- !identical(inferred, orient) ||
        func %in% c("count_distinct", "pct_distinct")
      args["orientation"] <- list(if (keep) orient else NULL)
    } else {
      args$x <- group
      args$y <- value
      args["orientation"] <- list(NULL)
    }
  }

  # One id column.
  band_id <- chr_state(take("band_id"))
  count_col <- chr_state(take("count_col"))
  if (is.null(args[["id"]])) args["id"] <- list(band_id %||% count_col)

  # The selection.
  ft <- take("filter_type")
  fl <- take("filters")
  fr <- take("filter_range")
  fp <- take("filter_point")
  fc <- take("filter_column")
  fv <- take("filter_values")
  if (is.null(args[["selection"]])) {
    fl <- chart_filters_state(fl) %||% chart_legacy_filters(fc, fv)
    args["selection"] <- list(chart_selection(
      ft, fl, null_state(fr), null_state(fp)
    ))
  }

  take("func_toggle")
  args
}

# The chart's old selection `{type, filters, range, point}` in the kit's
# shape: categorical, or a range (a point is a range with equal ends).
legacy_chart_selection <- function(args) {
  s <- args[["selection"]]
  if (!is.list(s) || !length(s)) {
    return(args)
  }
  type <- as.character(unlist(s$type))[1L] %|NA|% "categorical"
  old <- any(c("range", "point") %in% names(s)) ||
    (identical(type, "categorical") && !"columns" %in% names(s))
  if (!old) {
    return(args)
  }
  r <- s$range
  p <- s$point
  args["selection"] <- list(if (identical(type, "range") && !is.null(r)) {
    list(type = "range", x = r$x_col, y = r$y_col,
         x_range = as.numeric(unlist(r$x_range)),
         y_range = if (!is.null(r$y_range)) as.numeric(unlist(r$y_range)),
         filters = s$filters %||% list())
  } else if (identical(type, "point") && !is.null(p)) {
    list(type = "range", x = p$x_col, y = p$y_col,
         x_range = rep(as.numeric(p$x_val), 2L),
         y_range = rep(as.numeric(p$y_val), 2L), filters = list())
  } else if (length(s$filters)) {
    list(type = "categorical", filters = s$filters, columns = list())
  })
  args
}

# Whether an aggregated chart lies horizontally (without data).
legacy_chart_horizontal <- function(state) {
  type <- state[["type"]] %||% "bar"
  if (!type %in% c("bar", "boxplot", "pointrange")) {
    return(FALSE)
  }
  orient <- state[["orientation"]] %||% chart_infer_lie(state, NULL)
  identical(orient, "horizontal")
}
