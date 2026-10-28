# Row types and the displays each allows; the first is the default. `dot` is a
# single value as a positioned point on the zero-based lane. `dist` keeps its
# legacy `show` values because saved boards carry them; they are sugar over
# `style` / `inner` / `outer` (LANE_DIST_STYLES).
LANE_ROW_TYPES <- list(
  simple = c("bar", "number", "dot", "tile"),
  dist = c("pointrange", "box", "text"),
  field = "text",
  series = "sparkline",
  spans = "interval",
  pair = "dumbbell",
  expr = "text",
  custom = c("bar", "number", "dumbbell", "box", "pointrange", "text")
)

# The pair row: two summaries of one group drawn as a segment, a hollow diamond
# at `from` and a dot at `to`. `from_func` / `to_func` default to "identity"
# and "max".
LANE_PAIR_FUNCS <- c("identity", "mean", "median", "min", "max")

# A distribution glyph is ONE mark: a centre, an optional inner range and an
# optional outer range, drawn in a `style`:
#   "dot"  outer as a pale fence band (the mark's own rail), inner as a thick
#          bar, centre as a ringed dot. The default.
#   "box"  outer as a capped whisker, inner as a rectangle, centre as a tick.
# Both ranges take "none": an IQR bar is a box with no outer, a plain
# pointrange a dot with no outer, a bare dot a centre alone. The statistics are
# LANE_STATS / LANE_WHISKERS.
LANE_DIST_STYLES <- c("dot", "box")

# Ceiling on a mapped dimension's levels, the chart block's (MAX_COLOR_LEVELS
# in chart/model-common.js). In a facet a level is a COLUMN, so a wide table is
# no reading either. Pointing either at a subject id must fail loudly, naming
# the column.
LANE_MAX_LEVELS <- 15L

# A tile column is a matrix: an AE heatmap lays many terms side by side as
# square cells, so its facet takes more levels. The cap still stops a facet on
# a subject id.
LANE_MAX_TILE_LEVELS <- 300L

# A summary entry is saved and edited under PUBLIC keys, the chart block's
# vocabulary:
#   dist   summary, whiskers  (`summary` is also a text column's statistic)
#   pair   x, xend, x_func, xend_func, ref_lo, ref_hi
#   spans  tooltip
#   simple position  ("stack", "dodge", "fill")
# The engine reads INTERNAL names (inner / outer / stat, from / to / from_func /
# to_func / lo / hi, fields); lane_public_summary() and lane_internal_summary()
# convert.

LANE_PUBLIC_KEYS <- list(
  pair = c(x = "from", xend = "to", x_func = "from_func",
           xend_func = "to_func", ref_lo = "lo", ref_hi = "hi"),
  spans = c(tooltip = "fields")
)

LANE_POSITIONS <- c(stack = "stacked", dodge = "grouped", fill = "percent")

#' A summary entry under its public keys: what state and the gear hold.
#' @noRd
lane_public_summary <- function(s) {
  if (!is.list(s)) return(s)
  type <- summarize_chr1(s[["type"]]) %||% "simple"
  take <- function(new, old) {
    v <- s[[new]] %||% s[[old]]
    s[[old]] <<- NULL
    if (!is.null(v)) s[[new]] <<- v
  }
  if (identical(type, "dist")) {
    v <- s[["summary"]] %||% s[["inner"]] %||% s[["stat"]]
    s[c("inner", "stat")] <- NULL
    if (!is.null(v)) s[["summary"]] <- v
    take("whiskers", "outer")
  }
  map <- LANE_PUBLIC_KEYS[[type]]
  for (new in names(map)) take(new, map[[new]])
  pos <- summarize_chr1(s[["position"]])
  if (!is.null(pos) && pos %in% LANE_POSITIONS) {
    s[["position"]] <- names(LANE_POSITIONS)[match(pos, LANE_POSITIONS)]
  }
  s
}

#' A summary entry written with the spec's keys (R/spec-table-summaries.R)
#' under the keys the engine reads: a sparkline is a "series" with
#' `band = c(ymin, ymax)` and `ref` (`median_q1_q3` maps to `median_iqr`); a
#' pair's `ref_min`, `ref_max`, `x_lines`, `linetype` become `lo`, `hi`, `ref`,
#' `dash`; a custom summary's `id` is its `denom`. A key the engine has wins.
#' @noRd
lane_spec_summary <- function(s) {
  type <- summarize_chr1(s[["type"]]) %||% "simple"
  put <- function(key, v) {
    if (is.null(s[[key]]) && length(v)) s[[key]] <<- v
  }
  if (identical(type, "sparkline")) {
    s[["type"]] <- "series"
    if (length(s[["ymin"]]) && length(s[["ymax"]])) {
      put("band", c(summarize_chr1(s[["ymin"]]), summarize_chr1(s[["ymax"]])))
    }
    ref <- summarize_chr1(s[["summary"]])
    put("ref", if (identical(ref, "median_q1_q3")) "median_iqr" else ref)
    s[c("ymin", "ymax", "summary")] <- NULL
  } else if (identical(type, "pair")) {
    put("lo", s[["ref_min"]])
    put("hi", s[["ref_max"]])
    put("ref", unlist(s[["x_lines"]])[1L])
    put("dash", s[["linetype"]])
    s[c("ref_min", "ref_max", "x_lines", "linetype")] <- NULL
  } else if (identical(type, "custom")) {
    put("denom", s[["id"]])
    s[["id"]] <- NULL
  }
  s
}

#' A summary entry under the engine's internal keys. Run first thing in
#' lane_norm_summaries(); internal names already present win.
#' @noRd
lane_internal_summary <- function(s) {
  if (!is.list(s)) return(s)
  s <- lane_spec_summary(s)
  type <- summarize_chr1(s[["type"]]) %||% "simple"
  if (identical(type, "dist")) {
    sm <- s[["summary"]]
    if (!is.null(sm)) {
      if (identical(summarize_chr1(s[["show"]]), "text")) {
        s[["stat"]] <- s[["stat"]] %||% sm
      } else {
        s[["inner"]] <- s[["inner"]] %||% sm
      }
    }
    if (!is.null(s[["whiskers"]])) s[["outer"]] <- s[["outer"]] %||% s[["whiskers"]]
  }
  map <- LANE_PUBLIC_KEYS[[type]]
  for (new in names(map)) {
    old <- map[[new]]
    if (!is.null(s[[new]]) && is.null(s[[old]])) s[[old]] <- s[[new]]
  }
  s
}

#' Normalize a summaries list: types, required fields, `show` within the type's
#' set, `color` / `facet`, an auto `name`. Returns the list, or `list(err =)`
#' naming the first broken row.
#' @noRd
lane_norm_summaries <- function(summaries, data = NULL) {
  if (!is.list(summaries)) return(list(err = "`summaries` must be a list"))
  out <- vector("list", length(summaries))
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    if (!is.list(s)) {
      return(list(err = paste0("Summary ", i, " is not a list")))
    }
    r <- lane_norm_row(s, i, data)
    if (!is.null(r$err)) return(r)
    out[[i]] <- r$s
  }
  out
}

#' Normalize one summary row. Returns `list(s =)` or `list(err =)`.
#' @noRd
lane_norm_row <- function(s, i, data = NULL) {
  # An unset field is ABSENT. A typed tool call delivers the array as a table,
  # so unused fields arrive as NA, which a per-type branch would read as a
  # value (a pair row threw "invalid 'times'").
  s <- s[!vapply(s, function(v) {
    is.null(v) || (is.atomic(v) && length(v) == 1L && is.na(v))
  }, logical(1L))]
  s <- lane_internal_summary(s)
  type <- summarize_chr1(s$type) %||% "simple"
  if (!type %in% names(LANE_ROW_TYPES)) {
    return(list(err = paste0("Summary ", i, ": unknown type \"", type,
                             "\"")))
  }
  shows <- LANE_ROW_TYPES[[type]]
  show_given <- summarize_chr1(s$show)
  show <- show_given %||% shows[[1L]]
  if (!show %in% shows) show <- shows[[1L]]
  scope <- summarize_chr1(s$scope) %||% "cell"
  if (!scope %in% c("cell", "pooled")) scope <- "cell"
  # Fields are group facts: always pooled (they stand outside faceting).
  if (identical(type, "field")) scope <- "pooled"

  for (nm in lane_required_fields(type, s)) {
    if (is.null(summarize_chr1(s[[nm]]))) {
      return(list(err = paste0("Summary ", i, " (", type, "): `", nm,
                               "` is required")))
    }
  }

  s$type <- type
  s$show <- show
  # The colour split's layout (the chart's `position`); unset = decided by the
  # measure.
  pos <- summarize_chr1(s[["position"]])
  s$.mode <- if (!is.null(pos) && pos %in% names(LANE_POSITIONS)) {
    LANE_POSITIONS[[pos]]
  } else if (!is.null(pos) && pos %in% LANE_POSITIONS) {
    pos
  }
  # The tile: a square cell painted by the worst level of `fill`, else by the
  # value. Facet levels order by row count by default (commonest terms first).
  if (identical(type, "simple") && identical(show, "tile")) {
    s$fill <- summarize_chr1(s[["fill"]])
    fo <- summarize_chr1(s[["facet_order"]]) %||% "count"
    s$facet_order <- if (fo %in% c("count", "level")) fo else "count"
  }
  # A custom row's mark is read off its function unless one is named.
  if (identical(type, "custom")) s$show <- show_given
  s$scope <- scope
  # Colour and facet are the summary's OWN optional mappings. `scope` survives
  # only to seed `facet` for boards saved under the table-level pair
  # (lane_migrate_globals()).
  s$color <- summarize_chr1(s$color)
  # Fields are group facts: outside faceting.
  s$facet <- if (identical(type, "field")) NULL else summarize_chr1(s$facet)
  if (identical(type, "dist") && !identical(show, "text")) {
    s <- lane_norm_dist(s, show_given)
  }
  if (identical(type, "series")) s <- lane_norm_series(s)
  if (identical(type, "spans")) s <- lane_norm_spans(s)
  if (identical(type, "pair")) s <- lane_norm_pair(s)
  s$name <- summarize_chr1(s$name) %||% lane_summary_auto_name(s, data)
  list(s = s)
}

# The fields a row of this type cannot do without.
lane_required_fields <- function(type, s) {
  switch(type,
    simple = if (!identical(summarize_chr1(s$func) %||% "count", "count")) {
      "col"
    },
    dist = "col",
    field = "col",
    series = c("x", "col"),
    spans = c("x", "xend"),
    pair = c("from", "to"),
    expr = "expr",
    custom = "fn"
  )
}

# The distribution glyph's three axes. `show` is sugar: it seeds the style and,
# for "pointrange", the absent outer range. An explicit `style` / `inner` /
# `outer` wins.
lane_norm_dist <- function(s, show_given) {
  style <- summarize_chr1(s$style) %||%
    if (identical(show_given, "box")) "box" else "dot"
  if (!style %in% LANE_DIST_STYLES) style <- "dot"
  inner <- summarize_chr1(s$inner) %||% summarize_chr1(s$stat) %||%
    "median_q1_q3"
  if (!inner %in% c("none", LANE_STATS)) inner <- "median_q1_q3"
  outer <- summarize_chr1(s$outer) %||% summarize_chr1(s$whiskers) %||%
    if (identical(show_given, "pointrange")) "none" else "tukey"
  if (!outer %in% c("none", LANE_WHISKERS)) outer <- "tukey"
  s$style <- style
  s$inner <- inner
  s$outer <- outer
  # Legacy mirrors, read by the gear, tooltips and saved-state round trips.
  s$stat <- if (identical(inner, "none")) NULL else inner
  s$whiskers <- if (identical(outer, "none")) NULL else outer
  s$show <- if (identical(style, "box")) "box" else "pointrange"
  s
}

# The series row's reference (pooled lines/bands, computed in R); "none" = off.
lane_norm_series <- function(s) {
  ref <- summarize_chr1(s$ref) %||% "none"
  if (!ref %in% c("none", "mean", "mean_sd", "median_iqr")) ref <- "none"
  s$ref <- ref
  s
}

# The spans row's event identity: `label` headlines each segment's tooltip,
# `fields` add columns, `size` opts into the tall exhibit form.
lane_norm_spans <- function(s) {
  s$fields <- as.character(s$fields %||% character())
  size <- summarize_chr1(s$size) %||% "md"
  if (!size %in% c("md", "lg")) size <- "md"
  s$size <- size
  s$parent <- if (is.null(s$parent)) TRUE else isTRUE(s$parent)
  s
}

# The pair row: the two functions, an optional band (`lo` / `hi`, number or
# column), reference line (`ref`, a number) and `dash` column whose non-first
# levels draw dashed.
lane_norm_pair <- function(s) {
  ff <- summarize_chr1(s$from_func) %||% "identity"
  if (!ff %in% LANE_PAIR_FUNCS) ff <- "identity"
  tf <- summarize_chr1(s$to_func) %||% "max"
  if (!tf %in% LANE_PAIR_FUNCS) tf <- "max"
  s$from_func <- ff
  s$to_func <- tf
  s$lo <- lane_pair_bound(s$lo)
  s$hi <- lane_pair_bound(s$hi)
  ref <- suppressWarnings(as.numeric(s$ref %||% NA_real_))[1L]
  s$ref <- if (is.finite(ref)) ref
  s$dash <- summarize_chr1(s$dash)
  s
}

#' Does this summary draw a colour SPLIT inside its cell? Dist glyphs, simple
#' dots and pairs draw one mark per level. A simple bar draws stacked segments
#' (`barsplit`), since colour on a magnitude is its composition; with one level
#' per group it degenerates to a single bar in that level's colour. Numbers and
#' text have nothing to colour. lane_summary_plan() decides the shape.
#' @noRd
lane_takes_color <- function(s) {
  if (identical(s$type, "dist")) return(!identical(s$show, "text"))
  if (identical(s$type, "pair")) return(TRUE)
  identical(s$type, "simple") && s$show %in% c("dot", "bar")
}

#' Every summary a `color` mapping means something for: the ones that split
#' their cell, plus the swimlane, whose segments are coloured by the event
#' attribute rather than split into lanes.
#' @noRd
lane_color_capable <- function(s) {
  lane_takes_color(s) || identical(s$type, "spans")
}

#' A pair row's band bound: a finite number, a column name, or NULL. Numbers
#' may arrive as strings from the gear's text input.
#' @noRd
lane_pair_bound <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  x <- x[[1L]]
  if (is.numeric(x)) return(if (is.finite(x)) as.numeric(x))
  x <- trimws(as.character(x))
  if (is.na(x) || !nzchar(x)) return(NULL)
  num <- suppressWarnings(as.numeric(x))
  if (is.finite(num)) num else x
}

#' Fan the legacy table-level colour and facet down onto the rows: the colour
#' onto every column that can carry one, the facet onto every column whose
#' legacy `scope` was "cell". A row that names its own keeps it.
#' @noRd
lane_migrate_globals <- function(summaries, color = NULL, facet = NULL) {
  if (is.null(color) && is.null(facet)) return(summaries)
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    if (!is.null(color) && is.null(s$color) && lane_color_capable(s)) {
      s$color <- color
    }
    if (!is.null(facet) && is.null(s$facet) && !identical(s$type, "field") &&
          !identical(s$scope, "pooled")) {
      s$facet <- facet
    }
    summaries[[i]] <- s
  }
  summaries
}

#' The statistic that supplies a distribution glyph's CENTRE: the inner range's,
#' else the outer range's (every LANE_STATS entry is a centre plus bounds), else
#' the median. A centre is never absent.
#' @noRd
lane_dist_centre_stat <- function(s) {
  inner <- summarize_chr1(s$inner) %||% summarize_chr1(s$stat)
  if (!is.null(inner) && !identical(inner, "none")) return(inner)
  outer <- summarize_chr1(s$outer)
  if (!is.null(outer) && !identical(outer, "none") &&
        outer %in% LANE_STATS) {
    return(outer)
  }
  "median_q1_q3"
}

# The header of an unnamed column: the variable label where the data column
# has one ("Mean Start Day", not "Mean ASTDY"), else its name.
#' @noRd
lane_summary_auto_name <- function(s, data = NULL) {
  nm <- function(col) {
    col <- summarize_chr1(col)
    if (is.null(col) || !is.data.frame(data) || !col %in% names(data)) {
      return(col)
    }
    summarize_col_name(data, col)
  }
  switch(s$type,
    simple = {
      f <- summarize_chr1(s$func) %||% "count"
      if (identical(f, "count")) {
        "Rows"
      } else if (identical(f, "identity")) {
        nm(s$col) %||% "Value"
      } else {
        w <- unname(AGG_WORDS[f])
        paste0(if (is.na(w)) f else w, " ", nm(s$col))
      }
    },
    dist = nm(s$col),
    field = nm(s$col),
    series = nm(s$col),
    spans = paste0(nm(s$x), " \u2192 ", nm(s$xend)),
    pair = paste0(nm(s$from), " \u2192 ", nm(s$to)),
    expr = "Value",
    custom = "Value",
    "Value"
  )
}

# Fold cap for field cells: join up to LANE_FIELD_JOIN distinct values, then
# "+n more"; past LANE_FIELD_MANY say how many. Never first(): an arbitrary
# representative presented as data is a silent lie.
LANE_FIELD_JOIN <- 3L
LANE_FIELD_MANY <- 8L

#' @noRd
lane_field_join <- function(x) {
  u <- unique(as.character(x))
  u <- u[!is.na(u) & nzchar(u)]
  n <- length(u)
  if (n == 0L) return("")
  if (n > LANE_FIELD_MANY) return(paste0(n, " values"))
  if (n > LANE_FIELD_JOIN) {
    return(paste0(paste(u[seq_len(LANE_FIELD_JOIN)], collapse = ", "),
                  ", +", n - LANE_FIELD_JOIN, " more"))
  }
  paste(u, collapse = ", ")
}
