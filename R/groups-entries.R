# Summary entries: the types and their marks, and each entry normalized
# under the spec's keys (R/spec-table-summaries.R) before the engine reads it.

# The summary types, their keys, allowed values and defaults are the spec's
# (table_summary_specs(), R/spec-table-summaries.R), read once per process.
groups_summary_specs <- memoise0(function() table_summary_specs())

# The mark of a type whose spec has no `show`: it draws one. `dot` (simple) is
# a single value as a positioned point on the zero-based lane. A dist's `show`
# is sugar over `style`, `summary` and `whiskers` (GROUPS_DIST_STYLES).
GROUPS_ONE_MARK <- c(field = "text", sparkline = "sparkline",
                     spans = "interval", pair = "dumbbell", expr = "text")

# A distribution glyph is ONE mark: a centre, an optional inner range and an
# optional outer range, drawn in a `style`:
#   "dot"  outer as a pale fence band (the mark's own rail), inner as a thick
#          bar, centre as a ringed dot. The default.
#   "box"  outer as a capped whisker, inner as a rectangle, centre as a tick.
# Both ranges take "none": an IQR bar is a box with no outer, a plain
# pointrange a dot with no outer, a bare dot a centre alone. The statistics are
# GROUPS_STATS / GROUPS_WHISKERS.
GROUPS_DIST_STYLES <- c("dot", "box")

# Ceiling on a mapped dimension's levels, the chart block's (MAX_COLOR_LEVELS
# in chart/model-common.js). In a facet a level is a COLUMN, so a wide table is
# no reading either. Pointing either at a subject id must fail loudly, naming
# the column.
GROUPS_MAX_LEVELS <- 15L

# A tile column is a matrix: an AE heatmap lays many terms side by side as
# square cells, so its facet takes more levels. The cap still stops a facet on
# a subject id.
GROUPS_MAX_TILE_LEVELS <- 300L

# The colour split's `position` (the spec's values) is drawn as the mark's
# mode.
GROUPS_POSITIONS <- c(stack = "stacked", dodge = "grouped", fill = "percent")

#' Normalize a summaries list: types, required fields, `show` within the type's
#' set, `color` / `facet`, an auto `name`. Returns the list, or `list(err =)`
#' naming the first broken row.
#' @noRd
groups_norm_summaries <- function(summaries, data = NULL) {
  if (!is.list(summaries)) return(list(err = "`summaries` must be a list"))
  out <- vector("list", length(summaries))
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    if (!is.list(s)) {
      return(list(err = paste0("Summary ", i, " is not a list")))
    }
    r <- groups_norm_row(s, i, data)
    if (!is.null(r$err)) return(r)
    out[[i]] <- r$s
  }
  out
}

#' Normalize one summary row. Returns `list(s =)` or `list(err =)`.
#' @noRd
groups_norm_row <- function(s, i, data = NULL) {
  # An unset field is ABSENT. A typed tool call delivers the array as a table,
  # so unused fields arrive as NA, which a per-type branch would read as a
  # value (a pair row threw "invalid 'times'").
  s <- s[!vapply(s, function(v) {
    is.null(v) || (is.atomic(v) && length(v) == 1L && is.na(v))
  }, logical(1L))]
  type <- chr_state(s$type) %||% "simple"
  if (!type %in% names(groups_summary_specs())) {
    return(list(err = paste0("Summary ", i, ": unknown type \"", type,
                             "\"")))
  }
  show_given <- chr_state(s$show)
  r <- groups_norm_enums(s, type, i)
  if (!is.null(r$err)) return(r)
  s <- r$s
  show <- if (type %in% names(GROUPS_ONE_MARK)) {
    GROUPS_ONE_MARK[[type]]
  } else {
    s$show
  }
  scope <- chr_state(s$scope) %||% "cell"
  if (!scope %in% c("cell", "pooled")) scope <- "cell"
  # Fields are group facts: always pooled (they stand outside faceting).
  if (identical(type, "field")) scope <- "pooled"

  for (nm in groups_required_fields(type, s)) {
    if (is.null(chr_state(s[[nm]]))) {
      return(list(err = paste0("Summary ", i, " (", type, "): `", nm,
                               "` is required")))
    }
  }

  s$type <- type
  s$show <- show
  # The tile: a square cell painted by the worst level of `fill`, else by the
  # value. Facet levels order by row count by default (commonest terms first).
  if (identical(type, "simple") && identical(show, "tile")) {
    s$fill <- chr_state(s[["fill"]])
    fo <- chr_state(s[["facet_order"]]) %||% "count"
    s$facet_order <- if (fo %in% c("count", "level")) fo else "count"
  }
  # A custom row's mark is read off its function unless one is named.
  if (identical(type, "custom")) s$show <- show_given
  s$scope <- scope
  # Colour and facet are the summary's OWN optional mappings. `scope` survives
  # only to seed `facet` for boards saved under the table-level pair
  # (groups_migrate_globals()).
  s$color <- chr_state(s$color)
  # Fields are group facts: outside faceting.
  s$facet <- if (identical(type, "field")) NULL else chr_state(s$facet)
  if (identical(type, "dist") && !identical(show, "text")) {
    s <- groups_norm_dist(s, show_given)
  }
  if (identical(type, "spans")) s <- groups_norm_spans(s)
  if (identical(type, "pair")) s <- groups_norm_pair(s)
  s$name <- chr_state(s$name) %||% groups_summary_auto_name(s, data)
  list(s = s)
}

# The type's enum keys against its spec: an unknown value is an error, as in
# the spec check; an absent one takes the spec's default. Returns `list(s =)`
# or `list(err =)`.
groups_norm_enums <- function(s, type, i) {
  args <- groups_summary_specs()[[type]]$args
  for (nm in names(args)) {
    a <- args[[nm]]
    if (!identical(a$type$name, "enum")) next
    v <- chr_state(s[[nm]])
    if (is.null(v)) {
      s[[nm]] <- a$default
    } else if (!v %in% a$values) {
      return(list(err = paste0("Summary ", i, " (", type, "): unknown `", nm,
                               "` \"", v, "\"")))
    }
  }
  list(s = s)
}

# The fields a row of this type cannot do without.
groups_required_fields <- function(type, s) {
  switch(type,
    simple = if (!identical(chr_state(s$func) %||% "count", "count")) {
      "col"
    },
    dist = "col",
    field = "col",
    sparkline = c("x", "col"),
    spans = c("x", "xend"),
    pair = c("x", "xend"),
    expr = "expr",
    custom = "fn"
  )
}

# The distribution glyph's three axes. `show` is sugar: it seeds the style and,
# for "pointrange", the absent outer range. An explicit `style` / `inner` /
# `outer` wins.
groups_norm_dist <- function(s, show_given) {
  style <- chr_state(s$style) %||%
    if (identical(show_given, "box")) "box" else "dot"
  if (!style %in% GROUPS_DIST_STYLES) style <- "dot"
  # The spec leaves `whiskers` unset: Tukey on a box, none on a dot range.
  s$whiskers <- s$whiskers %||%
    if (identical(show_given, "pointrange")) "none" else "tukey"
  s$style <- style
  s$show <- if (identical(style, "box")) "box" else "pointrange"
  s
}

# The spans row's event identity: `label` headlines each segment's tooltip,
# `tooltip` adds columns.
groups_norm_spans <- function(s) {
  s$tooltip <- as.character(s$tooltip %||% character())
  s$parent <- if (is.null(s$parent)) TRUE else isTRUE(s$parent)
  for (nm in GROUPS_SPANS_OVERLAYS) s[[nm]] <- chr_state(s[[nm]])
  s
}

# The swimlane's optional overlays: ticks, the line (band_x / band_xend),
# point events (event_x / event_shape) and the open end. Each reads only the
# rows where its own columns are set, so they share one long frame with the
# segment rows.
GROUPS_SPANS_OVERLAYS <- c("ticks", "band_x", "band_xend", "event_x",
                           "event_shape", "open")

# The pair row: two summaries of one group drawn as a segment, a hollow
# diamond at `x` and a dot at `xend`, each read by its function (`x_func`,
# `xend_func`). An optional band (`ref_min` / `ref_max`, number or column),
# one reference line (`x_lines`, a number) and a `linetype` column whose
# non-first levels draw dashed.
groups_norm_pair <- function(s) {
  s$ref_min <- groups_pair_bound(s$ref_min)
  s$ref_max <- groups_pair_bound(s$ref_max)
  ref <- suppressWarnings(as.numeric(unlist(s$x_lines) %||% NA_real_))[1L]
  s$x_lines <- if (is.finite(ref)) ref
  s$linetype <- chr_state(s$linetype)
  s
}

#' Does this summary draw a colour SPLIT inside its cell? Dist glyphs, simple
#' dots and pairs draw one mark per level. A simple bar draws stacked segments
#' (`barsplit`), since colour on a magnitude is its composition; with one level
#' per group it degenerates to a single bar in that level's colour. Numbers and
#' text have nothing to colour. groups_summary_plan() decides the shape.
#' @noRd
groups_takes_color <- function(s) {
  if (identical(s$type, "dist")) return(!identical(s$show, "text"))
  if (identical(s$type, "pair")) return(TRUE)
  identical(s$type, "simple") && s$show %in% c("dot", "bar")
}

#' Every summary a `color` mapping means something for: the ones that split
#' their cell, plus the swimlane, whose segments are coloured by the event
#' attribute rather than split into lanes.
#' @noRd
groups_color_capable <- function(s) {
  groups_takes_color(s) || identical(s$type, "spans")
}

#' A pair row's band bound: a finite number, a column name, or NULL. Numbers
#' may arrive as strings from the gear's text input.
#' @noRd
groups_pair_bound <- function(x) {
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
groups_migrate_globals <- function(summaries, color = NULL, facet = NULL) {
  if (is.null(color) && is.null(facet)) return(summaries)
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    if (!is.null(color) && is.null(s$color) && groups_color_capable(s)) {
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
#' else the outer range's (every GROUPS_STATS entry is a centre plus bounds), else
#' the median. A centre is never absent.
#' @noRd
groups_dist_centre_stat <- function(s) {
  inner <- chr_state(s$summary)
  if (!is.null(inner) && !identical(inner, "none")) return(inner)
  outer <- chr_state(s$whiskers)
  if (!is.null(outer) && !identical(outer, "none") &&
        outer %in% GROUPS_STATS) {
    return(outer)
  }
  "median_q1_q3"
}

# The header of an unnamed column: the variable label where the data column
# has one ("Mean Start Day", not "Mean ASTDY"), else its name.
#' @noRd
groups_summary_auto_name <- function(s, data = NULL) {
  nm <- function(col) {
    col <- chr_state(col)
    if (is.null(col) || !is.data.frame(data) || !col %in% names(data)) {
      return(col)
    }
    groups_col_name(data, col)
  }
  switch(s$type,
    simple = {
      f <- chr_state(s$func) %||% "count"
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
    sparkline = nm(s$col),
    spans = paste0(nm(s$x), " \u2192 ", nm(s$xend)),
    pair = paste0(nm(s$x), " \u2192 ", nm(s$xend)),
    expr = "Value",
    custom = "Value",
    "Value"
  )
}

# Fold cap for field cells: join up to GROUPS_FIELD_JOIN distinct values, then
# "+n more"; past GROUPS_FIELD_MANY say how many. Never first(): an arbitrary
# representative presented as data is a silent lie.
GROUPS_FIELD_JOIN <- 3L
GROUPS_FIELD_MANY <- 8L

#' @noRd
groups_field_join <- function(x) {
  u <- unique(as.character(x))
  u <- u[!is.na(u) & nzchar(u)]
  n <- length(u)
  if (n == 0L) return("")
  if (n > GROUPS_FIELD_MANY) return(paste0(n, " values"))
  if (n > GROUPS_FIELD_JOIN) {
    return(paste0(paste(u[seq_len(GROUPS_FIELD_JOIN)], collapse = ", "),
                  ", +", n - GROUPS_FIELD_JOIN, " more"))
  }
  paste(u, collapse = ", ")
}
