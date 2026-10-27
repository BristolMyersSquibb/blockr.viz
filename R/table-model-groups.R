# Table model, groups layout: summaries vocabulary and normalizer --------------
#
# The row types and their displays, the public and internal summary keys, and
# the normalizer that turns a saved `summaries` list into the rows the
# preparer walks (table-model-groups-prepare.R). Pure compute, no Shiny.
#
# The generic column-list preparer is the lane chart's deeper model, per
# _blockr.design/open/summarize-table/: one row per group, each column a
# summarising function over the group's rows, rendered as text or as a glyph.
# `summaries` is an ordered list of typed row objects (the blockr.dplyr
# summarize block's schema plus a display axis). Every row type maps onto an
# EXISTING plan kind (bar / box / pointrange / interval / sparkline /
# num), so the emitters and the R/JS drift guard are untouched.

# Row types and the displays each allows. The first display is the type's
# default. "dot" is a single value as a positioned point on the zero-based
# lane -- less ink than a bar when magnitude comparison is not the point.
#
# `dist` keeps its legacy `show` values because saved boards carry them, but
# they are now SUGAR over the real axes (`style` / `inner` / `outer`) --
# see LANE_DIST_STYLES.
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

# The pair row: two summaries of the same group drawn as one segment, a
# hollow diamond at `from` and a dot at `to` (baseline to peak, first to last
# visit). `from_func` / `to_func` use the simple row's function vocabulary;
# the defaults read one value per subject as is, then its maximum.
LANE_PAIR_FUNCS <- c("identity", "mean", "median", "min", "max")

# A distribution glyph is ONE mark: a centre, an optional inner range and an
# optional outer range, drawn in a style. Box plot and dot range are two
# values of `style` over the same three numbers, not two marks
# (_blockr.design/open/summarize-table/mock-box/index.html, sections A-C).
#
#   style = "dot"  outer as a pale rounded fence band, inner as a thick
#                  rounded bar, centre as a ringed dot. The default: the band
#                  is the mark's own rail, so the cell needs no ground.
#   style = "box"  outer as a capped whisker, inner as a rectangle, centre as
#                  a tick across it. The echarts read, for a reader who wants
#                  a box.
#
# Both ranges take "none", which is where the degenerate family lives: an IQR
# bar is a box with no outer, a plain pointrange is a dot with no outer, a
# bare dot is a centre on its own. The statistic vocabulary is unchanged
# (LANE_STATS / LANE_WHISKERS in table-model-groups-stats.R), so mean/CI/SD and
# median/IQR/fences are the same glyph with different inputs.
LANE_DIST_STYLES <- c("dot", "box")

# The ceiling on a mapped dimension's levels, the chart block's exactly
# (MAX_COLOR_LEVELS in chart/model-common.js): a palette has about seven
# readable colours and fifteen is the hard stop. Facet shares it for a
# different reason -- a level is a COLUMN there, and a table sixty columns
# wide is not a reading. Colour and facet are per column now, so pointing
# either at a subject id is one click away; it must fail loudly, naming the
# column, not render two hundred glyphs per cell.
LANE_MAX_LEVELS <- 15L

# A tile column is a matrix, not a reading of fifteen glyphs: an AE heatmap
# lays twenty-five terms (or all of them) side by side as square cells, so its
# facet takes many more levels. The cap still stops a facet on a subject id.
LANE_MAX_TILE_LEVELS <- 300L

# --- public and internal summary keys ----------------------------------------
#
# A summary entry is saved, edited in the gear and documented under its
# PUBLIC keys, the chart block's vocabulary (dev/table-block-rewrite.md):
#
#   dist   summary, whiskers     (the inner and outer range of the glyph;
#                                 `summary` is also a text column's statistic)
#   pair   x, xend, x_func, xend_func, ref_lo, ref_hi
#   spans  tooltip               (extra tooltip columns)
#   simple position              (a colour split: "stack", "dodge", "fill")
#
# The engine below reads its own INTERNAL names (inner / outer / stat, from /
# to / from_func / to_func / lo / hi, fields), as it always has. One pair of
# functions crosses between the two, so a saved board, the gear and the code
# here each see the names they were written against.

LANE_PUBLIC_KEYS <- list(
  pair = c(x = "from", xend = "to", x_func = "from_func",
           xend_func = "to_func", ref_lo = "lo", ref_hi = "hi"),
  spans = c(tooltip = "fields")
)

LANE_POSITIONS <- c(stack = "stacked", dodge = "grouped", fill = "percent")

#' A summary entry under its public keys: what state and the gear hold.
#' Entries written with the old keys (boards saved before the table rewrite,
#' code calling the summarize table) come out the same.
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

#' A summary entry under the engine's internal keys. Run first thing in
#' lane_norm_summaries(); internal names already present win.
#' @noRd
lane_internal_summary <- function(s) {
  if (!is.list(s)) return(s)
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

#' Normalize a summaries list: known types, per-type required fields,
#' `show` within the type's set, the optional `color` / `facet` mappings,
#' an auto `name`. Returns the normalized list, or `list(err =)` naming the
#' first broken row (a config error must say which row, not throw a stack
#' trace).
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
  # An unset field is an ABSENT field. A typed tool call (the assistant, an
  # MCP client) delivers the array as a table, so every field a row does not
  # use arrives as NA rather than missing, and an NA reaching a per-type
  # branch below is read as a value: a pair row threw "invalid 'times'".
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
  # The colour split's layout, the chart's `position`: stacked segments,
  # bars side by side, or each row as 100%. Unset = decided by the measure.
  pos <- summarize_chr1(s[["position"]])
  s$.mode <- if (!is.null(pos) && pos %in% names(LANE_POSITIONS)) {
    LANE_POSITIONS[[pos]]
  } else if (!is.null(pos) && pos %in% LANE_POSITIONS) {
    pos
  }
  # The tile: a square cell showing the value, painted by the worst level
  # of `fill` (or, without one, by the value itself). Its facet levels
  # order by row count unless asked otherwise: an AE heatmap reads the
  # commonest terms first.
  if (identical(type, "simple") && identical(show, "tile")) {
    s$fill <- summarize_chr1(s[["fill"]])
    fo <- summarize_chr1(s[["facet_order"]]) %||% "count"
    s$facet_order <- if (fo %in% c("count", "level")) fo else "count"
  }
  # A custom row's mark is read off what its function returns unless one
  # is named (lane_custom_setup()).
  if (identical(type, "custom")) s$show <- show_given
  s$scope <- scope
  # Colour and facet are the summary's OWN optional mappings: absent means
  # the column carries no such dimension, and two columns may map different
  # ones. `scope` survives normalization only to seed `facet` when a board
  # saved under the table-level pair opens (lane_migrate_globals()).
  s$color <- summarize_chr1(s$color)
  # Fields are group facts: they stand outside faceting, as they always did.
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

# The distribution glyph's three axes. `show` is legacy sugar: it seeds
# the style and, for "pointrange", the absent outer range -- so a saved
# board restores byte-identically while a new one configures the axes
# directly. An explicit `style` / `inner` / `outer` always wins.
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
  # Legacy mirrors, so the gear, the tooltips and any saved-state
  # round-trip keep reading the fields they always read.
  s$stat <- if (identical(inner, "none")) NULL else inner
  s$whiskers <- if (identical(outer, "none")) NULL else outer
  s$show <- if (identical(style, "box")) "box" else "pointrange"
  s
}

# The series row's computed reference: pooled orientation lines/bands,
# computed in R (never client-side). "none" = off.
lane_norm_series <- function(s) {
  ref <- summarize_chr1(s$ref) %||% "none"
  if (!ref %in% c("none", "mean", "mean_sd", "median_iqr")) ref <- "none"
  s$ref <- ref
  s
}

# The spans row's event identity: `label` headlines each segment's
# tooltip (the chart gantt's label role), `fields` append extra columns,
# `size` opts the lane into the tall exhibit form.
lane_norm_spans <- function(s) {
  s$fields <- as.character(s$fields %||% character())
  size <- summarize_chr1(s$size) %||% "md"
  if (!size %in% c("md", "lg")) size <- "md"
  s$size <- size
  s$parent <- if (is.null(s$parent)) TRUE else isTRUE(s$parent)
  s
}

# The pair row: the two functions, an optional band (`lo` / `hi`, each a
# number or a column), an optional reference line (`ref`, a number) and
# an optional `dash` column whose non-first levels draw dashed.
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

#' Does this summary draw a colour SPLIT inside its cell? Everything that
#' draws a value does -- what CHANGES is the shape the split takes, which
#' lane_summary_plan() decides:
#'
#'   dist (glyph), simple dot  one mark per level in the cell, on the
#'                             column's one scale
#'   simple bar                stacked segments (`barsplit`), because colour
#'                             on a magnitude is its composition: the bar
#'                             still means the group's total, and when a
#'                             group has rows of ONE level (a table by
#'                             subject) it degenerates to a single bar in
#'                             that level's colour, which is the answer
#'                             anyone looking at it expects
#'   pair                      one dumbbell per level, like the glyph: a
#'                             group of one level (a table by subject) draws
#'                             a single dumbbell in that level's colour
#'
#' A number and a text cell have nothing to colour.
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

#' Boards saved before colour and facet moved onto the summary carry them as
#' ONE table-level pair. Fan them down onto the rows they used to apply to,
#' so an old board opens on exactly what it drew: the colour onto every
#' column that could carry one, the facet onto every column whose legacy
#' `scope` was "cell" (the others were the "Overall" columns, which is now
#' simply a row with no facet). A row that names its own keeps it.
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

#' The statistic that supplies a distribution glyph's CENTRE. The inner range
#' names it when there is one; with the inner range off the outer range's
#' centre stands in (every LANE_STATS entry is a centre plus a pair of
#' bounds), and with both off the median. A centre is never absent: a
#' distribution cell that shows nothing is not a column.
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

# The header of an unnamed column. A data column reads as its variable
# label where it has one ("Mean Start Day", not "Mean ASTDY"), else its
# name, as the single ranked bar headed its column.
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
      # As is: the column is the value, so it names the column.
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

# The fold cap for field cells: join up to LANE_FIELD_JOIN distinct values,
# then "+n more"; past LANE_FIELD_MANY just say how many. Never first():
# an arbitrary representative presented as data is a silent lie, and the
# join makes a broken constancy assumption visible ("Placebo, Active").
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
