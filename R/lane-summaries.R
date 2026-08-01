# Summarize table: the generic column-list preparer ---------------------------
#
# The lane chart's deeper model, per _blockr.design/open/summarize-table/:
# one row per group, each column a summarising function over the group's
# rows, rendered as text or as a glyph. `summaries` is an ordered list of
# typed row objects (the blockr.dplyr summarize block's schema plus a
# display axis); this file normalizes it and prepares the same prep
# contract the renderer already walks. Every row type maps onto an
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
  simple = c("bar", "number", "dot"),
  dist = c("pointrange", "box", "text"),
  field = "text",
  series = "sparkline",
  spans = "interval",
  expr = "text"
)

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
# (LANE_STATS / LANE_WHISKERS in lane-stats.R), so mean/CI/SD and
# median/IQR/fences are the same glyph with different inputs.
LANE_DIST_STYLES <- c("dot", "box")

# The ceiling on a mapped dimension's levels, the chart block's exactly
# (MAX_COLOR_LEVELS in inst/js/chart.js): a palette has about seven readable
# colours and fifteen is the hard stop. Facet shares it for a different
# reason -- a level is a COLUMN there, and a table sixty columns wide is not
# a reading. Colour and facet are per column now, so pointing either at a
# subject id is one click away; it must fail loudly, naming the column, not
# render two hundred glyphs per cell.
LANE_MAX_LEVELS <- 15L

#' Normalize a summaries list: known types, per-type required fields,
#' `show` within the type's set, the optional `color` / `facet` mappings,
#' an auto `name`. Returns the normalized list, or `list(err =)` naming the
#' first broken row (a config error must say which row, not throw a stack
#' trace).
#' @noRd
lane_norm_summaries <- function(summaries) {
  if (!is.list(summaries)) return(list(err = "`summaries` must be a list"))
  out <- vector("list", length(summaries))
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    if (!is.list(s)) {
      return(list(err = paste0("Summary ", i, " is not a list")))
    }
    type <- rank_chr1(s$type) %||% "simple"
    if (!type %in% names(LANE_ROW_TYPES)) {
      return(list(err = paste0("Summary ", i, ": unknown type \"", type,
                               "\"")))
    }
    shows <- LANE_ROW_TYPES[[type]]
    show_given <- rank_chr1(s$show)
    show <- show_given %||% shows[[1L]]
    if (!show %in% shows) show <- shows[[1L]]
    scope <- rank_chr1(s$scope) %||% "cell"
    if (!scope %in% c("cell", "pooled")) scope <- "cell"
    # Fields are group facts: always pooled (they stand outside faceting).
    if (identical(type, "field")) scope <- "pooled"

    need <- switch(type,
      simple = if (!identical(rank_chr1(s$func) %||% "count", "count")) "col",
      dist = "col",
      field = "col",
      series = c("x", "col"),
      spans = c("x", "xend"),
      expr = "expr"
    )
    for (nm in need) {
      if (is.null(rank_chr1(s[[nm]]))) {
        return(list(err = paste0("Summary ", i, " (", type, "): `", nm,
                                 "` is required")))
      }
    }

    s$type <- type
    s$show <- show
    s$scope <- scope
    # Colour and facet are the summary's OWN optional mappings: absent means
    # the column carries no such dimension, and two columns may map different
    # ones. `scope` survives normalization only to seed `facet` when a board
    # saved under the table-level pair opens (lane_migrate_globals()).
    s$color <- rank_chr1(s$color)
    # Fields are group facts: they stand outside faceting, as they always did.
    s$facet <- if (identical(type, "field")) NULL else rank_chr1(s$facet)
    # The distribution glyph's three axes. `show` is legacy sugar: it seeds
    # the style and, for "pointrange", the absent outer range -- so a saved
    # board restores byte-identically while a new one configures the axes
    # directly. An explicit `style` / `inner` / `outer` always wins.
    if (identical(type, "dist") && !identical(show, "text")) {
      style <- rank_chr1(s$style) %||%
        if (identical(show_given, "box")) "box" else "dot"
      if (!style %in% LANE_DIST_STYLES) style <- "dot"
      inner <- rank_chr1(s$inner) %||% rank_chr1(s$stat) %||% "median_q1_q3"
      if (!inner %in% c("none", LANE_STATS)) inner <- "median_q1_q3"
      outer <- rank_chr1(s$outer) %||% rank_chr1(s$whiskers) %||%
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
    }
    # The series row's computed reference: pooled orientation lines/bands,
    # computed in R (never client-side). "none" = off.
    if (identical(type, "series")) {
      ref <- rank_chr1(s$ref) %||% "none"
      if (!ref %in% c("none", "mean", "mean_sd", "median_iqr")) ref <- "none"
      s$ref <- ref
    }
    # The spans row's event identity: `label` headlines each segment's
    # tooltip (the chart gantt's label role), `fields` append extra columns,
    # `size` opts the lane into the tall exhibit form.
    if (identical(type, "spans")) {
      s$fields <- as.character(s$fields %||% character())
      size <- rank_chr1(s$size) %||% "md"
      if (!size %in% c("md", "lg")) size <- "md"
      s$size <- size
    }
    s$name <- rank_chr1(s$name) %||% lane_summary_auto_name(s)
    out[[i]] <- s
  }
  out
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
#'
#' A number and a text cell have nothing to colour.
#' @noRd
lane_takes_color <- function(s) {
  if (identical(s$type, "dist")) return(!identical(s$show, "text"))
  identical(s$type, "simple") && s$show %in% c("dot", "bar")
}

#' Every summary a `color` mapping means something for: the ones that split
#' their cell, plus the swimlane, whose segments are coloured by the event
#' attribute rather than split into lanes.
#' @noRd
lane_color_capable <- function(s) {
  lane_takes_color(s) || identical(s$type, "spans")
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
  inner <- rank_chr1(s$inner) %||% rank_chr1(s$stat)
  if (!is.null(inner) && !identical(inner, "none")) return(inner)
  outer <- rank_chr1(s$outer)
  if (!is.null(outer) && !identical(outer, "none") &&
        outer %in% LANE_STATS) {
    return(outer)
  }
  "median_q1_q3"
}

#' @noRd
lane_summary_auto_name <- function(s) {
  switch(s$type,
    simple = {
      f <- rank_chr1(s$func) %||% "count"
      if (identical(f, "count")) "Rows" else
        paste0(AGG_WORDS[[f]] %||% f, " ", rank_chr1(s$col))
    },
    dist = rank_chr1(s$col),
    field = rank_chr1(s$col),
    series = rank_chr1(s$col),
    spans = paste0(rank_chr1(s$x), " \u2192 ", rank_chr1(s$xend)),
    expr = "Value",
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

#' The generic preparer: group_by(by) plus the summaries list.
#'
#' `by` is outer -> inner; the renderer nests one level, so at most two
#' columns (the outer becomes the expandable parent).
#'
#' Colour and facet are the SUMMARY's mappings, not the table's: a column
#' carries a colour split, or repeats per level of a facet column, or
#' neither, and two columns may map different ones. A summary that names a
#' facet repeats per level (copies adjacent, by_summary layout); one that
#' does not renders once. Each plan entry carries its own domain, shared
#' across that summary's facet copies -- mixed marks with different units
#' must not share a scale.
#'
#' `facet` / `color` are the legacy table-level pair: they are fanned down
#' onto the rows (lane_migrate_globals()) and never read again.
#' @noRd
lane_prepare_summaries <- function(data, by, summaries, facet = NULL,
                                   facet_layout = "by_summary",
                                   color = NULL,
                                   sort_by = "value", sort_dir = "desc",
                                   top_n = NULL, scale_map = NULL) {
  bad <- function(msg) list(err = msg)

  by <- as.character(by %||% character())
  by <- by[nzchar(by)]
  if (!length(by)) return(bad("Pick a Group by column in the gear"))
  if (length(by) > 2L) {
    return(bad("Group by supports at most two columns (outer, inner)"))
  }
  miss <- setdiff(by, names(data))
  if (length(miss)) {
    return(bad(paste0("Mapped column not in data: ",
                      paste0("\"", miss, "\"", collapse = ", "),
                      ". Re-pick it in the gear.")))
  }
  group <- by[[length(by)]]
  parent <- if (length(by) == 2L) by[[1L]]
  keys <- c(parent, group)

  summaries <- lane_norm_summaries(summaries)
  if (!is.null(summaries$err)) return(bad(summaries$err))
  if (!length(summaries)) return(bad("Add at least one summary column"))

  present <- function(col) {
    col <- rank_chr1(col)
    if (is.null(col) || !col %in% names(data)) NULL else col
  }
  summaries <- lane_migrate_globals(summaries, present(color), present(facet))

  # Every summary's mapped columns must exist -- reported by row name.
  for (s in summaries) {
    cols <- unlist(s[intersect(names(s), c("col", "x", "xend", "band"))])
    cols <- as.character(cols)
    miss <- setdiff(cols[nzchar(cols)], names(data))
    if (length(miss)) {
      return(bad(paste0(
        "Summary \"", s$name, "\": column not in data: ",
        paste0("\"", miss, "\"", collapse = ", "), "."
      )))
    }
  }

  # --- per-summary colour and facet -----------------------------------------
  # Both optional, both the summary's own. Levels come from the FULL data,
  # never a facet slice: fills and formats must agree across a summary's
  # copies and across every row.
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    cc <- present(s$color)
    if (!is.null(cc) && lane_color_capable(s)) {
      lv <- rank_levels(data[[cc]])
      if (length(lv) > LANE_MAX_LEVELS) {
        return(bad(paste0(
          "Summary \"", s$name, "\": colour column \"", cc, "\" has ",
          length(lv), " levels. Pick one with at most ", LANE_MAX_LEVELS,
          " -- a palette has about seven readable colours."
        )))
      }
      s$color <- cc
      # A lane mark splits its cell (one glyph per level); the swimlane
      # colours its segments instead, and reads `color` in the plan.
      s$.color <- if (lane_takes_color(s)) cc
      s$.levels <- lv
    }
    fc <- present(s$facet)
    if (!is.null(fc)) {
      lv <- rank_levels(data[[fc]])
      if (length(lv) < 2L) {
        return(bad(paste0(
          "Summary \"", s$name, "\": facet column \"", fc, "\" has fewer ",
          "than two levels; nothing to compare across columns."
        )))
      }
      if (length(lv) > LANE_MAX_LEVELS) {
        return(bad(paste0(
          "Summary \"", s$name, "\": facet column \"", fc, "\" has ",
          length(lv), " levels, so the column would repeat ", length(lv),
          " times. Pick one with at most ", LANE_MAX_LEVELS, "."
        )))
      }
      s$.facet <- fc
      s$.flevels <- lv
    }
    if (s$type %in% c("series", "spans")) {
      s$.date <- inherits(data[[rank_chr1(s$x)]], "Date")
    }
    summaries[[i]] <- s
  }

  # One facet column across the table is the ordinary case: the level alone
  # labels a copy, and the by_level layout can span level groups across the
  # summaries. With SEVERAL, adjacent column groups answer different
  # questions, so each copy names its column ("SEX: F") and by_level has no
  # shared groups to span -- the layout stays by_summary.
  facet_cols <- unique(unlist(lapply(summaries, function(s) s$.facet)))
  shared_facet <- if (length(facet_cols) == 1L) facet_cols else NULL
  qualify <- length(facet_cols) > 1L
  facet_levels <- if (is.null(shared_facet)) {
    character()
  } else {
    rank_levels(data[[shared_facet]])
  }

  # --- leaf / parent skeletons ----------------------------------------------
  skel <- unique(data[keys])
  skel <- skel[do.call(order, unname(as.list(skel))), , drop = FALSE]
  leaf <- data.frame(.label = as.character(skel[[group]]),
                     .parent = if (is.null(parent)) NA_character_ else
                       as.character(skel[[parent]]),
                     stringsAsFactors = FALSE)
  for (k in keys) leaf[[k]] <- skel[[k]]
  # The data's own order for the row labels (factor levels, else
  # first-appearance): `sort_by = "data"` reads it, everything else ignores
  # it. A visit-keyed table is unreadable alphabetically.
  leaf$.ord <- rank_data_ord(data[[group]], leaf$.label)
  par_rows <- NULL
  if (!is.null(parent)) {
    pv <- rank_levels(data[[parent]])
    par_rows <- data.frame(.label = pv, .parent = NA_character_,
                           stringsAsFactors = FALSE)
    par_rows[[parent]] <- pv
    par_rows$.ord <- rank_data_ord(data[[parent]], pv)
  }

  # --- per-summary build -----------------------------------------------------
  # One builder, applied per target frame (leaf keyed by `keys`, parents
  # keyed by `parent`) and per data slice (pooled = all rows; cell = one
  # slice per facet level). Each application fills `<sid>_*` columns and
  # returns the plan-entry payload description.
  note <- NULL
  plan <- list()
  s_primary <- NULL

  fill <- function(target, tkeys, slice, s, sid) {
    switch(s$type,
      simple = {
        f <- rank_chr1(s$func) %||% "count"
        put <- function(target, slice, out) {
          agg <- rank_aggregate(slice, tkeys, f, rank_chr1(s$col),
                                rank_chr1(s$col))
          target[[out]] <- rank_match_col(target, agg, tkeys, ".v")
          target
        }
        target <- put(target, slice, paste0(sid, "_v"))
        # The pooled value above stays the sort key and the label either
        # way; the per-level values are what the split draws. The dot names
        # them for lane_color_split()'s geometry keys, the bar for the
        # barsplit emitter's `prefix + level` lookup.
        cc <- s$.color
        if (!is.null(cc)) {
          dot <- identical(s$show, "dot")
          for (j in seq_along(s$.levels)) {
            lv <- s$.levels[[j]]
            out <- if (dot) {
              paste0(sid, "_L", j, "_bc")
            } else {
              paste0(sid, "_S_", lv)
            }
            target <- put(target,
                          slice[as.character(slice[[cc]]) == lv, ,
                                drop = FALSE], out)
          }
        }
        target
      },
      dist = {
        # `.b` carries the centre and (unless inner is off) the inner range;
        # `.w` the outer range. Both styles compute both -- the style decides
        # how they are drawn, never which numbers exist.
        stats <- list(.b = lane_dist_centre_stat(s))
        if (!identical(s$outer %||% "none", "none")) {
          stats$.w <- s$outer
        }
        put <- function(target, slice, prefix) {
          agg <- lane_stat_agg(slice, tkeys, rank_chr1(s$col), stats)
          for (nm in setdiff(names(agg), tkeys)) {
            target[[paste0(prefix, "_", sub("^\\.", "", nm))]] <-
              rank_match_col(target, agg, tkeys, nm)
          }
          target
        }
        # The pooled glyph is computed either way: it is the column's sort
        # key and its fallback when no colour splits it.
        target <- put(target, slice, sid)
        # The colour dimension (chart parity: colour splits a distribution
        # into one glyph per level). Each level gets its own stat columns;
        # a level with no rows in this group stays NA and draws no lane.
        cc <- s$.color
        if (!is.null(cc)) {
          for (j in seq_along(s$.levels)) {
            lv <- s$.levels[[j]]
            target <- put(target, slice[as.character(slice[[cc]]) == lv, ,
                                        drop = FALSE],
                          paste0(sid, "_L", j))
          }
        }
        target
      },
      field = {
        g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
        agg <- as.data.frame(dplyr::summarise(
          g, .t = lane_field_join(.data[[rank_chr1(s$col)]]),
          .groups = "drop"
        ))
        m <- rank_match_field(target, agg, tkeys, ".t")
        m[is.na(m)] <- ""
        target[[paste0(sid, "_t")]] <- m
        target
      },
      expr = {
        ex <- tryCatch(rlang::parse_expr(rank_chr1(s$expr)),
                       error = function(e) NULL)
        if (is.null(ex)) {
          target[[paste0(sid, "_t")]] <- rep("(parse error)", nrow(target))
          return(target)
        }
        g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
        agg <- tryCatch(
          as.data.frame(dplyr::summarise(g, .t = !!ex, .groups = "drop")),
          error = function(e) NULL
        )
        if (is.null(agg)) {
          target[[paste0(sid, "_t")]] <- rep("(error)", nrow(target))
          return(target)
        }
        v <- agg$.t
        if (is.numeric(v)) {
          agg$.t <- as.numeric(v)
          target[[paste0(sid, "_v")]] <-
            rank_match_col(target, agg, tkeys, ".t")
        } else {
          agg$.t <- as.character(v)
          m <- rank_match_field(target, agg, tkeys, ".t")
          m[is.na(m)] <- ""
          target[[paste0(sid, "_t")]] <- m
        }
        target
      },
      series = {
        pts <- lane_series_split(slice, target, tkeys, s)
        target[[paste0(sid, "_pts")]] <- I(pts)
        target[[paste0(sid, "_last")]] <- vapply(pts, function(p1) {
          if (length(p1$y)) p1$y[[length(p1$y)]] else NA_real_
        }, numeric(1L))
        target
      },
      spans = {
        segs <- lane_spans_split(slice, target, tkeys, s)
        target[[paste0(sid, "_segs")]] <- I(segs)
        target[[paste0(sid, "_start")]] <- vapply(segs, function(ss) {
          if (length(ss)) ss[[1L]]$s else NA_real_
        }, numeric(1L))
        target[[paste0(sid, "_nseg")]] <- lengths(segs)
        target
      },
      target
    )
  }

  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    fl <- s$.flevels
    copies <- if (is.null(fl)) {
      list(list(suffix = paste0(".s", i), slice = data, level = NULL))
    } else {
      lapply(seq_along(fl), function(j) {
        lv <- fl[[j]]
        list(suffix = paste0(".s", i, "f", j),
             slice = data[as.character(data[[s$.facet]]) == lv, ,
                          drop = FALSE],
             level = lv, fcol = s$.facet, qualify = qualify)
      })
    }
    for (cp in copies) {
      leaf <- fill(leaf, keys, cp$slice, s, cp$suffix)
      if (!is.null(par_rows)) {
        par_rows <- fill(par_rows, parent, cp$slice, s, cp$suffix)
      }
      # The series reference is computed per COPY over the slice's values:
      # every sparkline in the column is oriented against the same line
      # (per facet level, its own level's line).
      if (identical(s$type, "series") &&
            !identical(s$ref %||% "none", "none")) {
        ys <- as.numeric(cp$slice[[rank_chr1(s$col)]])
        ys <- ys[is.finite(ys)]
        cp$ref <- switch(s$ref,
          mean = list(center = mean(ys)),
          mean_sd = lane_summarize(ys, "mean_sd"),
          median_iqr = lane_summarize(ys, "median_q1_q3")
        )
      }
      plan <- c(plan, list(lane_summary_plan(s, cp, data, scale_map)))
    }
    if (is.null(s_primary)) s_primary <- list(s = s, sid = copies[[1L]]$suffix)
  }

  # `.v` = the FIRST summary's primary value (its sort key): rank order
  # follows the leading column unless sort_by picks another.
  primary_col <- lane_primary_col(s_primary$s, s_primary$sid)
  leaf$.v <- lane_primary_value(leaf, s_primary$s, primary_col)
  if (!is.null(par_rows)) {
    par_rows$.v <- lane_primary_value(par_rows, s_primary$s, primary_col)
  }

  srt <- rank_resolve_sort(sort_by, plan, data, leaf, par_rows, group, parent)
  asm <- rank_assemble_rows(srt$leaf, srt$par_rows, parent, srt$key, sort_dir,
                            top_n)
  rows <- asm$rows

  # --- per-entry domains ------------------------------------------------------
  # Computed over the ASSEMBLED rows (parents included) and shared across a
  # summary's facet copies: one scale per summary, never one scale per table.
  plan <- lane_summary_domains(plan, rows)

  # --- facet layout -----------------------------------------------------------
  # by_summary (default): each summary's level copies sit adjacent, in
  # authored order (adjacency is the comparison affordance). by_level: the
  # Table-1 reading -- unfaceted columns and fields lead, then one column
  # group per facet level spanning the summaries; the header grows a
  # spanning row (rank_thead) and each copy is re-labelled by its SUMMARY
  # (the level moves up into the group header). That reading needs ONE facet
  # column for the whole table (`facet_levels` is empty otherwise), because
  # a group header spanning "F | M" and "Placebo | Active" is not a span.
  facet_spans <- NULL
  has_cell <- any(vapply(plan, function(p) !is.null(p$flevel), logical(1L)))
  if (identical(rank_chr1(facet_layout), "by_level") &&
        length(facet_levels) && has_cell) {
    lead <- Filter(function(p) is.null(p$flevel), plan)
    groups <- lapply(facet_levels, function(lv) {
      Filter(function(p) identical(p$flevel, lv), plan)
    })
    groups <- lapply(groups, function(g) {
      lapply(g, function(p) {
        p$label <- p$sname %||% p$label
        p$sub_label <- NULL
        p
      })
    })
    plan <- c(lead, do.call(c, groups))
    facet_spans <- list(
      lead = length(lead),
      groups = lapply(seq_along(facet_levels), function(j) {
        list(label = facet_levels[[j]], n = length(groups[[j]]))
      })
    )
  }

  # Legend: ONE group per colour column in use, in column order. Colour
  # identity must not ride on colour alone, and with colour on the summary
  # a table can carry more than one colour dimension -- which is exactly why
  # each group is titled by its column rather than merged into one strip.
  legend_cols <- unique(unlist(lapply(summaries, function(s) {
    if (lane_color_capable(s)) present(s$color)
  })))
  color_groups <- Filter(Negate(is.null), lapply(legend_cols, function(cc) {
    lv <- rank_levels(data[[cc]])
    if (length(lv) < 2L) return(NULL)
    list(column = cc, levels = lv,
         palette = rank_level_colors(scale_map, cc, lv, data[[cc]]))
  }))
  # The first group also fills the single-dimension slots the rank path's
  # emitters fall back on (a plan entry carries its own fills and levels, so
  # these are a fallback, never the source of truth).
  lead_group <- if (length(color_groups)) color_groups[[1L]]
  series_lv <- lead_group$levels
  pal <- lead_group$palette %||% character()

  list(
    rows = rows, plan = plan,
    layout = if (!length(facet_cols)) "simple" else "facet",
    mark = "summaries", bar_max = 0, bar_min = 0,
    group_label = rank_group_label(data, group, parent),
    series = series_lv,
    palette = pal, facet_levels = facet_levels,
    denoms = c(all = nrow(data)), group = group, parent = parent,
    color = lead_group$column, color_groups = color_groups,
    facet = shared_facet,
    folded = asm$folded, fold_max = asm$fold_max,
    n_total = if (is.null(parent)) nrow(leaf) else nrow(par_rows),
    note = note, pct_ok = FALSE, func = "identity",
    facet_spans = facet_spans
  )
}

#' The colour dimension of a distribution column: the per-level statistic
#' columns, the level names and their colours. Empty (no extra plan fields)
#' when the summary has no colour, so an uncoloured column is untouched.
#'
#' Colours come from the same resolver as every other mark
#' (`rank_level_colors`: board scale map -> theme -> palette), so a box
#' split by SEX matches the chart's boxplot split by SEX.
#' @noRd
lane_color_split <- function(s, sid, stats, data, scale_map) {
  lv <- s$.levels
  if (is.null(s$.color) || !length(lv)) return(list())
  list(
    levels = lv,
    lcols = lapply(seq_along(lv), function(j) {
      stats::setNames(paste0(sid, "_L", j, "_", stats), stats)
    }),
    fills = unname(rank_level_colors(scale_map, s$.color, lv,
                                     data[[s$.color]])[lv]),
    cvar = s$.color
  )
}

#' One plan entry for one (summary x facet-copy). The entry carries the
#' leaf column names, the display kind, and (after lane_summary_domains)
#' its own scale.
#' @noRd
lane_summary_plan <- function(s, cp, data, scale_map = NULL) {
  sid <- cp$suffix
  # A facet copy is headed by its level, the summary name moving to the
  # sub-label. Where the table faceted by SEVERAL columns the level alone is
  # ambiguous next to another summary's groups, so it names its column too.
  label <- if (is.null(cp$level)) {
    s$name
  } else if (isTRUE(cp$qualify)) {
    paste0(cp$fcol, ": ", cp$level)
  } else {
    cp$level
  }
  sub <- if (is.null(cp$level)) NULL else s$name
  base <- list(label = label, sub_label = sub, sid = sid, stype = s$type,
               flevel = cp$level, sname = s$name)
  if (identical(s$type, "simple")) {
    if (identical(s$show, "dot")) {
      # A single value as a positioned point on the zero-based lane: the
      # pointrange emitter with no interval. Less ink than a bar.
      c(base, list(kind = "pointrange", key = paste0(sid, "_v"),
                   cols = c(bc = paste0(sid, "_v")),
                   # Length-from-zero semantics, like the bar it replaces:
                   # the domain keeps its zero (lane_summary_domains).
                   zero = TRUE,
                   # No range of any kind, so nothing spans the cell: this
                   # is the mark that keeps its hairline.
                   bare = TRUE,
                   words = list(center = s$name), show_val = TRUE),
        lane_color_split(s, sid, "bc", data, scale_map))
    } else if (identical(s$show, "bar") && !is.null(s$.color) &&
                 length(s$.levels)) {
      # Colour on a magnitude is its composition: the bar keeps meaning the
      # group's total and the segments say what it is made of. A group whose
      # rows are all one level draws ONE segment, in that level's colour.
      # Only for an ADDITIVE measure, though: the parts of a mean do not add
      # up to it, so those segments sit side by side (rank_additive()) rather
      # than stacking into a length nothing computes.
      c(base, list(kind = "barsplit", key = paste0(sid, "_v"),
                   prefix = paste0(sid, "_S_"), series = s$.levels,
                   mode = if (rank_additive(rank_chr1(s$func) %||% "count")) {
                     "stacked"
                   } else {
                     "grouped"
                   },
                   show_val = TRUE,
                   fills = unname(rank_level_colors(
                     scale_map, s$.color, s$.levels, data[[s$.color]]
                   )[s$.levels])))
    } else {
      kind <- if (identical(s$show, "bar")) "bar" else "num"
      c(base, list(kind = kind, key = paste0(sid, "_v"),
                   fill = if (identical(kind, "bar")) dd_palette(1L),
                   show_val = identical(kind, "bar")))
    }
  } else if (identical(s$type, "dist")) {
    stat <- rank_chr1(s$stat) %||% "median_q1_q3"
    meta <- LANE_STAT_META[[stat]] %||% LANE_STAT_META$median_q1_q3
    if (identical(s$show, "text")) {
      c(base, list(kind = "num", key = paste0(sid, "_bc"),
                   dispkey = "dist_text",
                   lo = paste0(sid, "_bl"), hi = paste0(sid, "_bh"),
                   sub_label = sub %||% meta$label))
    } else {
      # ONE glyph, two styles. The leaf columns follow the pieces that are
      # switched on, so an absent range ships no columns at all and the
      # emitter draws what it is given (never re-derived arithmetic).
      inner <- rank_chr1(s$inner) %||% "median_q1_q3"
      outer <- rank_chr1(s$outer) %||% "none"
      cmeta <- LANE_STAT_META[[lane_dist_centre_stat(s)]] %||%
        LANE_STAT_META$median_q1_q3
      cols <- c(bc = paste0(sid, "_bc"), n = paste0(sid, "_n"))
      if (!identical(inner, "none")) {
        cols <- c(cols, bl = paste0(sid, "_bl"), bh = paste0(sid, "_bh"))
      }
      if (!identical(outer, "none")) {
        cols <- c(cols, wl = paste0(sid, "_wl"), wh = paste0(sid, "_wh"))
      }
      wmeta <- LANE_STAT_META[[outer]] %||% LANE_STAT_META$tukey
      c(base, list(kind = if (identical(s$style, "box")) "box" else
                     "pointrange",
                   key = paste0(sid, "_bc"), cols = cols,
                   # The ground rule, decided here rather than in CSS: a mark
                   # that draws an outer range brings its own rail, so its
                   # lane is bare; one without gets the hairline back.
                   bare = identical(outer, "none"),
                   words = list(
                     center = cmeta$center,
                     range = if (!identical(inner, "none")) meta$range,
                     whisk = if (!identical(outer, "none")) wmeta$range
                   ),
                   sub_label = sub %||% lane_dist_sub_label(s, meta, wmeta),
                   show_val = TRUE),
        lane_color_split(s, sid, names(cols), data, scale_map))
    }
  } else if (identical(s$type, "field")) {
    c(base, list(kind = "num", key = paste0(sid, "_t"), raw = TRUE,
                 text = TRUE,
                 sub_label = sub %||% dt_col_label(data[[s$col]], s$col)))
  } else if (identical(s$type, "expr")) {
    # Numeric vs text is decided by the eval; the plan names the numeric
    # key and rank_cells falls back to the text column when it is absent.
    c(base, list(kind = "num", key = paste0(sid, "_v"),
                 alt_text = paste0(sid, "_t")))
  } else if (identical(s$type, "series")) {
    c(base, list(kind = "sparkline", key = paste0(sid, "_last"),
                 pts = paste0(sid, "_pts"), x = rank_chr1(s$x),
                 dom_date = isTRUE(s$.date), ref = cp$ref,
                 show_val = TRUE))
  } else {
    lv <- s$.levels
    c(base, list(kind = "interval", key = paste0(sid, "_start"),
                 segs = paste0(sid, "_segs"), x = rank_chr1(s$x),
                 xend = rank_chr1(s$xend), levels = lv,
                 dom_date = isTRUE(s$.date),
                 tfields = intersect(as.character(s$fields %||% character()),
                                     names(data)),
                 size = rank_chr1(s$size) %||% "md",
                 fills = if (!is.null(lv)) {
                   unname(rank_level_colors(
                     scale_map, rank_chr1(s$color), lv,
                     data[[rank_chr1(s$color)]]
                   )[lv])
                 } else {
                   dd_palette(1L)
                 }))
  }
}

#' The column's sub-label: what the glyph is made of, in the header, so the
#' cell never has to explain itself. "Median . Q1-Q3 . 1.5xIQR" reads as the
#' three pieces in the order they are drawn; a piece that is off is absent
#' rather than named as "none".
#' @noRd
lane_dist_sub_label <- function(s, meta, wmeta) {
  parts <- c(
    if (identical(rank_chr1(s$inner) %||% "none", "none")) {
      LANE_STAT_META[[lane_dist_centre_stat(s)]]$center
    } else {
      meta$label
    },
    if (!identical(rank_chr1(s$outer) %||% "none", "none")) wmeta$range
  )
  paste(parts, collapse = " \u00b7 ")
}

#' @noRd
lane_primary_col <- function(s, sid) {
  switch(s$type,
    simple = paste0(sid, "_v"),
    dist = paste0(sid, "_bc"),
    field = paste0(sid, "_t"),
    expr = paste0(sid, "_v"),
    series = paste0(sid, "_last"),
    spans = paste0(sid, "_start"),
    paste0(sid, "_v")
  )
}

#' @noRd
lane_primary_value <- function(target, s, col) {
  v <- target[[col]]
  if (is.null(v)) {
    # expr rows may have landed as text.
    v <- target[[sub("_v$", "_t", col)]]
  }
  if (is.null(v)) rep(NA_real_, nrow(target)) else v
}

# --- series / spans builders --------------------------------------------------
# Compact re-statements of the single-mark builders in lane-prepare.R,
# keyed by arbitrary key combos rather than one group column. Each returns
# a list-column aligned to `target`, plus stashes the raw extents as
# attributes for the domain pass.

#' @noRd
lane_key_string <- function(df, keys) {
  do.call(paste, c(lapply(keys, function(k) as.character(df[[k]])),
                   list(sep = "\r")))
}

#' @noRd
lane_as_num <- function(v) {
  if (inherits(v, "Date") || inherits(v, "POSIXct")) as.numeric(v)
  else if (is.numeric(v)) as.numeric(v)
  else NULL
}

#' @noRd
lane_series_split <- function(slice, target, tkeys, s) {
  xs <- lane_as_num(slice[[rank_chr1(s$x)]])
  ys <- as.numeric(slice[[rank_chr1(s$col)]])
  band <- as.character(s$band %||% character())
  band <- band[nzchar(band)]
  has_band <- length(band) == 2L && all(band %in% names(slice)) &&
    is.numeric(slice[[band[[1L]]]]) && is.numeric(slice[[band[[2L]]]])
  ok <- if (is.null(xs)) rep(FALSE, nrow(slice)) else is.finite(xs) & is.finite(ys)
  sk <- lane_key_string(slice, tkeys)
  tk <- lane_key_string(target, tkeys)
  lapply(tk, function(k) {
    idx <- which(sk == k & ok)
    idx <- idx[order(xs[idx])]
    list(
      x = xs[idx], y = ys[idx],
      lo = if (has_band) as.numeric(slice[[band[[1L]]]])[idx],
      hi = if (has_band) as.numeric(slice[[band[[2L]]]])[idx]
    )
  })
}

#' @noRd
lane_spans_split <- function(slice, target, tkeys, s) {
  xs <- lane_as_num(slice[[rank_chr1(s$x)]])
  xe <- lane_as_num(slice[[rank_chr1(s$xend)]])
  color <- rank_chr1(s$color)
  color <- if (!is.null(color) && color %in% names(slice)) color
  # Full-data level order rides on the summary (s$.levels), so a facet
  # slice missing a level cannot renumber the fills.
  lv <- s$.levels
  # Event identity: the label column headlines the segment tooltip and
  # keys the same-event hover highlight; fields append extra columns.
  lbcol <- rank_chr1(s$label)
  lbcol <- if (!is.null(lbcol) && lbcol %in% names(slice)) lbcol
  fcols <- intersect(as.character(s$fields %||% character()), names(slice))
  ok <- if (is.null(xs) || is.null(xe)) {
    rep(FALSE, nrow(slice))
  } else {
    is.finite(xs) & is.finite(xe)
  }
  sk <- lane_key_string(slice, tkeys)
  tk <- lane_key_string(target, tkeys)
  lapply(tk, function(k) {
    idx <- which(sk == k & ok)
    idx <- idx[order(xs[idx])]
    out <- list()
    for (r in idx) {
      f <- if (!is.null(lv) && !is.null(color)) {
        match(as.character(slice[[color]][[r]]), lv)
      } else {
        1L
      }
      if (is.na(f)) next
      sg <- list(s = xs[[r]], e = max(xe[[r]], xs[[r]]), f = f)
      if (!is.null(lbcol)) {
        lb <- as.character(slice[[lbcol]][[r]])
        if (!is.na(lb) && nzchar(lb)) sg$lb <- lb
      }
      if (length(fcols)) {
        sg$fv <- vapply(fcols, function(fc) {
          v <- as.character(slice[[fc]][[r]])
          if (is.na(v)) "" else v
        }, character(1L))
      }
      out[[length(out) + 1L]] <- sg
    }
    out
  })
}

#' Per-entry domains, computed over the assembled rows and SHARED across a
#' summary's facet copies (same `sid` stem before the `f<j>` suffix).
#' @noRd
lane_summary_domains <- function(plan, rows) {
  stem <- function(sid) sub("f[0-9]+$", "", sid)
  groups <- split(seq_along(plan), vapply(plan, function(p) {
    stem(p$sid %||% "")
  }, character(1L)))
  for (idx in groups) {
    kinds <- unique(vapply(plan[idx], function(p) p$kind, character(1L)))
    kind <- kinds[[1L]]
    if (kind %in% c("bar", "barsplit", "box", "pointrange")) {
      vals <- numeric()
      for (i in idx) {
        p <- plan[[i]]
        # POSITION columns only. A box also carries `n` (the group size, for
        # the tooltip); it is not a point on the value axis, and letting it
        # in stretched the domain to the sample size -- a 49-105 mmHg box on
        # a 0-1012 track.
        cols <- if (kind %in% c("bar", "barsplit")) {
          # The split bar scales on its TOTAL, the same number the plain bar
          # uses -- the segments are a share of it, so the two read alike.
          # Side-by-side segments are read one at a time instead, so the axis
          # has to reach the WIDEST of them, not just the pooled value.
          if (identical(p$mode, "grouped")) {
            c(p$key, paste0(p$prefix, p$series))
          } else {
            p$key
          }
        } else {
          pos <- function(cm) unname(cm[setdiff(names(cm), "n")])
          # A colour-split column's levels share ONE scale: every level's
          # glyph is read against the same axis, or the split lies.
          c(pos(p$cols), unlist(lapply(p$lcols %||% list(), pos)))
        }
        for (cn in cols) vals <- c(vals, rows[[cn]])
      }
      vals <- vals[is.finite(vals)]
      # Zero belongs to marks that encode value as LENGTH from a baseline --
      # the bar, and the simple row's dot, which is a bar's worth of meaning
      # with less ink. A box or a mean CI encodes value as POSITION: it is
      # read against the other rows, not against zero, so pinning zero only
      # squashes data that lives far from it (chart parity: `scale: true` on
      # the boxplot, blockr.viz d2dd210).
      # A bar encodes value as LENGTH from zero, and length has no way to say
      # "negative": mk_pct_w() takes abs(), so a fall of 30 draws as wide as a
      # rise of 30 and an all-negative column collapses to empty tracks. A
      # signed measure (mean/median of a change column, a waterfall's extreme
      # per subject) is the zero-centred DIVERGING bar instead -- same column,
      # same numbers, and every consumer downstream already switches on `kind`
      # (rank_axis_domain(), rank_cells(), rank_cells_html(), the JSON packer
      # and its rank-table.js twin). Decided here because this is the first
      # point that has seen the assembled values.
      # `barsplit` is deliberately excluded: stacked segments that go negative
      # are a different problem, and it has no diverging form.
      if (identical(kind, "bar") && any(vals < 0)) {
        amx <- if (length(vals)) max(abs(vals)) else 0
        for (i in idx) {
          plan[[i]]$kind <- "bardiv"
          plan[[i]]$dmax <- amx
          plan[[i]]$dmin <- -amx
        }
        next
      }
      zero <- kind %in% c("bar", "barsplit") ||
        any(vapply(plan[idx], function(p) isTRUE(p$zero), logical(1L)))
      if (zero) {
        dmax <- if (length(vals)) max(c(0, vals)) else 0
        dmin <- if (kind %in% c("bar", "barsplit")) {
          0
        } else {
          if (length(vals)) min(c(0, vals)) else 0
        }
      } else {
        rng <- if (length(vals)) range(vals) else c(0, 1)
        # A hair of padding so a glyph at the extreme is not flush against
        # the cell edge, and a degenerate (all-equal) column still draws.
        pad <- if (rng[[2L]] > rng[[1L]]) {
          (rng[[2L]] - rng[[1L]]) * 0.04
        } else {
          max(abs(rng[[1L]]) * 0.04, 0.5)
        }
        dmin <- rng[[1L]] - pad
        dmax <- rng[[2L]] + pad
      }
      for (i in idx) {
        plan[[i]]$dmax <- dmax
        plan[[i]]$dmin <- dmin
      }
    } else if (identical(kind, "sparkline")) {
      allx <- numeric()
      ally <- numeric()
      for (i in idx) {
        for (p1 in rows[[plan[[i]]$pts]]) {
          allx <- c(allx, p1$x)
          ally <- c(ally, p1$y, p1$lo, p1$hi)
        }
        # A mean +/- sd reference can exceed the observed range.
        r <- plan[[i]]$ref
        if (!is.null(r)) ally <- c(ally, r$center, r$lo, r$hi)
      }
      allx <- allx[is.finite(allx)]
      ally <- ally[is.finite(ally)]
      xd <- if (length(allx)) range(allx) else c(0, 1)
      yd <- if (length(ally)) range(ally) else c(0, 1)
      if (xd[[2L]] <= xd[[1L]]) xd[[2L]] <- xd[[1L]] + 1
      if (yd[[2L]] <= yd[[1L]]) yd[[2L]] <- yd[[1L]] + 1
      for (i in idx) {
        plan[[i]]$dom <- xd
        plan[[i]]$ydom <- yd
      }
    } else if (identical(kind, "interval")) {
      ext <- numeric()
      for (i in idx) {
        for (ss in rows[[plan[[i]]$segs]]) {
          for (sg in ss) ext <- c(ext, sg$s, sg$e)
        }
      }
      ext <- ext[is.finite(ext)]
      xd <- if (length(ext)) range(ext) else c(0, 1)
      if (xd[[2L]] <= xd[[1L]]) xd[[2L]] <- xd[[1L]] + 1
      for (i in idx) plan[[i]]$dom <- xd
    }
  }
  plan
}
