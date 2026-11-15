# The spans summary (swimlanes): reading its records, the main lane's
# segments and the thin lanes above it. Filled by groups_fill_spans() in
# groups-build.R.

# A spans row's levels read off the full data: event shapes and lane order.
groups_spans_levels <- function(s, data) {
  # Event shapes by level of the full data, so a facet slice missing a level
  # keeps every other level's shape. A factor's full level set decides the
  # shapes, so dropping a level does not move the rest; the legend lists
  # the levels the event rows carry.
  if (!is.null(groups_present(s$event_x, data)) &&
        !is.null(groups_present(s$event_shape, data))) {
    ex <- groups_as_num(data[[s$event_x]])
    ok <- if (is.null(ex)) logical(nrow(data)) else is.finite(ex)
    v <- data[[s$event_shape]]
    s$.shape_levels <- groups_levels(v)
    used <- unique(as.character(v[ok]))
    s$.shape_used <- s$.shape_levels[s$.shape_levels %in% used]
    # Levels whose events all draw as tags get a legend group of their own,
    # each with its tag's text.
    tg <- groups_present(s$event_tag, data)
    if (!is.null(tg)) {
      tv <- as.character(data[[tg]])
      has <- ok & !is.na(tv) & nzchar(tv)
      lv_ok <- as.character(v[ok])
      s$.shape_tags <- s$.shape_used[vapply(s$.shape_used, function(l) {
        all(has[ok][lv_ok %in% l])
      }, NA)]
      s$.tag_text <- vapply(s$.shape_tags, function(l) {
        tv[ok][lv_ok %in% l][[1L]]
      }, "")
      # A tagged level's group, from its first event, and the groups in
      # order: the column's levels, else as they come.
      gc <- groups_present(s$event_tag_group, data)
      if (!is.null(gc) && length(s$.shape_tags)) {
        gv <- as.character(data[[gc]])
        s$.tag_group <- vapply(s$.shape_tags, function(l) {
          g <- gv[ok][lv_ok %in% l]
          g <- g[!is.na(g) & nzchar(g)]
          if (length(g)) g[[1L]] else ""
        }, "")
        lv_g <- groups_levels(data[[gc]])
        lv_g <- lv_g[!is.na(lv_g) & nzchar(lv_g)]
        s$.tag_groups <- lv_g[lv_g %in% s$.tag_group]
        # A group's rank, which decides its look: a factor's own levels, so
        # a second level stays second when the first is absent; else the
        # groups present.
        s$.tag_rank <- if (is.factor(data[[gc]])) lv_g else s$.tag_groups
      }
    }
  }
  # Lane order from the full data too, so every row stacks its lanes alike.
  if (!is.null(groups_present(s$lane, data))) {
    lv <- groups_levels(data[[s$lane]])
    s$.lane_levels <- lv[!is.na(lv) & nzchar(lv)]
    # Full lanes, in lane order; the first is the main lane.
    if (!is.null(groups_present(s$lane_full, data))) {
      ln <- as.character(data[[s$lane]])
      full <- unique(ln[as.logical(data[[s$lane_full]]) %in% TRUE])
      s$.full_levels <- s$.lane_levels[s$.lane_levels %in% full]
    }
  }
  s
}

#' Which records draw on the main lane: an empty lane, or the first full lane.
#' @noRd
groups_spans_on_main <- function(ln, s) {
  empty <- is.na(ln) | !nzchar(ln)
  if (!length(s$.full_levels)) return(empty)
  empty | ln %in% s$.full_levels[[1L]]
}

#' The rows of `slice` under each row of `target`, in slice order: one list
#' entry per target row. One pass over the keys; a scan of the slice per
#' target row took the rows times the groups.
#' @noRd
groups_rows_by_key <- function(slice, target, tkeys) {
  sk <- groups_key_string(slice, tkeys)
  tk <- groups_key_string(target, tkeys)
  first <- match(tk, tk)
  by <- split(seq_along(sk), factor(match(sk, tk), levels = seq_along(tk)))
  unname(by[first])
}

#' The main lane's segments of a spans row, one list per target row: `s`, `e`,
#' fill index `f`, and when set the label `lb`, tooltip values `fv` and style
#' `st`. With a lane column only the records whose lane is empty. `g` is the
#' slice's reader, `by` its rows per target row (groups_rows_by_key()).
#' @noRd
groups_spans_split <- function(g, by) {
  draws <- g$ok & g$main
  lapply(by, function(rows) groups_spans_segs(g, rows[draws[rows]]))
}

#' The open end of a lane's records `rows`: the end of the latest-ending one
#' that is open (`op`) and draws, Inf for one without an end; NULL when none
#' is. The caret sits there, at the end of its own record's bar, so a record
#' that draws on another lane or track never moves it.
#' @noRd
groups_spans_open_end <- function(g, op, rows) {
  if (is.null(op)) return(NULL)
  rows <- rows[op[rows] %in% TRUE & g$ok[rows] & !is.na(g$fill[rows])]
  if (length(rows)) max(g$end[rows])
}

#' groups_spans_split() for stacked overlaps: each row's main lane keeps the
#' first track of its records, each record whole (groups_spans_tracks());
#' groups_spans_lanes() draws the other tracks.
#' @noRd
groups_spans_split_stacked <- function(g, by) {
  g1 <- utils::modifyList(g, list(worst = FALSE))
  lapply(groups_spans_main_rows(g, by, TRUE), function(rows) {
    groups_spans_segs(g1, rows)
  })
}

#' The records each row draws on its main lane: those that draw there, and
#' stacked only the first track of them.
#' @noRd
groups_spans_main_rows <- function(g, by, stack) {
  draws <- g$ok & g$main
  lapply(by, function(rows) {
    rows <- rows[draws[rows]]
    if (stack) groups_spans_tracks(g, rows)[[1L]] else rows
  })
}

#' What every part of a spans row reads off the slice: numeric start and end,
#' style codes, lane values, which records draw (`ok`) and which sit on the
#' main lane (`main`), and `seg(r)`, record `r` as a segment or NULL.
#' @noRd
groups_spans_reader <- function(slice, s) {
  xs <- groups_as_num(slice[[chr_state(s$x)]])
  xe <- groups_as_num(slice[[chr_state(s$xend)]])
  present <- function(nm) {
    v <- chr_state(s[[nm]])
    if (!is.null(v) && v %in% names(slice)) v
  }
  color <- present("color")
  # Full-data level order (s$.levels), so a facet slice missing a level cannot
  # renumber the fills.
  lv <- s$.levels
  # The label column headlines the segment tooltip and keys the same-event
  # hover highlight; fields add columns.
  lbcol <- present("label")
  fcols <- intersect(as.character(s$tooltip %||% character()), names(slice))
  tpcol <- present("tip")
  n <- nrow(slice)
  st <- groups_spans_styles(slice, present("seg_style"))
  ln <- if (!is.null(present("lane"))) as.character(slice[[present("lane")]])
  main <- if (is.null(ln)) rep(TRUE, n) else groups_spans_on_main(ln, s)
  if (is.null(xs)) xs <- rep(NA_real_, n)
  if (is.null(xe)) xe <- rep(NA_real_, n)
  # A dot or a step marks one moment: it needs no end.
  point <- st %in% c(4L, 5L)
  xe[point & !is.finite(xe)] <- xs[point & !is.finite(xe)]
  # An open record without an end is ongoing: it runs to the axis end (Inf
  # here, the domain's end in the cells), as the gantt draws it.
  opc <- present("open")
  if (!is.null(opc)) {
    ongo <- as.logical(slice[[opc]]) %in% TRUE & is.finite(xs) &
      !is.finite(xe)
    xe[ongo] <- Inf
  }
  ok <- is.finite(xs) & !is.na(xe) & xe > -Inf
  # Every record's parts as vectors, read off the slice once: seg() is called
  # per record, and a column lookup in a data frame per record is slow.
  fill <- if (!is.null(lv) && !is.null(color)) {
    match(as.character(slice[[color]]), lv)
  } else {
    rep(1L, n)
  }
  end <- pmax(xe, xs)
  text <- function(col) {
    if (is.null(col)) return(NULL)
    v <- as.character(slice[[col]])
    v[is.na(v)] <- ""
    v
  }
  lb <- text(lbcol)
  tp <- text(tpcol)
  # One column per tooltip field, named by it; a missing value is "".
  fv <- if (length(fcols) && n) {
    matrix(unlist(lapply(fcols, text), use.names = FALSE), nrow = n,
           dimnames = list(NULL, fcols))
  }
  seg <- function(r) {
    f <- fill[[r]]
    if (is.na(f)) return(NULL)
    sg <- list(s = xs[[r]], e = end[[r]], f = f)
    if (!is.null(lb) && nzchar(lb[[r]])) sg$lb <- lb[[r]]
    if (!is.null(fv)) sg$fv <- fv[r, ]
    if (st[[r]] > 1L) sg$st <- st[[r]]
    if (!is.null(tp) && nzchar(tp[[r]])) sg$tp <- tp[[r]]
    sg
  }
  list(xs = xs, ok = ok, main = main, ln = ln, st = st, seg = seg,
       strip = present("strip"), slice = slice,
       event = groups_spans_events(slice, s),
       worst = identical(chr_state(s$overlap), "worst"),
       end = end, fill = fill)
}

#' The events of a spans row, read off the slice once: `x` their positions
#' (NULL without an event column) and `at(j)`, record `j` as an event: its
#' position, its shape index `k` by the level of the shape column, that
#' level `lv`, and when set its tag `tg` and its own tooltip `tp`.
#' @noRd
groups_spans_events <- function(slice, s) {
  col <- function(nm) {
    v <- chr_state(s[[nm]])
    if (!is.null(v) && v %in% names(slice)) v
  }
  x <- if (!is.null(col("event_x"))) groups_as_num(slice[[col("event_x")]])
  shp <- col("event_shape")
  level <- if (!is.null(shp)) as.character(slice[[shp]])
  lv <- s$.shape_levels
  k <- if (!is.null(level) && length(lv)) match(level, lv)
  txt <- function(nm) {
    if (is.null(col(nm))) return(NULL)
    v <- as.character(slice[[col(nm)]])
    v[is.na(v)] <- ""
    v
  }
  tg <- txt("event_tag")
  tp <- txt("tip")
  at <- function(j) {
    l <- if (!is.null(level)) level[[j]]
    kk <- if (!is.null(k) && !is.na(l)) k[[j]] else NA
    out <- list(x = x[[j]], k = if (is.na(kk)) 1L else (kk - 1L) %% 5L + 1L,
                lv = if (!is.null(l) && !is.na(l)) l)
    if (!is.null(tg) && nzchar(tg[[j]])) {
      out$tg <- tg[[j]]
      if (!is.null(l) && !is.na(l) && groups_tag_inverted(s, l)) out$inv <- TRUE
    }
    if (!is.null(tp) && nzchar(tp[[j]])) out$tp <- tp[[j]]
    out
  }
  list(x = x, at = at)
}

#' The segments of records `idx`, solid ones first and each kind in start
#' order, so a delay or a skip draws over the period it interrupts.
#' @noRd
groups_spans_segs <- function(g, idx) {
  if (isTRUE(g$worst) && length(idx) > 1L) return(groups_spans_worst(g, idx))
  idx <- idx[order(g$st[idx] > 1L, g$xs[idx])]
  Filter(Negate(is.null), lapply(idx, g$seg))
}

#' The segments of records `idx` with overlaps cut away: between any two
#' consecutive starts or ends, the record active there with the highest fill
#' index draws (on a tie, the one that started last). Neighbouring pieces of
#' one record join. A piece carries its record's label, tooltip and style; bar
#' labels are left off, a piece is not the whole record.
#' @noRd
groups_spans_worst <- function(g, idx) {
  idx <- idx[!is.na(g$fill[idx])]
  if (length(idx) < 2L) return(Filter(Negate(is.null), lapply(idx, g$seg)))
  s <- g$xs[idx]
  e <- g$end[idx]
  f <- g$fill[idx]
  b <- sort(unique(c(s, e)))
  if (length(b) < 2L) return(Filter(Negate(is.null), lapply(idx, g$seg)))
  lo <- b[-length(b)]
  hi <- b[-1L]
  # Rank records worst first, then latest start, and give each piece the
  # first record in that order active over it.
  rank <- order(-f, -s)
  win <- rep(NA_integer_, length(lo))
  for (k in rank) {
    on <- is.na(win) & s[[k]] <= lo & e[[k]] >= hi
    win[on] <- k
    if (!anyNA(win)) break
  }
  keep <- !is.na(win)
  lo <- lo[keep]
  hi <- hi[keep]
  win <- win[keep]
  # A one-moment record (start = end) spans no piece: it draws as itself
  # where no piece covers it.
  pt <- which(s == e)
  pt <- pt[!vapply(pt, function(k) any(lo <= s[[k]] & hi >= s[[k]]), NA)]
  pts <- lapply(idx[pt], g$seg)
  if (!length(win)) return(Filter(Negate(is.null), pts))
  # A new piece starts where the record changes or a gap opens.
  new <- c(TRUE, win[-1L] != win[-length(win)] | lo[-1L] > hi[-length(hi)])
  first <- which(new)
  last <- c(first[-1L] - 1L, length(win))
  out <- c(lapply(seq_along(first), function(j) {
    sg <- g$seg(idx[[win[[first[[j]]]]]])
    sg$s <- lo[[first[[j]]]]
    sg$e <- hi[[last[[j]]]]
    sg
  }), pts)
  out <- Filter(Negate(is.null), out)
  out[order(vapply(out, function(sg) sg$s, numeric(1L)))]
}

# Segment styles by the value of the style column; anything else is solid.
GROUPS_SPANS_STYLES <- c(solid = 1L, dashed = 2L, hollow = 3L, dot = 4L,
                         step = 5L, outline = 6L, thinner = 7L, hairline = 8L)

#' One style code per record: 1 solid, 2 dashed, 3 hollow, 4 dot, 5 step,
#' 6 outline, 7 thinner, 8 hairline.
#' @noRd
groups_spans_styles <- function(slice, col) {
  if (is.null(col)) return(rep(1L, nrow(slice)))
  v <- GROUPS_SPANS_STYLES[tolower(trimws(as.character(slice[[col]])))]
  out <- unname(v)
  out[is.na(out)] <- 1L
  out
}

#' The thin lanes and the strips of a spans row, one list per target row:
#' `sub`, a list of lanes in level order (`nm`, `segs`, `op` its open end
#' (groups_spans_open_end()), `bx` its strip boxes), and `ms`, the main lane's strip boxes. A
#' box is `x`, its text `t` and its record's style `st`. A parent row pools
#' nothing: its lanes are one subject's facts. `g` is the slice's reader, `by`
#' its rows per target row (groups_rows_by_key()).
#' @noRd
groups_spans_lanes <- function(g, by, s, stack = FALSE) {
  slice <- g$slice
  opc <- chr_state(s$open)
  op <- if (!is.null(opc) && opc %in% names(slice)) {
    as.logical(slice[[opc]])
  }
  tx <- if (!is.null(g$strip)) as.character(slice[[g$strip]])
  has_tx <- if (is.null(tx)) {
    logical(nrow(slice))
  } else {
    !is.na(tx) & nzchar(tx) & is.finite(g$xs)
  }
  boxes <- function(rows) {
    rows <- rows[has_tx[rows]]
    rows <- rows[order(g$xs[rows])]
    lapply(rows, function(r) list(x = g$xs[[r]], t = tx[[r]], st = g$st[[r]]))
  }
  ex <- g$event$x
  events <- function(rows) {
    if (is.null(ex)) return(list())
    rows <- rows[is.finite(ex[rows])]
    rows <- rows[order(ex[rows])]
    lapply(rows, g$event$at)
  }
  lv <- s$.lane_levels %||% character()
  lapply(by, function(rows) {
    sub <- list()
    if (!is.null(g$ln)) {
      # The row's records per lane, once: a lane level no record of the row
      # carries draws nothing.
      on <- rows[!g$main[rows]]
      per <- split(on, g$ln[on])
      for (l in lv[lv %in% names(per)]) {
        lr <- per[[l]]
        # Stacked (`inner_overlap`): one copy of the lane per track of
        # records that do not overlap, each record drawn whole; the strip
        # and the events stay on the first.
        tracks <- if (stack) groups_spans_tracks(g, lr[g$ok[lr]]) else list(NULL)
        for (k in seq_along(tracks)) {
          tr <- tracks[[k]]
          segs <- if (is.null(tr)) {
            groups_spans_segs(g, lr[g$ok[lr]])
          } else {
            groups_spans_segs(utils::modifyList(g, list(worst = FALSE)), tr)
          }
          bx <- if (k == 1L) boxes(lr) else list()
          evs <- if (k == 1L) events(lr) else list()
          if (!length(segs) && !length(bx) && !length(evs)) next
          own <- if (is.null(tr)) lr else tr
          sub[[length(sub) + 1L]] <- list(
            nm = l, segs = segs, bx = bx, ev = evs,
            fl = l %in% s$.full_levels,
            op = groups_spans_open_end(g, op, own)
          )
        }
      }
    }
    # Stacked, the main lane's further tracks draw first among the full bars.
    if (stack) {
      main <- rows[(g$ok & g$main)[rows]]
      more <- groups_spans_tracks(g, main)[-1L]
      g1 <- utils::modifyList(g, list(worst = FALSE))
      sub <- c(lapply(more, function(tr) {
        list(nm = s$.full_levels[1L] %||% "", segs = groups_spans_segs(g1, tr),
             bx = list(), ev = list(), fl = TRUE,
             op = groups_spans_open_end(g, op, tr))
      }), sub)
    }
    list(sub = sub, ms = boxes(rows[g$main[rows]]))
  })
}

#' Does a spans row draw lanes or strips?
#' @noRd
groups_spans_has_lanes <- function(s) {
  !is.null(chr_state(s$lane)) || !is.null(chr_state(s$strip))
}

#' Records `rows` of one lane in tracks that do not overlap: each record in
#' start order goes on the first track whose last record ends at or before
#' its start: records that only touch (a run of response reads) share a
#' track. A record the same as one
#' already placed (start, end and colour: a repeat, not a second episode)
#' joins it. At least one track (empty when there is no record).
#' @noRd
groups_spans_tracks <- function(g, rows) {
  if (!length(rows)) return(list(integer()))
  rows <- rows[order(g$xs[rows], g$end[rows])]
  ends <- numeric()
  out <- list()
  seen <- character()
  at <- integer()
  for (r in rows) {
    key <- paste(g$xs[[r]], g$end[[r]], g$fill[[r]])
    k <- at[match(key, seen)]
    if (!is.na(k)) {
      out[[k]] <- c(out[[k]], r)
      next
    }
    k <- which(ends <= g$xs[[r]])[1L]
    if (is.na(k)) {
      k <- length(ends) + 1L
      out[[k]] <- integer()
    }
    out[[k]] <- c(out[[k]], r)
    ends[[k]] <- g$end[[r]]
    seen <- c(seen, key)
    at <- c(at, k)
  }
  out
}

#' Whether a tagged level draws inverted: its tag group ranks after the
#' first. A tag with no group draws as the first group's.
#' @noRd
groups_tag_inverted <- function(s, level) {
  k <- match(s$.tag_group[level], s$.tag_rank)
  length(k) == 1L && !is.na(k) && k > 1L
}
