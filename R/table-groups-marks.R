# The `mark` of each groups column (R/table-groups-view.R): the per-row
# numbers and per-column values the browser lays a bar, a box, a lane or a
# sparkline out from, in the units the cell model computed them in. Positions
# and widths are percentages of the cell (2 decimals, rounded in R). The
# shapes are TableMark in tests/js/fixtures/table/groups-marks.d.ts.txt.
#
# Names against summarize_payload_col() (the reference payload), where they
# changed. All kinds: `tt` -> `hover` (head, sub, `meas` -> `measure`,
# `cvar` -> `color_by`, `den` -> `base`, words). `v` is the column's `key`;
# `disp` is the column's `disp`; markup is gone, so a tooltip is plain text.
#   bar       w -> width, sub -> child
#   barsplit  names, fills, seg, segv, slab, sink, ldisp -> `levels`, one
#             object per level: name, color, width, amount, inside, ink,
#             label; scol -> level_col
#   bardiv    w -> width, pos -> positive
#   box       wl -> whisker_low, w1 -> whisker_low_width, bl -> box_low,
#             bw -> box_width, bc -> center, b2 -> box_high,
#             w2 -> whisker_high_width, wh -> whisker_high, nn -> n,
#             r -> stats (bc, bl, bh, wl, wh -> center, low, high,
#             whisker_low, whisker_high)
#   pointrange ol -> outer_low, ow -> outer_width, c -> center,
#             l -> inner_low, rw -> inner_width
#   pair      a -> from, b -> to, l -> link_left, w -> link_width,
#             bl -> band_left, bw -> band_width, rf -> ref, ab -> ends
#   interval  segs -> spans (fill index counts from 0), d0 and d1 -> domain,
#             dd -> domain.date, lg -> large
#   sparkline pl -> line, bd -> band, dx and dy -> end_x and end_y,
#             xs and ys -> x_text and y_text, nn -> n, rc -> ref_line,
#             rby -> ref_band_top, rbh -> ref_band_height
#   tile      bg -> fill, fg -> ink
# A colour split of a box, point range or pair (`multi` in the cell model) is
# `levels`: one object per level with name, color, the geometry above and
# its own `label`.

# A plain number or text has no mark; one with a "(43%)" tail, or a failed
# custom summary, has a `num` mark that holds them.
table_groups_mark <- function(c, p, prep, pick, axis) {
  mk <- switch(
    c$kind,
    bar = table_groups_mark_bar(c),
    barsplit = table_groups_mark_split(c, pick),
    bardiv = list(kind = "bardiv", width = table_groups_arr(c$w),
                  positive = table_groups_arr(c$pos)),
    box = table_groups_mark_lane(c),
    pointrange = table_groups_mark_lane(c),
    pair = table_groups_mark_pair(c),
    interval = table_groups_mark_interval(c),
    sparkline = table_groups_mark_spark(c),
    tile = list(kind = "tile", fill = table_groups_arr(c$bg),
                ink = table_groups_arr(c$fg)),
    num = table_groups_mark_num(c)
  )
  if (is.null(mk)) {
    return(NULL)
  }
  table_groups_compact(c(mk, table_groups_scale(c, p, prep, axis), list(
    dw = c$dw,
    pct = if (!is.null(c$dw) && !is.null(c$pct)) {
      table_groups_arr(as.character(c$pct))
    },
    hover = table_groups_hover(c$tt)
  )))
}

table_groups_mark_num <- function(c) {
  if (is.null(c$pct) && !isTRUE(c$failed)) {
    return(NULL)
  }
  list(kind = "num", failed = if (isTRUE(c$failed)) TRUE,
       pct = if (!is.null(c$pct)) table_groups_arr(as.character(c$pct)))
}

table_groups_mark_bar <- function(c) {
  list(kind = "bar", width = table_groups_arr(c$w), fill = c$fill,
       child = if (any(c$sub, na.rm = TRUE)) table_groups_arr(c$sub))
}

# The hover card's description of a column.
table_groups_hover <- function(tt) {
  if (!length(tt)) {
    return(NULL)
  }
  out <- list(head = tt$head, sub = tt$sub, measure = tt$meas,
              color_by = tt$cvar, base = tt$den,
              words = table_groups_compact(tt$words))
  table_groups_compact(out)
}

# A colour-split bar: one object per level. A click on a segment claims the
# level under the column `level_col`.
table_groups_mark_split <- function(c, pick) {
  levels <- lapply(seq_along(c$names), function(j) {
    table_groups_compact(list(
      name = as.character(c$names[[j]]), color = as.character(c$fills[[j]]),
      width = table_groups_arr(c$seg[[j]]),
      amount = table_groups_arr(c$segv[[j]]),
      inside = if (!is.null(c$slab)) table_groups_arr(c$slab[[j]]),
      ink = if (!is.null(c$slab)) as.character(c$sink[[j]]),
      label = if (!is.null(c$ldisp)) {
        table_groups_arr(as.character(c$ldisp[[j]]))
      }
    ))
  })
  list(kind = "barsplit", mode = c$mode, levels = levels,
       level_col = if (pick) c$scol)
}

# Box and point range. One geometry set, or one per level (`levels`).
table_groups_mark_lane <- function(c) {
  pack <- function(g) table_groups_lane_pack(g, c$kind)
  if (!isTRUE(c$multi)) {
    return(c(list(kind = c$kind, bare = if (isTRUE(c$bare)) TRUE), pack(c)))
  }
  list(kind = c$kind, bare = if (any(vapply(c$lv, function(g) isTRUE(g$bare),
                                            logical(1L)))) TRUE,
       levels = table_groups_levels(c, pack))
}

# The levels of a colour-split lane or pair: each level's geometry with its
# name, colour and value label.
table_groups_levels <- function(c, pack) {
  lapply(seq_along(c$lv), function(j) {
    g <- c$lv[[j]]
    c(list(name = as.character(c$levels[[j]]), color = as.character(c$fills[[j]])),
      pack(g),
      list(label = if (!is.null(g$disp)) {
        table_groups_arr(as.character(g$disp))
      }))
  })
}

table_groups_lane_pack <- function(g, kind) {
  geo <- if (identical(kind, "box")) {
    list(whisker_low = g$wl, whisker_low_width = g$w1, box_low = g$bl,
         box_width = g$bw, center = g$bc, box_high = g$b2,
         whisker_high_width = g$w2, whisker_high = g$wh)
  } else {
    list(center = g$c, inner_low = g$l, inner_width = g$rw)
  }
  out <- lapply(geo, table_groups_arr)
  # The dot style's fence band, only when the column has an outer range.
  if (!identical(kind, "box") && any(!is.na(g$ow))) {
    out$outer_low <- table_groups_arr(g$ol)
    out$outer_width <- table_groups_arr(g$ow)
  }
  out$n <- table_groups_arr(g$nn)
  out$tip <- table_groups_arr(table_groups_text(g$tip, escaped = TRUE))
  out$stats <- table_groups_stats(g$r)
  table_groups_compact(out)
}

# The statistics behind a glyph, in the data's units, for the hover card.
table_groups_stats <- function(r) {
  if (!length(r)) {
    return(NULL)
  }
  names <- c(bc = "center", bl = "low", bh = "high", wl = "whisker_low",
             wh = "whisker_high")
  out <- lapply(r, table_groups_arr)
  names(out) <- unname(names[names(r)])
  out
}

# The pair (dumbbell): the end columns, the segment between them, the band,
# and the reference line once per column.
table_groups_mark_pair <- function(c) {
  if (!isTRUE(c$multi)) {
    return(c(list(kind = "pair", ref = if (!is.na(c$rf)) c$rf,
                  fill = table_groups_arr(c$fill)),
             table_groups_pair_pack(c, FALSE)))
  }
  list(kind = "pair", ref = if (!is.na(c$rf)) c$rf,
       levels = table_groups_levels(c, function(g) {
         table_groups_pair_pack(g, TRUE)
       }))
}

table_groups_pair_pack <- function(g, multi) {
  out <- lapply(list(
    from = g$a, to = g$b, link_left = g$l, link_width = g$w,
    band_left = g$bl, band_width = g$bw, dash = g$dash, open = g$open
  ), table_groups_arr)
  out$tip <- table_groups_arr(table_groups_text(g$tip))
  if (multi) {
    out$ends <- table_groups_arr(table_groups_text(g$ab))
  }
  out
}

# The swimlane: per row a list of spans `[left, width, fill, label?]`, with
# the fill an index into `fills` (from 0), and one tooltip per span.
table_groups_mark_interval <- function(c) {
  spans <- lapply(c$segs, function(ss) {
    lapply(ss, function(sg) {
      out <- list(sg[[1L]], sg[[2L]], as.integer(sg[[3L]]) - 1L)
      if (length(sg) >= 4L) {
        out[[4L]] <- table_groups_plain(sg[[4L]])
      }
      out
    })
  })
  list(kind = "interval", spans = spans,
       tips = lapply(c$tips, function(tt) {
         table_groups_arr(table_groups_plain(tt))
       }),
       fills = table_groups_arr(as.character(c$fills)),
       large = if (isTRUE(c$lg)) TRUE)
}

# The sparkline: point strings "x,y x,y" in a 100 by 36 box (y grows
# downward), the band polygon, the end dot in percent, the x and y values of
# the points as comma-joined text for the hover readout, and the reference
# line and band as y coordinates in the same box.
table_groups_mark_spark <- function(c) {
  list(kind = "sparkline",
       line = table_groups_arr(as.character(c$pl)),
       band = table_groups_arr(c$bd),
       end_x = table_groups_arr(c$dx), end_y = table_groups_arr(c$dy),
       x_text = table_groups_arr(as.character(c$xs)),
       y_text = table_groups_arr(as.character(c$ys)),
       n = table_groups_arr(c$nn),
       ref_line = if (!is.na(c$rc)) c$rc,
       ref_band_top = if (!is.na(c$rby)) c$rby,
       ref_band_height = if (!is.na(c$rby)) c$rbh)
}
