# Swimlane segments: [left, width, fill-index] triples per row, plus a
# pre-escaped tooltip per segment. The domain is the observed x/xend range.
cells_interval <- function(ctx, p) {
  prep <- ctx$prep
  segs <- ctx$rows[[p$segs %||% ".segs"]]
  dom <- p$dom %||% prep$dom
  dd <- isTRUE(p$dom_date %||% prep$dom_date)
  bb <- cells_bounds(dom[[1L]], dom[[2L]])
  lv <- p$levels %||% prep$series
  tf <- as.character(p$tlabels %||% p$tfields %||% character())
  cvar <- chr_state(p$cvar)
  out_segs <- cells_interval_segs(segs, bb, dom[[2L]])
  out_tips <- cells_interval_tips(segs, lv, tf, cvar, dd)
  out_bl <- cells_interval_bar_labels(segs)
  out <- list(kind = "interval", segs = out_segs, tips = out_tips,
       fills = as.character(p$fills %||% prep$fills),
       d0 = round(dom[[1L]], 4L), d1 = round(dom[[2L]], 4L),
       dd = dd, lg = identical(p$size, "lg"), xl = identical(p$size, "xl"),
       lm = identical(p$lane_size, "md"),
       v = cells_sortv(ctx$rows[[p$key]] %||% ctx$rows$.v))
  if (!is.null(out_bl)) out$bl <- out_bl
  if (!is.null(p$ovl)) {
    out <- c(out, cells_interval_overlays(ctx$rows[[p$ovl]], segs, bb,
                                          c(p, list(d1_ = dom[[2L]])), dd))
  }
  if (!is.null(p$lanes)) {
    out <- c(out, cells_interval_lanes(ctx$rows[[p$lanes]], bb, lv, tf, cvar,
                                       dd, dom[[2L]], p$ename %||% "x"))
    # Without a record on the main lane anywhere, every lane is a full lane
    # and the overlays span the row.
    if (!any(lengths(segs) > 0L)) out$eq <- TRUE
  }
  out
}

# The thin lanes and strips in lane percent: `sl` per row a list of lanes,
# each its name `n`, segments `s` and their tips `t`, `o` where its open end's
# arrow sits (absent: closed), its strip boxes `b` and its events `e`
# [x, shape] with their tips `et`; `ms` per row the main lane's boxes. A box
# is [text, style].
cells_interval_lanes <- function(lanes, bb, lv, tf, cvar, dd, d1, ename) {
  box <- function(bx) lapply(bx, function(b) list(b$t, b$st))
  # Every lane of every row in one list, so their segments and tips are
  # computed together.
  subs <- lapply(lanes, function(r) r$sub)
  all <- unlist(subs, recursive = FALSE, use.names = FALSE)
  lsegs <- lapply(all, function(l) l$segs)
  s <- cells_interval_segs(lsegs, bb, d1)
  t <- cells_interval_tips(lsegs, lv, tf, cvar, dd)
  one <- lapply(seq_along(all), function(k) {
    l <- all[[k]]
    out <- list(n = l$nm, s = s[[k]], t = as.list(t[[k]]), b = box(l$bx))
    bl <- cells_interval_bar_labels(list(l$segs))
    if (!is.null(bl)) out$l <- bl[[1L]]
    if (length(l$ev)) {
      out$e <- lapply(l$ev, cells_event_tuple, bb = bb)
      out$et <- as.list(vapply(l$ev, cells_event_tip, "", ename = ename,
                               dd = dd))
    }
    if (isTRUE(l$op) && length(l$segs)) {
      out$o <- bb$pos(min(max(vapply(l$segs, function(sg) sg$e, 0)),
                             d1))[[1L]]
    }
    out
  })
  sl <- cells_regroup(one, lengths(subs))
  out <- list()
  if (any(lengths(sl) > 0L)) out$sl <- sl
  ms <- lapply(lanes, function(r) box(r$ms))
  if (any(lengths(ms) > 0L)) out$ms <- ms
  out
}

# A flat vector or list cut back into consecutive groups of `n` elements.
cells_regroup <- function(x, n) {
  end <- cumsum(n)
  lapply(seq_along(n), function(i) {
    x[seq.int(end[[i]] - n[[i]] + 1L, length.out = n[[i]])]
  })
}

# The overlays in lane percent, only the parts the row asked for: `tk` tick
# positions, `bd` line pieces [left, width], `ev` events [x, shape] with
# their tooltip in `et`, and `op` the open end's position (NA: closed).
cells_interval_overlays <- function(ovl, segs, bb, p, dd) {
  has <- function(nm) any(vapply(ovl, function(o) !is.null(o[[nm]]), NA))
  out <- list()
  if (has("tk")) {
    out$tk <- lapply(ovl, function(o) bb$pos(as.numeric(o$tk)))
  }
  if (has("bd")) {
    out$bd <- lapply(ovl, function(o) {
      lapply(o$bd, function(b) {
        list(bb$pos(b$s)[[1L]], bb$span(b$s, b$e)[[1L]])
      })
    })
  }
  if (has("ev")) {
    ename <- p$ename %||% "x"
    out$ev <- lapply(ovl, function(o) {
      lapply(o$ev, cells_event_tuple, bb = bb)
    })
    out$et <- lapply(ovl, function(o) {
      vapply(o$ev, cells_event_tip, character(1L), ename = ename, dd = dd)
    })
  }
  if (has("op")) {
    out$op <- vapply(seq_along(ovl), function(i) {
      ss <- segs[[i]]
      if (!isTRUE(ovl[[i]]$op) || !length(ss)) return(NA_real_)
      bb$pos(min(max(vapply(ss, function(sg) sg$e, numeric(1L))),
                 p$d1_ %||% Inf))[[1L]]
    }, numeric(1L))
  }
  out
}

# An event as the cells send it: [x, shape], with its tag when it has one.
cells_event_tuple <- function(e, bb) {
  out <- list(bb$pos(e$x)[[1L]], e$k)
  if (!is.null(e$tg)) out[[3L]] <- e$tg
  out
}

# An event's tooltip: its own text, else its level and position.
cells_event_tip <- function(e, ename, dd) {
  e$tp %||% paste0(e$lv %||% "Event", " \u00b7 ", ename, ": ",
                   cells_fmt_x(e$x, dd))
}

# Per row, each segment's bar label ("" for none), or NULL when no segment
# has one.
cells_interval_bar_labels <- function(segs) {
  has <- any(vapply(segs, function(ss) {
    any(vapply(ss, function(sg) !is.null(sg$bl), NA))
  }, NA))
  if (!has) return(NULL)
  lapply(segs, function(ss) {
    as.list(vapply(ss, function(sg) sg$bl %||% "", ""))
  })
}

# An x value as the card prints it: a date for a date domain.
cells_fmt_x <- function(v, dd) {
  if (dd) format(as.Date(v, origin = "1970-01-01")) else groups_fmt(v)
}

# Segment tuples [left, width, fill-index (, label (, style))] up to `d1`, the
# axis end an ongoing segment runs to; the optional
# 4th slot keys the same-event hover highlight (data-l), "" for none when a
# style follows. `segs` is one list of segments per row (or lane); the
# positions of all of them are computed in one pass.
cells_interval_segs <- function(segs, bb, d1) {
  all <- unlist(segs, recursive = FALSE, use.names = FALSE)
  start <- vapply(all, function(sg) sg$s, numeric(1L))
  end <- vapply(all, function(sg) sg$e, numeric(1L))
  # An ongoing record (no end, open) runs to the axis end.
  end[!is.finite(end)] <- d1
  left <- bb$pos(start)
  width <- bb$span(start, end)
  tuples <- lapply(seq_along(all), function(j) {
    sg <- all[[j]]
    base <- list(left[[j]], width[[j]], sg$f)
    if (!is.null(sg$st)) return(c(base, list(sg$lb %||% "", sg$st)))
    if (!is.null(sg$lb)) c(base, list(sg$lb)) else base
  })
  cells_regroup(tuples, lengths(segs))
}

# Tooltip: event label, colour level, span, then extra field pairs. One
# character vector per row (or lane) of `segs`, all built in one pass: the
# number formatting is the slow part, and it is vectorised.
cells_interval_tips <- function(segs, lv, tf, cvar, dd) {
  all <- unlist(segs, recursive = FALSE, use.names = FALSE)
  n <- lengths(segs)
  if (!length(all)) {
    return(lapply(n, function(i) character()))
  }
  start <- vapply(all, function(sg) sg$s, numeric(1L))
  end <- vapply(all, function(sg) sg$e, numeric(1L))
  label <- vapply(all, function(sg) sg$lb %||% NA_character_, character(1L))
  tip <- ifelse(is.na(label), "", paste0(label, " \u00b7 "))
  if (!is.null(lv)) {
    level <- lv[as.integer(vapply(all, function(sg) sg$f, numeric(1L)))]
    # The colour level is left out when the label already says it.
    said <- !is.na(label) & label == level
    tip <- paste0(tip, ifelse(said, "", paste0(
      if (!is.null(cvar)) paste0(cvar, " "), level, " \u00b7 "
    )))
  }
  tip <- paste0(
    tip, cells_fmt_x(start, dd), "\u2013",
    ifelse(is.finite(end), cells_fmt_x(end, dd), "ongoing")
  )
  tip <- paste0(tip, cells_interval_fields(all, tf))
  # A record with its own tooltip text says that instead.
  own <- vapply(all, function(sg) sg$tp %||% NA_character_, character(1L))
  tip[!is.na(own)] <- own[!is.na(own)]
  cells_regroup(tip, n)
}

# The extra field pairs of each segment's tooltip, " \u00b7 name: value" per
# field. A record without a value in a field leaves the field out.
cells_interval_fields <- function(all, tf) {
  out <- character(length(all))
  has <- !vapply(all, function(sg) is.null(sg$fv), logical(1L))
  if (!length(tf) || !any(has)) {
    return(out)
  }
  fv <- matrix(unlist(lapply(all[has], function(sg) sg$fv), use.names = FALSE),
               ncol = length(tf), byrow = TRUE)
  out[has] <- do.call(paste0, lapply(seq_along(tf), function(j) {
    ifelse(nzchar(fv[, j]), paste0(" \u00b7 ", tf[[j]], ": ", fv[, j]), "")
  }))
  out
}

# One inline SVG per cell, geometry pre-printed as point strings. viewBox
# 0 0 100 36 with 1 unit of vertical padding; y grows downward.
CELLS_SPARK_H <- 36
CELLS_SPARK_PAD <- 1

cells_sparkline <- function(ctx, p) {
  prep <- ctx$prep
  rows <- ctx$rows
  xd <- p$dom %||% prep$dom
  yd <- p$ydom %||% prep$ydom
  pts <- rows[[p$pts %||% ".pts"]]
  dd <- isTRUE(p$dom_date %||% prep$dom_date)
  per <- lapply(pts, function(p1) cells_spark_row(p1, xd, yd, dd))
  # The computed reference (p$ref): one center line and optional band per
  # column, as scalar coordinates.
  ref <- cells_spark_ref(p$ref, yd)
  # Sorts and labels by the last value; with a companion rank bar, `.v` carries
  # that bar's aggregate.
  last_y <- rows[[p$key %||% ".last"]] %||% rows$.last %||% rows$.v
  disp <- cells_val_str(last_y, cells_val_digits(last_y, yd[[1L]], yd[[2L]]))
  lab <- list(disp = disp, dw = max(c(1L, nchar(disp))))
  list(kind = "sparkline", pl = cells_pull(per, "pl"),
       bd = cells_pull(per, "bd"),
       dx = cells_pull(per, "dx"), dy = cells_pull(per, "dy"),
       xs = cells_pull(per, "xs"),
       ys = cells_pull(per, "ys"), rc = ref$rc, rby = ref$rby, rbh = ref$rbh,
       nn = vapply(pts, function(p1) length(p1$y), integer(1L)),
       v = cells_sortv(last_y)) |> c(lab)
}

cells_pull <- function(per, nm) {
  unlist(lapply(per, `[[`, nm), use.names = FALSE)
}

cells_spark_xf <- function(v, xd) {
  round((v - xd[[1L]]) / (xd[[2L]] - xd[[1L]]) * 100, 2L)
}

cells_spark_yf <- function(v, yd) {
  round(CELLS_SPARK_PAD + (CELLS_SPARK_H - 2 * CELLS_SPARK_PAD) *
          (1 - (v - yd[[1L]]) / (yd[[2L]] - yd[[1L]])), 2L)
}

# One row's polyline, band polygon, end point and the x / y strings the
# hover card prints.
cells_spark_row <- function(p1, xd, yd, dd) {
  n1 <- length(p1$y)
  if (!n1) {
    return(list(pl = "", bd = NA_character_, dx = NA_real_,
                dy = NA_real_, xs = "", ys = ""))
  }
  xs <- cells_spark_xf(p1$x, xd)
  ys <- cells_spark_yf(p1$y, yd)
  bd <- if (!is.null(p1$lo) && !is.null(p1$hi) &&
              all(is.finite(p1$lo)) && all(is.finite(p1$hi))) {
    paste0(
      paste0(xs, ",", cells_spark_yf(p1$lo, yd), collapse = " "), " ",
      paste0(rev(xs), ",", rev(cells_spark_yf(p1$hi, yd)), collapse = " ")
    )
  } else {
    NA_character_
  }
  list(
    pl = paste0(xs, ",", ys, collapse = " "), bd = bd,
    dx = xs[[n1]], dy = round(ys[[n1]] / CELLS_SPARK_H * 100, 2L),
    xs = paste(cells_fmt_x(p1$x, dd), collapse = ","),
    ys = paste(groups_fmt(p1$y), collapse = ",")
  )
}

# The reference line's centre and optional band as y coordinates, NA when
# the column has none.
cells_spark_ref <- function(ref, yd) {
  rc <- NA_real_
  rby <- NA_real_
  rbh <- NA_real_
  if (!is.null(ref) && is.finite(ref$center)) {
    rc <- cells_spark_yf(ref$center, yd)
    if (is.finite(ref$lo %||% NA_real_) &&
          is.finite(ref$hi %||% NA_real_)) {
      rby <- cells_spark_yf(ref$hi, yd)
      rbh <- round(cells_spark_yf(ref$lo, yd) - cells_spark_yf(ref$hi, yd), 2L)
    }
  }
  list(rc = rc, rby = rby, rbh = rbh)
}
