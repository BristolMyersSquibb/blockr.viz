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
       dd = dd, lg = identical(p$size, "lg"),
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
  sl <- lapply(lanes, function(r) {
    lapply(r$sub, function(l) {
      out <- list(n = l$nm, s = cells_interval_segs(list(l$segs), bb,
                                                    d1)[[1L]],
                  t = as.list(cells_interval_tips(list(l$segs), lv, tf, cvar,
                                                  dd)[[1L]]),
                  b = box(l$bx))
      bl <- cells_interval_bar_labels(list(l$segs))
      if (!is.null(bl)) out$l <- bl[[1L]]
      if (length(l$ev)) {
        out$e <- lapply(l$ev, function(e) list(bb$pos(e$x)[[1L]], e$k))
        out$et <- as.list(vapply(l$ev, function(e) {
          paste0(e$lv %||% "Event", " \u00b7 ", ename, ": ",
                 cells_fmt_x(e$x, dd))
        }, ""))
      }
      if (isTRUE(l$op) && length(l$segs)) {
        out$o <- bb$pos(min(max(vapply(l$segs, function(sg) sg$e, 0)),
                               d1))[[1L]]
      }
      out
    })
  })
  out <- list()
  if (any(lengths(sl) > 0L)) out$sl <- sl
  ms <- lapply(lanes, function(r) box(r$ms))
  if (any(lengths(ms) > 0L)) out$ms <- ms
  out
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
      lapply(o$ev, function(e) list(bb$pos(e$x)[[1L]], e$k))
    })
    out$et <- lapply(ovl, function(o) {
      vapply(o$ev, function(e) {
        paste0(e$lv %||% "Event", " \u00b7 ", ename, ": ",
               cells_fmt_x(e$x, dd))
      }, character(1L))
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
# style follows.
cells_interval_segs <- function(segs, bb, d1) {
  lapply(segs, function(ss) {
    lapply(ss, function(sg) {
      # An ongoing record (no end, open) runs to the axis end.
      e <- if (is.finite(sg$e)) sg$e else d1
      base <- list(bb$pos(sg$s)[[1L]], bb$span(sg$s, e)[[1L]], sg$f)
      if (!is.null(sg$st)) return(c(base, list(sg$lb %||% "", sg$st)))
      if (!is.null(sg$lb)) c(base, list(sg$lb)) else base
    })
  })
}

# Tooltip: event label, colour level, span, then extra field pairs.
cells_interval_tips <- function(segs, lv, tf, cvar, dd) {
  lapply(segs, function(ss) {
    vapply(ss, function(sg) {
      paste0(
        if (!is.null(sg$lb)) paste0(sg$lb, " \u00b7 "),
        # The colour level is left out when the label already says it.
        if (!is.null(lv) && !identical(sg$lb, lv[[sg$f]])) {
          paste0(if (!is.null(cvar)) paste0(cvar, " "), lv[[sg$f]], " \u00b7 ")
        },
        cells_fmt_x(sg$s, dd), "\u2013",
        if (is.finite(sg$e)) cells_fmt_x(sg$e, dd) else "ongoing",
        # A record without a value in a field leaves the field out.
        if (length(tf) && !is.null(sg$fv) && any(nzchar(sg$fv))) {
          k <- nzchar(sg$fv)
          paste0(" \u00b7 ", paste0(tf[k], ": ", sg$fv[k],
                                     collapse = " \u00b7 "))
        } else {
          ""
        }
      )
    }, character(1L))
  })
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
