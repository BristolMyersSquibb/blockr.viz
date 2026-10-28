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
  out_segs <- cells_interval_segs(segs, bb)
  out_tips <- cells_interval_tips(segs, lv, tf, cvar, dd)
  list(kind = "interval", segs = out_segs, tips = out_tips,
       fills = as.character(p$fills %||% prep$fills),
       d0 = round(dom[[1L]], 4L), d1 = round(dom[[2L]], 4L),
       dd = dd, lg = identical(p$size, "lg"),
       v = cells_sortv(ctx$rows[[p$key]] %||% ctx$rows$.v))
}

# An x value as the card prints it: a date for a date domain.
cells_fmt_x <- function(v, dd) {
  if (dd) format(as.Date(v, origin = "1970-01-01")) else groups_fmt(v)
}

# Segment tuples [left, width, fill-index (, escaped label)]; the optional 4th
# slot keys the same-event hover highlight (data-l).
cells_interval_segs <- function(segs, bb) {
  lapply(segs, function(ss) {
    lapply(ss, function(sg) {
      base <- list(bb$pos(sg$s)[[1L]], bb$span(sg$s, sg$e)[[1L]], sg$f)
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
        if (!is.null(lv)) {
          paste0(if (!is.null(cvar)) paste0(cvar, " "), lv[[sg$f]], " \u00b7 ")
        },
        cells_fmt_x(sg$s, dd), "\u2013", cells_fmt_x(sg$e, dd),
        if (length(tf) && !is.null(sg$fv)) {
          paste0(" \u00b7 ", paste0(tf, ": ", sg$fv, collapse = " \u00b7 "))
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
