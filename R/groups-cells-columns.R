# The cell model of the bar, split bar, tile and text columns.

cells_bar <- function(ctx, p) {
  vraw <- ctx$rows[[p$key]]
  lab <- cells_val_parts(ctx, p, vraw)
  v <- vraw
  if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
    v <- v / p$denom * 100
  }
  c(list(kind = "bar", w = cells_pct_w(v, p$dmax %||% ctx$mx),
         v = cells_sortv(v), fill = p$fill,
         # A child row's bar is drawn lighter.
         child = ctx$rows$.level > 0L), lab)
}

cells_tile <- function(ctx, p) {
  # The matrix cell. An empty count is an empty cell: no square, no "0".
  v <- ctx$rows[[p$key]]
  paint <- groups_tile_paint(p, ctx$rows)
  list(kind = "tile", v = cells_sortv(v), bg = paint$bg, fg = paint$fg,
       disp = ifelse(paint$has, groups_fmt(v), ""))
}

cells_barsplit <- function(ctx, p) {
  # Per-entry scale, like the plain bar (bar_max is 0 on the summaries path).
  emx <- p$dmax %||% ctx$mx
  mat <- cells_split_matrix(ctx, p)
  # `segv` stays in the measure's own unit; only the widths are scaled, against
  # the column max or the facet's N (`denom`).
  segv <- lapply(seq_along(p$series), function(j) mat[, j])
  if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
    mat <- mat / p$denom * 100
  }
  tot <- rowSums(mat)
  seg <- cells_split_seg(p, mat, tot, emx)
  vraw <- if (!is.null(p$key)) ctx$rows[[p$key]] else rowSums(
    vapply(segv, identity, numeric(ctx$n)))
  lab <- cells_split_label(ctx, p, segv, vraw)
  v <- if (is.null(p$key)) {
    tot
  } else {
    if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
      vraw / p$denom * 100
    } else {
      vraw
    }
  }
  fills <- as.character(p$fills %||%
                          unname(ctx$prep$palette[as.character(p$series)]))
  inside <- cells_split_inside(ctx, p, seg, segv)
  c(list(kind = "barsplit", mode = p$mode %||% "stacked",
         names = as.character(p$series),
         # The plan's own colours win, so a split bar and a split box agree on
         # which level is which.
         fills = fills,
         seg = seg, segv = segv,
         slab = inside, sink = if (!is.null(inside)) cells_ink(fills),
         v = cells_sortv(v)), lab)
}

# The row-by-series value matrix of a split bar; a series the rows do not
# carry is all zero, a non-finite value counts as zero.
cells_split_matrix <- function(ctx, p) {
  n <- ctx$n
  mat <- vapply(p$scols, function(cn) {
    x <- ctx$rows[[cn]]
    if (is.null(x)) rep(0, n) else as.numeric(x)
  }, numeric(n))
  mat <- matrix(mat, nrow = n, dimnames = list(NULL, p$series))
  mat[!is.finite(mat)] <- 0
  mat
}

# Segment widths: stacked scales the row total against the column max,
# percent normalises each row to 100, grouped scales each series on its own.
cells_split_seg <- function(p, mat, tot, emx) {
  if (identical(p$mode, "grouped")) {
    lapply(seq_along(p$series), function(j) cells_pct_w(mat[, j], emx))
  } else {
    scale <- if (identical(p$mode, "percent")) {
      ifelse(tot > 0, 100, 0)
    } else if (is.finite(emx) && emx > 0) {
      tot / emx * 100
    } else {
      tot * 0
    }
    share <- ifelse(tot > 0, 1, 0) * mat / ifelse(tot > 0, tot, 1)
    lapply(seq_along(p$series), function(j) round(share[, j] * scale, 2L))
  }
}

# Grouped: each level prints its own value (`ldisp`) in the column's one slot
# width. Stacked and 100% keep the one total.
cells_split_label <- function(ctx, p, segv, vraw) {
  if (identical(p$mode, "grouped")) {
    dom <- cells_axis_domain(p, ctx$prep)
    dig <- cells_val_digits(unlist(segv), dom$d0, dom$d1)
    ldisp <- lapply(segv, function(v) {
      ifelse(!is.na(v) & v > 0, cells_val_str(v, dig), "")
    })
    list(ldisp = ldisp,
         dw = max(c(1L, nchar(unlist(ldisp)))))
  } else {
    cells_val_parts(ctx, p, vraw)
  }
}

# Stacked / 100%: each segment carries its number inside it (the total stays
# at the end). The stylesheet hides it where the segment is too narrow.
cells_split_inside <- function(ctx, p, seg, segv) {
  if (!identical(p$mode, "grouped")) {
    if (identical(p$mode, "percent")) {
      lapply(seg, function(s) {
        ifelse(!is.na(s) & s > 0, paste0(round(s), "%"), "")
      })
    } else {
      dom <- cells_axis_domain(p, ctx$prep)
      dig <- cells_val_digits(unlist(segv), dom$d0, dom$d1)
      lapply(segv, function(v) {
        ifelse(!is.na(v) & v > 0, cells_val_str(v, dig), "")
      })
    }
  }
}

cells_bardiv <- function(ctx, p) {
  v <- ctx$rows[[p$key]]
  lab <- cells_val_parts(ctx, p, v, signed = TRUE)
  # Per-entry scale, like the plain bar (bar_max is 0 on the summaries path).
  dmx <- p$dmax %||% ctx$mx
  w <- if (is.finite(dmx) && dmx > 0) pmin(50, abs(v) / dmx * 50) else v * 0
  w[!is.finite(w)] <- 0
  c(list(kind = "bardiv", w = round(w, 2L), v = cells_sortv(v),
         pos = !is.na(v) & v >= 0), lab)
}

cells_text_raw <- function(ctx, p) {
  # The value is the display and the sort key.
  v <- as.character(ctx$rows[[p$key]])
  v[is.na(v)] <- ""
  c(list(kind = "num", text = TRUE, v = v, disp = v),
    if (!is.null(p$failed)) list(failed = TRUE))
}

cells_dist_text <- function(ctx, p) {
  # A distribution as text: "10.4 (7.6-13.2)". Sorts by the center, numerically.
  cc <- ctx$rows[[p$key]]
  ll <- ctx$rows[[p$lo]]
  hh <- ctx$rows[[p$hi]]
  disp <- ifelse(
    is.na(cc), "",
    paste0(groups_fmt(cc),
           ifelse(is.na(ll) | is.na(hh), "",
                  paste0(" (", groups_fmt(ll), "\u2013", groups_fmt(hh), ")")))
  )
  list(kind = "num", v = cells_sortv(cc), disp = disp)
}

cells_alt_text <- function(ctx, p) {
  # An expr summary that evaluated to text: sorted as text.
  v <- as.character(ctx$rows[[p$alt_text]])
  v[is.na(v)] <- ""
  list(kind = "num", text = TRUE, v = v, disp = v)
}

cells_num <- function(ctx, p) {
  prep <- ctx$prep
  v <- ctx$rows[[p$key]]
  parts <- cells_num_parts(v, denom = p$denom,
                               combined = isTRUE(p$combined),
                               signed = isTRUE(p$signed),
                               pct_only = isTRUE(p$pct_only),
                               fmt = if (!is.null(prep$format)) {
                                 function(x) dt_format_num(x, prep$format)
                               })
  c(list(kind = "num", v = cells_sortv(v)), parts)
}

#' The two parts of a numeric cell: the value string, and the percent string
#' when the column shows both.
#' @noRd
cells_num_parts <- function(v, denom = NULL, combined = FALSE,
                                signed = FALSE, pct_only = FALSE,
                                fmt = NULL) {
  if (isTRUE(signed)) {
    return(list(disp = ifelse(
      is.na(v), "",
      paste0(ifelse(v > 0, "+", ifelse(v < 0, "\u2212", "")),
             formatC(abs(v), format = "f", digits = 1L))
    )))
  }
  pct <- if (!is.null(denom) && is.finite(denom) && denom > 0) {
    v / denom * 100
  } else {
    NULL
  }
  if (isTRUE(pct_only)) {
    if (is.null(pct)) return(list(disp = rep("", length(v))))
    return(list(disp = ifelse(
      is.na(v), "", paste0(formatC(pct, format = "f", digits = 1L), "%")
    )))
  }
  # Four significant digits, the budget groups_fmt() prints a statistic at, so a
  # split bar's label and a box's centre read alike. "fg" keeps whole numbers
  # whole. `fmt`, when given, is the label's own precision
  # (cells_val_digits()).
  n <- ifelse(is.na(v), "", if (is.null(fmt)) {
    formatC(v, format = "fg", digits = 4L, big.mark = "")
  } else {
    fmt(v)
  })
  if (isTRUE(combined) && !is.null(pct)) {
    return(list(
      disp = n,
      pct = ifelse(is.na(v), "",
                   paste0("(", formatC(pct, format = "f", digits = 0L), "%)"))
    ))
  }
  list(disp = n)
}
