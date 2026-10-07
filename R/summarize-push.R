# Summarize table: cell model + data-push payload ------------------------------
#
# Same architecture as the table block's flat path (dev/table-data-push-design.md):
# ONE builder computes every per-column vector, and two consumers turn it into
# either markup or JSON.
#
# summarize_cells() -> the cell model (per-column widths / display strings /
#                          fills, plus the row meta and the <thead> tag)
# summarize_cells_html() -> pastes it into the historical <table> (the exported
# summarize_table(), the static / report path, the tests)
# summarize_flat_payload() -> emits it as the JSON cell model summarize-table.js
#                          assembles client-side
#
# The two MUST NOT drift: the JS assembler applies the same escaping rules
# htmltools does (& < > escaped, quotes left alone) and the same class /
# attribute order, and test-summarize-push.R pins one against the other.
#
# Why bother: server-rendered HTML for a 790-term AE table is 380-780 KB per
# render, ~93-95% of which is per-cell tag overhead. The cell model ships the
# widths and the display strings only (27-40 KB), and a payload cached per
# elem id re-renders a re-mounted dock panel with no R round trip.

#' The cell model: everything both consumers need, computed once.
#'
#' Per-column entries carry `kind` plus only the vectors that kind uses:
#'   bar      -> `w` (percent widths, NA = no value = no fill), `v` (sort
#'               values), `fill`, `sub`, and the in-bar label (`disp`/`pct`/
#'               `dw`) when the plan asks for one
#'   barsplit -> `seg` (list of per-series width vectors), `segv`, `fills`,
#'               `names`, `mode`, plus the in-bar label
#'   bardiv   -> `w`, `v`, `pos` (polarity), plus the signed in-bar label
#'   num      -> `disp` (display strings), `v`; `text` marks a left-aligned
#'               raw field column
#' Widths are already rounded to the 2dp both consumers print. `dw` is the
#' label slot's width in ch -- ONE number per column, computed here, so every
#' row reserves the same slot and the tracks stay aligned.
#' @noRd
summarize_cells <- function(prep, drill = NULL, active = NULL, cfg = NULL) {
  rows <- prep$rows
  plan <- prep$plan
  n <- nrow(rows)
  mx <- prep$bar_max

  # `v` is the sort key and the data-v attribute, nothing else, so full float
  # precision is pure payload: a faceted percentage like 23.734177215189873
  # costs 18 bytes per cell against 5 for 23.73. Rounded HERE so both consumers
  # print the same string and the drift guard stays green.
  sortv <- function(v) if (is.numeric(v)) round(v, 4L) else v

  # The table's Values switch (`value_labels`, default on): off drops every
  # value label beside a mark, whatever the mark. Number and text columns ARE
  # their values and are not touched.
  vals_on <- !isFALSE(cfg$value_labels)

  # NA stays NA: a no-value cell (the identity measure's absent facet) draws
  # NO fill at all, where 0 draws the visible zero-width sliver.
  # Parametric over the scale: summaries-path plan entries carry their own
  # domain (mixed marks must not share one), single-mark paths use the
  # prep-level scale.
  mk_pct_w <- function(e_mx) {
    function(v) {
      w <- if (is.finite(e_mx) && e_mx > 0) {
        pmax(0, pmin(100, abs(v) / e_mx * 100))
      } else {
        rep(0, length(v))
      }
      w[!is.finite(w)] <- 0
      w[is.na(v)] <- NA_real_
      round(w, 2L)
    }
  }
  pct_w <- mk_pct_w(mx)

  # The in-bar value label: the raw measure (never the width percentage),
  # with the counting measures' "(43%)" tail when the plan carries a base.
  val_parts <- function(p, vraw, signed = FALSE) {
    if (!vals_on || !isTRUE(p$show_val)) return(NULL)
    dom <- summarize_axis_domain(p, prep)
    dig <- summarize_val_digits(vraw, dom$d0, dom$d1)
    parts <- summarize_num_parts(vraw, denom = p$val_denom,
                                 combined = !is.null(p$val_denom),
                                 signed = signed,
                                 fmt = if (!is.na(dig)) {
                                   function(v) summarize_val_str(v, dig)
                                 })
    # formatC pads "fg" output to a common width; harmless in a collapsing
    # HTML cell but it would inflate the label slot -- trim before measuring.
    parts$disp <- trimws(parts$disp)
    len <- nchar(parts$disp) +
      if (is.null(parts$pct)) 0L else ifelse(nzchar(parts$pct),
                                             nchar(parts$pct) + 1L, 0L)
    c(parts, list(dw = max(c(1L, len))))
  }

  # Position/width on the lane marks' shared domain [bar_min, bar_max]: the
  # bar's pct_w() generalized to a domain that need not start at zero (a
  # box of change-from-baseline values has a negative lo). Positions and
  # widths are BOTH computed from raw values here and shipped rounded, so the
  # two consumers never subtract floats themselves (String(24.13 - 10.5) in
  # JS is not "13.63").
  mn <- prep$bar_min %||% 0
  mk_bounds <- function(e_mn, e_mx) {
    rng <- if (is.finite(e_mx) && is.finite(e_mn) && e_mx > e_mn) {
      e_mx - e_mn
    } else {
      NA_real_
    }
    list(
      pos = function(v) {
        w <- if (is.finite(rng)) (v - e_mn) / rng * 100 else rep(0, length(v))
        w[!is.finite(w) & !is.na(v)] <- 0
        w[is.na(v)] <- NA_real_
        round(pmax(0, pmin(100, w)), 2L)
      },
      span = function(a, b) {
        w <- if (is.finite(rng)) (b - a) / rng * 100 else rep(0, length(a))
        w[!is.finite(w) & !(is.na(a) | is.na(b))] <- 0
        w[is.na(a) | is.na(b)] <- NA_real_
        round(pmax(0, pmin(100, w)), 2L)
      }
    )
  }

  cols <- lapply(seq_along(plan), function(i) {
    p <- plan[[i]]
    # Per-entry scale, falling back to the prep-level one.
    b <- mk_bounds(p$dmin %||% mn, p$dmax %||% mx)
    pos_w <- b$pos
    span_w <- b$span
    if (identical(p$kind, "bar")) {
      vraw <- rows[[p$key]]
      lab <- val_parts(p, vraw)
      v <- vraw
      if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
        v <- v / p$denom * 100
      }
      c(list(kind = "bar", w = mk_pct_w(p$dmax %||% mx)(v), v = sortv(v),
             fill = p$fill, sub = rows$.level > 0L), lab)
    } else if (identical(p$kind, "barsplit")) {
      prefix <- p$prefix %||% ".s_"
      # Per-entry scale, like the plain bar: on the summaries path every
      # column owns its domain, and the prep-level bar_max is 0 there.
      emx <- p$dmax %||% mx
      mat <- vapply(p$series, function(lv) {
        x <- rows[[paste0(prefix, lv)]]
        if (is.null(x)) rep(0, n) else as.numeric(x)
      }, numeric(n))
      mat <- matrix(mat, nrow = n, dimnames = list(NULL, p$series))
      mat[!is.finite(mat)] <- 0
      # `segv` (the segment tooltips) stays in the measure's own unit; only
      # the WIDTHS are scaled -- against the column max, or the facet's own N
      # when the plan carries a width denominator.
      segv <- lapply(seq_along(p$series), function(j) mat[, j])
      if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
        mat <- mat / p$denom * 100
      }
      tot <- rowSums(mat)
      # Segment widths: stacked scales the row total against the column max,
      # percent normalises each row to 100, grouped scales each series
      # independently. Computed here so both consumers print the same numbers.
      seg <- if (identical(p$mode, "grouped")) {
        lapply(seq_along(p$series), function(j) mk_pct_w(emx)(mat[, j]))
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
      vraw <- if (!is.null(p$key)) rows[[p$key]] else rowSums(
        vapply(segv, identity, numeric(n)))
      # Grouped: the levels sit side by side, each its own bar, so each
      # prints its own value beside it (`ldisp`, one vector per level) in the
      # column's one slot width. Stacked and 100% keep the one total.
      lab <- if (identical(p$mode, "grouped") && vals_on &&
                   isTRUE(p$show_val)) {
        dom <- summarize_axis_domain(p, prep)
        dig <- summarize_val_digits(unlist(segv), dom$d0, dom$d1)
        ldisp <- lapply(segv, function(v) {
          ifelse(!is.na(v) & v > 0, summarize_val_str(v, dig), "")
        })
        list(ldisp = ldisp,
             dw = max(c(1L, nchar(unlist(ldisp)))))
      } else {
        val_parts(p, vraw)
      }
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
                              unname(prep$palette[as.character(p$series)]))
      # Stacked / 100%: each segment carries its own number INSIDE it (the
      # total stays at the end). The stylesheet hides one its segment is too
      # narrow for, the painter measures; the ink is white or dark against
      # the level's fill.
      inside <- if (!identical(p$mode, "grouped") && vals_on &&
                      isTRUE(p$show_val)) {
        if (identical(p$mode, "percent")) {
          lapply(seg, function(s) {
            ifelse(!is.na(s) & s > 0, paste0(round(s), "%"), "")
          })
        } else {
          dom <- summarize_axis_domain(p, prep)
          dig <- summarize_val_digits(unlist(segv), dom$d0, dom$d1)
          lapply(segv, function(v) {
            ifelse(!is.na(v) & v > 0, summarize_val_str(v, dig), "")
          })
        }
      }
      c(list(kind = "barsplit", mode = p$mode %||% "stacked",
             names = as.character(p$series),
             # The plan's own colours win: a summaries column resolves them
             # through the same resolver as every other mark, so a split bar
             # and a split box agree about which level is which.
             fills = fills,
             seg = seg, segv = segv,
             slab = inside, sink = if (!is.null(inside)) summarize_ink(fills),
             v = sortv(v)), lab)
    } else if (identical(p$kind, "bardiv")) {
      v <- rows[[p$key]]
      lab <- val_parts(p, v, signed = TRUE)
      # Per-entry scale, like the plain bar: on the summaries path every
      # column owns its domain and the prep-level bar_max is 0 there, so
      # reading it would zero every width.
      dmx <- p$dmax %||% mx
      w <- if (is.finite(dmx) && dmx > 0) pmin(50, abs(v) / dmx * 50) else v * 0
      w[!is.finite(w)] <- 0
      c(list(kind = "bardiv", w = round(w, 2L), v = sortv(v),
             pos = !is.na(v) & v >= 0), lab)
    } else if (p$kind %in% c("box", "pointrange")) {
      # The distribution lanes: every geometric number (positions AND widths)
      # rounded here; the emitters only print. See lane-prepare.R for the
      # cell's statistics and _blockr.design/open/lane-chart/spec.md for the
      # glyph.
      wd <- p$words %||% list(center = "Center", range = "Range")
      # Column-level, not row-level: TRUE when this column's mark draws NO
      # outer range, and so has nothing spanning the cell to be read
      # against. Those lanes keep a hairline; every other lane is bare,
      # because the whisker (or the dot style's fence band) IS the rail
      # (see LANE_DIST_STYLES). Derived from the leaf columns when the plan
      # does not say, so the lane chart's own marks follow the same rule.
      bare <- if (is.null(p$bare)) {
        !("wl" %in% names(p$cols %||% character()))
      } else {
        isTRUE(p$bare)
      }
      # One glyph from one set of stat columns. Called once for a plain
      # column, once per level for a colour-split one (`p$lcols`), so both
      # shapes ship exactly the same geometry per glyph.
      # The value label: the centre, at one precision for the whole column
      # (every colour level's centres together).
      lab_on <- vals_on && isTRUE(p$show_val)
      dig <- if (lab_on) {
        centres <- unlist(lapply(c(list(p$cols), p$lcols), function(cn) {
          if (!is.na(cn["bc"])) rows[[cn[["bc"]]]]
        }))
        summarize_val_digits(centres, p$dmin, p$dmax)
      }
      glyph <- function(cn, lvl = NULL) {
      # A missing stat column (the single-value "dot": a pointrange with no
      # interval) reads as all-NA, which the emitters draw as center-only.
      col_or_na <- function(nm) {
        if (!is.na(cn[nm]) && !is.null(rows[[cn[[nm]]]])) {
          rows[[cn[[nm]]]]
        } else {
          rep(NA_real_, nrow(rows))
        }
      }
      bc <- col_or_na("bc")
      bl <- col_or_na("bl")
      bh <- col_or_na("bh")
      nn <- col_or_na("n")
      lab <- if (lab_on) {
        disp <- summarize_val_str(bc, dig)
        list(disp = disp, dw = max(c(1L, nchar(disp))))
      }
      # A split glyph's tooltip names its level first: colour is never the
      # only thing saying which level this is.
      pre <- if (is.null(lvl)) "" else paste0(lvl, " \u00b7 ")
      wl <- col_or_na("wl")
      wh <- col_or_na("wh")
      # One tooltip for both styles: the pieces that are drawn, in the order
      # they are drawn. A piece that is off is absent, never named as
      # "none"; NA bounds (the n < 2 CI) say so rather than showing a
      # zero-width interval, which would read as certainty.
      clause <- function(word, lo, hi) {
        if (is.null(word)) return("")
        paste0(" \u00b7 ", word, " ",
               ifelse(is.na(lo) | is.na(hi), "undefined (n < 2)",
                      paste0(lane_fmt(lo), "\u2013", lane_fmt(hi))))
      }
      # The statistics themselves, for the hover card (summarize-table.js): the
      # geometry above is in percent of the lane and cannot be read back.
      raw_stats <- function() {
        st <- list(bc = bc, bl = bl, bh = bh, wl = wl, wh = wh)
        lapply(st[vapply(st, function(x) any(!is.na(x)), logical(1L))],
               function(x) round(x, 4L))
      }
      tip <- ifelse(is.na(bc), "", summarize_esc(paste0(
        pre, ifelse(is.na(nn), "", paste0("n=", nn, " \u00b7 ")),
        wd$center, " ", lane_fmt(bc),
        clause(wd$range, bl, bh), clause(wd$whisk, wl, wh)
      )))
      if (identical(p$kind, "box")) {
        # Whisker segments live OUTSIDE the body; a degenerate side (whisker
        # meets the box) ships NA and draws nothing.
        list(kind = "box", bare = bare,
             wl = pos_w(wl), w1 = span_w(wl, bl),
             bl = pos_w(bl), bw = span_w(bl, bh), bc = pos_w(bc),
             b2 = pos_w(bh), w2 = span_w(bh, wh), wh = pos_w(wh),
             nn = nn, tip = tip, v = sortv(bc),
             r = raw_stats()) |> c(lab)
      } else {
        # The dot style: the outer range as a fence band BEHIND the inner
        # bar (ol/ow), the inner range as the bar (l/rw), the centre as the
        # dot. Any of the three may be absent.
        list(kind = "pointrange", bare = bare,
             ol = pos_w(wl), ow = span_w(wl, wh),
             c = pos_w(bc), l = pos_w(bl), rw = span_w(bl, bh),
             nn = nn, tip = tip, v = sortv(bc),
             r = raw_stats()) |> c(lab)
      }
      }
      base <- glyph(p$cols)
      if (is.null(p$lcols)) {
        base
      } else {
        # Colour split: one lane per level inside the cell, in level order,
        # sharing the column's scale. The pooled glyph still supplies the
        # sort value; its own lane is not drawn. Each level carries its own
        # value label beside its lane; the column keeps one slot width.
        lv <- lapply(seq_along(p$lcols), function(j) {
          g <- glyph(p$lcols[[j]], p$levels[[j]])
          g$dw <- NULL
          g$kind <- NULL
          g$v <- NULL
          g
        })
        c(list(kind = p$kind, multi = TRUE, levels = as.character(p$levels),
               fills = as.character(p$fills), v = base$v, lv = lv),
          if (lab_on) list(dw = summarize_multi_dw(lv)))
      }
    } else if (identical(p$kind, "pair")) {
      # The dumbbell: `from` and `to` as positions, the segment between them
      # as [left, width] (either direction), the band per row, the reference
      # line once per column. Every number rounded here; emitters only print.
      dmn <- p$dmin %||% mn
      dmx <- p$dmax %||% mx
      wd <- p$words %||% list(from = "From", to = "To")
      fills <- as.character(p$fills %||% character())
      # The value label is the change from `from` to `to`, at one precision
      # for the whole column. A "+" only where the column also holds a
      # negative change: a column of durations reads "18", not "+18".
      lab_on <- vals_on && isTRUE(p$show_val)
      deltas <- unlist(lapply(c(list(p$cols), p$lcols), function(cn) {
        rows[[cn[["b"]]]] - rows[[cn[["a"]]]]
      }))
      dig <- if (lab_on) summarize_val_digits(deltas, p$dmin, p$dmax)
      signed <- any(deltas < 0, na.rm = TRUE)
      # One dumbbell from one set of end columns, like the glyph above:
      # once for a plain column, once per level for a colour-split one.
      dumbbell <- function(cn, didx, fill, lvl = NULL) {
        a <- rows[[cn[["a"]]]]
        b <- rows[[cn[["b"]]]]
        lo <- rows[[cn[["lo"]]]]
        hi <- rows[[cn[["hi"]]]]
        band <- !(is.na(lo) & is.na(hi))
        blo <- ifelse(is.na(lo), dmn, lo)
        bhi <- ifelse(is.na(hi), dmx, hi)
        both <- !is.na(a) & !is.na(b)
        di <- rows[[didx]]
        # A `to` outside the band is drawn open: the range is the question
        # the band asks, so the mark answers it without a tooltip.
        open <- band & !is.na(b) & (b < blo | b > bhi)
        delta <- b - a
        pre <- if (is.null(lvl)) "" else paste0(lvl, " \u00b7 ")
        tip <- ifelse(is.na(a) & is.na(b), "", summarize_esc(paste0(
          pre,
          ifelse(is.na(a), "", paste0(wd$from, " ", lane_fmt(a))),
          ifelse(both, " \u2192 ", ""),
          ifelse(is.na(b), "", paste0(wd$to, " ", lane_fmt(b))),
          ifelse(both, paste0(" (", ifelse(delta >= 0, "+", ""),
                              lane_fmt(delta), ")"), ""),
          ifelse(band, paste0(" \u00b7 range ",
                              ifelse(is.na(lo), "", lane_fmt(lo)), "\u2013",
                              ifelse(is.na(hi), "", lane_fmt(hi))), "")
        )))
        disp <- if (lab_on) {
          ifelse(both, paste0(ifelse(signed & delta >= 0, "+", ""),
                              summarize_val_str(delta, dig)), "")
        }
        list(kind = "pair",
             a = pos_w(a), b = pos_w(b),
             l = ifelse(both, pos_w(pmin(a, b)), NA_real_),
             w = ifelse(both, span_w(pmin(a, b), pmax(a, b)), NA_real_),
             bl = ifelse(band, pos_w(blo), NA_real_),
             bw = ifelse(band, span_w(blo, bhi), NA_real_),
             rf = if (!is.null(p$ref)) pos_w(p$ref) else NA_real_,
             fill = fill,
             dash = !is.na(di) & di > 1L,
             open = open, tip = tip, v = sortv(b),
             disp = disp, dw = if (lab_on) max(c(1L, nchar(disp))))
      }
      fi <- rows[[p$fidx]]
      base <- dumbbell(p$cols, p$didx, if (length(fills)) {
        ifelse(is.na(fi), NA_character_, fills[pmax(1L, fi)])
      } else {
        rep(NA_character_, n)
      })
      if (is.null(p$lcols)) {
        base
      } else {
        # Colour split: one dumbbell per level, in level order, on the
        # column's one scale; a level with no rows in the group draws none.
        # The pooled pair supplies the sort value. Each level carries its own
        # change beside its dumbbell; the column keeps one slot width.
        lv <- lapply(seq_along(p$lcols), function(j) {
          g <- dumbbell(p$lcols[[j]], p$ldidx[[j]], rep(fills[[j]], n),
                        p$levels[[j]])
          g[c("kind", "rf", "v", "dw")] <- NULL
          g
        })
        list(kind = "pair", multi = TRUE,
             levels = as.character(p$levels), fills = fills,
             rf = base$rf, v = base$v, lv = lv,
             dw = if (lab_on) summarize_multi_dw(lv))
      }
    } else if (identical(p$kind, "interval")) {
      # Swimlane segments: [left, width, fill-index] triples per row, plus a
      # pre-escaped tooltip per segment. The domain is the observed x/xend
      # range (per entry on the summaries path, prep-level otherwise),
      # not zero-based.
      segs <- rows[[p$segs %||% ".segs"]]
      dom <- p$dom %||% prep$dom
      dd <- isTRUE(p$dom_date %||% prep$dom_date)
      bb <- mk_bounds(dom[[1L]], dom[[2L]])
      fmt_d <- function(v) {
        if (dd) format(as.Date(v, origin = "1970-01-01")) else lane_fmt(v)
      }
      lv <- p$levels %||% prep$series
      tf <- as.character(p$tfields %||% character())
      # Segment tuples [left, width, fill-index (, escaped label)]: the
      # optional 4th slot keys the same-event hover highlight (data-l).
      out_segs <- lapply(segs, function(ss) {
        lapply(ss, function(sg) {
          base <- list(bb$pos(sg$s)[[1L]], bb$span(sg$s, sg$e)[[1L]], sg$f)
          if (!is.null(sg$lb)) c(base, list(summarize_esc(sg$lb))) else base
        })
      })
      # Tooltip: the event label headlines (chart-gantt parity), then the
      # colour level, the span, and any extra field pairs.
      out_tips <- lapply(segs, function(ss) {
        vapply(ss, function(sg) {
          summarize_esc(paste0(
            if (!is.null(sg$lb)) paste0(sg$lb, " \u00b7 "),
            if (!is.null(lv)) paste0(lv[[sg$f]], " \u00b7 "),
            fmt_d(sg$s), "\u2013", fmt_d(sg$e),
            if (length(tf) && !is.null(sg$fv)) {
              paste0(" \u00b7 ", paste0(tf, ": ", sg$fv, collapse = " \u00b7 "))
            } else {
              ""
            }
          ))
        }, character(1L))
      })
      list(kind = "interval", segs = out_segs, tips = out_tips,
           fills = as.character(p$fills %||% prep$fills),
           d0 = round(dom[[1L]], 4L), d1 = round(dom[[2L]], 4L),
           dd = dd, lg = identical(p$size, "lg"),
           v = sortv(rows[[p$key]] %||% rows$.v))
    } else if (identical(p$kind, "sparkline")) {
      # One inline SVG per cell, geometry PRE-PRINTED as point strings so the
      # emitters paste rather than format floats. viewBox 0 0 100 36 (taller
      # than the 12px lanes: amplitude is the point), a token 1 unit of
      # vertical padding -- the trajectory uses the full row; y grows
      # downward.
      H <- 36
      PAD <- 1
      xd <- p$dom %||% prep$dom
      yd <- p$ydom %||% prep$ydom
      xf <- function(v) round((v - xd[[1L]]) / (xd[[2L]] - xd[[1L]]) * 100, 2L)
      yf <- function(v) {
        round(PAD + (H - 2 * PAD) *
                (1 - (v - yd[[1L]]) / (yd[[2L]] - yd[[1L]])), 2L)
      }
      fmt_x <- function(v) {
        if (isTRUE(p$dom_date %||% prep$dom_date)) {
          format(as.Date(v, origin = "1970-01-01"))
        } else {
          lane_fmt(v)
        }
      }
      pts <- rows[[p$pts %||% ".pts"]]
      one_row <- function(p1) {
        n1 <- length(p1$y)
        if (!n1) {
          return(list(pl = "", bd = NA_character_, dx = NA_real_,
                      dy = NA_real_, xs = "", ys = ""))
        }
        xs <- xf(p1$x)
        ys <- yf(p1$y)
        bd <- if (!is.null(p1$lo) && !is.null(p1$hi) &&
                    all(is.finite(p1$lo)) && all(is.finite(p1$hi))) {
          paste0(
            paste0(xs, ",", yf(p1$lo), collapse = " "), " ",
            paste0(rev(xs), ",", rev(yf(p1$hi)), collapse = " ")
          )
        } else {
          NA_character_
        }
        list(
          pl = paste0(xs, ",", ys, collapse = " "), bd = bd,
          dx = xs[[n1]], dy = round(ys[[n1]] / H * 100, 2L),
          xs = paste(fmt_x(p1$x), collapse = ","),
          ys = paste(lane_fmt(p1$y), collapse = ",")
        )
      }
      per <- lapply(pts, one_row)
      pull <- function(nm) unlist(lapply(per, `[[`, nm), use.names = FALSE)
      # The computed reference (p$ref, from the series row's `ref` option):
      # ONE center line and optional dispersion band per COLUMN, printed
      # here as scalar coordinates so every cell draws the same reference.
      rc <- NA_real_
      rby <- NA_real_
      rbh <- NA_real_
      if (!is.null(p$ref) && is.finite(p$ref$center)) {
        rc <- yf(p$ref$center)
        if (is.finite(p$ref$lo %||% NA_real_) &&
              is.finite(p$ref$hi %||% NA_real_)) {
          rby <- yf(p$ref$hi)
          rbh <- round(yf(p$ref$lo) - yf(p$ref$hi), 2L)
        }
      }
      # The sparkline column always sorts (and labels) by the LAST value;
      # with a companion rank bar, `.v` carries that bar's aggregate instead.
      last_y <- rows[[p$key %||% ".last"]] %||% rows$.last %||% rows$.v
      lab <- if (vals_on && isTRUE(p$show_val)) {
        disp <- summarize_val_str(last_y, summarize_val_digits(
          last_y, yd[[1L]], yd[[2L]]
        ))
        list(disp = disp, dw = max(c(1L, nchar(disp))))
      }
      list(kind = "sparkline", pl = pull("pl"), bd = pull("bd"),
           dx = pull("dx"), dy = pull("dy"), xs = pull("xs"),
           ys = pull("ys"), rc = rc, rby = rby, rbh = rbh,
           nn = vapply(pts, function(p1) length(p1$y), integer(1L)),
           v = sortv(last_y)) |> c(lab)
    } else if (isTRUE(p$raw) && isTRUE(p$text)) {
      # A raw text field: the value IS the display (escaped once, here, so
      # both consumers paste it as-is), and the sort key is the text itself.
      v <- as.character(rows[[p$key]])
      v[is.na(v)] <- ""
      list(kind = "num", text = TRUE, v = v, disp = summarize_esc(v))
    } else if (identical(p$dispkey, "dist_text")) {
      # A distribution shown as TEXT: "10.4 (7.6–13.2)". Sorts numerically
      # by the center (a text sort would rank "9" above "10").
      cc <- rows[[p$key]]
      ll <- rows[[p$lo]]
      hh <- rows[[p$hi]]
      disp <- ifelse(
        is.na(cc), "",
        paste0(lane_fmt(cc),
               ifelse(is.na(ll) | is.na(hh), "",
                      paste0(" (", lane_fmt(ll), "\u2013", lane_fmt(hh), ")")))
      )
      list(kind = "num", v = sortv(cc), disp = disp)
    } else if (!is.null(p$alt_text) && is.null(rows[[p$key]])) {
      # An expr summary that evaluated to text: fall back to the text
      # column, sorted as text.
      v <- as.character(rows[[p$alt_text]])
      v[is.na(v)] <- ""
      list(kind = "num", text = TRUE, v = v, disp = summarize_esc(v))
    } else {
      v <- rows[[p$key]]
      parts <- summarize_num_parts(v, denom = p$denom,
                                   combined = isTRUE(p$combined),
                                   signed = isTRUE(p$signed),
                                   pct_only = isTRUE(p$pct_only))
      c(list(kind = "num", v = sortv(v)), parts)
    }
  })
  # What the hover card says about each column, beside the row's numbers:
  # its header and sub-line (the arm and its N, or the summary's name), the
  # measure a bar shows, the colour column a split names its levels by, the
  # percentage base, and the statistic words of a distribution.
  for (i in seq_along(cols)) {
    cols[[i]]$tt <- summarize_tip_meta(plan[[i]])
  }

  on <- summarize_drill_on(
    rows, summarize_chr1(prep$group), summarize_chr1(prep$parent),
    summarize_drill_filters(active$col, active$vals)
  )

  list(
    n = n,
    thead = summarize_thead(prep, sortable = isTRUE(cfg$sortable %||% TRUE),
                            cols = cols, axis = isTRUE(cfg$axis %||% TRUE)),
    ncol = length(plan) + 1L,
    nested = !is.null(prep$parent),
    label = as.character(rows$.label),      # PLAIN: each consumer escapes
    parent = as.character(rows$.parent),
    level = as.integer(rows$.level),
    parent_row = rows$.is_parent,
    on = on,
    pick = !is.null(drill),
    # The columns a row click claims, outer -> inner: a parent row claims the
    # first, a child row both (R/summarize-drill.R).
    path = c(summarize_chr1(prep$parent), summarize_chr1(prep$group)),
    cols = cols,
    fold = summarize_fold_text(prep),
    attrs = summarize_table_attrs(prep, cfg)
  )
}

#' The `<thead>` tag: the table block's own header cells (dt_th), so the two
#' blocks share one header object.
#'
#' The by_level facet layout (prep$facet_spans) grows a SPANNING first row:
#' the label column and the leading (pooled / field) columns span both rows,
#' each facet level spans its summary group, and the per-column sortable
#' cells move to the second row -- the Table-1 header. Everything else keeps
#' the single row.
#' @noRd
summarize_thead <- function(prep, sortable = TRUE, cols = NULL, axis = TRUE) {
  col_th <- function(p, i) {
    as.character(dt_th(
      p$label, i, label = p$sub_label,
      numeric = identical(p$kind, "num") && !isTRUE(p$text),
      sortable = sortable,
      extra = if (isTRUE(axis)) summarize_axis_strip(p, cols[[i]], prep)
    ))
  }
  stub <- as.character(dt_th(
    summarize_label_header(prep), 0L, stub = TRUE,
    label = prep$group_label, sortable = sortable
  ))

  fs <- prep$facet_spans
  if (is.null(fs)) {
    th <- c(stub, vapply(seq_along(prep$plan), function(i) {
      col_th(prep$plan[[i]], i)
    }, character(1L)))
    return(paste0("<thead><tr>", paste(th, collapse = ""), "</tr></thead>"))
  }

  span2 <- function(th) sub("^<th ", "<th rowspan=\"2\" ", th)
  lead_idx <- seq_len(fs$lead)
  row1 <- c(
    span2(stub),
    vapply(lead_idx, function(i) span2(col_th(prep$plan[[i]], i)),
           character(1L)),
    vapply(fs$groups, function(g) {
      paste0("<th class=\"blockr-col-header blockr-th-group\" colspan=\"",
             g$n, "\"><span class=\"blockr-col-name\">",
             summarize_esc(g$label), "</span></th>")
    }, character(1L))
  )
  row2 <- vapply(seq.int(fs$lead + 1L, length(prep$plan)), function(i) {
    col_th(prep$plan[[i]], i)
  }, character(1L))
  paste0("<thead><tr>", paste(row1, collapse = ""), "</tr><tr>",
         paste(row2, collapse = ""), "</tr></thead>")
}

#' The column axis: the glyph column's domain printed ONCE, in the header,
#' instead of a track repeated on every row. EVERY mark that sits on a scale
#' gets one, and they all read the same way: a box or a dot range against its
#' value domain, a bar against its column max, a difference bar against its
#' zero-centred one, a swimlane or a sparkline against its x domain (dates
#' printed as dates). Text and number columns have no scale, so no strip.
#'
#' The strip mirrors `.blockr-summarize-barwrap`'s flex geometry (a ticked span
#' that flexes, then the same fixed value slot in ch), so a tick sits exactly
#' over the position the mark uses: both are percentages of the SAME box. A
#' column whose cells carry no value label (the swimlane) has no slot to
#' reserve, and the strip spans the cell.
#' @noRd
summarize_axis_strip <- function(p, cl = NULL, prep = NULL) {
  dom <- summarize_axis_domain(p, prep)
  if (is.null(dom)) return(NULL)
  t <- if (isTRUE(dom$date)) {
    summarize_axis_date_ticks(dom$d0, dom$d1)
  } else {
    at <- summarize_axis_ticks(dom$d0, dom$d1)
    list(at = at, labels = lane_fmt(at))
  }
  if (length(t$at) < 2L) return(NULL)
  last <- length(t$at)
  # A percentage scale says so ONCE, on the last tick: "0 25 50 75 100%" is
  # the unit named without repeating it four times.
  if (isTRUE(dom$pct)) {
    t$labels[[last]] <- paste0(t$labels[[last]], "%")
  }
  pct <- (t$at - dom$d0) / (dom$d1 - dom$d0) * 100
  ticks <- lapply(seq_len(last), function(k) {
    # A label at the column EDGE anchors inward so it never overhangs; one
    # anywhere else stays centred on its tick. Keyed on the position, not on
    # the index: a domain whose last tick sits mid-column (a sparkline over
    # study days padded to the right) would otherwise print that label a
    # half-width left of where it means.
    cls <- if (pct[[k]] <= 8) {
      "is-first"
    } else if (pct[[k]] >= 92) {
      "is-last"
    }
    htmltools::tags$span(
      class = cls, style = paste0("left:", round(pct[[k]], 2L), "%"),
      t$labels[[k]]
    )
  })
  # `has-val`: the strip of a column whose cells carry a value label, so its
  # ticked span is the bar width's lane (`bar_width`, summarize-table-css.R). A
  # swimlane's spans the cell.
  htmltools::tags$div(
    class = paste0("blockr-summarize-axis", if (!is.null(cl$dw)) " has-val"),
    htmltools::tags$span(class = "blockr-summarize-axis-in", ticks),
    if (!is.null(cl$dw)) {
      htmltools::tags$span(class = "blockr-summarize-axis-pad",
                           style = paste0("width:", cl$dw, "ch"))
    }
  )
}

#' The domain a column's marks are drawn against, as the AXIS has to print it:
#' the same numbers `summarize_cells()` scales the geometry with, never
#' re-derived from the data. `NULL` for a column with no scale.
#' @noRd
summarize_axis_domain <- function(p, prep = NULL) {
  ok <- function(a, b) {
    length(a) && length(b) && is.finite(a) && is.finite(b) && b > a
  }
  kind <- p$kind %||% ""
  if (kind %in% c("box", "pointrange", "pair")) {
    if (!ok(p$dmin, p$dmax)) return(NULL)
    return(list(d0 = p$dmin, d1 = p$dmax))
  }
  if (kind %in% c("bar", "barsplit")) {
    # Length from a zero baseline: the width IS value / column max, so the
    # domain starts at zero whatever the data's minimum is. A 100% split
    # normalises every row, which makes the domain the percentage itself.
    d1 <- if (identical(p$mode, "percent")) 100 else p$dmax %||% prep$bar_max
    if (!ok(0, d1)) return(NULL)
    list(d0 = 0, d1 = d1,
         # A faceted percentage bar (`denom`) and the 100% split are read as
         # percentages; a count column is read in its own unit.
         pct = identical(p$mode, "percent") || !is.null(p$denom))
  } else if (identical(kind, "bardiv")) {
    # Zero-centred: the cell spans -max .. +max around the middle, which is
    # where the comparator sits.
    mx <- p$dmax %||% prep$bar_max
    if (!ok(0, mx)) return(NULL)
    list(d0 = -mx, d1 = mx)
  } else if (kind %in% c("interval", "sparkline")) {
    # The x domain, shared by the column: a swimlane's spans and a
    # sparkline's trajectory are both positioned along it.
    d <- p$dom %||% prep$dom
    if (length(d) < 2L || !ok(d[[1L]], d[[2L]])) return(NULL)
    list(d0 = d[[1L]], d1 = d[[2L]],
         date = isTRUE(p$dom_date %||% prep$dom_date))
  } else {
    NULL
  }
}

#' Nice tick values across a domain: the 1 / 2 / 2.5 / 5 / 10 ladder, picking
#' the step CLOSEST to an even split rather than the first one at least as
#' big (which drops to half the ticks whenever the domain sits just above a
#' rung). Ticks outside the padded domain are cut, never clamped onto the
#' edge, so every printed number is where it says it is.
#' @noRd
summarize_axis_ticks <- function(d0, d1, k = 4L) {
  raw <- (d1 - d0) / max(1L, k - 1L)
  if (!is.finite(raw) || raw <= 0) return(numeric())
  mag <- 10^floor(log10(raw))
  cand <- c(1, 2, 2.5, 5, 10) * mag
  step <- cand[[which.min(abs(cand - raw))]]
  t <- seq(ceiling(d0 / step) * step, d1, by = step)
  t[t >= d0 & t <= d1]
}

#' How many decimals a column's value labels print: one finer than its axis
#' ticks, so ticks every 10 read "17.7" and ticks every 0.5 read "2.25".
#' Whole numbers (counts) stay whole. `NA` when the column has no axis to
#' read the step off: those labels keep lane_fmt()'s significant digits.
#' `x` is every value the column prints, all colour levels together, so one
#' column prints one precision.
#' @noRd
summarize_val_digits <- function(x, d0 = NULL, d1 = NULL) {
  x <- x[is.finite(x)]
  if (length(x) && all(abs(x - round(x)) < 1e-9)) return(0L)
  if (!length(d0) || !length(d1) || !is.finite(d0) || !is.finite(d1)) {
    return(NA_integer_)
  }
  at <- summarize_axis_ticks(d0, d1)
  if (length(at) < 2L) return(NA_integer_)
  step <- at[[2L]] - at[[1L]]
  dig <- 0L
  while (dig < 6L && abs(round(step, dig) - step) > 1e-9 * max(1, step)) {
    dig <- dig + 1L
  }
  dig + 1L
}

#' A value label at `dig` decimals (summarize_val_digits()); `NA` digits fall
#' back to lane_fmt(). An NA value prints as "".
#' @noRd
summarize_val_str <- function(x, dig) {
  if (is.na(dig)) return(lane_fmt(x))
  out <- formatC(x, format = "f", digits = dig, big.mark = "")
  out[is.na(x)] <- ""
  out
}

#' The value slot of a colour-split column: one width for every level's
#' label, so the lanes of all levels and rows end at the same x.
#' @noRd
summarize_multi_dw <- function(lv) {
  max(c(1L, unlist(lapply(lv, function(g) nchar(g$disp %||% "")))))
}

#' The ink for a number printed ON a fill: white on a dark fill, the text
#' colour on a light one (the palette's yellow would swallow white).
#' @noRd
summarize_ink <- function(fills) {
  vapply(fills, function(f) {
    rgb <- tryCatch(grDevices::col2rgb(f)[, 1L] / 255,
                    error = function(e) c(0, 0, 0))
    lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    lum <- sum(c(0.2126, 0.7152, 0.0722) * lin)
    if (lum > 0.4) "#1f2937" else "#ffffff"
  }, character(1L), USE.NAMES = FALSE)
}

# The calendar ladder: step, its length in days (for picking) and the format a
# tick at that step is printed in. A date axis is only honest on calendar
# boundaries -- "every 30 days" drifts through the months -- so the step is a
# real unit and seq() walks it.
SUMMARIZE_DATE_STEPS <- list(
  list(by = "1 day", days = 1, fmt = "%d %b", unit = "day"),
  list(by = "2 days", days = 2, fmt = "%d %b", unit = "day"),
  list(by = "1 week", days = 7, fmt = "%d %b", unit = "week"),
  list(by = "2 weeks", days = 14, fmt = "%d %b", unit = "week"),
  list(by = "1 month", days = 30.44, fmt = "%b %Y", unit = "month"),
  list(by = "3 months", days = 91.3, fmt = "%b %Y", unit = "month"),
  list(by = "6 months", days = 182.6, fmt = "%b %Y", unit = "month"),
  list(by = "1 year", days = 365.25, fmt = "%Y", unit = "year"),
  list(by = "2 years", days = 730.5, fmt = "%Y", unit = "year"),
  list(by = "5 years", days = 1826.2, fmt = "%Y", unit = "year"),
  list(by = "10 years", days = 3652.5, fmt = "%Y", unit = "year")
)

#' Nice tick DATES across a day-numbered domain (a swimlane or a sparkline
#' over a Date column): the calendar ladder above, three ticks rather than
#' four because a date label is three times the width of a number.
#' @noRd
summarize_axis_date_ticks <- function(d0, d1, k = 3L) {
  raw <- (d1 - d0) / max(1L, k - 1L)
  if (!is.finite(raw) || raw <= 0) {
    return(list(at = numeric(), labels = character()))
  }
  st <- SUMMARIZE_DATE_STEPS[[which.min(abs(
    vapply(SUMMARIZE_DATE_STEPS, `[[`, numeric(1L), "days") - raw
  ))]]
  from <- summarize_axis_date_start(as.Date(d0, origin = "1970-01-01"), st)
  at <- seq(from, as.Date(ceiling(d1), origin = "1970-01-01"), by = st$by)
  at <- at[as.numeric(at) >= d0 & as.numeric(at) <= d1]
  list(at = as.numeric(at), labels = format(at, st$fmt))
}

#' The first tick: the earliest boundary of the step's unit at or after the
#' domain start, so the labels land on month firsts and January 1sts rather
#' than on whatever day the data happens to open.
#' @noRd
summarize_axis_date_start <- function(d, st) {
  if (identical(st$unit, "day")) return(d)
  if (identical(st$unit, "week")) {
    # Monday: the week boundary the ISO calendar (and every study calendar
    # built on it) uses.
    return(d + (8L - as.integer(format(d, "%u"))) %% 7L)
  }
  yr <- as.integer(format(d, "%Y"))
  if (identical(st$unit, "month")) {
    first <- as.Date(sprintf("%04d-%02d-01", yr, as.integer(format(d, "%m"))))
    return(if (first >= d) {
      first
    } else {
      seq(first, by = "1 month", length.out = 2L)[[2L]]
    })
  }
  jan <- as.Date(sprintf("%04d-01-01", yr))
  if (jan >= d) jan else as.Date(sprintf("%04d-01-01", yr + 1L))
}

#' The fold row's text, or NULL when nothing was capped. Never a silent
#' truncation: `top_n` always says what fell below the cut.
#' @noRd
summarize_fold_text <- function(prep) {
  if (!isTRUE(prep$folded > 0L)) return(NULL)
  paste0(
    "Other \u2014 ", prep$folded, " ",
    if (is.null(prep$parent)) "rows" else "groups", " below the cut",
    if (is.finite(prep$fold_max)) {
      paste0(", each with a value \u2264 ",
             format(prep$fold_max, scientific = FALSE, trim = TRUE))
    } else {
      ""
    }
  )
}

# --- consumer 1: markup ------------------------------------------------------

#' `expanded = TRUE` opens every nested row: the collapse is a dashboard
#' affordance (a chevron to click), and an EXPORT has nobody to click it --
#' a printed AE table that shows only its system organ classes has lost its
#' numbers. The default is unchanged, so the drift guard against the JS
#' assembler still compares like with like.
#' @noRd
summarize_cells_html <- function(m, expanded = FALSE) {
  cells <- lapply(m$cols, function(c) {
    if (identical(c$kind, "bar")) {
      paste0("<td class=\"blockr-summarize-bar-col\"",
             summarize_data_v(c$v), ">",
             summarize_barwrap(summarize_track_html(c$w, c$fill, c$sub), c),
             "</td>")
    } else if (identical(c$kind, "barsplit")) {
      paste0("<td class=\"blockr-summarize-bar-col\"",
             summarize_data_v(c$v), ">",
             summarize_barwrap(summarize_split_html(c), c), "</td>")
    } else if (identical(c$kind, "bardiv")) {
      paste0("<td class=\"blockr-summarize-bar-col\"",
             summarize_data_v(c$v), ">",
             summarize_barwrap(summarize_dv_html(c$w, c$pos), c), "</td>")
    } else if (c$kind %in% c("box", "pointrange")) {
      inner <- if (isTRUE(c$multi)) {
        summarize_multi_html(c)
      } else if (identical(c$kind, "box")) {
        summarize_box_html(c)
      } else {
        summarize_pr_html(c)
      }
      paste0("<td class=\"blockr-summarize-bar-col\"",
             summarize_data_v(c$v), ">",
             summarize_barwrap(inner, c), "</td>")
    } else if (identical(c$kind, "pair")) {
      inner <- if (isTRUE(c$multi)) {
        summarize_multi_html(c)
      } else {
        summarize_pair_html(c)
      }
      paste0("<td class=\"blockr-summarize-bar-col\"",
             summarize_data_v(c$v), ">",
             summarize_barwrap(inner, c), "</td>")
    } else if (identical(c$kind, "interval")) {
      paste0("<td class=\"blockr-summarize-bar-col",
             if (isTRUE(c$lg)) " blockr-summarize-wide" else "", "\"",
             summarize_data_v(c$v), ">",
             summarize_iv_html(c), "</td>")
    } else if (identical(c$kind, "sparkline")) {
      paste0("<td class=\"blockr-summarize-bar-col\"",
             summarize_data_v(c$v), ">",
             summarize_barwrap(summarize_sp_html(c), c), "</td>")
    } else if (isTRUE(c$text)) {
      paste0("<td class=\"blockr-summarize-txt\"", summarize_data_v(c$v), ">",
             c$disp, "</td>")
    } else {
      paste0("<td class=\"blockr-summarize-num dt-col-num\"",
             summarize_data_v(c$v), ">",
             c$disp,
             if (is.null(c$pct)) {
               ""
             } else {
               paste0(" <span class=\"blockr-summarize-pct\">", c$pct,
                      "</span>")
             },
             "</td>")
    }
  })

  chev <- paste0(
    "<button class=\"blockr-indent-btn\" type=\"button\" tabindex=\"-1\"",
    " aria-expanded=\"false\">", as.character(section_chevron_svg()), "</button>"
  )
  # Child rows sit 24px deeper than their parent, whose label cell has 24px
  # of padding on a nested table (summarize-table-css.R) so the chevron
  # hanging into it (margin-left:-18px) clears a clicked row's 3px bar.
  lbl <- paste0(
    "<td class=\"blockr-summarize-label-col blockr-stub",
    ifelse(m$parent_row, " blockr-has-toggle", ""), "\"",
    ifelse(m$level > 0L, " style=\"padding-left:48px;\"", ""), ">",
    ifelse(m$parent_row, chev, ""),
    "<span class=\"blockr-summarize-label\">", summarize_esc(m$label),
    "</span></td>"
  )
  tr <- paste0(
    "<tr class=\"blockr-summarize-row",
    ifelse(m$parent_row,
           if (isTRUE(expanded)) {
             " is-parent blockr-indent-toggle"
           } else {
             " is-parent blockr-indent-toggle collapsed"
           }, ""),
    ifelse(m$level > 0L,
           if (isTRUE(expanded)) " is-child" else " is-child collapsed-hidden",
           ""),
    if (isTRUE(m$pick)) " is-pick" else "",
    ifelse(m$on, " is-on", ""),
    "\" data-summarize-label=\"", summarize_esc(m$label), "\"",
    ifelse(m$level > 0L,
           paste0(" data-summarize-parent=\"", summarize_esc(m$parent), "\""),
           ""),
    " data-summarize-level=\"", m$level,
    # The order the row arrived in: the third header click sorts back to it,
    # so the configured order (visits in visit order) is never lost to a
    # stray click. Emitted by BOTH assemblers -- the drift test pins it.
    "\" data-summarize-ord=\"", seq_along(m$level) - 1L, "\">"
  )
  body <- paste0(tr, lbl, do.call(paste0, cells), "</tr>")
  fold <- if (is.null(m$fold)) {
    ""
  } else {
    paste0("<tr class=\"blockr-summarize-fold\"><td colspan=\"", m$ncol, "\">",
           summarize_esc(m$fold), "</td></tr>")
  }
  paste0(
    "<table class=\"blockr-table blockr-summarize-table\"",
    " data-summarize-nested=\"",
    if (isTRUE(m$nested)) "1" else "0", "\"", m$attrs, ">",
    m$thead, "<tbody>", paste(body, collapse = ""), fold, "</tbody></table>"
  )
}

#' The in-bar value label: track (or split / diff bar) left, the value in a
#' fixed-width right-aligned slot -- ONE width per column (`dw`, in ch), so
#' every row's track spans the same range and the bars stay comparable.
#' Columns without a label (explicit separate cols) pass through untouched.
#' @noRd
summarize_barwrap <- function(inner, c) {
  if (is.null(c$disp)) return(inner)
  paste0(
    "<div class=\"blockr-summarize-barwrap\">", inner,
    "<span class=\"blockr-summarize-barval\" style=\"width:", c$dw, "ch\">",
    c$disp,
    if (is.null(c$pct)) {
      ""
    } else {
      ifelse(nzchar(c$pct),
             paste0(" <span class=\"blockr-summarize-pct\">", c$pct, "</span>"),
             "")
    },
    "</span></div>"
  )
}

#' One bar: a track div plus a fill div, vectorised over the column. An NA
#' width (a cell with NO value, the identity measure's absent facet) and a
#' zero both render an empty track: the fill's 2px floor is for small values,
#' and on a zero it reads as a little.
#' @noRd
summarize_track_html <- function(w, fill = NULL, sub = FALSE) {
  sub <- rep_len(isTRUE(sub) | (is.logical(sub) & !is.na(sub) & sub), length(w))
  paste0(
    "<div class=\"blockr-summarize-track", ifelse(sub, " is-sub", ""), "\">",
    ifelse(is.na(w) | w <= 0, "", paste0(
      "<div class=\"blockr-summarize-fill\" style=\"width:",
      summarize_fmt_w(w), "%",
      if (!is.null(fill)) paste0(";background:", fill) else "", "\"></div>"
    )),
    "</div>"
  )
}

#' @noRd
summarize_split_html <- function(c) {
  n <- length(c$v)
  k <- length(c$names)
  if (!n || !k) return(rep("<div class=\"blockr-summarize-track\"></div>", n))
  grouped <- identical(c$mode, "grouped")
  seg <- vapply(seq_len(k), function(j) {
    # A stacked segment's own number, inside it (see summarize_cells()).
    lab <- if (!grouped && !is.null(c$slab)) c$slab[[j]] else rep("", n)
    body <- paste0(
      "<div class=\"blockr-summarize-fill", ifelse(nzchar(lab), " has-lab", ""),
      "\" style=\"width:",
      summarize_fmt_w(c$seg[[j]]),
      "%;background:", c$fills[[j]], "\" data-summarize-tip=\"",
      summarize_esc(c$names[[j]]), ": ", c$segv[[j]], "\">",
      ifelse(nzchar(lab), paste0(
        "<span class=\"blockr-summarize-seglab w", nchar(lab),
        "\" style=\"color:", c$sink[[j]], "\">", lab, "</span>"
      ), ""),
      "</div>"
    )
    has <- !is.na(c$segv[[j]]) & c$segv[[j]] > 0
    if (grouped && !is.null(c$ldisp)) {
      # With value labels each level is its own barwrap: a thin track, then
      # the level's value in the column's one slot width.
      ifelse(has, paste0(
        "<div class=\"blockr-summarize-lv blockr-summarize-barwrap\">",
        "<div class=\"blockr-summarize-track is-lv\">", body, "</div>",
        "<span class=\"blockr-summarize-barval\" style=\"width:", c$dw,
        "ch\">", c$ldisp[[j]], "</span></div>"
      ), "")
    } else if (grouped) {
      # A level with no value in this row gets no track, as the lollipop
      # draws no lane for it.
      ifelse(has, paste0("<div class=\"blockr-summarize-row3\">", body,
                         "</div>"), "")
    } else {
      ifelse(has, body, "")
    }
  }, character(n))
  seg <- matrix(seg, nrow = n)
  if (grouped && !is.null(c$ldisp)) {
    return(paste0("<div class=\"blockr-summarize-multi\">",
                  apply(seg, 1L, paste0, collapse = ""), "</div>"))
  }
  paste0("<div class=\"blockr-summarize-track", if (grouped) " is-tall" else "",
         "\">", apply(seg, 1L, paste0, collapse = ""), "</div>")
}

#' @noRd
summarize_dv_html <- function(w, pos) {
  ifelse(
    is.na(w) | w <= 0,
    "<div class=\"blockr-summarize-dv\"></div>",
    paste0(
      "<div class=\"blockr-summarize-dv\"><div class=\"blockr-summarize-fill ",
      ifelse(pos, "is-pos", "is-neg"), "\" style=\"width:", summarize_fmt_w(w),
      "%\"></div></div>"
    )
  )
}

# --- lane mark emitters -------------------------------------------------------
# Each of these has a byte-identical twin in summarize-table.js (boxHtml /
# prHtml / ivHtml / spHtml); test-summarize-push.R pins the pair. Emission
# conditions key on shipped NA/null values, never on re-derived arithmetic, so
# the two consumers cannot disagree about what to draw.

#' Box cell: two whisker segments (never through the body), caps, IQR body,
#' median tick, all absolutely positioned in a track-coloured lane.
#' @noRd
summarize_box_html <- function(c) {
  n <- length(c$bc)
  cls <- if (isTRUE(c$bare)) " is-bare" else ""
  vapply(seq_len(n), function(i) {
    if (is.na(c$bc[[i]])) {
      return(paste0(
        "<div class=\"blockr-summarize-lane blockr-summarize-boxcell", cls,
        "\"></div>"
      ))
    }
    paste0(
      "<div class=\"blockr-summarize-lane blockr-summarize-boxcell", cls,
      "\" data-summarize-tip=\"", c$tip[[i]], "\">",
      if (!is.na(c$w1[[i]])) {
        paste0("<i class=\"lane-wh\" style=\"left:", summarize_fmt_w(c$wl[[i]]),
               "%;width:", summarize_fmt_w(c$w1[[i]]), "%\"></i>")
      } else {
        ""
      },
      if (!is.na(c$w2[[i]])) {
        paste0("<i class=\"lane-wh\" style=\"left:", summarize_fmt_w(c$b2[[i]]),
               "%;width:", summarize_fmt_w(c$w2[[i]]), "%\"></i>")
      } else {
        ""
      },
      if (!is.na(c$w1[[i]])) {
        paste0("<i class=\"lane-cap\" style=\"left:",
               summarize_fmt_w(c$wl[[i]]), "%\"></i>")
      } else {
        ""
      },
      if (!is.na(c$w2[[i]])) {
        paste0("<i class=\"lane-cap\" style=\"left:",
               summarize_fmt_w(c$wh[[i]]), "%\"></i>")
      } else {
        ""
      },
      if (!is.na(c$bw[[i]])) {
        paste0("<i class=\"lane-box\" style=\"left:",
               summarize_fmt_w(c$bl[[i]]), "%;width:",
               summarize_fmt_w(c$bw[[i]]), "%\"></i>")
      } else {
        ""
      },
      "<i class=\"lane-med\" style=\"left:", summarize_fmt_w(c$bc[[i]]),
      "%\"></i></div>"
    )
  }, character(1L))
}

#' Dot-style distribution cell: the outer range as a pale fence band, the
#' inner range as a rounded bar on top of it, the centre as a ringed dot.
#' Each piece draws only when its columns arrived, so the same emitter
#' covers the full dot range, the plain point range and the bare dot. NA
#' bounds (the n < 2 CI) draw the centre alone -- never a zero-width
#' interval, which would read as certainty.
#' @noRd
summarize_pr_html <- function(c) {
  n <- length(c$c)
  cls <- if (isTRUE(c$bare)) " is-bare" else ""
  vapply(seq_len(n), function(i) {
    if (is.na(c$c[[i]])) {
      return(paste0(
        "<div class=\"blockr-summarize-lane blockr-summarize-prcell", cls,
        "\"></div>"
      ))
    }
    paste0(
      "<div class=\"blockr-summarize-lane blockr-summarize-prcell", cls,
      "\" data-summarize-tip=\"", c$tip[[i]], "\">",
      if (!is.null(c$ow) && !is.na(c$ow[[i]])) {
        paste0("<i class=\"lane-fence\" style=\"left:",
               summarize_fmt_w(c$ol[[i]]), "%;width:",
               summarize_fmt_w(c$ow[[i]]), "%\"></i>")
      } else {
        ""
      },
      if (!is.na(c$rw[[i]])) {
        paste0("<i class=\"lane-rng\" style=\"left:", summarize_fmt_w(c$l[[i]]),
               "%;width:", summarize_fmt_w(c$rw[[i]]), "%\"></i>")
      } else {
        ""
      },
      "<i class=\"lane-ctr\" style=\"left:", summarize_fmt_w(c$c[[i]]),
      "%\"></i></div>"
    )
  }, character(1L))
}

#' The pair cell: band, reference line, the linking segment, then the two
#' marks, in that order so the marks paint on top. Byte-identical to
#' pairHtml() in summarize-table.js.
#' @noRd
summarize_pair_html <- function(c) {
  n <- length(c$a)
  pos <- function(v) paste0("left:", summarize_fmt_w(v), "%")
  vapply(seq_len(n), function(i) {
    fill <- c$fill[[i]]
    paste0(
      "<div class=\"blockr-summarize-lane blockr-summarize-pacell",
      if (isTRUE(c$dash[[i]])) " is-dash" else "", "\"",
      if (!is.na(fill)) {
        paste0(" style=\"--blockr-summarize-fill:", fill, "\"")
      } else {
        ""
      },
      if (nzchar(c$tip[[i]])) {
        paste0(" data-summarize-tip=\"", c$tip[[i]], "\"")
      } else {
        ""
      },
      ">",
      if (!is.na(c$bw[[i]])) {
        paste0("<i class=\"lane-band\" style=\"", pos(c$bl[[i]]), ";width:",
               summarize_fmt_w(c$bw[[i]]), "%\"></i>")
      } else {
        ""
      },
      if (!is.na(c$rf)) {
        paste0("<i class=\"lane-ref\" style=\"", pos(c$rf), "\"></i>")
      } else {
        ""
      },
      if (!is.na(c$w[[i]])) {
        paste0("<i class=\"lane-link\" style=\"", pos(c$l[[i]]), ";width:",
               summarize_fmt_w(c$w[[i]]), "%\"></i>")
      } else {
        ""
      },
      if (!is.na(c$a[[i]])) {
        paste0("<i class=\"lane-from\" style=\"", pos(c$a[[i]]), "\"></i>")
      } else {
        ""
      },
      if (!is.na(c$b[[i]])) {
        paste0("<i class=\"lane-to", if (isTRUE(c$open[[i]])) " is-open" else "",
               "\" style=\"", pos(c$b[[i]]), "\"></i>")
      } else {
        ""
      },
      "</div>"
    )
  }, character(1L))
}

#' A colour-split cell (a distribution glyph or a pair): one lane per level,
#' in level order, each in the level's colour and each carrying its own
#' tooltip. A level
#' with no rows in this group draws NO lane, so a table grouped by subject
#' (where every row belongs to exactly one level) reads as one coloured
#' glyph per row rather than one glyph and a gap.
#'
#' The colour rides as a CSS custom property on the wrapper, so the glyph
#' emitters are reused untouched -- every part of a box (whiskers, caps,
#' body, median) already paints from `--blockr-summarize-fill`.
#' @noRd
summarize_multi_html <- function(c) {
  parts <- lapply(seq_along(c$lv), function(j) {
    g <- c$lv[[j]]
    if (identical(c$kind, "pair")) {
      g$rf <- c$rf
      html <- summarize_pair_html(g)
      key <- ifelse(is.na(g$a) & is.na(g$b), NA_real_, 0)
    } else {
      html <- if (identical(c$kind, "box")) summarize_box_html(g) else
        summarize_pr_html(g)
      key <- if (identical(c$kind, "box")) g$bc else g$c
    }
    # With value labels on, each level is its own barwrap: the lane, then
    # the level's number in the column's one slot width, so the numbers
    # stack beside the lanes they belong to.
    lab <- !is.null(g$disp) && !is.null(c$dw)
    ifelse(is.na(key), "",
           paste0("<div class=\"blockr-summarize-lv",
                  if (lab) " blockr-summarize-barwrap", "\"",
                  " style=\"--blockr-summarize-fill:", c$fills[[j]], "\">",
                  html,
                  if (lab) {
                    paste0("<span class=\"blockr-summarize-barval\"",
                           " style=\"width:", c$dw, "ch\">", g$disp,
                           "</span>")
                  } else {
                    ""
                  },
                  "</div>"))
  })
  paste0("<div class=\"blockr-summarize-multi\">",
         do.call(paste0, parts), "</div>")
}

#' Interval cell: the swimlane. One segment per (x, xend) span, coloured by
#' fill index; the domain bounds ride as data attributes for the hover
#' readout (approximate day under the cursor).
#' @noRd
summarize_iv_html <- function(c) {
  n <- length(c$v)
  vapply(seq_len(n), function(i) {
    segs <- c$segs[[i]]
    paste0(
      "<div class=\"blockr-summarize-lane blockr-summarize-ivcell\" data-d0=\"",
      summarize_fmt_n(c$d0), "\" data-d1=\"", summarize_fmt_n(c$d1), "\"",
      if (isTRUE(c$dd)) " data-dd=\"1\"" else "", ">",
      paste0(vapply(seq_along(segs), function(j) {
        sg <- segs[[j]]
        paste0("<i class=\"lane-seg\" style=\"left:", summarize_fmt_w(sg[[1L]]),
               "%;width:", summarize_fmt_w(sg[[2L]]), "%;background:",
               c$fills[[sg[[3L]]]], "\"",
               if (length(sg) >= 4L) {
                 paste0(" data-l=\"", sg[[4L]], "\"")
               } else {
                 ""
               },
               " data-tip=\"", c$tips[[i]][[j]],
               "\"></i>")
      }, character(1L)), collapse = ""),
      "</div>"
    )
  }, character(1L))
}

#' Sparkline cell: one inline SVG (band polygon under a polyline) plus a
#' last-value dot. The geometry arrives as pre-printed point strings; the raw
#' x/y display values ride as data attributes for the hover readout.
#' @noRd
summarize_sp_html <- function(c) {
  n <- length(c$v)
  vapply(seq_len(n), function(i) {
    paste0(
      "<div class=\"blockr-summarize-lane blockr-summarize-spcell\" data-xs=\"",
      c$xs[[i]], "\" data-ys=\"", c$ys[[i]], "\">",
      "<svg viewBox=\"0 0 100 36\" preserveAspectRatio=\"none\">",
      if (!is.na(c$rby)) {
        paste0("<rect class=\"lane-refband\" x=\"0\" y=\"",
               summarize_fmt_w(c$rby), "\" width=\"100\" height=\"",
               summarize_fmt_w(c$rbh), "\"></rect>")
      } else {
        ""
      },
      if (!is.na(c$bd[[i]])) {
        paste0("<polygon class=\"lane-band\" points=\"", c$bd[[i]],
               "\"></polygon>")
      } else {
        ""
      },
      if (!is.na(c$rc)) {
        paste0("<line class=\"lane-refline\" x1=\"0\" y1=\"",
               summarize_fmt_w(c$rc), "\" x2=\"100\" y2=\"",
               summarize_fmt_w(c$rc),
               "\" vector-effect=\"non-scaling-stroke\"></line>")
      } else {
        ""
      },
      if (nzchar(c$pl[[i]])) {
        paste0("<polyline class=\"lane-ln\" points=\"", c$pl[[i]],
               "\" vector-effect=\"non-scaling-stroke\"></polyline>")
      } else {
        ""
      },
      "</svg>",
      if (!is.na(c$dx[[i]])) {
        paste0("<i class=\"lane-dot\" style=\"left:",
               summarize_fmt_w(c$dx[[i]]), "%;top:",
               summarize_fmt_w(c$dy[[i]]), "%\"></i>")
      } else {
        ""
      },
      "</div>"
    )
  }, character(1L))
}

# Per-element formatting: format() would align decimals across the vector
# ("100.00" beside "57.14"), while the JS assembler prints String(n) = "100".
# as.character() on a rounded value matches it exactly -- the same reason
# dt_bar_style() avoids format() for the table block's data bars.
#' @noRd
summarize_fmt_w <- function(w) {
  as.character(w)
}

# Large-magnitude numbers (the interval domain: dates as days or seconds):
# as.character() goes scientific at 1e5 where JS String() never does. Values
# are rounded to <= 4 decimals at the source, so digits = 15 prints them
# exactly, matching String(n).
#' @noRd
summarize_fmt_n <- function(x) {
  format(x, scientific = FALSE, trim = TRUE, digits = 15L)
}

# --- consumer 2: JSON payload ------------------------------------------------

#' Emit the cell model as the `flat` payload summarize-table.js assembles.
#'
#' `I()` keeps every per-row vector a JSON array even at length 1 (auto_unbox
#' would collapse a one-row table's columns to scalars, and the JS assembler
#' indexes them).
#' The hover card's column description (see summarize_cells()). NULL for the
#' columns whose cell already prints everything (numbers, text).
#' @noRd
summarize_tip_meta <- function(p) {
  if (identical(p$kind, "num")) return(NULL)
  den <- p$val_denom
  out <- list(
    head = summarize_chr1(p$label),
    sub = summarize_chr1(p$sub_label),
    meas = summarize_chr1(p$meas),
    cvar = summarize_chr1(p$cvar),
    den = if (is.numeric(den) && length(den) == 1L && is.finite(den) &&
                den > 0) den,
    words = p$words
  )
  out[!vapply(out, is.null, logical(1L))]
}

#' @noRd
summarize_flat_payload <- function(m) {
  arr <- function(x) I(unname(x))
  # A vector that is constant-false (or all-NA) carries no information: 4-5 KB
  # per column at 790 rows for `sub` / `on` / `parent_row` on a flat table.
  # Omitted entirely; the assembler reads an absent vector as all-false.
  arr_if <- function(x) if (any(x, na.rm = TRUE)) arr(x) else NULL
  one <- function(c) {
    out <- list(kind = c$kind)
    if (identical(c$kind, "num")) {
      out$disp <- arr(as.character(c$disp))
      if (!is.null(c$pct)) out$pct <- arr(as.character(c$pct))
      if (isTRUE(c$text)) out$text <- TRUE
    } else if (identical(c$kind, "barsplit")) {
      out$mode <- c$mode
      out$names <- arr(as.character(c$names))
      out$fills <- arr(as.character(c$fills))
      out$seg <- lapply(c$seg, arr)
      out$segv <- lapply(c$segv, arr)
      if (!is.null(c$ldisp)) {
        out$ldisp <- lapply(c$ldisp, function(x) arr(as.character(x)))
        out$dw <- c$dw
      }
      if (!is.null(c$slab)) {
        out$slab <- lapply(c$slab, function(x) arr(as.character(x)))
        out$sink <- arr(as.character(c$sink))
      }
    } else if (identical(c$kind, "bardiv")) {
      out$w <- arr(c$w)
      out$pos <- arr(c$pos)
    } else if (c$kind %in% c("box", "pointrange")) {
      geom <- if (identical(c$kind, "box")) {
        c("wl", "w1", "bl", "bw", "bc", "b2", "w2", "wh", "nn")
      } else {
        c("c", "l", "rw", "nn")
      }
      pack <- function(g) {
        o <- list()
        for (nm in geom) o[[nm]] <- arr(g[[nm]])
        # The dot style's fence band: shipped only when the column HAS an
        # outer range, so a mark without one (the plain point range, the
        # single-value dot) keeps the payload it always had.
        if (!identical(c$kind, "box") && any(!is.na(g$ow))) {
          o$ol <- arr(g$ol)
          o$ow <- arr(g$ow)
        }
        o$tip <- arr(as.character(g$tip))
        if (isTRUE(c$multi) && !is.null(g$disp)) {
          o$disp <- arr(as.character(g$disp))
        }
        # The statistics, for the hover card: one array per stat that exists.
        if (length(g$r)) o$r <- lapply(g$r, arr)
        # Column-level, and only when true: the assembler reads an absent
        # flag as "this mark draws its own rail".
        if (isTRUE(g$bare)) o$bare <- TRUE
        o
      }
      if (isTRUE(c$multi)) {
        # A colour-split column ships one geometry set per level plus the
        # level names and their fills; the assembler walks them in order.
        out$multi <- TRUE
        out$levels <- arr(as.character(c$levels))
        out$fills <- arr(as.character(c$fills))
        out$lv <- lapply(c$lv, pack)
      } else {
        out <- c(out, pack(c))
      }
    } else if (identical(c$kind, "pair")) {
      pack_pair <- function(g) {
        o <- list()
        for (nm in c("a", "b", "l", "w", "bl", "bw")) o[[nm]] <- arr(g[[nm]])
        o$fill <- arr(g$fill)
        o$dash <- arr(g$dash)
        o$open <- arr(g$open)
        o$tip <- arr(as.character(g$tip))
        if (isTRUE(c$multi) && !is.null(g$disp)) {
          o$disp <- arr(as.character(g$disp))
        }
        o
      }
      if (!is.na(c$rf)) out$rf <- c$rf
      if (isTRUE(c$multi)) {
        out$multi <- TRUE
        out$levels <- arr(as.character(c$levels))
        out$fills <- arr(as.character(c$fills))
        out$lv <- lapply(c$lv, pack_pair)
      } else {
        out <- c(out, pack_pair(c))
      }
    } else if (identical(c$kind, "interval")) {
      # Per-row lists stay arrays even at length one: a collapsed tips vector
      # would index as characters in JS (the auto_unbox trap).
      out$segs <- lapply(c$segs, function(ss) if (length(ss)) ss else arr(list()))
      out$tips <- lapply(c$tips, function(tt) arr(as.character(tt)))
      out$fills <- arr(as.character(c$fills))
      out$d0 <- c$d0
      out$d1 <- c$d1
      if (isTRUE(c$dd)) out$dd <- TRUE
      if (isTRUE(c$lg)) out$lg <- TRUE
    } else if (identical(c$kind, "sparkline")) {
      out$pl <- arr(as.character(c$pl))
      out$bd <- arr(c$bd)
      out$dx <- arr(c$dx)
      out$dy <- arr(c$dy)
      out$xs <- arr(as.character(c$xs))
      out$ys <- arr(as.character(c$ys))
      out$nn <- arr(c$nn)
      # Column-level reference coordinates (scalars; NA drops to absent).
      if (!is.na(c$rc)) out$rc <- c$rc
      if (!is.na(c$rby)) {
        out$rby <- c$rby
        out$rbh <- c$rbh
      }
    } else {
      out$w <- arr(c$w)
      out$sub <- arr_if(c$sub)
      if (!is.null(c$fill)) out$fill <- c$fill
    }
    # The in-bar value label (any bar kind): the display strings plus the
    # column's ONE label-slot width.
    if (!identical(c$kind, "num") && !is.null(c$disp)) {
      out$disp <- arr(as.character(c$disp))
      if (!is.null(c$pct)) out$pct <- arr(as.character(c$pct))
      out$dw <- c$dw
    } else if (isTRUE(c$multi) && !is.null(c$dw)) {
      # A colour-split column's labels ride on its levels; the slot width
      # is the column's.
      out$dw <- c$dw
    }
    out$v <- arr(c$v)
    if (length(c$tt)) out$tt <- c$tt
    out
  }
  out <- list(
    kind = "flat",
    n = m$n,
    head = paste0(
      "<table class=\"blockr-table blockr-summarize-table\"",
      " data-summarize-nested=\"",
      if (isTRUE(m$nested)) "1" else "0", "\"", m$attrs, ">",
      m$thead, "<tbody></tbody></table>"
    ),
    ncol = m$ncol,
    label = arr(m$label),
    parent = if (isTRUE(m$nested)) arr(m$parent) else NULL,
    # The LEVEL itself (an integer), omitted only when every row is level 0 --
    # arr_if() on the comparison would have shipped logicals and printed
    # data-summarize-level="true".
    level = if (any(m$level > 0L)) arr(m$level) else NULL,
    parent_row = arr_if(m$parent_row),
    on = arr_if(m$on),
    pick = isTRUE(m$pick),
    path = if (isTRUE(m$pick)) arr(as.character(m$path)) else NULL,
    cols = lapply(m$cols, one),
    fold = m$fold
  )
  # A NULL entry would serialize as {} (jsonlite with no null="null") and read
  # as truthy in JS -- the trap dt_payload_json documents. Drop them instead.
  summarize_drop_null(out)
}

#' @noRd
summarize_drop_null <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, is.null, logical(1L))]
  lapply(x, function(e) {
    if (is.list(e) && !inherits(e, "AsIs")) summarize_drop_null(e) else e
  })
}

#' Build the body payload for the block server: the same dispatch the render
#' path uses. A non-renderable state (no group picked, a vanished column, no
#' rows) is small, so it ships as `kind = "html"` -- the complete message table
#' from the existing builder, injected as-is, zero markup duplication. Only the
#' row path, the one that scales, ships the cell model.
#'
#' `chrome` carries what the container's own slots show: the resolved title /
#' subtitle / caption (plus the raw states the gear needs), the legend, and the
#' footer's count line and note.
#' @noRd
summarize_build_payload <- function(data, chrome = list(), drill = NULL,
                                    active = NULL, cfg = NULL, ...) {
  prep <- summarize_prepare(data, ...)
  body <- if (!is.null(prep$err)) {
    list(kind = "html",
         html = paste0(
           "<table class=\"blockr-table blockr-summarize-table\"",
           summarize_table_attrs(prep, cfg),
           "><tbody><tr><td class=\"blockr-data\">",
           summarize_esc(prep$err), "</td></tr></tbody></table>"
         ))
  } else {
    summarize_flat_payload(summarize_cells(prep, drill = drill, active = active,
                                           cfg = cfg))
  }
  body$chrome <- summarize_drop_null(c(chrome, list(
    legend = summarize_legend_spec(prep),
    foot = summarize_foot_spec(prep, drill = drill, active = active)
  )))
  body
}

#' Serialize a payload ONCE, R-side. The server sends the string, not the list:
#' pre-serializing dodges Shiny's auto_unbox scalar-collapse on the envelope,
#' gives the server a plain string-identity re-send guard (the chart block's
#' last_msg pattern), and lets the browser skip JSON.parse on an unchanged rev.
#' @noRd
summarize_payload_json <- function(p) {
  as.character(jsonlite::toJSON(p, auto_unbox = TRUE, na = "null"))
}
