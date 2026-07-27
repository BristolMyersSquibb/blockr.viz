# Rank table: cell model + data-push payload -----------------------------------
#
# Same architecture as the table block's flat path (dev/table-data-push-design.md):
# ONE builder computes every per-column vector, and two consumers turn it into
# either markup or JSON.
#
#   rank_cells()        -> the cell model (per-column widths / display strings /
#                          fills, plus the row meta and the <thead> tag)
#   rank_cells_html()   -> pastes it into the historical <table> (the exported
#                          rank_table(), the static / report path, the tests)
#   rank_flat_payload() -> emits it as the JSON cell model rank-table.js
#                          assembles client-side
#
# The two MUST NOT drift: the JS assembler applies the same escaping rules
# htmltools does (& < > escaped, quotes left alone) and the same class /
# attribute order, and test-rank-push.R pins one against the other.
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
rank_cells <- function(prep, drill = NULL, active = NULL, cfg = NULL) {
  rows <- prep$rows
  plan <- prep$plan
  n <- nrow(rows)
  mx <- prep$bar_max

  # `v` is the sort key and the data-v attribute, nothing else, so full float
  # precision is pure payload: a faceted percentage like 23.734177215189873
  # costs 18 bytes per cell against 5 for 23.73. Rounded HERE so both consumers
  # print the same string and the drift guard stays green.
  sortv <- function(v) if (is.numeric(v)) round(v, 4L) else v

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
    if (!isTRUE(p$show_val)) return(NULL)
    parts <- rank_num_parts(vraw, denom = p$val_denom,
                            combined = !is.null(p$val_denom), signed = signed)
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
        lapply(seq_along(p$series), function(j) pct_w(mat[, j]))
      } else {
        scale <- if (identical(p$mode, "percent")) {
          ifelse(tot > 0, 100, 0)
        } else if (is.finite(mx) && mx > 0) {
          tot / mx * 100
        } else {
          tot * 0
        }
        share <- ifelse(tot > 0, 1, 0) * mat / ifelse(tot > 0, tot, 1)
        lapply(seq_along(p$series), function(j) round(share[, j] * scale, 2L))
      }
      vraw <- if (!is.null(p$key)) rows[[p$key]] else rowSums(
        vapply(segv, identity, numeric(n)))
      lab <- val_parts(p, vraw)
      v <- if (is.null(p$key)) tot else {
        if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
          vraw / p$denom * 100
        } else {
          vraw
        }
      }
      c(list(kind = "barsplit", mode = p$mode %||% "stacked",
             names = as.character(p$series),
             fills = unname(prep$palette[as.character(p$series)]),
             seg = seg, segv = segv,
             v = sortv(v)), lab)
    } else if (identical(p$kind, "bardiv")) {
      v <- rows[[p$key]]
      lab <- val_parts(p, v, signed = TRUE)
      w <- if (is.finite(mx) && mx > 0) pmin(50, abs(v) / mx * 50) else v * 0
      w[!is.finite(w)] <- 0
      c(list(kind = "bardiv", w = round(w, 2L), v = sortv(v),
             pos = !is.na(v) & v >= 0), lab)
    } else if (p$kind %in% c("box", "pointrange")) {
      # The distribution lanes: every geometric number (positions AND widths)
      # rounded here; the emitters only print. See lane-prepare.R for the
      # cell's statistics and _blockr.design/open/lane-chart/spec.md for the
      # glyph.
      cn <- p$cols
      wd <- p$words %||% list(center = "Center", range = "Range")
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
      lab <- if (isTRUE(p$show_val)) {
        disp <- lane_fmt(bc)
        list(disp = disp, dw = max(c(1L, nchar(disp))))
      }
      if (identical(p$kind, "box")) {
        wl <- rows[[cn[["wl"]]]]
        wh <- rows[[cn[["wh"]]]]
        # Whisker segments live OUTSIDE the body; a degenerate side (whisker
        # meets the box) ships NA and draws nothing.
        tip <- ifelse(is.na(bc), "", rank_esc(paste0(
          "n=", nn, " \u00b7 ", wd$center, " ", lane_fmt(bc),
          " \u00b7 ", wd$range, " ", lane_fmt(bl), "\u2013", lane_fmt(bh),
          " \u00b7 ", wd$whisk %||% "Whiskers", " ",
          lane_fmt(wl), "\u2013", lane_fmt(wh)
        )))
        list(kind = "box",
             wl = pos_w(wl), w1 = span_w(wl, bl),
             bl = pos_w(bl), bw = span_w(bl, bh), bc = pos_w(bc),
             b2 = pos_w(bh), w2 = span_w(bh, wh), wh = pos_w(wh),
             nn = nn, tip = tip, v = sortv(bc)) |> c(lab)
      } else {
        # The dot (words$range NULL) has no interval clause and no n.
        tip <- ifelse(is.na(bc), "", rank_esc(paste0(
          ifelse(is.na(nn), "", paste0("n=", nn, " \u00b7 ")),
          wd$center, " ", lane_fmt(bc),
          if (!is.null(wd$range)) {
            paste0(" \u00b7 ", wd$range, " ",
                   ifelse(is.na(bl) | is.na(bh), "undefined (n < 2)",
                          paste0(lane_fmt(bl), "\u2013", lane_fmt(bh))))
          } else {
            ""
          }
        )))
        list(kind = "pointrange",
             c = pos_w(bc), l = pos_w(bl), rw = span_w(bl, bh),
             nn = nn, tip = tip, v = sortv(bc)) |> c(lab)
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
          if (!is.null(sg$lb)) c(base, list(rank_esc(sg$lb))) else base
        })
      })
      # Tooltip: the event label headlines (chart-gantt parity), then the
      # colour level, the span, and any extra field pairs.
      out_tips <- lapply(segs, function(ss) {
        vapply(ss, function(sg) {
          rank_esc(paste0(
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
      lab <- if (isTRUE(p$show_val)) {
        disp <- lane_fmt(last_y)
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
      list(kind = "num", text = TRUE, v = v, disp = rank_esc(v))
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
      list(kind = "num", text = TRUE, v = v, disp = rank_esc(v))
    } else {
      v <- rows[[p$key]]
      parts <- rank_num_parts(v, denom = p$denom,
                              combined = isTRUE(p$combined),
                              signed = isTRUE(p$signed),
                              pct_only = isTRUE(p$pct_only))
      c(list(kind = "num", v = sortv(v)), parts)
    }
  })

  act_vals <- as.character(unlist(active$vals %||% character()))
  on <- if (length(act_vals) && !is.null(rank_chr1(active$col))) {
    rows$.label %in% act_vals
  } else {
    rep(FALSE, n)
  }

  list(
    n = n,
    thead = rank_thead(prep, sortable = isTRUE(cfg$sortable %||% TRUE)),
    ncol = length(plan) + 1L,
    nested = !is.null(prep$parent),
    label = as.character(rows$.label),      # PLAIN: each consumer escapes
    parent = as.character(rows$.parent),
    level = as.integer(rows$.level),
    parent_row = rows$.is_parent,
    on = on,
    pick = !is.null(drill),
    cols = cols,
    fold = rank_fold_text(prep),
    attrs = rank_table_attrs(prep, cfg)
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
rank_thead <- function(prep, sortable = TRUE) {
  col_th <- function(p, i) {
    as.character(dt_th(
      p$label, i, label = p$sub_label,
      numeric = identical(p$kind, "num") && !isTRUE(p$text),
      sortable = sortable
    ))
  }
  stub <- as.character(dt_th(
    rank_label_header(prep), 0L, stub = TRUE,
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
             rank_esc(g$label), "</span></th>")
    }, character(1L))
  )
  row2 <- vapply(seq.int(fs$lead + 1L, length(prep$plan)), function(i) {
    col_th(prep$plan[[i]], i)
  }, character(1L))
  paste0("<thead><tr>", paste(row1, collapse = ""), "</tr><tr>",
         paste(row2, collapse = ""), "</tr></thead>")
}

#' The fold row's text, or NULL when nothing was capped. Never a silent
#' truncation: `top_n` always says what fell below the cut.
#' @noRd
rank_fold_text <- function(prep) {
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

#' @noRd
rank_cells_html <- function(m) {
  cells <- lapply(m$cols, function(c) {
    if (identical(c$kind, "bar")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_barwrap(rank_track_html(c$w, c$fill, c$sub), c), "</td>")
    } else if (identical(c$kind, "barsplit")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_barwrap(rank_split_html(c), c), "</td>")
    } else if (identical(c$kind, "bardiv")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_barwrap(rank_dv_html(c$w, c$pos), c), "</td>")
    } else if (identical(c$kind, "box")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_barwrap(rank_box_html(c), c), "</td>")
    } else if (identical(c$kind, "pointrange")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_barwrap(rank_pr_html(c), c), "</td>")
    } else if (identical(c$kind, "interval")) {
      paste0("<td class=\"blockr-rank-bar-col",
             if (isTRUE(c$lg)) " blockr-rank-wide" else "", "\"",
             rank_data_v(c$v), ">",
             rank_iv_html(c), "</td>")
    } else if (identical(c$kind, "sparkline")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_barwrap(rank_sp_html(c), c), "</td>")
    } else if (isTRUE(c$text)) {
      paste0("<td class=\"blockr-rank-txt\"", rank_data_v(c$v), ">",
             c$disp, "</td>")
    } else {
      paste0("<td class=\"blockr-rank-num dt-col-num\"", rank_data_v(c$v), ">",
             c$disp,
             if (is.null(c$pct)) {
               ""
             } else {
               paste0(" <span class=\"blockr-rank-pct\">", c$pct, "</span>")
             },
             "</td>")
    }
  })

  chev <- paste0(
    "<button class=\"blockr-indent-btn\" type=\"button\" tabindex=\"-1\"",
    " aria-expanded=\"false\">", as.character(section_chevron_svg()), "</button>"
  )
  # Child rows are indented on the same 24 + 16px step the structured table
  # uses, and the chevron hangs into that gutter (margin-left:-18px).
  lbl <- paste0(
    "<td class=\"blockr-rank-label-col blockr-stub",
    ifelse(m$parent_row, " blockr-has-toggle", ""), "\"",
    ifelse(m$level > 0L, " style=\"padding-left:40px;\"", ""), ">",
    ifelse(m$parent_row, chev, ""),
    "<span class=\"blockr-rank-label\">", rank_esc(m$label), "</span></td>"
  )
  tr <- paste0(
    "<tr class=\"blockr-rank-row",
    ifelse(m$parent_row, " is-parent blockr-indent-toggle collapsed", ""),
    ifelse(m$level > 0L, " is-child collapsed-hidden", ""),
    if (isTRUE(m$pick)) " is-pick" else "",
    ifelse(m$on, " is-on", ""),
    "\" data-rank-label=\"", rank_esc(m$label), "\"",
    ifelse(m$level > 0L,
           paste0(" data-rank-parent=\"", rank_esc(m$parent), "\""), ""),
    " data-rank-level=\"", m$level,
    # The order the row arrived in: the third header click sorts back to it,
    # so the configured order (visits in visit order) is never lost to a
    # stray click. Emitted by BOTH assemblers -- the drift test pins it.
    "\" data-rank-ord=\"", seq_along(m$level) - 1L, "\">"
  )
  body <- paste0(tr, lbl, do.call(paste0, cells), "</tr>")
  fold <- if (is.null(m$fold)) {
    ""
  } else {
    paste0("<tr class=\"blockr-rank-fold\"><td colspan=\"", m$ncol, "\">",
           rank_esc(m$fold), "</td></tr>")
  }
  paste0(
    "<table class=\"blockr-table blockr-rank-table\" data-rank-nested=\"",
    if (isTRUE(m$nested)) "1" else "0", "\"", m$attrs, ">",
    m$thead, "<tbody>", paste(body, collapse = ""), fold, "</tbody></table>"
  )
}

#' The in-bar value label: track (or split / diff bar) left, the value in a
#' fixed-width right-aligned slot -- ONE width per column (`dw`, in ch), so
#' every row's track spans the same range and the bars stay comparable.
#' Columns without a label (explicit separate cols) pass through untouched.
#' @noRd
rank_barwrap <- function(inner, c) {
  if (is.null(c$disp)) return(inner)
  paste0(
    "<div class=\"blockr-rank-barwrap\">", inner,
    "<span class=\"blockr-rank-barval\" style=\"width:", c$dw, "ch\">",
    c$disp,
    if (is.null(c$pct)) {
      ""
    } else {
      ifelse(nzchar(c$pct),
             paste0(" <span class=\"blockr-rank-pct\">", c$pct, "</span>"),
             "")
    },
    "</span></div>"
  )
}

#' One bar: a track div plus a fill div, vectorised over the column. An NA
#' width is a cell with NO value (the identity measure's absent facet): the
#' track renders empty -- no fill, no zero sliver.
#' @noRd
rank_track_html <- function(w, fill = NULL, sub = FALSE) {
  sub <- rep_len(isTRUE(sub) | (is.logical(sub) & !is.na(sub) & sub), length(w))
  paste0(
    "<div class=\"blockr-rank-track", ifelse(sub, " is-sub", ""), "\">",
    ifelse(is.na(w), "", paste0(
      "<div class=\"blockr-rank-fill\" style=\"width:", rank_fmt_w(w), "%",
      if (!is.null(fill)) paste0(";background:", fill) else "", "\"></div>"
    )),
    "</div>"
  )
}

#' @noRd
rank_split_html <- function(c) {
  n <- length(c$v)
  k <- length(c$names)
  if (!n || !k) return(rep("<div class=\"blockr-rank-track\"></div>", n))
  grouped <- identical(c$mode, "grouped")
  seg <- vapply(seq_len(k), function(j) {
    body <- paste0(
      "<div class=\"blockr-rank-fill\" style=\"width:", rank_fmt_w(c$seg[[j]]),
      "%;background:", c$fills[[j]], "\" title=\"", rank_esc(c$names[[j]]),
      ": ", c$segv[[j]], "\"></div>"
    )
    if (grouped) {
      paste0("<div class=\"blockr-rank-row3\">", body, "</div>")
    } else {
      ifelse(c$segv[[j]] > 0, body, "")
    }
  }, character(n))
  seg <- matrix(seg, nrow = n)
  paste0("<div class=\"blockr-rank-track", if (grouped) " is-tall" else "",
         "\">", apply(seg, 1L, paste0, collapse = ""), "</div>")
}

#' @noRd
rank_dv_html <- function(w, pos) {
  paste0(
    "<div class=\"blockr-rank-dv\"><div class=\"blockr-rank-fill ",
    ifelse(pos, "is-pos", "is-neg"), "\" style=\"width:", rank_fmt_w(w),
    "%\"></div></div>"
  )
}

# --- lane mark emitters -------------------------------------------------------
# Each of these has a byte-identical twin in rank-table.js (boxHtml / prHtml /
# ivHtml / spHtml); test-rank-push.R pins the pair. Emission conditions key on
# shipped NA/null values, never on re-derived arithmetic, so the two consumers
# cannot disagree about what to draw.

#' Box cell: two whisker segments (never through the body), caps, IQR body,
#' median tick, all absolutely positioned in a track-coloured lane.
#' @noRd
rank_box_html <- function(c) {
  n <- length(c$v)
  vapply(seq_len(n), function(i) {
    if (is.na(c$bc[[i]])) {
      return("<div class=\"blockr-rank-lane blockr-rank-boxcell\"></div>")
    }
    paste0(
      "<div class=\"blockr-rank-lane blockr-rank-boxcell\" title=\"",
      c$tip[[i]], "\">",
      if (!is.na(c$w1[[i]])) {
        paste0("<i class=\"lane-wh\" style=\"left:", rank_fmt_w(c$wl[[i]]),
               "%;width:", rank_fmt_w(c$w1[[i]]), "%\"></i>")
      } else "",
      if (!is.na(c$w2[[i]])) {
        paste0("<i class=\"lane-wh\" style=\"left:", rank_fmt_w(c$b2[[i]]),
               "%;width:", rank_fmt_w(c$w2[[i]]), "%\"></i>")
      } else "",
      if (!is.na(c$w1[[i]])) {
        paste0("<i class=\"lane-cap\" style=\"left:", rank_fmt_w(c$wl[[i]]),
               "%\"></i>")
      } else "",
      if (!is.na(c$w2[[i]])) {
        paste0("<i class=\"lane-cap\" style=\"left:", rank_fmt_w(c$wh[[i]]),
               "%\"></i>")
      } else "",
      if (!is.na(c$bw[[i]])) {
        paste0("<i class=\"lane-box\" style=\"left:", rank_fmt_w(c$bl[[i]]),
               "%;width:", rank_fmt_w(c$bw[[i]]), "%\"></i>")
      } else "",
      "<i class=\"lane-med\" style=\"left:", rank_fmt_w(c$bc[[i]]),
      "%\"></i></div>"
    )
  }, character(1L))
}

#' Point range cell: interval line plus a ringed center dot. NA bounds (the
#' n < 2 CI) draw the center alone -- never a zero-width interval, which
#' would read as certainty.
#' @noRd
rank_pr_html <- function(c) {
  n <- length(c$v)
  vapply(seq_len(n), function(i) {
    if (is.na(c$c[[i]])) {
      return("<div class=\"blockr-rank-lane blockr-rank-prcell\"></div>")
    }
    paste0(
      "<div class=\"blockr-rank-lane blockr-rank-prcell\" title=\"",
      c$tip[[i]], "\">",
      if (!is.na(c$rw[[i]])) {
        paste0("<i class=\"lane-rng\" style=\"left:", rank_fmt_w(c$l[[i]]),
               "%;width:", rank_fmt_w(c$rw[[i]]), "%\"></i>")
      } else "",
      "<i class=\"lane-ctr\" style=\"left:", rank_fmt_w(c$c[[i]]),
      "%\"></i></div>"
    )
  }, character(1L))
}

#' Interval cell: the swimlane. One segment per (x, xend) span, coloured by
#' fill index; the domain bounds ride as data attributes for the hover
#' readout (approximate day under the cursor).
#' @noRd
rank_iv_html <- function(c) {
  n <- length(c$v)
  vapply(seq_len(n), function(i) {
    segs <- c$segs[[i]]
    paste0(
      "<div class=\"blockr-rank-lane blockr-rank-ivcell\" data-d0=\"",
      rank_fmt_n(c$d0), "\" data-d1=\"", rank_fmt_n(c$d1), "\"",
      if (isTRUE(c$dd)) " data-dd=\"1\"" else "", ">",
      paste0(vapply(seq_along(segs), function(j) {
        sg <- segs[[j]]
        paste0("<i class=\"lane-seg\" style=\"left:", rank_fmt_w(sg[[1L]]),
               "%;width:", rank_fmt_w(sg[[2L]]), "%;background:",
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
rank_sp_html <- function(c) {
  n <- length(c$v)
  vapply(seq_len(n), function(i) {
    paste0(
      "<div class=\"blockr-rank-lane blockr-rank-spcell\" data-xs=\"",
      c$xs[[i]], "\" data-ys=\"", c$ys[[i]], "\">",
      "<svg viewBox=\"0 0 100 36\" preserveAspectRatio=\"none\">",
      if (!is.na(c$rby)) {
        paste0("<rect class=\"lane-refband\" x=\"0\" y=\"",
               rank_fmt_w(c$rby), "\" width=\"100\" height=\"",
               rank_fmt_w(c$rbh), "\"></rect>")
      } else "",
      if (!is.na(c$bd[[i]])) {
        paste0("<polygon class=\"lane-band\" points=\"", c$bd[[i]],
               "\"></polygon>")
      } else "",
      if (!is.na(c$rc)) {
        paste0("<line class=\"lane-refline\" x1=\"0\" y1=\"",
               rank_fmt_w(c$rc), "\" x2=\"100\" y2=\"", rank_fmt_w(c$rc),
               "\" vector-effect=\"non-scaling-stroke\"></line>")
      } else "",
      if (nzchar(c$pl[[i]])) {
        paste0("<polyline class=\"lane-ln\" points=\"", c$pl[[i]],
               "\" vector-effect=\"non-scaling-stroke\"></polyline>")
      } else "",
      "</svg>",
      if (!is.na(c$dx[[i]])) {
        paste0("<i class=\"lane-dot\" style=\"left:", rank_fmt_w(c$dx[[i]]),
               "%;top:", rank_fmt_w(c$dy[[i]]), "%\"></i>")
      } else "",
      "</div>"
    )
  }, character(1L))
}

# Per-element formatting: format() would align decimals across the vector
# ("100.00" beside "57.14"), while the JS assembler prints String(n) = "100".
# as.character() on a rounded value matches it exactly -- the same reason
# dt_bar_style() avoids format() for the table block's data bars.
#' @noRd
rank_fmt_w <- function(w) {
  as.character(w)
}

# Large-magnitude numbers (the interval domain: dates as days or seconds):
# as.character() goes scientific at 1e5 where JS String() never does. Values
# are rounded to <= 4 decimals at the source, so digits = 15 prints them
# exactly, matching String(n).
#' @noRd
rank_fmt_n <- function(x) {
  format(x, scientific = FALSE, trim = TRUE, digits = 15L)
}

# --- consumer 2: JSON payload ------------------------------------------------

#' Emit the cell model as the `flat` payload rank-table.js assembles.
#'
#' `I()` keeps every per-row vector a JSON array even at length 1 (auto_unbox
#' would collapse a one-row table's columns to scalars, and the JS assembler
#' indexes them).
#' @noRd
rank_flat_payload <- function(m) {
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
    } else if (identical(c$kind, "bardiv")) {
      out$w <- arr(c$w)
      out$pos <- arr(c$pos)
    } else if (identical(c$kind, "box")) {
      for (nm in c("wl", "w1", "bl", "bw", "bc", "b2", "w2", "wh", "nn")) {
        out[[nm]] <- arr(c[[nm]])
      }
      out$tip <- arr(as.character(c$tip))
    } else if (identical(c$kind, "pointrange")) {
      for (nm in c("c", "l", "rw", "nn")) out[[nm]] <- arr(c[[nm]])
      out$tip <- arr(as.character(c$tip))
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
    }
    out$v <- arr(c$v)
    out
  }
  out <- list(
    kind = "flat",
    n = m$n,
    head = paste0(
      "<table class=\"blockr-table blockr-rank-table\" data-rank-nested=\"",
      if (isTRUE(m$nested)) "1" else "0", "\"", m$attrs, ">",
      m$thead, "<tbody></tbody></table>"
    ),
    ncol = m$ncol,
    label = arr(m$label),
    parent = if (isTRUE(m$nested)) arr(m$parent) else NULL,
    # The LEVEL itself (an integer), omitted only when every row is level 0 --
    # arr_if() on the comparison would have shipped logicals and printed
    # data-rank-level="true".
    level = if (any(m$level > 0L)) arr(m$level) else NULL,
    parent_row = arr_if(m$parent_row),
    on = arr_if(m$on),
    pick = isTRUE(m$pick),
    cols = lapply(m$cols, one),
    fold = m$fold
  )
  # A NULL entry would serialize as {} (jsonlite with no null="null") and read
  # as truthy in JS -- the trap dt_payload_json documents. Drop them instead.
  rank_drop_null(out)
}

#' @noRd
rank_drop_null <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, is.null, logical(1L))]
  lapply(x, function(e) if (is.list(e) && !inherits(e, "AsIs")) rank_drop_null(e) else e)
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
rank_build_payload <- function(data, chrome = list(), drill = NULL,
                               active = NULL, cfg = NULL, ...) {
  prep <- rank_prepare(data, ...)
  body <- if (!is.null(prep$err)) {
    list(kind = "html",
         html = paste0(
           "<table class=\"blockr-table blockr-rank-table\"",
           rank_table_attrs(prep, cfg), "><tbody><tr><td class=\"blockr-data\">",
           rank_esc(prep$err), "</td></tr></tbody></table>"
         ))
  } else {
    rank_flat_payload(rank_cells(prep, drill = drill, active = active,
                                cfg = cfg))
  }
  body$chrome <- rank_drop_null(c(chrome, list(
    legend = rank_legend_spec(prep),
    foot = rank_foot_spec(prep, drill = drill, active = active)
  )))
  body
}

#' Serialize a payload ONCE, R-side. The server sends the string, not the list:
#' pre-serializing dodges Shiny's auto_unbox scalar-collapse on the envelope,
#' gives the server a plain string-identity re-send guard (the chart block's
#' last_msg pattern), and lets the browser skip JSON.parse on an unchanged rev.
#' @noRd
rank_payload_json <- function(p) {
  as.character(jsonlite::toJSON(p, auto_unbox = TRUE, na = "null"))
}
