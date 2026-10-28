# Summarize table: HTML emitters + data-push payload ---------------------------
#
# Same architecture as the table block's flat path (dev/table-data-push-design.md):
# ONE builder computes every per-column vector, and two consumers turn it into
# either markup or JSON.
#
# summarize_cells() -> the cell model (per-column widths / display strings /
#                          fills, plus the row meta and the <thead> tag); it
#                          lives in table-model-cells.R
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
             summarize_esc(g$label), "</span>",
             if (!is.null(g$pop)) {
               paste0(" <span class=\"blockr-col-label\">N = ", g$pop,
                      "</span>")
             },
             "</th>")
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
    at <- summarize_axis_ticks(
      dom$d0, dom$d1,
      k = if (identical(p$kind, "interval")) 7L else 4L
    )
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

#' Bottom axis for large swimlane columns
#' @noRd
summarize_tfoot <- function(prep, cols = NULL, axis = TRUE) {
  if (!isTRUE(axis) || !length(prep$plan)) return("")
  cells <- vapply(seq_along(prep$plan), function(i) {
    p <- prep$plan[[i]]
    if (!identical(p$kind, "interval") || !identical(p$size, "lg")) {
      return("<td></td>")
    }
    cl <- if (!is.null(cols) && length(cols) >= i) cols[[i]]
    paste0(
      "<td class=\"blockr-summarize-axis-cell\">",
      summarize_axis_strip(p, cl, prep),
      "</td>"
    )
  }, character(1L))
  if (!any(grepl("blockr-summarize-axis", cells, fixed = TRUE))) return("")
  paste0(
    "<tfoot><tr><td class=\"blockr-summarize-axis-stub\"></td>",
    paste(cells, collapse = ""),
    "</tr></tfoot>"
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
  expanded <- isTRUE(expanded) || isTRUE(m$expanded)
  cells <- lapply(m$cols, summarize_cell_html)

  tr <- summarize_tr_html(m, expanded)
  lbl <- summarize_stub_html(m)
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
    m$thead, "<tbody>", paste(body, collapse = ""), fold, "</tbody>",
    m$tfoot, "</table>"
  )
}

#' One column's <td> for every row, by kind.
#' @noRd
summarize_cell_html <- function(c) {
  if (identical(c$kind, "tile")) {
    paste0("<td class=\"blockr-summarize-tile-col\"",
           summarize_data_v(c$v), ">",
           summarize_tile_html(c$bg, c$fg, c$disp), "</td>")
  } else if (identical(c$kind, "bar")) {
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
    paste0("<td class=\"blockr-summarize-txt",
           if (isTRUE(c$failed)) " is-failed", "\"", summarize_data_v(c$v), ">",
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
}

#' The label cell of every row, with the chevron on a parent row.
#' @noRd
summarize_stub_html <- function(m) {
  chev <- paste0(
    "<button class=\"blockr-indent-btn\" type=\"button\" tabindex=\"-1\"",
    " aria-expanded=\"false\">", as.character(section_chevron_svg()), "</button>"
  )
  # Child rows start where their parent's name starts: every label cell of
  # a nested table has 24px of padding (summarize-table-css.R), and the
  # parent's chevron hangs into it (margin-left:-18px).
  paste0(
    "<td class=\"blockr-summarize-label-col blockr-stub",
    ifelse(m$parent_row, " blockr-has-toggle", ""), "\">",
    ifelse(m$parent_row, chev, ""),
    "<span class=\"blockr-summarize-label\">", summarize_esc(m$label),
    "</span></td>"
  )
}

#' The opening <tr> tag of every row.
#' @noRd
summarize_tr_html <- function(m, expanded) {
  paste0(
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
    ifelse(m$level > 0L,
           paste0(" data-summarize-parent-key=\"",
                  summarize_esc(m$parent_key), "\""),
           ""),
    ifelse(m$parent_row,
           paste0(" data-summarize-key=\"", summarize_esc(m$parent_key), "\""),
           ""),
    " data-summarize-level=\"", m$level,
    # The order the row arrived in: the third header click sorts back to it,
    # so the configured order (visits in visit order) is never lost to a
    # stray click. Emitted by BOTH assemblers -- the drift test pins it.
    "\" data-summarize-ord=\"", seq_along(m$level) - 1L, "\">"
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
      # The segment's index: a click on a colour segment claims its level
      # (summarize-table.js reads it), so the static markup carries it too.
      "\" data-seg=\"", j - 1L, "\" style=\"width:",
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
        paste0(" data-summarize-tip=\"", summarize_esc(c$tip[[i]]), "\"")
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
#' @noRd
summarize_flat_payload <- function(m) {
  out <- list(
    kind = "flat",
    n = m$n,
    head = paste0(
      "<table class=\"blockr-table blockr-summarize-table\"",
      " data-summarize-nested=\"",
      if (isTRUE(m$nested)) "1" else "0", "\"", m$attrs, ">",
      m$thead, "<tbody></tbody>", m$tfoot, "</table>"
    ),
    ncol = m$ncol,
    label = summarize_arr(m$label),
    expanded = isTRUE(m$expanded),
    parent = if (isTRUE(m$nested)) summarize_arr(m$parent) else NULL,
    parent_key = if (isTRUE(m$nested)) summarize_arr(m$parent_key) else NULL,
    # The LEVEL itself (an integer), omitted only when every row is level 0 --
    # arr_if() on the comparison would have shipped logicals and printed
    # data-summarize-level="true".
    level = if (any(m$level > 0L)) summarize_arr(m$level) else NULL,
    parent_row = summarize_arr_if(m$parent_row),
    on = summarize_arr_if(m$on),
    on_cell = m$on_cell,
    pick = isTRUE(m$pick),
    path = if (isTRUE(m$pick)) {
      summarize_arr(as.character(m$path))
    } else {
      NULL
    },
    cols = lapply(m$cols, summarize_payload_col),
    fold = m$fold
  )
  # A NULL entry would serialize as {} (jsonlite with no null="null") and read
  # as truthy in JS -- the trap dt_payload_json documents. Drop them instead.
  summarize_drop_null(out)
}

#' @noRd
summarize_arr <- function(x) I(unname(x))

# A vector that is constant-false (or all-NA) carries no information: 4-5 KB
# per column at 790 rows for `sub` / `on` / `parent_row` on a flat table.
# Omitted entirely; the assembler reads an absent vector as all-false.
#' @noRd
summarize_arr_if <- function(x) {
  if (any(x, na.rm = TRUE)) summarize_arr(x) else NULL
}

# One column of the cell model as its payload entry.
#' @noRd
summarize_payload_col <- function(c) {
  arr <- summarize_arr
  out <- list(kind = c$kind)
  # What a click on this column's cell adds to the row's claim.
  out$fkey <- c$fkey
  out$scol <- c$scol
  if (identical(c$kind, "tile")) {
    out$bg <- arr(c$bg)
    out$fg <- arr(c$fg)
    out$disp <- arr(as.character(c$disp))
    out$v <- arr(c$v)
    if (length(c$tt)) out$tt <- c$tt
    return(out)
  }
  if (identical(c$kind, "num")) {
    out$disp <- arr(as.character(c$disp))
    if (!is.null(c$pct)) out$pct <- arr(as.character(c$pct))
    if (isTRUE(c$text)) out$text <- TRUE
  } else if (identical(c$kind, "barsplit")) {
    out <- summarize_payload_split(out, c)
  } else if (identical(c$kind, "bardiv")) {
    out$w <- arr(c$w)
    out$pos <- arr(c$pos)
  } else if (c$kind %in% c("box", "pointrange")) {
    out <- summarize_payload_lane(out, c)
  } else if (identical(c$kind, "pair")) {
    out <- summarize_payload_pair(out, c)
  } else if (identical(c$kind, "interval")) {
    out <- summarize_payload_interval(out, c)
  } else if (identical(c$kind, "sparkline")) {
    out <- summarize_payload_spark(out, c)
  } else {
    out$w <- arr(c$w)
    out$sub <- summarize_arr_if(c$sub)
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

#' @noRd
summarize_payload_split <- function(out, c) {
  arr <- summarize_arr
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
  out
}

# Box and point range: one geometry set, or one per level for a colour split.
#' @noRd
summarize_payload_lane <- function(out, c) {
  arr <- summarize_arr
  if (isTRUE(c$multi)) {
    # A colour-split column ships one geometry set per level plus the
    # level names and their fills; the assembler walks them in order.
    out$multi <- TRUE
    out$levels <- arr(as.character(c$levels))
    out$fills <- arr(as.character(c$fills))
    out$lv <- lapply(c$lv, function(g) summarize_payload_lane_pack(g, c))
  } else {
    out <- c(out, summarize_payload_lane_pack(c, c))
  }
  out
}

#' @noRd
summarize_payload_lane_pack <- function(g, c) {
  arr <- summarize_arr
  geom <- if (identical(c$kind, "box")) {
    c("wl", "w1", "bl", "bw", "bc", "b2", "w2", "wh", "nn")
  } else {
    c("c", "l", "rw", "nn")
  }
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

#' @noRd
summarize_payload_pair <- function(out, c) {
  arr <- summarize_arr
  if (!is.na(c$rf)) out$rf <- c$rf
  if (isTRUE(c$multi)) {
    out$multi <- TRUE
    out$levels <- arr(as.character(c$levels))
    out$fills <- arr(as.character(c$fills))
    out$lv <- lapply(c$lv, function(g) summarize_payload_pair_pack(g, c))
  } else {
    out <- c(out, summarize_payload_pair_pack(c, c))
  }
  out
}

#' @noRd
summarize_payload_pair_pack <- function(g, c) {
  arr <- summarize_arr
  o <- list()
  for (nm in c("a", "b", "l", "w", "bl", "bw")) o[[nm]] <- arr(g[[nm]])
  o$fill <- arr(g$fill)
  o$dash <- arr(g$dash)
  o$open <- arr(g$open)
  o$tip <- arr(as.character(g$tip))
  if (isTRUE(c$multi) && !is.null(g$disp)) {
    o$disp <- arr(as.character(g$disp))
  }
  if (isTRUE(c$multi)) o$ab <- arr(as.character(g$ab))
  o
}

#' @noRd
summarize_payload_interval <- function(out, c) {
  arr <- summarize_arr
  # Per-row lists stay arrays even at length one: a collapsed tips vector
  # would index as characters in JS (the auto_unbox trap).
  out$segs <- lapply(c$segs, function(ss) if (length(ss)) ss else arr(list()))
  out$tips <- lapply(c$tips, function(tt) arr(as.character(tt)))
  out$fills <- arr(as.character(c$fills))
  out$d0 <- c$d0
  out$d1 <- c$d1
  if (isTRUE(c$dd)) out$dd <- TRUE
  if (isTRUE(c$lg)) out$lg <- TRUE
  out
}

#' @noRd
summarize_payload_spark <- function(out, c) {
  arr <- summarize_arr
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
  out
}

#' @noRd
summarize_drop_null <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, is.null, logical(1L))]
  lapply(x, function(e) {
    if (is.list(e) && !inherits(e, "AsIs")) summarize_drop_null(e) else e
  })
}

#' A tile cell's mark: a painted square with the value in it, or nothing.
#' Vectorised over rows; NA paint = an empty cell.
#' @noRd
summarize_tile_html <- function(bg, fg, disp) {
  ifelse(
    is.na(bg), "",
    paste0("<span class=\"blockr-summarize-tile\" style=\"background:", bg,
           ";color:", fg, "\">", disp, "</span>")
  )
}
