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
#'   bar      -> `w` (percent widths), `v` (sort values), `fill`, `sub`
#'   barsplit -> `seg` (list of per-series width vectors), `segv`, `fills`,
#'               `names`, `mode`
#'   bardiv   -> `w`, `v`, `pos` (polarity)
#'   num      -> `disp` (display strings), `v`
#' Widths are already rounded to the 2dp both consumers print.
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

  pct_w <- function(v) {
    w <- if (is.finite(mx) && mx > 0) {
      pmax(0, pmin(100, abs(v) / mx * 100))
    } else {
      rep(0, length(v))
    }
    w[!is.finite(w)] <- 0
    round(w, 2L)
  }

  cols <- lapply(seq_along(plan), function(i) {
    p <- plan[[i]]
    if (identical(p$kind, "bar")) {
      v <- rows[[p$key]]
      if (!is.null(p$denom) && is.finite(p$denom) && p$denom > 0) {
        v <- v / p$denom * 100
      }
      list(kind = "bar", w = pct_w(v), v = sortv(v), fill = p$fill,
           sub = rows$.level > 0L)
    } else if (identical(p$kind, "barsplit")) {
      mat <- vapply(p$series, function(lv) {
        x <- rows[[paste0(".s_", lv)]]
        if (is.null(x)) rep(0, n) else as.numeric(x)
      }, numeric(n))
      mat <- matrix(mat, nrow = n, dimnames = list(NULL, p$series))
      mat[!is.finite(mat)] <- 0
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
      list(kind = "barsplit", mode = p$mode %||% "stacked",
           names = as.character(p$series),
           fills = unname(prep$palette[as.character(p$series)]),
           seg = seg, segv = lapply(seq_along(p$series), function(j) mat[, j]),
           v = sortv(tot))
    } else if (identical(p$kind, "bardiv")) {
      v <- rows[[p$key]]
      w <- if (is.finite(mx) && mx > 0) pmin(50, abs(v) / mx * 50) else v * 0
      w[!is.finite(w)] <- 0
      list(kind = "bardiv", w = round(w, 2L), v = sortv(v),
           pos = !is.na(v) & v >= 0)
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
    thead = rank_thead(prep),
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
#' @noRd
rank_thead <- function(prep) {
  th <- c(
    as.character(dt_th(
      rank_label_header(prep), 0L, stub = TRUE,
      label = prep$group_label, sortable = TRUE
    )),
    vapply(seq_along(prep$plan), function(i) {
      p <- prep$plan[[i]]
      as.character(dt_th(
        p$label, i, label = p$sub_label,
        numeric = identical(p$kind, "num"), sortable = TRUE
      ))
    }, character(1L))
  )
  paste0("<thead><tr>", paste(th, collapse = ""), "</tr></thead>")
}

#' The fold row's text, or NULL when nothing was capped. Never a silent
#' truncation: `top_n` always says what fell below the cut.
#' @noRd
rank_fold_text <- function(prep) {
  if (!isTRUE(prep$folded > 0L)) return(NULL)
  paste0(
    "Other — ", prep$folded, " ",
    if (is.null(prep$parent)) "rows" else "groups", " below the cut",
    if (is.finite(prep$fold_max)) {
      paste0(", each with a value ≤ ",
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
             rank_track_html(c$w, c$fill, c$sub), "</td>")
    } else if (identical(c$kind, "barsplit")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_split_html(c), "</td>")
    } else if (identical(c$kind, "bardiv")) {
      paste0("<td class=\"blockr-rank-bar-col\"", rank_data_v(c$v), ">",
             rank_dv_html(c$w, c$pos), "</td>")
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
    " data-rank-level=\"", m$level, "\">"
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

#' One bar: a track div plus a fill div, vectorised over the column.
#' @noRd
rank_track_html <- function(w, fill = NULL, sub = FALSE) {
  sub <- rep_len(isTRUE(sub) | (is.logical(sub) & !is.na(sub) & sub), length(w))
  paste0(
    "<div class=\"blockr-rank-track", ifelse(sub, " is-sub", ""), "\">",
    "<div class=\"blockr-rank-fill\" style=\"width:", rank_fmt_w(w), "%",
    if (!is.null(fill)) paste0(";background:", fill) else "", "\"></div></div>"
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

# Per-element formatting: format() would align decimals across the vector
# ("100.00" beside "57.14"), while the JS assembler prints String(n) = "100".
# as.character() on a rounded value matches it exactly -- the same reason
# dt_bar_style() avoids format() for the table block's data bars.
#' @noRd
rank_fmt_w <- function(w) {
  as.character(w)
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
    } else if (identical(c$kind, "barsplit")) {
      out$mode <- c$mode
      out$names <- arr(as.character(c$names))
      out$fills <- arr(as.character(c$fills))
      out$seg <- lapply(c$seg, arr)
      out$segv <- lapply(c$segv, arr)
    } else if (identical(c$kind, "bardiv")) {
      out$w <- arr(c$w)
      out$pos <- arr(c$pos)
    } else {
      out$w <- arr(c$w)
      out$sub <- arr_if(c$sub)
      if (!is.null(c$fill)) out$fill <- c$fill
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
