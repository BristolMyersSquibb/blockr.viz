# Summarize table: the header markup and the marks as JSON ---------------------
#
# summarize_cells() -> the cell model (per-column widths / display strings /
#                          fills, plus the row meta and the <thead> tag); it
#                          lives in table-model-cells.R
# summarize_payload_col() -> one column's mark as the table view reads it
#                          (table_groups_view(), R/table-groups-view.R)

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

# --- the marks as JSON ----------------------------------------------------------

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
