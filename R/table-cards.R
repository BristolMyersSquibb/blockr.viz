# The cards look of the table block -------------------------------------------
# `look = "cards"` draws the same table as a grid of cards: one card per
# summary (groups layout) or per column (records layout), one row of cards per
# row of the table.

# The summary types a card cannot draw: a swimlane needs the width of a table
# row, and an expr summary has no main value to print large.
CARDS_SKIP_TYPES <- c("spans", "expr")

# A records table with more rows than this draws its first rows as cards and
# says how many it left out. Cards are for a handful of numbers.
CARDS_MAX_ROWS <- 60L

#' A card's number: one precision per column, thousands separated.
#' @noRd
cards_num <- function(x, dig = NA_integer_) {
  x <- suppressWarnings(as.numeric(x))
  out <- if (is.na(dig)) {
    trimws(formatC(x, format = "fg", digits = 4L, big.mark = ","))
  } else {
    formatC(x, format = "f", digits = dig, big.mark = ",")
  }
  out[is.na(x)] <- ""
  # A minus sign, not a hyphen: at this size the hyphen reads as a dash.
  sub("^-", "\u2212", out)
}

#' The card model of a groups-layout table.
#'
#' @param m The cell model, [summarize_cells()].
#' @param prep The prepared table, [summarize_prepare()].
#' @param data The table's input, for the row colours' scale-map lookup.
#' @param scale_map The board scale map, or `NULL`.
#' @return A list: `n`, `multi` (draw row heads and column heads), `label`
#'   and `hex` per row, `cols` (one entry per card column), `skipped` (the
#'   summaries left out).
#' @noRd
table_cards_groups <- function(m, prep, data = NULL, scale_map = NULL) {
  rows <- prep$rows
  n <- nrow(rows)
  plan <- prep$plan
  cols <- list()
  skipped <- character()
  for (k in seq_along(plan)) {
    p <- plan[[k]]
    c <- m$cols[[k]]
    if (isTRUE(p$stype %in% CARDS_SKIP_TYPES) ||
          identical(p$kind, "interval")) {
      skipped <- c(skipped, p$sname %||% p$label %||% "")
      next
    }
    cols[[length(cols) + 1L]] <- cards_groups_col(p, c, k, rows, prep)
  }
  ord <- unique(skipped)
  list(
    n = n,
    # The single row of an ungrouped table needs no row head, and its cards
    # carry their own labels.
    multi = n > 1L || !isTRUE(prep$all),
    label = as.character(rows$.label),
    hex = cards_row_hex(rows, prep, data, scale_map),
    cols = cols,
    skipped = ord
  )
}

#' One column of cards from one plan entry.
#' @noRd
cards_groups_col <- function(p, c, k, rows, prep) {
  n <- nrow(rows)
  kind <- p$kind %||% "num"
  get <- function(nm) {
    if (is.null(nm) || is.na(nm) || is.null(rows[[nm]])) {
      rep(NA_real_, n)
    } else {
      rows[[nm]]
    }
  }
  dom <- summarize_axis_domain(p, prep)
  digits <- function(...) {
    summarize_val_digits(c(...), dom$d0 %||% p$dmin, dom$d1 %||% p$dmax)
  }
  sub <- NULL
  # The statistic the grey line holds ("Q1-Q3", "95% CI"): a card in a
  # single row prints it, a column of cards names it once in its head.
  word <- NULL
  text <- FALSE
  if (kind %in% c("box", "pointrange") && !is.null(p$cols)) {
    cn <- p$cols
    bc <- get(cn["bc"])
    bl <- get(cn["bl"])
    bh <- get(cn["bh"])
    dig <- digits(bc, bl, bh)
    main <- cards_num(bc, dig)
    if (!is.na(cn["bl"])) {
      word <- p$words$range %||% "Range"
      sub <- ifelse(
        is.na(bl) | is.na(bh), "",
        paste0(cards_num(bl, dig), " to ", cards_num(bh, dig))
      )
    }
  } else if (identical(kind, "pair")) {
    a <- get(p$cols[["a"]])
    b <- get(p$cols[["b"]])
    dig <- digits(a, b)
    main <- cards_num(b, dig)
    sub <- ifelse(is.na(a), "", paste0("from ", cards_num(a, dig)))
  } else if (identical(kind, "sparkline")) {
    last <- get(p$key)
    pts <- rows[[p$pts %||% ".pts"]]
    first <- vapply(pts %||% vector("list", n), function(s) {
      y <- s$y
      if (length(y)) as.numeric(y[[1L]]) else NA_real_
    }, numeric(1L))
    dig <- digits(last, first)
    main <- cards_num(last, dig)
    sub <- ifelse(is.na(first), "", paste0("from ", cards_num(first, dig)))
  } else if (identical(p$dispkey, "dist_text")) {
    bc <- get(p$key)
    bl <- get(p$lo)
    bh <- get(p$hi)
    dig <- summarize_val_digits(c(bc, bl, bh))
    main <- cards_num(bc, dig)
    sub <- ifelse(is.na(bl) | is.na(bh), "",
                  paste0(cards_num(bl, dig), " to ", cards_num(bh, dig)))
  } else if (isTRUE(p$text) || (!is.null(p$alt_text) && is.null(rows[[p$key]]))) {
    v <- as.character(rows[[p$key]] %||% rows[[p$alt_text]])
    v[is.na(v)] <- ""
    main <- v
    text <- TRUE
  } else {
    v <- if (identical(kind, "barsplit") && is.null(p$key)) {
      rowSums(vapply(p$series, function(lv) {
        x <- rows[[paste0(p$prefix %||% ".s_", lv)]]
        if (is.null(x)) rep(0, n) else as.numeric(x)
      }, numeric(n)))
    } else {
      get(p$key)
    }
    main <- if (is.null(prep$format)) {
      cards_num(v, summarize_val_digits(v))
    } else {
      out <- rep("", n)
      ok <- !is.na(v)
      out[ok] <- sub("^-", "\u2212", dt_format_num(v[ok], prep$format,
                                                    big.mark = ","))
      out
    }
    # The unit after the number, as on a records card.
    if (!is.null(prep$unit) && isTRUE(p$unit_ok)) {
      main <- ifelse(nzchar(main), paste(main, prep$unit), main)
    }
    # A count against an N prints its share in the table ("72 (28%)"): the
    # card keeps it as the grey line.
    den <- p$val_denom
    # A single row is all of its N, which says nothing.
    if (is.numeric(den) && length(den) == 1L && is.finite(den) && den > 0 &&
          !all(is.na(v) | abs(v - den) < 1e-9)) {
      sub <- ifelse(is.na(v), "",
                    paste0(formatC(v / den * 100, format = "f", digits = 0L),
                           "% of ", cards_num(den, 0L)))
    }
  }

  glyph <- kind %in% c("bar", "barsplit", "bardiv", "box", "pointrange",
                       "pair", "sparkline")
  range <- if (glyph && !is.null(dom) && !isTRUE(dom$date)) {
    d0 <- dom$d0
    d1 <- dom$d1
    unit <- if (isTRUE(dom$pct)) "%" else ""
    # A count drawn as a share of its N (a faceted count): the card's number
    # is the count, so the head names the count scale, not the percentage.
    den <- p$val_denom
    if (isTRUE(dom$pct) && is.numeric(den) && length(den) == 1L &&
          is.finite(den) && den > 0 && !identical(p$mode, "percent")) {
      d0 <- d0 * den / 100
      d1 <- d1 * den / 100
      unit <- ""
    }
    # The column's domain in two significant digits: the cards have no
    # axis, so the head says what the marks are drawn against.
    paste0(lane_fmt(signif(d0, 2L)), unit, " to ",
           lane_fmt(signif(d1, 2L)), unit)
  }
  legend <- if (identical(kind, "barsplit")) {
    cards_legend(c$names, c$fills)
  } else if (isTRUE(c$multi)) {
    cards_legend(c$levels, c$fills)
  }
  label <- p$sname %||% p$label %||% ""
  if (!is.null(p$flevel)) label <- paste0(label, " \u00b7 ", p$flevel)

  list(
    label = label,
    range = range,
    word = word,
    legend = legend,
    # Into the payload's `cols` (the stub is 0): the glyph the table draws.
    glyph = if (glyph) k,
    text = text,
    main = as.character(main),
    sub = if (!is.null(sub)) as.character(sub)
  )
}

#' @noRd
cards_legend <- function(levels, fills) {
  levels <- as.character(levels %||% character())
  fills <- as.character(fills %||% character())
  if (length(levels) < 2L || length(fills) < length(levels)) return(NULL)
  lapply(seq_along(levels), function(j) {
    list(label = if (nzchar(trimws(levels[[j]]))) levels[[j]] else "(Missing)",
         color = fills[[j]])
  })
}

#' A row's colour strip: its group level's colour in the board scale map.
#' Map-bound only, as the records table's default row tint is: an unbound
#' grouping (a subject id) gets no strip rather than a rainbow.
#' @noRd
cards_row_hex <- function(rows, prep, data, scale_map) {
  n <- nrow(rows)
  if (is.null(scale_map) || !is.data.frame(data) || !has_blockr_theme()) {
    return(NULL)
  }
  pal_for <- function(col) {
    if (is.null(col) || !col %in% names(data)) return(NULL)
    tryCatch(dd_resolve_scales(scale_map, col, data[[col]])$color,
             error = function(e) NULL)
  }
  inner <- pal_for(chr_state(prep$group))
  outer <- pal_for(chr_state(prep$parent))
  if (is.null(inner) && is.null(outer)) return(NULL)
  lab <- as.character(rows$.label)
  par <- if (is.null(rows$.is_parent)) {
    rep(FALSE, n)
  } else {
    rows$.is_parent %in% TRUE
  }
  key <- as.character(rows$.parent_key %||% rows$.label)
  out <- rep(NA_character_, n)
  if (!is.null(inner)) out[!par] <- unname(inner[lab[!par]])
  if (!is.null(outer)) out[par] <- unname(outer[key[par]])
  if (all(is.na(out))) NULL else out
}

#' The card model of a records-layout table: each input row is a row of
#' cards, each value column a card with the column's number format.
#' @param data The displayed frame.
#' @param label_col,value_cols The stub and the value columns (`NULL` = the
#'   first column, and every other column).
#' @param digits The number format of numeric cells (table_format_state()).
#' @param unit Printed after each number, as on a tile.
#' @param row_hex Per-row colour strip, or `NULL`.
#' @noRd
table_cards_records <- function(data, label_col = NULL, value_cols = NULL,
                                digits = "number", unit = NULL, row_hex = NULL) {
  unit <- chr_state(trimws(as.character(unlist(unit))))
  if (is.null(label_col) || !label_col %in% names(data)) {
    label_col <- names(data)[1L]
  }
  if (is.null(value_cols)) value_cols <- setdiff(names(data), label_col)
  value_cols <- intersect(value_cols, names(data))
  n_all <- nrow(data)
  n <- min(n_all, CARDS_MAX_ROWS)
  idx <- seq_len(n)
  cols <- lapply(value_cols, function(vc) {
    x <- data[[vc]][idx]
    num <- is.numeric(x)
    main <- if (num) {
      out <- rep("", n)
      ok <- !is.na(x)
      out[ok] <- dt_format_num(x[ok], digits, big.mark = ",")
      if (!is.null(unit)) out[ok] <- paste(out[ok], unit)
      sub("^-", "\u2212", out)
    } else {
      v <- as.character(x)
      v[is.na(v)] <- ""
      v
    }
    list(label = dt_col_label(data[[vc]], vc) %||% vc, text = !num,
         main = main)
  })
  stub <- as.character(data[[label_col]][idx])
  stub[is.na(stub)] <- "NA"
  list(
    n = n,
    multi = n > 1L,
    label = stub,
    hex = if (length(row_hex) == n_all) row_hex[idx],
    cols = cols,
    fold = if (n_all > n) {
      paste0("Showing the first ", n, " of ", n_all, " rows as cards.")
    }
  )
}

#' The card model as JSON-ready lists: every per-row vector stays an array.
#' @noRd
cards_payload <- function(model) {
  if (is.null(model)) return(NULL)
  arr <- function(x) I(unname(as.character(x)))
  summarize_drop_null(list(
    n = model$n,
    multi = isTRUE(model$multi),
    label = arr(model$label),
    hex = if (!is.null(model$hex)) arr(model$hex),
    cols = lapply(model$cols, function(c) {
      summarize_drop_null(list(
        label = c$label, range = c$range, word = c$word,
        legend = c$legend, glyph = c$glyph,
        text = if (isTRUE(c$text)) TRUE,
        main = arr(c$main),
        sub = if (!is.null(c$sub)) arr(c$sub)
      ))
    }),
    skipped = if (length(model$skipped)) I(as.character(model$skipped)),
    fold = model$fold
  ))
}
