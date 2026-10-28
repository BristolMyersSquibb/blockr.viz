# Measuring text and fitting column widths for static_table()
# (R/export-pptx-table.R).

# Left plus right cell padding in inches. `NULL` means the flextable default.
ft_pad_width <- function(cell_padding) {
  if (is.null(cell_padding)) ft_side_padding() / 72 else 2 * cell_padding / 72
}

# Cell padding in points for a table that cannot fit any other way. The
# flextable default is 5.
TIGHT_PAD <- 2

# ---- measured column widths ------------------------------------------------
# Gives every column the width its own text needs, in this priority: a data
# cell never wraps, a header never breaks inside a word, headers unwrap toward
# their full text, and only then does the stub grow past `stub_share` of
# `total`. The stub is the only column with prose in it, so it is the one that
# wraps. Text is measured with systemfonts at the table's font and size;
# `blockr.viz.ft_width_slack` covers the error when the font is not installed.
#
# Returns widths in inches summing to `total`, or NULL when the table cannot be
# measured (the caller then falls back to the positional split). Attributes:
#   `header_squeeze` -- the headers break inside a word.
#   `cell_squeeze`   -- the data cells no longer fit, so PowerPoint would stack
#     their characters. Only less padding or a smaller font helps.
#   `word_fit`       -- the fraction of the longest header word the narrowest
#     column holds: 1 never breaks, 0.5 takes two lines.
ft_measured_widths <- function(stub, stub_indent, stub_label, cells, leaf, top,
                               font, font_size, total, banner = character(),
                               pad = ft_side_padding() / 72,
                               slack = getOption("blockr.viz.ft_width_slack",
                                                 1.04),
                               stub_min = 1.2,
                               stub_share = getOption("blockr.viz.ft_stub_share",
                                                      0.3)) {

  n_data <- ncol(cells)

  if (!requireNamespace("systemfonts", quietly = TRUE) ||
        !is.numeric(total) || length(total) != 1L || !is.finite(total) ||
        total <= 0 || n_data < 1L) {
    return(NULL)
  }

  squeeze <- c(header = FALSE, cell = FALSE)
  word_fit <- 1

  out <- tryCatch(
    {
      # Section header rows are merged across the table, so they are not in
      # `stub` and never widen the column.
      stub_cells <- ft_text_widths(stub, font, font_size) + stub_indent / 72
      # The stub never gets so narrow that a word breaks.
      stub_floor <- max(
        min(stub_min, total / 3),
        ft_word_width(c(stub, stub_label), font, font_size) + pad
      )
      stub_nat <- max(c(stub_cells, 0)) + pad
      stub_want <- max(stub_nat, stub_floor)
      # What the stub keeps when the table is squeezed: its floor, or its own
      # width when that is smaller.
      stub_keep <- min(stub_nat, stub_floor)

      # A data column asks for three widths. `want`: its widest cell and its
      # header's longest word. `lux`: also the whole header on its written
      # lines. `bare`: the least it can take, its longest word.
      cell_w <- vapply(
        seq_len(n_data),
        function(j) max(c(ft_text_widths(cells[, j], font, font_size), 0)),
        numeric(1L)
      ) + pad
      cell_word <- vapply(
        seq_len(n_data),
        function(j) ft_word_width(cells[, j], font, font_size),
        numeric(1L)
      ) + pad
      head_word <- vapply(leaf, ft_word_width, numeric(1L),
                          font = font, size = font_size) + pad
      head_full <- ft_text_widths(leaf, font, font_size) + pad

      # A spanner's demand is shared across its group.
      span_word <- rep(0, n_data)
      span_full <- rep(0, n_data)
      if (any(nzchar(top))) {
        runs <- rle(top)
        at <- 0L
        for (i in seq_along(runs$values)) {
          k <- runs$lengths[[i]]
          if (nzchar(runs$values[[i]])) {
            span_word[at + seq_len(k)] <-
              (ft_word_width(runs$values[[i]], font, font_size) + pad) / k
            span_full[at + seq_len(k)] <-
              (ft_text_widths(runs$values[[i]], font, font_size) + pad) / k
          }
          at <- at + k
        }
      }

      need <- cell_w * slack
      want <- pmax(need, head_word, span_word)
      lux <- pmax(want, head_full, span_full)
      # Nothing may break inside a word, in a cell or in a header.
      bare <- pmax(cell_word, head_word, span_word)

      # Columns with the same leaf label (the same statistic under different
      # arms) get the same width. With all-distinct labels the whole data side
      # is one class.
      role <- if (anyDuplicated(leaf)) leaf else rep("", n_data)
      need <- ft_group_max(need, role)
      want <- ft_group_max(want, role)
      lux <- ft_group_max(lux, role)
      bare <- ft_group_max(bare, role)

      if (sum(want) + stub_floor > total) {
        # Nothing fits at full width. Every column, the stub included, is
        # filled from its longest word toward its whole text to a common
        # level: counts (one word) reach full width at once, prose wraps.
        squeeze[["header"]] <- TRUE
        w <- ft_water_fill(c(stub_want, want), total, c(stub_floor, bare))
        stub_w <- w[[1L]]
        data_w <- w[-1L]
        # Only a cell narrower than its longest word counts as a cell
        # squeeze. A header breaking is reported through `word_fit`.
        squeeze[["cell"]] <- any(data_w < cell_word - 1e-9)
      } else {
        # Whether the table needs enough of the slide to fill it.
        fills <- stub_want + sum(want) >= 0.7 * total

        # What the stub claims before the data columns have had their turn.
        stub_cap <- max(stub_floor, stub_share * total)
        stub_w <- min(stub_want, stub_cap, total - sum(want))
        data_w <- want
        room <- total - stub_w - sum(data_w)

        # First, unwrap the headers in proportion to how far each is from its
        # full text.
        short <- pmax(lux - data_w, 0)
        if (room > 0 && sum(short) > 0) {
          data_w <- data_w + short * min(1, room / sum(short))
          room <- total - stub_w - sum(data_w)
        }

        # Second, the stub up toward its own text.
        if (room > 0 && stub_w < stub_want) {
          stub_w <- stub_w + min(room, stub_want - stub_w)
          room <- total - stub_w - sum(data_w)
        }

        if (fills) {
          # Spread what is left evenly over the data columns.
          if (room > 0) data_w <- data_w + room / n_data
        } else {
          # A small table keeps its natural size (the pptx writer centres
          # it). It grows only enough to hold its title line on one line.
          banner_w <- max(c(
            ft_text_widths(banner, font, font_size + 2) + pad, 0
          ))
          grow <- min(banner_w, total) - (stub_w + sum(data_w))
          if (is.finite(grow) && grow > 0) {
            data_w <- data_w + grow / n_data
          }
        }
      }

      # How much of its longest word the tightest header keeps.
      word_need <- pmax(head_word, span_word)
      if (any(word_need > 0)) {
        word_fit <- min(1, min(data_w[word_need > 0] / word_need[word_need > 0]))
      }

      c(stub_w, data_w)
    },
    error = function(e) NULL
  )

  if (!is.numeric(out) || length(out) != n_data + 1L || any(!is.finite(out)) ||
        any(out <= 0)) {
    return(NULL)
  }

  attr(out, "header_squeeze") <- unname(squeeze[["header"]])
  attr(out, "cell_squeeze") <- unname(squeeze[["cell"]])
  attr(out, "word_fit") <- word_fit
  out
}

# ---- estimated rendered heights --------------------------------------------
# How tall each row of a part will come out, in inches, once the text is
# wrapped into the table's column widths. The stated row height is a floor,
# since PowerPoint grows a row to fit its content. The estimate wraps greedily
# on whitespace and is not exact when the font is not installed.
ft_part_heights <- function(ft, part = "body") {

  p <- ft[[part]]

  if (is.null(p) || !nrow(p$dataset) ||
        !requireNamespace("systemfonts", quietly = TRUE)) {
    return(numeric(0))
  }

  w <- ft$body$colwidths
  spans <- p$spans$rows
  n_row <- nrow(p$dataset)
  n_col <- length(w)

  sz <- p$styles$text$font.size$data
  bold <- p$styles$text$bold$data
  fam <- p$styles$text$font.family$data
  pl <- p$styles$pars$padding.left$data
  pr <- p$styles$pars$padding.right$data
  pt <- p$styles$pars$padding.top$data
  pb <- p$styles$pars$padding.bottom$data

  out <- rep(0, n_row)

  for (j in seq_len(n_col)) {
    txt <- ft_cell_text(p, j)
    span <- spans[, j]
    # A merged cell is measured across the columns it covers; the cells it
    # covers are skipped.
    avail <- vapply(seq_len(n_row), function(i) {
      if (span[[i]] < 1) return(NA_real_)
      sum(w[j:min(n_col, j + span[[i]] - 1L)]) -
        (pl[i, j] + pr[i, j]) / 72
    }, numeric(1L))

    for (i in seq_len(n_row)) {
      if (is.na(avail[[i]])) next
      size <- sz[i, j]
      lines <- ft_line_count(txt[[i]], avail[[i]], fam[i, j], size,
                             isTRUE(bold[i, j]))
      h <- lines * size * 1.2 / 72 + (pt[i, j] + pb[i, j]) / 72
      if (h > out[[i]]) out[[i]] <- h
    }
  }

  stated <- p$rowheights
  if (is.numeric(stated) && length(stated) == n_row) {
    out <- pmax(out, stated)
  }

  out
}

# The text one column of a flextable part prints, row by row. Read from the
# chunk structure, because `part$dataset` holds the column keys and not the
# header labels. The dataset is the fallback when there are no chunks.
ft_cell_text <- function(p, j) {

  n_row <- nrow(p$dataset)
  chunks <- p$content$data

  if (is.null(chunks) || ncol(chunks) < j) {
    out <- as.character(p$dataset[[j]])
    out[is.na(out)] <- ""
    return(out)
  }

  vapply(seq_len(n_row), function(i) {
    cell <- chunks[[i, j]]
    if (is.null(cell) || is.null(cell$txt)) {
      return("")
    }
    txt <- as.character(cell$txt)
    txt[is.na(txt)] <- ""
    paste(txt, collapse = "")
  }, character(1L))
}

# Lines a string takes at a given width, wrapping greedily on whitespace and
# breaking hard at "\n". A word wider than the column gets its own line.
ft_line_count <- function(x, width, font, size, bold = FALSE) {

  x <- as.character(x)

  if (!length(x) || is.na(x) || !nzchar(x) || !is.finite(width) ||
        width <= 0) {
    return(1L)
  }

  weight <- if (bold) "bold" else "normal"
  wid <- function(s) {
    systemfonts::string_width(s, family = font, size = size, weight = weight,
                              res = 72) / 72
  }

  if (wid(x) <= width && !grepl("\n", x, fixed = TRUE)) {
    return(1L)
  }

  lines <- 0L
  for (para in strsplit(x, "\n", fixed = TRUE)[[1L]]) {
    words <- strsplit(para, "[[:space:]]+")[[1L]]
    words <- words[nzchar(words)]
    if (!length(words)) {
      lines <- lines + 1L
      next
    }
    ww <- wid(words)
    # The width of a space differs between faces.
    space <- max(0, wid("x x") - wid("xx"))
    n <- 1L
    cur <- ww[[1L]]
    for (k in seq_along(words)[-1L]) {
      if (cur + space + ww[[k]] <= width) {
        cur <- cur + space + ww[[k]]
      } else {
        n <- n + 1L
        cur <- ww[[k]]
      }
    }
    lines <- lines + n
  }

  max(1L, lines)
}

# Lift every element to the largest value in its group, keeping the order.
ft_group_max <- function(x, by) {
  as.numeric(stats::ave(x, by, FUN = max))
}

# Shrink the widest columns first (max-min fair), never below `mins`, and
# rescale to `budget`. When even the minimums do not fit, the result is
# proportional to what each column needs.
ft_water_fill <- function(want, budget, mins) {
  cap <- function(x) sum(pmax(mins, pmin(want, x)))
  lo <- 0
  hi <- max(want)
  for (i in seq_len(60L)) {
    mid <- (lo + hi) / 2
    if (cap(mid) > budget) hi <- mid else lo <- mid
  }
  w <- pmax(mins, pmin(want, lo))
  w * budget / sum(w)
}

# Width in inches of each string's longest line ("\n" is a hard break). Bold,
# because emphasis rows are the widest and must not wrap.
ft_text_widths <- function(x, font, size) {

  x <- as.character(x)
  x[is.na(x)] <- ""

  if (!length(x)) {
    return(numeric(0))
  }

  w <- systemfonts::string_width(x, family = font, size = size,
                                 weight = "bold", res = 72) / 72
  for (i in which(grepl("\n", x, fixed = TRUE))) {
    parts <- strsplit(x[[i]], "\n", fixed = TRUE)[[1L]]
    w[[i]] <- max(systemfonts::string_width(parts, family = font, size = size,
                                            weight = "bold", res = 72)) / 72
  }

  w
}

# Width in inches of the widest single word.
ft_word_width <- function(x, font, size) {

  words <- unlist(strsplit(as.character(x), "[[:space:]]+"))
  words <- words[!is.na(words) & nzchar(words)]

  if (!length(words)) {
    return(0)
  }

  max(systemfonts::string_width(words, family = font, size = size,
                                weight = "bold", res = 72)) / 72
}

# Left plus right cell padding in points: the flextable default.
ft_side_padding <- function() {
  d <- flextable::get_flextable_defaults()
  sum(vapply(c("padding.left", "padding.right"),
             function(k) if (is.numeric(d[[k]])) d[[k]] else 5,
             numeric(1L)))
}

# Readable text color for a fill, by luminance (Rec. 601). Takes hex or R color
# names; NA gives NA (no override).
ft_contrast_text <- function(bg) {
  vapply(bg, function(col) {
    if (is.na(col)) return(NA_character_)
    lum <- tryCatch(
      sum(grDevices::col2rgb(col)[, 1L] * c(0.299, 0.587, 0.114)),
      error = function(e) 255
    )
    if (lum < 140) "#FFFFFF" else "#333333"
  }, character(1L), USE.NAMES = FALSE)
}

# The emphasis ramp for column .strong / .emph: normal, emph and strong map to
# light gray, dark gray and an accent. Override with
# `blockr.viz.ft_emphasis_colors`.
FT_EMPHASIS_DEFAULT <- c(normal = "#EEEEEE", emph = "#9AA3B0",
                         strong = "#2563EB")

# A theme's `bands` role as the emphasis triple: the `.stub` band is `normal`
# and the first pool entry is the accent. NULL unless the theme supplies both.
ft_emphasis_from_theme <- function() {
  bands <- viz_palette("bands")
  if (is.null(bands)) {
    return(NULL)
  }
  nm <- names(bands) %||% rep("", length(bands))
  stub <- bands[nm == ".stub"]
  pool <- unname(bands[nm != ".stub"])
  if (!length(stub) || !length(pool)) {
    return(NULL)
  }
  c(normal = unname(stub[[1L]]),
    emph = pool[[min(2L, length(pool))]],
    strong = pool[[1L]])
}

# Per-column bg / text for emphasis mode. The stub takes the normal band.
ft_emphasis_bands <- function(col_strong, col_emph) {
  cols <- getOption("blockr.viz.ft_emphasis_colors", NULL) %||%
    ft_emphasis_from_theme() %||% FT_EMPHASIS_DEFAULT
  pick <- function(key) cols[[key]]
  bg <- ifelse(col_strong, pick("strong"),
               ifelse(col_emph, pick("emph"), pick("normal")))
  list(
    bg = unname(bg), text = ft_contrast_text(unname(bg)),
    stub_bg = unname(pick("normal")),
    stub_text = ft_contrast_text(unname(pick("normal")))
  )
}

# Resolve `header_bg` to per-data-column fills, keyed on the column groups the
# annotated df carries. `header_bg` is a color vector (hex or R names):
#   * Named entries pin a group: the name is the spanner top for nested `by`,
#     else the data-column name. ".stub" pins the row-stub header.
#   * Unnamed entries are the cycle pool for the unpinned groups, in order of
#     first appearance.
#   * The two mix: `c(.stub = "#EEE", Placebo = "grey", "#33D6F1")`.
# `NULL`, `FALSE` and `"none"` mean no fill. Returns per-column bg / text (NA =
# unfilled) plus stub_bg / stub_text, or NULL when there is nothing to fill.
resolve_header_bands <- function(header_bg, top, data_cols) {
  n <- length(data_cols)
  if (n == 0L) return(NULL)
  if (is.null(header_bg) || isFALSE(header_bg) ||
        identical(header_bg, "none")) {
    return(NULL)
  }

  nm <- names(header_bg)
  header_bg <- as.character(header_bg)
  if (is.null(nm)) nm <- rep("", length(header_bg))
  pins <- header_bg[nzchar(nm)]
  names(pins) <- nm[nzchar(nm)]
  pool <- header_bg[!nzchar(nm)]

  # Group key per data column: the spanner top, else the column name.
  keys <- ifelse(nzchar(top), top, data_cols)
  groups <- unique(keys)
  gcol <- stats::setNames(rep(NA_character_, length(groups)), groups)
  taken <- 0L
  for (g in groups) {
    if (g %in% names(pins)) {
      gcol[[g]] <- pins[[g]]
    } else if (length(pool)) {
      taken <- taken + 1L
      gcol[[g]] <- pool[[((taken - 1L) %% length(pool)) + 1L]]
    }
  }

  bg <- unname(gcol[keys])
  stub_bg <- if (".stub" %in% names(pins)) pins[[".stub"]] else NA_character_

  list(
    bg = bg, text = ft_contrast_text(bg),
    stub_bg = stub_bg, stub_text = ft_contrast_text(stub_bg)
  )
}
