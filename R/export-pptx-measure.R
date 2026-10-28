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
# `total`. The stub is the only column with prose, so it is the one that wraps.
# Text is measured with systemfonts; `blockr.viz.ft_width_slack` covers the
# error when the font is not installed.
#
# Returns widths in inches summing to `total`, or NULL when the table cannot be
# measured. Attributes: `header_squeeze` (a header breaks inside a word),
# `cell_squeeze` (the data cells no longer fit, so only less padding or a
# smaller font helps) and `word_fit` (the fraction of the longest header word
# the narrowest column holds).
ft_measured_widths <- function(stub, stub_indent, stub_label, cells, leaf, top,
                               font, font_size, total, banner = character(),
                               pad = ft_side_padding() / 72,
                               slack = getOption("blockr.viz.ft_width_slack",
                                                 1.04),
                               stub_min = 1.2,
                               stub_share = getOption("blockr.viz.ft_stub_share",
                                                      0.3)) {

  if (!requireNamespace("systemfonts", quietly = TRUE) ||
        !is.numeric(total) || length(total) != 1L || !is.finite(total) ||
        total <= 0 || ncol(cells) < 1L) {
    return(NULL)
  }

  out <- tryCatch(
    {
      d <- ft_width_demands(stub, stub_indent, stub_label, cells, leaf, top,
                            font, font_size, total, pad, slack, stub_min)
      ft_allocate_widths(d, total, banner, font, font_size, pad, stub_share)
    },
    error = function(e) NULL
  )

  if (!is.numeric(out) || length(out) != ncol(cells) + 1L ||
        any(!is.finite(out)) || any(out <= 0)) {
    return(NULL)
  }
  out
}

# What the stub and each data column ask for, in inches. A data column asks
# for three widths. `want`: its widest cell and its header's longest word.
# `lux`: also the whole header on its written lines. `bare`: the least it can
# take, its longest word. Section header rows are merged across the table, so
# they are not in `stub` and never widen the column.
ft_width_demands <- function(stub, stub_indent, stub_label, cells, leaf, top,
                             font, font_size, total, pad, slack, stub_min) {
  n_data <- ncol(cells)

  stub_cells <- ft_text_widths(stub, font, font_size) + stub_indent / 72
  # The stub never gets so narrow that a word breaks.
  stub_floor <- max(
    min(stub_min, total / 3),
    ft_word_width(c(stub, stub_label), font, font_size) + pad
  )
  stub_want <- max(max(c(stub_cells, 0)) + pad, stub_floor)

  per_col <- function(f) {
    vapply(seq_len(n_data), function(j) f(cells[, j]), numeric(1L)) + pad
  }
  cell_w <- per_col(function(x) max(c(ft_text_widths(x, font, font_size), 0)))
  cell_word <- per_col(function(x) ft_word_width(x, font, font_size))
  head_word <- vapply(leaf, ft_word_width, numeric(1L),
                      font = font, size = font_size) + pad
  head_full <- ft_text_widths(leaf, font, font_size) + pad

  # A spanner's demand is shared across its group.
  runs <- rle(top)
  span <- function(f) {
    w <- vapply(runs$values, function(s) {
      if (nzchar(s)) f(s, font, font_size) + pad else 0
    }, numeric(1L))
    rep(w / runs$lengths, runs$lengths)
  }
  span_word <- span(ft_word_width)
  span_full <- span(ft_text_widths)

  want <- pmax(cell_w * slack, head_word, span_word)
  lux <- pmax(want, head_full, span_full)
  # Nothing may break inside a word, in a cell or in a header.
  bare <- pmax(cell_word, head_word, span_word)

  # Columns with the same leaf label (the same statistic under different
  # arms) get the same width. With all-distinct labels the whole data side is
  # one class.
  role <- if (anyDuplicated(leaf)) leaf else rep("", n_data)

  list(
    stub_floor = stub_floor, stub_want = stub_want,
    want = ft_group_max(want, role), lux = ft_group_max(lux, role),
    bare = ft_group_max(bare, role), cell_word = cell_word,
    word_need = pmax(head_word, span_word)
  )
}

# Widths of the stub and data columns from their demands `d`, with the
# squeeze attributes.
ft_allocate_widths <- function(d, total, banner, font, font_size, pad,
                               stub_share) {
  cell_squeeze <- FALSE
  squeezed <- sum(d$want) + d$stub_floor > total
  if (squeezed) {
    # Nothing fits at full width. Every column, the stub included, is filled
    # from its longest word toward its whole text to a common level: counts
    # (one word) reach full width at once, prose wraps.
    w <- ft_water_fill(c(d$stub_want, d$want), total,
                       c(d$stub_floor, d$bare))
    # Only a cell narrower than its longest word counts as a cell squeeze. A
    # header breaking is reported through `word_fit`.
    cell_squeeze <- any(w[-1L] < d$cell_word - 1e-9)
  } else {
    w <- ft_spare_widths(d, total, banner, font, font_size, pad, stub_share)
  }

  # How much of its longest word the tightest header keeps.
  word_fit <- 1
  need <- d$word_need
  if (any(need > 0)) {
    word_fit <- min(1, min(w[-1L][need > 0] / need[need > 0]))
  }
  attr(w, "header_squeeze") <- squeezed
  attr(w, "cell_squeeze") <- cell_squeeze
  attr(w, "word_fit") <- word_fit
  w
}

# Widths when every column gets what it wants and there is room left over.
ft_spare_widths <- function(d, total, banner, font, font_size, pad,
                            stub_share) {
  n_data <- length(d$want)
  data_w <- d$want
  # What the stub claims before the data columns have had their turn.
  stub_cap <- max(d$stub_floor, stub_share * total)
  stub_w <- min(d$stub_want, stub_cap, total - sum(data_w))
  room <- total - stub_w - sum(data_w)

  # First, unwrap the headers in proportion to how far each is from its full
  # text.
  short <- pmax(d$lux - data_w, 0)
  if (room > 0 && sum(short) > 0) {
    data_w <- data_w + short * min(1, room / sum(short))
    room <- total - stub_w - sum(data_w)
  }

  # Second, the stub up toward its own text.
  if (room > 0 && stub_w < d$stub_want) {
    stub_w <- stub_w + min(room, d$stub_want - stub_w)
    room <- total - stub_w - sum(data_w)
  }

  if (d$stub_want + sum(d$want) >= 0.7 * total) {
    # The table fills the slide: spread what is left over the data columns.
    if (room > 0) data_w <- data_w + room / n_data
  } else {
    # A small table keeps its natural size (the pptx writer centres it). It
    # grows only enough to hold its title line on one line.
    banner_w <- max(c(ft_text_widths(banner, font, font_size + 2) + pad, 0))
    grow <- min(banner_w, total) - (stub_w + sum(data_w))
    if (is.finite(grow) && grow > 0) data_w <- data_w + grow / n_data
  }
  c(stub_w, data_w)
}

# ---- estimated rendered heights --------------------------------------------
# How tall each row of a part will come out, in inches, once the text is
# wrapped into the column widths. The stated row height is a floor. The
# estimate wraps greedily on whitespace.
ft_part_heights <- function(ft, part = "body") {

  p <- ft[[part]]

  if (is.null(p) || !nrow(p$dataset) ||
        !requireNamespace("systemfonts", quietly = TRUE)) {
    return(numeric(0))
  }

  w <- ft$body$colwidths
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
    span <- p$spans$rows[, j]
    for (i in seq_len(n_row)) {
      # A merged cell is measured across the columns it covers; the cells it
      # covers are skipped.
      if (span[[i]] < 1) next
      avail <- sum(w[j:min(n_col, j + span[[i]] - 1L)]) -
        (pl[i, j] + pr[i, j]) / 72
      lines <- ft_line_count(txt[[i]], avail, fam[i, j], sz[i, j],
                             isTRUE(bold[i, j]))
      h <- lines * sz[i, j] * 1.2 / 72 + (pt[i, j] + pb[i, j]) / 72
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
# chunk structure, since `part$dataset` holds column keys, not header labels.
ft_cell_text <- function(p, j) {

  chunks <- p$content$data

  if (is.null(chunks) || ncol(chunks) < j) {
    out <- as.character(p$dataset[[j]])
    out[is.na(out)] <- ""
    return(out)
  }

  vapply(seq_len(nrow(p$dataset)), function(i) {
    cell <- chunks[[i, j]]
    if (is.null(cell) || is.null(cell$txt)) {
      return("")
    }
    txt <- as.character(cell$txt)
    txt[is.na(txt)] <- ""
    paste(txt, collapse = "")
  }, character(1L))
}

# Width in inches of each string, with bold weight unless `weight` says
# otherwise.
ft_string_width <- function(x, font, size, weight = "bold") {
  systemfonts::string_width(x, family = font, size = size, weight = weight,
                            res = 72) / 72
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
  wid <- function(s) ft_string_width(s, font, size, weight)

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

  w <- ft_string_width(x, font, size)
  for (i in which(grepl("\n", x, fixed = TRUE))) {
    w[[i]] <- max(ft_string_width(strsplit(x[[i]], "\n", fixed = TRUE)[[1L]],
                                  font, size))
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

  max(ft_string_width(words, font, size))
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

# The emphasis ramp for column .strong / .emph. Override with
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
  bg <- unname(ifelse(col_strong, cols[["strong"]],
                      ifelse(col_emph, cols[["emph"]], cols[["normal"]])))
  list(
    bg = bg, text = ft_contrast_text(bg),
    stub_bg = unname(cols[["normal"]]),
    stub_text = ft_contrast_text(unname(cols[["normal"]]))
  )
}

# Resolve `header_bg` (see static_table()) to per-data-column fills, keyed on
# the column groups: the spanner top, else the data-column name. Named entries
# pin a group (".stub" pins the row-stub header). Unnamed entries cycle over
# the unpinned groups in order of first appearance. `NULL`, `FALSE` and
# `"none"` mean no fill. Returns per-column bg / text (NA = unfilled) plus
# stub_bg / stub_text, or NULL when there is nothing to fill.
resolve_header_bands <- function(header_bg, top, data_cols) {
  if (length(data_cols) == 0L) return(NULL)
  if (is.null(header_bg) || isFALSE(header_bg) ||
        identical(header_bg, "none")) {
    return(NULL)
  }

  nm <- names(header_bg) %||% rep("", length(header_bg))
  header_bg <- as.character(header_bg)
  pins <- header_bg[nzchar(nm)]
  names(pins) <- nm[nzchar(nm)]
  pool <- header_bg[!nzchar(nm)]

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
