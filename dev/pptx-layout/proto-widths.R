# Prototype of a content-measured width allocator for static_table(), plus the
# height budget that a pagination pass would need. Nothing here touches the
# package yet -- this is the arithmetic the study renders, kept in one file so
# it can be lifted into R/static-table.R once the shape is agreed.
#
# The rule the current code follows is positional: the stub gets
# `first_col_width` (capped at half the slide) and the data columns split
# whatever is left, equally. That is where both complaints come from -- a stub
# wider than its own text while the header cells wrap two and three lines deep.
#
# The rule below is measured: every column asks for what its content needs,
# numbers are never allowed to wrap, headers are never allowed to break inside
# a word, and the stub -- the one column whose content is prose and wraps
# gracefully -- absorbs the difference.

# Width of the widest line in each string, in inches, at a given face.
str_w <- function(x, font, size, bold = FALSE) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # A cell's own "\n" is a hard break (the Big-N convention), so a multi-line
  # cell asks only for its longest line.
  parts <- strsplit(x, "\n", fixed = TRUE)
  vapply(parts, function(p) {
    if (!length(p)) return(0)
    max(systemfonts::string_width(
      p, family = font, size = size,
      weight = if (bold) "bold" else "normal", res = 72
    )) / 72
  }, numeric(1))
}

# Widest single word -- the width below which a header cell starts breaking
# mid-word, which is the ugly failure mode.
word_w <- function(x, font, size, bold = TRUE) {
  words <- unlist(strsplit(as.character(x), "[ \n]+"))
  if (!length(words)) return(0)
  max(str_w(words, font, size, bold))
}

#' Measure a table's column demands
#'
#' @param body character matrix of formatted cells (stub in column 1)
#' @param leaf,top leaf header labels / spanner tops for the data columns
#' @return a list of per-column widths in inches: `body` (widest data cell),
#'   `word` (widest unbreakable header word), `head` (header on one line)
measure_cols <- function(body, stub_label, leaf, top, font = "Arial",
                         size = 14, pad = 0.07) {

  n <- ncol(body)
  body_w <- vapply(seq_len(n), function(j) {
    if (!nrow(body)) return(0)
    max(str_w(body[, j], font, size, bold = TRUE))
  }, numeric(1))

  head_txt <- c(stub_label, leaf)
  head_w <- str_w(head_txt, font, size, bold = TRUE)
  word <- vapply(head_txt, word_w, numeric(1), font = font, size = size)

  # A spanner sits over its whole group, so its demand is shared: it only
  # forces width when the group is narrower than the spanner's longest word.
  if (any(nzchar(top))) {
    runs <- rle(top)
    at <- 1L
    for (i in seq_along(runs$values)) {
      k <- runs$lengths[[i]]
      j <- at + seq_len(k)                       # +1 for the stub column
      if (nzchar(runs$values[[i]])) {
        w <- word_w(runs$values[[i]], font, size)
        word[j] <- pmax(word[j], (w + 2 * pad) / k - 2 * pad)
      }
      at <- at + k
    }
  }

  list(body = body_w + 2 * pad, head = head_w + 2 * pad,
       word = word + 2 * pad)
}

#' Distribute a width budget over measured columns
#'
#' Priority, highest first: data cells never wrap; header words never break;
#' the stub takes what is left, floored so it never collapses; slack goes back
#' to the stub up to its own natural width, then spreads over the data columns.
#'
#' @return numeric widths, or `NULL` when even the minimum does not fit (the
#'   caller's cue to drop a font step or split the columns over two slides)
allocate_widths <- function(m, total, stub_floor = 1.6) {

  n <- length(m$body)
  data_j <- seq_len(n)[-1]

  want <- pmax(m$body, m$word)[data_j]           # never wrap, never break
  need <- m$body[data_j]                          # absolute floor: the numbers
  stub_nat <- max(m$body[1], m$word[1])

  if (sum(need) + stub_floor > total) {
    return(NULL)
  }

  if (sum(want) + stub_floor <= total) {
    stub <- min(stub_nat, total - sum(want))
    slack <- total - stub - sum(want)
    data <- want + slack / n
  } else {
    data <- water_fill(want, total - stub_floor, need)
    stub <- total - sum(data)
  }

  c(stub, data)
}

# Shrink the widest columns first (max-min fair), never below `mins`.
water_fill <- function(want, budget, mins) {
  f <- function(cap) sum(pmax(mins, pmin(want, cap)))
  lo <- 0
  hi <- max(want)
  for (i in seq_len(60)) {
    mid <- (lo + hi) / 2
    if (f(mid) > budget) hi <- mid else lo <- mid
  }
  w <- pmax(mins, pmin(want, lo))
  w * budget / sum(w)
}

#' The largest font size at which the table still fits its width budget
#'
#' The manual version of PowerPoint's "shrink text on overflow": step down the
#' ladder until the measured minimum fits, and report what it cost.
fit_font_size <- function(body, stub_label, leaf, top, total, font = "Arial",
                          sizes = c(14, 13, 12, 11, 10, 9), pad = 0.07,
                          stub_floor = 1.6) {
  for (s in sizes) {
    m <- measure_cols(body, stub_label, leaf, top, font, s, pad)
    w <- allocate_widths(m, total, stub_floor)
    if (!is.null(w)) {
      return(list(size = s, widths = w, measure = m))
    }
  }
  NULL
}

#' How many body rows fit on one slide
#'
#' @param top,bottom slide margins in inches above / below the table
#' @param row_h,head_h,foot_h measured row heights in inches
rows_per_slide <- function(slide_h = 7.5, top = 1.4, bottom = 0.45,
                           head_h = 0.6, foot_h = 0.35, row_h = 0.3) {
  max(1L, floor((slide_h - top - bottom - head_h - foot_h) / row_h))
}

#' Break points that neither orphan nor widow a section
#'
#' Walks the rows cutting at `per`, then moves the cut back for two reasons:
#' a section heading never ends a slide with fewer than `keep` rows under it,
#' and a section never spills fewer than `keep` rows onto the next slide. Both
#' fixes are the same move, pushing the whole section over the break.
#'
#' @param starts logical, `TRUE` where a new section begins
break_rows <- function(n, per, starts = logical(n), keep = 2L) {
  cuts <- integer(0)
  i <- 1L
  while (i + per - 1L < n) {
    end <- i + per - 1L
    st <- which(starts[i:end]) + i - 1L          # section starts on this page
    if (length(st)) {
      last <- max(st)
      orphan <- end - last + 1L < keep           # heading alone at the foot
      widow <- {
        nxt <- which(starts & seq_len(n) > last)
        stop_at <- if (length(nxt)) min(nxt) - 1L else n
        stop_at - end < keep && stop_at > end    # a tail alone at the head
      }
      if ((orphan || widow) && last > i) end <- last - 1L
    }
    cuts <- c(cuts, end)
    i <- end + 1L
  }
  c(cuts, n)
}
