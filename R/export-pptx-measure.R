# PowerPoint and Word tables: measuring text and fitting column widths
# for static_table() (R/export-pptx-table.R).

# Left plus right cell padding as a width in inches. `NULL` means the
# flextable default, which is what static_table() leaves in place.
ft_pad_width <- function(cell_padding) {
  if (is.null(cell_padding)) ft_side_padding() / 72 else 2 * cell_padding / 72
}

# Cell padding, in points, for a table that cannot fit any other way. Two
# points instead of the flextable default of five: on a 36-column table the
# default spends 5in of a 12.5in slide on padding alone, which is the
# difference between a legible table and stacked characters.
TIGHT_PAD <- 2

# ---- measured column widths ------------------------------------------------
# Give every column the width its own text needs, in this priority: a data
# cell never wraps, a header never breaks inside a word, the headers unwrap
# toward their full text, and only then does the stub grow past its share.
# The stub is the only column whose content is prose, so it is the one that
# survives wrapping; a count like "143 (41.2%)" broken over two lines costs a
# readable row and doubles its height.
#
# The stub does not hold its whole label on one line while the data columns
# are pinched: `stub_share` caps what it claims until everything else is
# served, and it takes the leftovers afterwards. A long system organ class
# reading over two lines costs a row; the same label spread over five inches
# is what pushes a table onto a second set of slides.
#
# Everything is measured through systemfonts at the table's own font and size,
# which is the same engine flextable itself measures with. When the deck's
# typeface is not installed on the machine doing the export (a house template
# naming Trebuchet MS on a Linux server, say) systemfonts substitutes and the
# numbers are approximate, which is what `blockr.viz.ft_width_slack` covers.
#
# Returns widths in inches summing to `total`, or NULL when the table cannot
# be measured -- the caller then falls back to the positional split, so a
# missing measurement never fails an export.
#
# Three attributes come back with them, all saying how close the slide came to
# being too narrow and what would help:
#   `header_squeeze` -- the headers will break inside a word. Nothing here can
#     fix that; fewer columns or a smaller font can.
#   `cell_squeeze`   -- the DATA cells no longer fit. Below that the columns
#     are narrower than a character and PowerPoint stacks them one per line,
#     which is the 36-column grade table. Only less padding or a smaller font
#     helps, because a digit and its padding is all that is left.
#   `word_fit`       -- how much of the longest header word the narrowest
#     column can hold, as a fraction. 1 is a header that never breaks, 0.5 a
#     word taking two lines, 0.1 the stacked-letters case. It is the difference
#     between a header squeeze worth living with and one that is not, and the
#     pptx writer reads it to decide whether to deal the columns over slides.
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
      # The stub asks for its longest label at its own indent depth. Section
      # header rows are merged across the table, so they are not in `stub`
      # and never widen the column.
      stub_cells <- ft_text_widths(stub, font, font_size) + stub_indent / 72
      # ... but never so narrow that a single word has to break, and never
      # hairline thin because the table happens to have no rows yet.
      stub_floor <- max(
        min(stub_min, total / 3),
        ft_word_width(c(stub, stub_label), font, font_size) + pad
      )
      stub_nat <- max(c(stub_cells, 0)) + pad
      stub_want <- max(stub_nat, stub_floor)
      # What the stub keeps when the table has to be squeezed: its floor, or
      # its own width when that is smaller. A stub of short codes must not
      # hold half an inch back from columns that are being cut off.
      stub_keep <- min(stub_nat, stub_floor)

      # A data column asks for two widths. `want` is the one it must have:
      # its widest cell, and its header's longest word, so the numbers never
      # wrap and the header never breaks mid-word. `lux` is the one it would
      # like: the whole header on as few lines as it was written with. A
      # header may wrap between words when the slide is tight; it should not
      # have to when the slide has room to spare.
      cell_w <- vapply(
        seq_len(n_data),
        function(j) max(c(ft_text_widths(cells[, j], font, font_size), 0)),
        numeric(1L)
      ) + pad
      # And the width below which it cannot go: its longest word. A count is
      # very nearly one word, so a column of counts asks for the whole cell
      # either way; a cell of several short words can give most of its width
      # back and read as two lines. That is what lets a table whose cells are
      # more text than number share the shortfall, instead of spending the
      # slide on cells that would wrap perfectly well.
      cell_word <- vapply(
        seq_len(n_data),
        function(j) ft_word_width(cells[, j], font, font_size),
        numeric(1L)
      ) + pad
      head_word <- vapply(leaf, ft_word_width, numeric(1L),
                          font = font, size = font_size) + pad
      head_full <- ft_text_widths(leaf, font, font_size) + pad

      # A spanner sits over its whole group, so its demand is shared out: it
      # only widens columns when the group is narrower than the spanner.
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
      # Nothing may break inside a word, in a cell or in a header. Everything
      # above that is negotiable.
      bare <- pmax(cell_word, head_word, span_word)

      # Columns that carry the same statistic get the same width, whatever
      # their own arm happens to need: "n (%)" under Placebo and under a
      # 200-subject arm are read across, and a table whose arms are visibly
      # different widths reads as a mistake. Leaf labels name the statistic;
      # when they are all distinct they ARE the groups (one column per arm),
      # so the whole data side is one class.
      role <- if (anyDuplicated(leaf)) leaf else rep("", n_data)
      need <- ft_group_max(need, role)
      want <- ft_group_max(want, role)
      lux <- ft_group_max(lux, role)
      bare <- ft_group_max(bare, role)

      if (sum(want) + stub_floor > total) {
        # Nothing fits at its full width, so everything gives a little rather
        # than one column being crushed. The stub takes part in the fill
        # instead of being handed the remainder: it used to be pinned at its
        # floor while columns of medium-long text kept every character on one
        # line, which is the wide-spread table -- three lines of stub beside
        # data columns with air in them.
        #
        # Each column is filled from what it cannot go below (its longest
        # word) toward what it would like (its whole text), all to a common
        # level. A column of counts reaches its full width almost at once
        # because a count is one word; a column of prose keeps taking room as
        # long as there is any, and wraps when there is not. Which is the
        # right answer either way round: with a system organ class in the stub
        # and counts beside it, the counts are served and the stub wraps.
        squeeze[["header"]] <- TRUE
        w <- ft_water_fill(c(stub_want, want), total, c(stub_floor, bare))
        stub_w <- w[[1L]]
        data_w <- w[-1L]
        # A cell narrower than its own longest word is the defect worth
        # reporting: below that a number is broken mid-token and, further
        # down, PowerPoint stacks the characters one per line. A cell that
        # merely wraps between words is not, and neither is a HEADER whose
        # longest word breaks -- that is what `word_fit` is for, and calling
        # it a cell squeeze is what sent seven-column tables sideways.
        squeeze[["cell"]] <- any(data_w < cell_word - 1e-9)
      } else {
        # Whether this table is close enough to the slide to fill it, decided
        # on the widths it needs rather than the ones it ends up with.
        fills <- stub_want + sum(want) >= 0.7 * total

        # What the stub claims before the data columns have had their turn.
        # Past this it is spending slide on a label that wraps perfectly well,
        # and the columns that pay for it are the ones a reader compares.
        stub_cap <- max(stub_floor, stub_share * total)
        stub_w <- min(stub_want, stub_cap, total - sum(want))
        data_w <- want
        room <- total - stub_w - sum(data_w)

        # First call on the room: unwrap the headers, in proportion to how
        # far each is from its own text, so no column takes the whole surplus.
        short <- pmax(lux - data_w, 0)
        if (room > 0 && sum(short) > 0) {
          data_w <- data_w + short * min(1, room / sum(short))
          room <- total - stub_w - sum(data_w)
        }

        # Second call: the stub back up to its own text, now that nothing else
        # is waiting for the room. A four-column table on a wide slide still
        # ends with its label on one line; a twelve-column one does not.
        if (room > 0 && stub_w < stub_want) {
          stub_w <- stub_w + min(room, stub_want - stub_w)
          room <- total - stub_w - sum(data_w)
        }

        if (fills) {
          # What is left over is spread evenly. The stub is already at its
          # natural width and more of it would only reopen the gap this
          # replaces.
          if (room > 0) data_w <- data_w + room / n_data
        } else {
          # A small table on a wide slide. Stretching four columns across
          # thirteen inches to honour `fit_width` gives cavernous cells with
          # a number lost in the middle of each; the table keeps its natural
          # size and the pptx writer centres it instead. It does grow far
          # enough to hold its own title line, which is merged across the
          # width and would otherwise wrap over a narrow table.
          banner_w <- max(c(
            ft_text_widths(banner, font, font_size + 2) + pad, 0
          ))
          grow <- min(banner_w, total) - (stub_w + sum(data_w))
          if (is.finite(grow) && grow > 0) {
            data_w <- data_w + grow / n_data
          }
        }
      }

      # How much of its longest word the tightest header keeps. A word that
      # loses a syllable to the next line is a blemish; one holding two
      # characters of six is the stacked-letters failure, and only the reader
      # of this number can tell them apart.
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
# How tall each row of a part will come out, in inches, once PowerPoint has
# wrapped the text into the column widths the table carries. flextable states
# a row height, but PowerPoint treats it as a minimum and grows the row to fit
# its content, so the stated height is a floor and not an answer.
#
# Needed to cut a long table into slides: the page break has to be decided
# before the slide exists, and a wrapped stub label costs a line that a row
# count knows nothing about.
#
# Measured, not exact. The wrap is greedy on whitespace (which is what a
# renderer does) but the font may not be the one PowerPoint will use, and
# borders add a fraction of a point per row.
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
    # A merged cell is measured across the columns it covers, and the cells
    # it swallowed are not measured at all.
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

  # The stated row height is a floor, not a cap.
  stated <- p$rowheights
  if (is.numeric(stated) && length(stated) == n_row) {
    out <- pmax(out, stated)
  }

  out
}

# The text one column of a flextable part actually PRINTS, row by row.
#
# Not `part$dataset`, which for a header still holds the column keys the table
# was built from ("Placebo (N=143)||n (%)"), never the labels
# set_header_labels() put there. Measuring those would size the header band
# against strings no reader ever sees. The rendered runs live in the chunk
# structure; the dataset is the fallback for a flextable that has none.
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
# breaking hard at "\n". A word wider than the column still gets its own line
# rather than being counted twice.
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
    # Measured rather than assumed: a space is not the same width in every
    # face, and at eight columns the error adds up.
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
# rescale to the budget. When even the minimums do not fit -- forty columns on
# a widescreen slide -- the result is proportional to what each column needed,
# which is the best a fixed width can do.
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

# Width in inches of each string's longest line. A cell's own "\n" is a hard
# break (the Big-N convention), so a two-line header asks only for its wider
# line. Bold throughout: the emphasis rows are the widest ones, and a table
# sized for regular weight wraps as soon as a row is bolded.
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

# Width of the widest single word: the point below which a cell stops wrapping
# and starts breaking words.
ft_word_width <- function(x, font, size) {

  words <- unlist(strsplit(as.character(x), "[[:space:]]+"))
  words <- words[!is.na(words) & nzchar(words)]

  if (!length(words)) {
    return(0)
  }

  max(systemfonts::string_width(words, family = font, size = size,
                                weight = "bold", res = 72)) / 72
}

# Left plus right cell padding in points. static_table() sets the vertical
# padding and leaves these at the flextable defaults, so this is what a cell
# spends before its first character.
ft_side_padding <- function() {
  d <- flextable::get_flextable_defaults()
  sum(vapply(c("padding.left", "padding.right"),
             function(k) if (is.numeric(d[[k]])) d[[k]] else 5,
             numeric(1L)))
}

# Readable text color for a fill, by luminance (Rec. 601). Any hex or R
# color name; NA in -> NA out (no fill, no text override). blockr.viz carries
# no house palette, so this is the only color knowledge here.
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

# The emphasis ramp for column .strong / .emph: normal / emph / strong ->
# light gray / dark gray / accent. Generic UI values (not a house palette),
# overridable via option -- the same guarded-fallback shape the chart uses
# for its palette (blockr.theme has no R-side token API, so these are local
# defaults; the accent doubles as the chart's first series color feel).
FT_EMPHASIS_DEFAULT <- c(normal = "#EEEEEE", emph = "#9AA3B0",
                         strong = "#2563EB")

# A theme's `bands` role, read as the emphasis triple. Themes state bands the
# way header fills want them (a `.stub` light grey then a cycle pool), which is
# the same vocabulary: the stub is the `normal` band and the first pool entry
# is the accent. Returns NULL unless the theme supplies both, so a partial
# answer falls through to FT_EMPHASIS_DEFAULT rather than half-applying.
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

# Per-column bg / text for emphasis mode. strong -> accent, emph -> dark gray,
# normal -> light gray; the stub takes the normal (light) band so the header
# reads as one strip. Contrast text by luminance.
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

# Resolve `header_bg` to per-data-column fills, keyed on the column-group
# structure the annotated df carries. `header_bg` is a color vector:
#   * NAMED entries PIN a group: the name is a by-level value (the spanner
#     top for nested `by`, else the data-column name); ".stub" pins the row-
#     stub header. So `c(Placebo = "grey", "Drug" = "#33D6F1")` colours those
#     arms explicitly (placebo grey, treatment blue) whatever their position.
#   * UNNAMED entries are the cycle pool for the remaining, unpinned groups
#     (in order of first appearance) -- `c("#A59F9F", "#33D6F1")` bands them.
#   * The two mix: `c(.stub = "#EEE", Placebo = "grey", "#33D6F1", "#FDA97C")`.
# Colors are hex or R color names -- NO palette-name lookup, so no house
# style lives in blockr.viz; the app supplies the actual values (typically via
# getOption("blockr.viz.ft_header_bg")). `NULL` / `FALSE` / `"none"` -> no
# fill. Returns per-column bg/text (length = n data cols; NA = unfilled) plus
# stub_bg/stub_text, or NULL when there is nothing to fill.
resolve_header_bands <- function(header_bg, top, data_cols) {
  n <- length(data_cols)
  if (n == 0L) return(NULL)
  if (is.null(header_bg) || isFALSE(header_bg) ||
        identical(header_bg, "none")) {
    return(NULL)
  }

  nm <- names(header_bg)
  header_bg <- as.character(header_bg)   # (drops names -- captured above)
  if (is.null(nm)) nm <- rep("", length(header_bg))
  pins <- header_bg[nzchar(nm)]
  names(pins) <- nm[nzchar(nm)]
  pool <- header_bg[!nzchar(nm)]

  # Group key per data column: the spanner top when nested, else the column
  # name itself. Unpinned groups draw from the cycle pool in appearance order.
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
