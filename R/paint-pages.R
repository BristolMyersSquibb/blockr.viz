# Paging for the groups-table picture of R/paint-table.R. A picture cannot
# reflow, so a table too tall for its slide is cut on the cell model before
# painting; a slice of rows is a valid table.

# Slice a column's per-row vectors and leave its per-column ones alone. A
# vector is per-row when its length is n. `seg`, `segv`, `ldisp` and `slab`
# hold one full-length vector per series, and `lv` one nested cell per colour
# level.
paint_slice_col <- function(c, idx, n) {
  nm <- names(c)
  out <- c
  for (k in seq_along(c)) {
    v <- c[[k]]
    if (nm[[k]] %in% c("seg", "segv", "ldisp", "slab")) {
      out[[k]] <- lapply(v, function(s) if (length(s) == n) s[idx] else s)
    } else if (identical(nm[[k]], "lv")) {
      out[[k]] <- lapply(v, paint_slice_col, idx = idx, n = n)
    } else if (length(v) == n && (is.atomic(v) || is.list(v))) {
      out[[k]] <- v[idx]
    }
  }
  out
}

paint_slice <- function(m, idx) {
  n <- m$n
  m$cols <- lapply(m$cols, paint_slice_col, idx = idx, n = n)
  for (k in c("label", "parent", "level", "parent_row", "on")) {
    if (length(m[[k]]) == n) m[[k]] <- m[[k]][idx]
  }
  m$n <- length(idx)
  m
}

# Where to cut. With uniform rows the count follows from `per_page`; with
# `heights` a page takes rows while they fit `budget`. A page never ends on a
# parent row, and a page that opens mid-group repeats the parent row, marked
# as carried over (`attr(, "carried")`).
paint_page_rows <- function(m, per_page, heights = NULL, budget = NULL) {
  n <- m$n
  if (per_page >= n) return(list(seq_len(n)))
  fit <- function(from, room) {
    if (is.null(heights)) return(room)
    used <- cumsum(heights[seq.int(from, n)])
    max(1L, sum(used <= room + 1e-9))
  }
  lvl <- as.integer(m$level %||% rep(0L, n))
  par <- as.logical(m$parent_row %||% rep(FALSE, n))
  pages <- list()
  at <- 1L
  while (at <= n) {
    # A carried-over parent costs a slot, so it is decided before the cut:
    # dropping a row afterwards can move the cursor backwards.
    lead <- integer()
    take <- fit(at, if (is.null(heights)) per_page else budget)
    if (at > 1L && lvl[[at]] > 0L && take >= 2L) {
      p <- at - 1L
      while (p >= 1L && lvl[[p]] > 0L) p <- p - 1L
      if (p >= 1L) {
        lead <- p
        take <- if (is.null(heights)) {
          per_page - 1L
        } else {
          fit(at, budget - heights[[p]])
        }
      }
    }
    end <- min(at + take - 1L, n)
    # Never below `at`, so the cursor always advances.
    while (end > at && isTRUE(par[[end]])) end <- end - 1L
    idx <- c(lead, seq.int(at, end))
    attr(idx, "carried") <- length(lead) > 0L
    pages[[length(pages) + 1L]] <- idx
    at <- end + 1L
  }
  pages
}

# How many rows fit one page at this type size, without painting anything.
summarize_paint_per_page <- function(m, prep, width_in = 12.5, max_height = 5.4,
                                     fs = 9, family = "sans", row_h = NULL,
                                     title = NULL, subtitle = NULL,
                                     caption = NULL) {

  lay <- paint_layout(m, prep, width_in, fs = fs, family = family)
  hh <- paint_heights(m, prep, lay, fs, row_h, title, subtitle, caption)

  # A page marker can push a one-line title to two, so budget for the chrome
  # of a continuation page, which is never less than page 1's.
  budget <- max_height - hh$chrome - hh$fold_h

  if (all(hh$rows == hh$row_h)) return(max(1L, floor(budget / hh$row_h)))
  max(1L, sum(cumsum(hh$rows) <= budget + 1e-9))
}

# One table as a list of pictures. `max_height` is the slide's body box. The
# title grows a "(2 of 7)" marker and the fold row rides on the last page.
summarize_paint_pages <- function(m, prep, width_in = 12.5, max_height = 5.4,
                                  fs = 9, family = "sans", row_h = NULL,
                                  title = NULL, subtitle = NULL,
                                  caption = NULL) {

  paint <- function(mk, ttl, sub, cap) {
    summarize_paint_grob(mk, prep, width_in, fs, family, row_h, ttl, sub, cap)
  }
  per_page <- summarize_paint_per_page(m, prep, width_in, max_height, fs,
                                       family, row_h, title, subtitle, caption)
  if (per_page >= m$n) return(list(paint(m, title, subtitle, caption)))

  lay <- paint_layout(m, prep, width_in, fs = fs, family = family)
  hh <- paint_heights(m, prep, lay, fs, row_h, title, subtitle, caption)
  pages <- paint_page_rows(
    m, per_page,
    heights = if (all(hh$rows == hh$row_h)) NULL else hh$rows,
    budget = max_height - hh$chrome - hh$fold_h
  )
  n_pg <- length(pages)
  lapply(seq_along(pages), function(k) {
    idx <- pages[[k]]
    mk <- paint_slice(m, as.integer(idx))
    if (k < n_pg) mk$fold <- NULL
    ttl <- if (paint_has(title)) paste0(title, "  (", k, " of ", n_pg, ")") else title
    sub <- if (isTRUE(attr(idx, "carried")) && !is.null(subtitle)) {
      paste0(subtitle, " \u00b7 continued")
    } else {
      subtitle
    }
    paint(mk, ttl, sub, if (k == n_pg) caption)
  })
}
