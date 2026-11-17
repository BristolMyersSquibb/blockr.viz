# The section titles of a nested table's inner rows (`row_sections`).

#' Section titles of the inner rows (`row_sections`): `.title` on the first
#' inner row of each section under an outer row whose inner rows fall in more
#' than one section, `NA` elsewhere. An inner row's section is the first value
#' of the column among its records. Rows in their drawn order, so a section's
#' rows must sit together (the data's order puts them so).
#' @noRd
groups_row_titles <- function(rows, data, by, col) {
  if (is.null(col) || !col %in% names(data) || length(by) < 2L ||
        is.null(rows$.level)) {
    return(rows)
  }
  rows$.title <- NA_character_
  inner <- which(!rows$.is_parent %in% TRUE & !is.na(rows$.label))
  if (!length(inner)) return(rows)
  v <- as.character(data[[col]])
  key <- paste(data[[by[[1L]]]], data[[by[[2L]]]], sep = "\r")
  ok <- !is.na(v) & nzchar(v)
  sec <- v[ok][match(paste(rows$.parent[inner], rows$.label[inner],
                           sep = "\r"), key[ok])]
  par <- rows$.parent[inner]
  many <- tapply(sec, par, function(x) length(unique(x[!is.na(x)])) > 1L)
  n <- length(sec)
  first <- !is.na(sec) &
    c(TRUE, (sec[-1L] != sec[-n]) %in% TRUE | is.na(sec[-n]) |
            par[-1L] != par[-n])
  show <- first & many[par] %in% TRUE
  rows$.title[inner[show]] <- sec[show]
  rows
}

# The outer rows grouped by a column (`row_groups`): each group's rows
# together, the groups in the column's level order, a section heading at the
# first row of each. An outer row's group is the first value of the column
# among its records; rows without one come last, under no heading. The rows
# keep their order within a group. Returns the rows and the sections, or the
# rows alone (sections NULL) when there is nothing to group.
#' @noRd
groups_row_groups <- function(rows, data, by, col) {
  out <- list(rows = rows, sections = NULL)
  if (is.null(col) || !col %in% names(data) || !length(by) ||
        is.null(rows$.label) || !nrow(rows)) {
    return(out)
  }
  x <- data[[col]]
  v <- as.character(x)
  ok <- !is.na(v) & nzchar(v)
  if (!any(ok)) return(out)
  # A nested table without inner rows to open is flat: its rows are the
  # outer ones.
  nested <- any(rows$.is_parent %in% TRUE)
  par <- if (nested) rows$.is_parent %in% TRUE else rep(TRUE, nrow(rows))
  okey <- if (nested) {
    ifelse(par, as.character(rows$.parent_key %||% rows$.label),
           as.character(rows$.parent))
  } else {
    as.character(rows$.label)
  }
  # The rows' column: the outer one, else (a flat table) the one they name.
  gv <- NULL
  for (b in by) {
    key <- as.character(data[[b]])
    gv <- v[ok][match(okey, key[ok])]
    if (any(!is.na(gv))) break
  }
  blk <- cumsum(par)
  blk[blk == 0L] <- 1L
  # A block's group is its outer row's.
  head <- which(par)
  bg <- gv[head][blk]
  lv <- if (is.factor(x)) levels(x) else sort(unique(v[ok]))
  lv <- lv[lv %in% bg]
  o <- order(match(bg, lv), seq_along(bg), na.last = TRUE)
  rows <- rows[o, , drop = FALSE]
  bg <- bg[o]
  n <- length(bg)
  first <- !is.na(bg) & c(TRUE, bg[-1L] != bg[-n] | is.na(bg[-n]))
  lbl <- attr(x, "label", exact = TRUE)
  label <- if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl) &&
                 lbl != col) {
    paste0(lbl, ": ")
  }
  secs <- lapply(which(first), function(i) {
    s <- list(at = i - 1L, level = 1L, value = bg[[i]], label = label,
              open = TRUE)
    s[!vapply(s, is.null, logical(1L))]
  })
  rownames(rows) <- NULL
  list(rows = rows, sections = if (length(secs)) secs)
}
