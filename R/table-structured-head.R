# The header rows of the structured table (R/table-structured.R): the
# spanners as TableHeadCell lists.

# Header rows, top to bottom.
table_structured_head <- function(data, data_cols, stub_col, col_keys = NULL) {
  parts <- strsplit(data_cols, "||", fixed = TRUE)
  depths <- lengths(parts)
  max_depth <- max(depths)
  ctx <- list(
    data = data, data_cols = data_cols, parts = parts, depths = depths,
    max_depth = max_depth, col_keys = col_keys,
    spanners = annotated_spanner_labels(data),
    offset = if (is.null(stub_col)) 0L else 1L
  )
  stub <- table_structured_stub_head(data, stub_col, max_depth)
  lapply(seq_len(max_depth), function(L) {
    c(stub[[L]], table_structured_head_row(L, ctx))
  })
}

# The stub's header cells, one list per header row. One title spans every
# row; a title per row (a vector label) takes one cell each.
table_structured_stub_head <- function(data, stub_col, max_depth) {
  out <- rep(list(list()), max_depth)
  if (is.null(stub_col)) {
    return(out)
  }
  lbl <- attr(data[[stub_col]], "label", exact = TRUE)
  cell <- function(text) list(text = text, stub = TRUE)
  if (is.character(lbl) && length(lbl) > 1L) {
    for (L in seq_len(max_depth)) {
      out[[L]] <- list(cell(table_structured_stub_text(
        if (L <= length(lbl)) lbl[[L]] else ""
      )))
    }
    return(out)
  }
  head <- cell(table_structured_stub_text(lbl))
  if (max_depth > 1L) head$rows <- max_depth
  out[[1L]] <- list(head)
  out
}

# A stub title: its non-empty lines, "" without any.
table_structured_stub_text <- function(label) {
  if (!is.character(label) || length(label) != 1L || is.na(label)) {
    return("")
  }
  lines <- strsplit(label, "\n", fixed = TRUE)[[1L]]
  paste(lines[nzchar(lines)], collapse = "\n")
}

# The cells of header row `L` (not the stub's).
table_structured_head_row <- function(L, ctx) {
  cells <- list()
  n <- length(ctx$data_cols)
  i <- 1L
  while (i <= n) {
    if (ctx$depths[i] < L) {
      i <- i + 1L
      next
    }
    prefix <- ctx$parts[[i]][seq_len(L)]
    j <- i
    while (j < n && ctx$depths[j + 1L] >= L &&
             identical(ctx$parts[[j + 1L]][seq_len(L)], prefix)) {
      j <- j + 1L
    }
    cells[[length(cells) + 1L]] <- table_structured_head_cell(
      L, i, j - i + 1L, prefix, ctx
    )
    i <- j + 1L
  }
  cells
}

# One cell: a leaf when every column it covers ends at this row, a spanner
# otherwise.
table_structured_head_cell <- function(L, i, span, prefix, ctx) {
  leaf <- all(ctx$depths[i:(i + span - 1L)] == L)
  name <- ctx$data_cols[i]
  if (leaf) {
    lbl <- if (span == 1L) attr(ctx$data[[name]], "label", exact = TRUE)
    key <- name
  } else {
    key <- paste(prefix, collapse = "||")
    lbl <- if (key %in% names(ctx$spanners)) ctx$spanners[[key]]
  }
  cell <- table_structured_label(lbl, prefix[[L]])
  rows <- ctx$max_depth - L + 1L
  cell$span <- if (span > 1L) span
  cell$rows <- if (leaf && rows > 1L) rows
  cell$col <- if (leaf && span == 1L) (i - 1L) + ctx$offset
  entry <- ctx$col_keys[[key]]
  cell$keys <- if (length(entry)) {
    table_structured_claim(
      vapply(entry, function(p) as.character(p[["column"]]), ""),
      lapply(entry, function(p) as.character(p[["values"]]))
    )
  }
  cell[!vapply(cell, is.null, logical(1L))]
}

# A header label as `text` and `sub`: the name lines, and the Big N ("N = 86")
# that every producer puts last.
table_structured_label <- function(label, fallback) {
  if (!is.character(label) || length(label) != 1L || is.na(label) ||
        !nzchar(label)) {
    return(list(text = fallback))
  }
  split <- header_label_split(table_structured_plain(label))
  if (is.null(split)) {
    return(list(text = fallback))
  }
  out <- list(text = paste(split$name, collapse = "\n"))
  if (!is.null(split$n)) out$sub <- split$n
  out
}

# Text that carries markup, as the text it shows: `<br>` breaks the line, a
# tag goes, an entity is its character. A `<65` is no markup and stays.
table_structured_plain <- function(x) {
  if (!looks_like_markup(x)) {
    return(x)
  }
  x <- gsub("<br */?>", "\n", x, ignore.case = TRUE)
  x <- gsub("</?[[:alpha:]][^>]*>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}
