# The sections and the drill claims of the structured table (R/table-
# structured.R): the section headers and the row and header claims.

# A claim, `{column: [values]}`: the filter a click sends. Values are kept
# as arrays so a single one is not unboxed on the way to the browser.
table_structured_claim <- function(columns, values) {
  values <- lapply(values, as.character)
  if (anyDuplicated(columns)) {
    f <- factor(columns, levels = unique(columns))
    values <- lapply(split(values, f), function(v) unlist(v, use.names = FALSE))
  } else {
    names(values) <- columns
  }
  lapply(values, I)
}

# The section headers, as TableSection lists.
table_structured_sections <- function(data, section_cols, drill_on) {
  n <- nrow(data)
  k <- length(section_cols)
  if (k == 0L || n == 0L) {
    return(NULL)
  }
  path <- vapply(section_cols, function(sc) {
    v <- as.character(data[[sc]])
    v[is.na(data[[sc]])] <- "(missing)"
    v
  }, character(n))
  if (is.null(dim(path))) path <- matrix(path, nrow = n)
  from <- table_structured_diff_from(path)
  out <- list()
  for (i in which(!is.na(from))) {
    for (L in seq.int(from[[i]], k)) {
      out[[length(out) + 1L]] <- table_structured_section(
        data, section_cols, path, i, L, drill_on
      )
    }
  }
  if (length(out)) out
}

# The outermost section level that changed against the previous row, `NA`
# where none did.
table_structured_diff_from <- function(path) {
  n <- nrow(path)
  from <- rep(NA_integer_, n)
  for (L in rev(seq_len(ncol(path)))) {
    col <- path[, L]
    from[c(TRUE, col[-1L] != col[-n])] <- L
  }
  from
}

# One section: its level, text, the "Sex: " in front when the column carries a
# label other than its name, and the claim of a click on the value.
table_structured_section <- function(data, section_cols, path, i, L,
                                     drill_on) {
  sc <- section_cols[[L]]
  lbl <- attr(data[[sc]], "label", exact = TRUE)
  label <- if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl) &&
                 lbl != sc) {
    paste0(lbl, ": ")
  }
  keys <- if (drill_on && !identical(sc, ".variable_block")) {
    g <- table_structured_group_pairs(data, section_cols[seq_len(L)], i)
    if (length(g$columns)) table_structured_claim(g$columns, g$values)
  }
  out <- list(at = i - 1L, level = L, value = unname(path[i, L]), label = label,
              keys = keys)
  out[!vapply(out, is.null, logical(1L))]
}

# The `.group<k>_level` values of row `i` among `cols`, as parallel
# `columns` and `values`; a missing value claims nothing.
table_structured_group_pairs <- function(data, cols, i) {
  cols <- cols[grepl("^\\.group\\d+_level$", cols)]
  vals <- vapply(cols, function(sc) as.character(data[[sc]][i]), "")
  list(columns = cols[!is.na(vals)], values = unname(vals[!is.na(vals)]))
}

# What a click on each data row claims, a list with `NULL` for the rows that
# claim nothing.
table_structured_row_keys <- function(data, section_cols, drill_col = NULL) {
  n <- nrow(data)
  keys <- vector("list", n)
  has_leaf <- all(c(".variable", ".variable_level") %in% names(data))
  has_fil <- ".filter" %in% names(data)
  groups <- grep("^\\.group\\d+_level$", section_cols, value = TRUE)
  col_vals <- dd_nest_drill_values(data, drill_col)
  if (!has_leaf && !has_fil) {
    for (i in which(!is.na(col_vals))) {
      g <- table_structured_group_pairs(data, groups, i)
      keys[[i]] <- table_structured_claim(
        c(g$columns, drill_col), c(as.list(g$values), col_vals[[i]])
      )
    }
    return(keys)
  }
  chr <- function(col) {
    if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
  }
  v <- list(var = chr(".variable"), lvl = chr(".variable_level"),
            vlb = chr(".variable_label"), fil = chr(".filter"))
  for (i in seq_len(n)) {
    keys[i] <- list(table_structured_var_claim(data, groups, v, i,
                                                    has_leaf, has_fil))
  }
  keys
}

# The claim of row `i` of a Table 1, or `NULL`.
table_structured_var_claim <- function(data, groups, v, i, has_leaf,
                                            has_fil) {
  leaf <- has_leaf && !is.na(v$var[i]) && !is.na(v$lvl[i])
  fil <- has_fil && !is.na(v$fil[i]) && nzchar(v$fil[i])
  if (!leaf && !fil) {
    return(NULL)
  }
  g <- table_structured_group_pairs(data, groups, i)
  pairs <- list(
    if (!is.na(v$vlb[i])) list(".variable_label", v$vlb[i]),
    if (leaf) list(".variable", v$var[i]),
    if (leaf) list(".variable_level", v$lvl[i]),
    if (fil) list(".filter", v$fil[i])
  )
  pairs <- pairs[!vapply(pairs, is.null, logical(1L))]
  table_structured_claim(
    c(g$columns, vapply(pairs, `[[`, "", 1L)),
    c(as.list(g$values), lapply(pairs, `[[`, 2L))
  )
}
