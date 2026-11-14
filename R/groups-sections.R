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
