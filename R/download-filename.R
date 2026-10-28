# Download file names: the dataset (when the data carries one), the title the
# file prints, then the time.
#
#   AQ-001_Ozone_by_month_2026-10-02_1432.xlsx
#   Ozone_by_month_2026-10-02_1432.xlsx       (no dataset on the data)
#   chart_2026-10-02_1432.xlsx                (no title either)
#
# `dataset` is `dl_dataset()` of the block's data, or NULL; `fallback` is the
# word used when there is no title; `ext` has no dot.
dl_filename <- function(dataset, title, fallback, ext, time = Sys.time()) {
  parts <- c(
    dl_name_part(dataset),
    dl_name_part(title, max = 60L) %||% fallback,
    format(time, "%Y-%m-%d_%H%M")
  )
  paste0(paste(parts, collapse = "_"), ".", ext)
}

# The `dataset` field of the `blockr_provenance` attribute, or NULL.
dl_dataset <- function(data) {
  ds <- attr(data, "blockr_provenance", exact = TRUE)$dataset
  if (is.character(ds) && length(ds) == 1L && !is.na(ds) && nzchar(ds)) ds
}

# One part of the name: tags out, ASCII only, then the words joined by `_`.
# A word is a run of letters, digits, `.` and `-` holding at least one letter
# or digit, so "AQ-001" stays whole and a lone dash goes. Case is kept. NULL
# when nothing is left.
dl_name_part <- function(x, max = 40L) {
  if (!is.character(x) || !length(x) || is.na(x[[1L]])) {
    return(NULL)
  }
  x <- gsub("<[^>]+>", " ", x[[1L]])
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  words <- strsplit(gsub("[^A-Za-z0-9.-]+", " ", x), " ", fixed = TRUE)[[1L]]
  words <- gsub("^[.-]+|[.-]+$", "", words[grepl("[A-Za-z0-9]", words)])
  x <- paste(words, collapse = "_")
  x <- sub("_+$", "", substr(x, 1L, max))
  if (nzchar(x)) x
}
