# Download file names.
#
# One rule for every block that downloads: the dataset the board read, when
# the data says, then the title the file prints, then the time.
#
#   AQ-001_Ozone_by_month_2026-10-02_1432.xlsx
#   Ozone_by_month_2026-10-02_1432.xlsx       (no dataset on the data)
#   chart_2026-10-02_1432.xlsx                (no title either)
#
# The dataset is a `blockr_dataset` attribute, set by whatever read the data
# (blockr.sandbox's study reader writes the study code) and carried down to the
# chart by blockr.dm's filter trail. Read here as a plain attribute, so this
# package needs neither.
#
# The time is always there, so two downloads of one block after a filter
# change do not collide into "(1)".

#' The download file name for one block and format
#'
#' @param dataset The dataset name, `dl_dataset()` of the block's data, or
#'   `NULL`.
#' @param title The resolved title, as printed on the file, or `NULL`.
#' @param fallback Word used when there is no title ("chart", "table").
#' @param ext File extension, without the dot.
#' @param time When the file was asked for.
#' @return A file name.
#' @noRd
dl_filename <- function(dataset, title, fallback, ext, time = Sys.time()) {
  parts <- c(
    dl_name_part(dataset),
    dl_name_part(title, max = 60L) %||% fallback,
    format(time, "%Y-%m-%d_%H%M")
  )
  paste0(paste(parts, collapse = "_"), ".", ext)
}

#' The `blockr_dataset` attribute, or `NULL`
#' @noRd
dl_dataset <- function(data) {
  ds <- attr(data, "blockr_dataset", exact = TRUE)
  if (is.character(ds) && length(ds) == 1L && !is.na(ds) && nzchar(ds)) ds
}

# One part of the name: tags out, ASCII only (a title can carry "±" or
# an em dash), then the words joined by `_`. A word is a run of letters,
# digits, `.` and `-` holding at least one letter or digit, so "AQ-001"
# stays whole and a lone dash between words goes. Case is kept. NULL when
# nothing is left.
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
