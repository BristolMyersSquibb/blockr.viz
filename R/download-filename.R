#' Download filename helpers
#'
#' @noRd
viz_download_filename <- function(kind = "table", ext = "xlsx", title = NULL,
                                  data = NULL, id = NULL, pattern = NULL,
                                  study = NULL, time = Sys.time()) {
  kind <- download_filename_text(kind, "table")
  ext <- download_filename_slug(ext, "xlsx")
  pattern <- download_filename_text(pattern, "")
  if (!nzchar(pattern)) {
    pattern <- paste("{study}", "{kind}", "{title}", "{datetime}", sep = "_")
  }

  values <- list(
    study = download_filename_study(data = data, id = id, study = study),
    kind = kind,
    title = download_filename_text(title, ""),
    datetime = format(as.POSIXct(time), "%Y%m%dT%H%M%S")
  )

  out <- pattern
  for (nm in names(values)) {
    out <- gsub(paste0("{", nm, "}"), download_filename_slug(values[[nm]]),
                out, fixed = TRUE)
  }
  if (is.data.frame(data)) {
    out <- resolve_title_template(out, data)
  }
  out <- download_filename_slug(out, "download")
  paste0(out, ".", ext)
}

#' @noRd
download_filename_state <- function(x) {
  x <- download_filename_text(x, "")
  if (!nzchar(x)) NULL else x
}

#' @noRd
download_filename_text <- function(x, fallback = "") {
  x <- as.character(unlist(x, use.names = FALSE))
  x <- trimws(x[!is.na(x) & nzchar(trimws(x))])
  if (length(x)) x[[1L]] else fallback
}

#' @noRd
download_filename_slug <- function(x, fallback = "") {
  x <- download_filename_text(x, fallback)
  if (!nzchar(x)) {
    return(fallback)
  }
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("[`*~#]+", "", x)
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) fallback else substr(x, 1L, 120L)
}

#' @noRd
download_filename_study <- function(data = NULL, id = NULL, study = NULL) {
  study <- download_filename_text(study, "")
  if (!nzchar(study)) {
    study <- download_filename_text(getOption("blockr.viz.study_id"), "")
  }
  if (!nzchar(study)) {
    study <- download_filename_text(Sys.getenv("BLOCKR_STUDY_ID"), "")
  }
  if (!nzchar(study)) {
    study <- download_filename_text(Sys.getenv("CDEX_STUDY_ID"), "")
  }
  if (!nzchar(study) && !is.null(data)) {
    study <- download_filename_text(c(
      attr(data, "study_id", exact = TRUE),
      attr(data, "study", exact = TRUE)
    ), "")
  }
  if (!nzchar(study)) {
    id <- download_filename_text(id, "")
    match <- regexpr("[A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+", id)
    if (match > 0L) {
      study <- regmatches(id, match)
    }
  }
  download_filename_text(study, "study")
}
