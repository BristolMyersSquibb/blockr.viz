# Heal corrupted block state written by the DAG clipboard.
#
# Older blockr.dag versions serialize live block state without
# `null = "null"`, so every NULL state field is written as `{}` and comes back
# from a paste as an empty named list(). blockr.core hands that to the
# constructor verbatim, and a saved board keeps the list() and re-emits `{}` on
# the next copy. The heal therefore lives in the constructor, which paste,
# restore and AI-set config all go through.
#
# Symptoms: "[object Object]" in a gear mapping row that should be hidden;
# "`needed` must be logical, numeric, or character, not a list"; "argument is
# of length zero" from nzchar() on a list(). All normalizers are idempotent.

# Any slot: an empty list() (the corrupted NULL) becomes NULL, everything else
# passes through with its type intact. For non-character slots (numeric
# ranges, points, sizes).
null_state <- function(x) {
  if (is.list(x) && !length(x)) NULL else x
}

# A scalar column-role slot: NULL or a length-1 character vector.
chr_state <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NULL else x[[1L]]
}

# A multi-column role slot: NULL or a character vector.
chr_vec_state <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x)]
  if (!length(x)) NULL else x
}

# A boolean toggle slot: always a length-1 logical. The JS gear transports a
# segmented control as "on" / "off" and older chart boards carry "off" for
# `identity_line`, so accept a logical, "on" / "off", "true" / "false" (a JSON
# round-trip) and 1 / 0.
bool_state <- function(x, default = FALSE) {
  if (is.null(x)) return(default)

  x <- unlist(x, use.names = FALSE)
  x <- x[!is.na(x)]
  if (!length(x)) return(default)

  x <- x[[1L]]
  if (is.logical(x)) return(isTRUE(x))

  if (is.character(x)) {
    v <- tolower(trimws(x))
    if (identical(v, "on")) return(TRUE)
    if (identical(v, "off")) return(FALSE)
    return(isTRUE(as.logical(v)))
  }

  isTRUE(as.logical(x))
}
