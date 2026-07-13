# Heal corrupted block state written by the DAG clipboard.
#
# blockr.dag's copy_selection_to_clipboard() used to serialize live block state
# with jsonlite::toJSON(auto_unbox = TRUE) but WITHOUT null = "null", so every
# NULL state field was written as `{}` and came back from the paste as an empty
# named list(). blockr.core hands that payload to the constructor verbatim, so
# the pasted block holds list() where it should hold NULL. Fixed upstream in
# blockr.dag#144 -- but that only stops NEW corruption: a block whose state was
# poisoned by a pre-fix paste and then SAVED keeps its list() on disk, restores
# with it, and re-emits `{}` on the next copy (an empty list serializes to an
# empty object no matter what `null` is set to). The corruption propagates
# through save/restore, so the heal has to live in the constructor, which every
# entry path goes through -- paste, restore of an already-poisoned board, and
# AI-set config.
#
# Symptoms this prevents: "[object Object]" in a gear mapping row that should
# be hidden; `\`needed\` must be logical, numeric, or character, not a list`
# from c()-ing the roles together; `argument is of length zero` from nzchar()
# on a list(). All normalizers are idempotent -- NULL and correct values pass
# through untouched.

# Any slot: an empty list() (the corrupted NULL) becomes NULL, everything else
# passes through with its type intact. Use for non-character slots (numeric
# ranges, points, sizes) where coercing to character would be wrong.
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

# A multi-value slot (tooltip fields, waterfall totals, ...): NULL or a
# character vector. Empty stays NULL so "unset" and "set to nothing" agree.
chr_vec_state <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x)]
  if (!length(x)) NULL else x
}
