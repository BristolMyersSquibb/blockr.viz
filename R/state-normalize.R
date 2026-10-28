# Heal corrupted block state written by the DAG clipboard.
#
# Older blockr.dag versions' copy_selection_to_clipboard() serialize live block
# state with jsonlite::toJSON(auto_unbox = TRUE) but WITHOUT null = "null", so
# every NULL state field is written as `{}` and comes back from the paste as an
# empty named list(). blockr.core hands that payload to the constructor
# verbatim, so the pasted block holds list() where it should hold NULL. A
# block pasted that way and then SAVED keeps its list() on disk, restores
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

chr_vec_state <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x)]
  if (!length(x)) NULL else x
}

# A boolean toggle slot: always a length-1 logical, never a string.
#
# The JS gear renders a two-option segmented control and transports its value
# as the strings "on" / "off". That is the CONTROL's
# wire format, not the block's API: table-block coerces it at the boundary
# (`as_toggle()`), so `sortable` et al. are plain logicals. Older chart
# boards carry the literal "off" for `identity_line`.
#
# So accept every shape the value can arrive in: a logical (the API), "on" /
# "off" (the gear + legacy state), "true" / "false" (a JSON round-trip), 1 / 0.
# Total and idempotent -- feeding it its own output changes nothing.
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
