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
# A chart's `format`: "number", "compact", "percent", or a whole number of
# decimals (0 to 6, as an integer). NULL for anything else.
chart_format_state <- function(x) {
  x <- unlist(x, use.names = FALSE)
  if (!length(x) || is.na(x[[1L]])) return(NULL)
  x <- x[[1L]]
  if (is.character(x) && x %in% c("number", "compact", "percent")) return(x)
  d <- suppressWarnings(as.numeric(x))
  if (length(d) == 1L && !is.na(d) && d == round(d) && d >= 0 && d <= 6) {
    return(as.integer(d))
  }
  NULL
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
# as the strings "on" / "off" (chart/roles.js ROLES, table.js SORTABLE_OPT).
# That is the CONTROL's wire format, not the block's API: table-block already
# coerces it at the boundary (`as_toggle()`), so `sortable` et al. are plain
# logicals. `identity_line` did not, and let the transport string leak into
# block state -- every chart board saved before that fix carries the literal
# "off".
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

# A numeric multi-value slot (`y_lines`): NULL or a numeric vector.
#
# Values reach the ctor as a bare numeric (code), a list of numbers (a JSON
# round-trip through saved state), or text (the gear, the sentence, the
# assistant): "2, 5" or "Target: 4; 6" (see line_text_split()). Non-numeric
# junk drops rather than poisoning the slot with NA -- a guide line at NA is not
# a line, and the renderer would silently skip it anyway.
num_vec_state <- function(x) {

  x <- line_values(x)
  if (is.null(x)) return(NULL)

  nms <- names(x)
  x <- suppressWarnings(as.numeric(x))
  names(x) <- nms
  x <- x[!is.na(x)]
  if (!is.null(names(x)) && !any(nzchar(names(x)))) names(x) <- NULL

  if (!length(x)) NULL else x
}

# Positions of `x_lines`: NULL, a numeric vector, or a character vector of
# dates and category values. A date is kept as ISO text, as `x_ranges` keeps
# it; the browser places it on the axis. Numbers stay numbers, so boards saved
# with numeric lines read back unchanged. A value that matches nothing on the
# axis draws nothing.
x_vec_state <- function(x) {

  x <- line_values(x)
  if (is.null(x)) return(NULL)

  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NULL)

  num <- suppressWarnings(as.numeric(x))
  if (!anyNA(num)) {
    names(num) <- names(x)
    x <- num
  }
  if (!is.null(names(x)) && !any(nzchar(names(x)))) names(x) <- NULL
  x
}

# Line positions as one named character vector, from whatever the caller
# wrote: numbers, dates, a list of either (saved state), or text.
line_values <- function(x) {

  if (is.null(x) || !length(x)) return(NULL)

  # Dates format before unlist(), which would drop their class.
  dated <- function(v) {
    if (inherits(v, c("Date", "POSIXt"))) {
      stats::setNames(format(v), names(v))
    } else {
      v
    }
  }
  x <- if (is.list(x)) lapply(x, dated) else dated(x)
  x <- unlist(x)
  if (!length(x)) return(NULL)

  if (is.character(x)) {
    x <- line_text_split(x)
  } else {
    x <- stats::setNames(as.character(x), names(x))
  }
  if (!length(x)) NULL else x
}

# Text the gear, the sentence or the assistant writes: lines separated by ";"
# or ",", each optionally named as "Name: value", as the gear writes shaded
# ranges. "Listed: 2026-10-21; 3" -> c(Listed = "2026-10-21", "3"). The colon
# needs a space after it, so a time of day ("10:30") is no name. Numbers
# separated by spaces ("2 5") are two lines, as the field always took them.
line_text_split <- function(x) {

  nms <- names(x) %||% rep("", length(x))
  vals <- character()
  out_nms <- character()

  for (i in seq_along(x)) {
    if (is.na(x[[i]])) next
    parts <- trimws(strsplit(x[[i]], "[;,]")[[1L]])
    parts <- parts[nzchar(parts)]
    for (p in parts) {
      m <- regmatches(p, regexec("^([^:]+):[[:space:]]+(.*)$", p))[[1L]]
      nm <- if (length(m)) trimws(m[[2L]]) else ""
      v <- if (length(m)) trimws(m[[3L]]) else p
      if (!nzchar(nm) && length(parts) == 1L) nm <- nms[[i]]
      tok <- strsplit(v, "[[:space:]]+")[[1L]]
      if (length(tok) > 1L && !anyNA(suppressWarnings(as.numeric(tok)))) {
        v <- tok
        nm <- c(nm, rep("", length(tok) - 1L))
      }
      vals <- c(vals, v)
      out_nms <- c(out_nms, nm)
    }
  }

  names(vals) <- out_nms
  vals
}

# Panels per row in a facet grid: NULL (auto) or a length-1 character holding
# a positive whole number.
#
# A string, because that is what every surface it travels through speaks: the
# gear's select, the config channel, and `{@facet_cols}` in a title template.
# NULL is auto, and it is the one value `arg_token_value()` reads as "nothing
# to print", so a clause like `[, in {@facet_cols} columns]` drops out of the
# sentence while the setting is on auto and comes back with it.
#
# Anything that is not a whole number >= 1 heals to auto rather than erroring:
# the value arrives from a select, from MCP and from saved state, and a chart
# that refuses to draw because a layout hint was junk is the wrong trade. The
# cap is generous and only there to stop a typed 300 from rendering 300 empty
# tracks.
FACET_COLS_MAX <- 12L

facet_cols_state <- function(x) {

  x <- chr_state(x)

  if (is.null(x)) return(NULL)

  n <- suppressWarnings(as.numeric(x))

  if (is.na(n) || n < 1) return(NULL)

  as.character(min(as.integer(n), FACET_COLS_MAX))
}

# The same slot as an integer, for the renderers that do geometry with it.
facet_cols_n <- function(x) {
  x <- facet_cols_state(x)
  if (is.null(x)) NULL else as.integer(x)
}
