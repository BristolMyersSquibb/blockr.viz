# A matrix's tile columns, written as JSON here rather than by Shiny.
#
# Shiny writes an output's value with jsonlite, which dispatches once per
# list and vector. A matrix of 242 tile columns is some 5,000 small objects
# (a column, its arrays, its claim, its mark, its text colours), and writing
# them took longer than building them. Here every string and number of all
# the tile columns goes through jsonlite in one call per kind
# (`collapse = FALSE` returns one token per element), and the brackets and
# keys around them are pasted. The text is what shiny_json() writes for the
# same column; a `json` string is spliced into the message as it is
# (`json_verbatim`).
#
# Only the shapes table_groups_col() gives a tile column are written here. A
# column with anything else stays a list and Shiny writes it.

# The TableModel with its tile columns as `json` strings, and the header
# rows and the row claims as one each. A model without tile columns comes
# back as it is.
table_tile_json <- function(model) {
  cols <- model$cols
  tile <- vapply(cols, function(cc) {
    is.list(cc) && is.list(cc$mark) && identical(cc$mark$kind, "tile")
  }, logical(1L))
  if (!any(tile)) {
    return(model)
  }
  js <- tile_json_cols(cols[tile])
  at <- which(tile)[!is.na(js)]
  model$cols[at] <- lapply(js[!is.na(js)], tile_json_verbatim)
  model$head <- lapply(model$head, tile_json_head)
  keys <- model$rows$keys
  if (is.list(keys) && length(keys)) {
    js <- tile_json_claims(keys)
    if (!anyNA(js)) {
      model$rows$keys <- tile_json_verbatim(
        paste0("[", paste(js, collapse = ","), "]")
      )
    }
  }
  model
}

tile_json_verbatim <- function(x) structure(x, class = "json")

# One header row: its cells are small objects of unboxed values, one per
# column. A cell with a claim (a facet spanner) stays a list.
tile_json_head <- function(row) {
  if (!is.list(row) || !is.null(oldClass(row)) || !is.null(names(row)) ||
        !length(row)) {
    return(row)
  }
  js <- tile_json_records(row)
  if (!anyNA(js)) {
    return(tile_json_verbatim(paste0("[", paste(js, collapse = ","), "]")))
  }
  row[!is.na(js)] <- lapply(js[!is.na(js)], tile_json_verbatim)
  row
}

# One token per element of an atomic vector, as shiny_json() writes it
# inside an array: a quoted and escaped string, a number, `true`, `null`.
tile_json_tokens <- function(x) {
  if (!length(x)) {
    return(character())
  }
  as.character(jsonlite::toJSON(
    x, collapse = FALSE, null = "null", na = "null", auto_unbox = TRUE,
    digits = getOption("shiny.json.digits", I(16)), use_signif = TRUE
  ))
}

# The JSON object of each column, NA for one that does not fit. Columns are
# written by groups that hold the same fields in the same order.
tile_json_cols <- function(cols) {
  sig <- vapply(cols, function(cc) paste(names(cc), collapse = "\r"),
                character(1L))
  out <- rep(NA_character_, length(cols))
  for (idx in split(seq_along(cols), sig)) {
    out[idx] <- tile_json_group(cols[idx])
  }
  out
}

tile_json_group <- function(cols) {
  nms <- names(cols[[1L]])
  unfit <- rep(NA_character_, length(cols))
  if (!length(nms) || anyDuplicated(nms)) {
    return(unfit)
  }
  parts <- vector("list", length(nms))
  bad <- logical(length(cols))
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    vals <- lapply(cols, `[[`, nm)
    js <- switch(
      nm,
      name = tile_json_scalars(vals, is.character),
      num = , zero = tile_json_scalars(vals, is.logical),
      disp = , bg = , fg = tile_json_arrays(vals, is.character),
      key = , at = tile_json_arrays(vals, is.numeric),
      keys = tile_json_claims(vals),
      mark = tile_json_marks(vals),
      ink = tile_json_records(vals),
      NULL
    )
    if (is.null(js)) {
      return(unfit)
    }
    # A field that does not fit (NA) leaves its column to Shiny.
    bad <- bad | is.na(js)
    parts[[i]] <- paste0("\"", nm, "\":", js)
  }
  out <- paste0("{", do.call(paste, c(parts, sep = ",")), "}")
  out[bad] <- NA_character_
  out
}

# A plain vector of the type `is` tests: no class, no names.
tile_json_plain <- function(x, is) {
  is(x) && is.null(attributes(x))
}

# One value per column, unboxed.
tile_json_scalars <- function(vals, is) {
  ok <- vapply(vals, function(x) {
    length(x) == 1L && tile_json_plain(x, is) && !is.na(x)
  }, logical(1L))
  out <- rep(NA_character_, length(vals))
  out[ok] <- tile_json_tokens(unlist(vals[ok], use.names = FALSE))
  out
}

# One array per column: a vector marked `I()`, of any length.
tile_json_arrays <- function(vals, is) {
  ok <- vapply(vals, function(x) {
    is(x) && identical(oldClass(x), "AsIs") && is.null(names(x)) &&
      is.null(dim(x))
  }, logical(1L))
  out <- rep(NA_character_, length(vals))
  out[ok] <- tile_json_collapse(
    tile_json_tokens(unlist(lapply(vals[ok], unclass), use.names = FALSE)),
    lengths(vals[ok]), "[", "]"
  )
  out
}

# Tokens joined per group of `n` consecutive ones, between `open` and
# `close`; an empty group is `open` and `close` alone.
tile_json_collapse <- function(tokens, n, open, close) {
  g <- factor(rep.int(seq_along(n), n), levels = seq_along(n))
  body <- vapply(split(tokens, g), paste, character(1L), collapse = ",",
                 USE.NAMES = FALSE)
  paste0(open, body, close)
}

# The names of a list as object keys, or NULL when jsonlite would change
# them (it numbers empty names and makes repeated ones unique).
tile_json_names <- function(x) {
  nm <- names(x)
  if (is.null(nm) || anyNA(nm) || !all(nzchar(nm)) || anyDuplicated(nm)) {
    return(NULL)
  }
  nm
}

# One object of unboxed values per column: `{name: value}`, the values plain
# strings, numbers or flags.
tile_json_records <- function(vals) {
  ok <- vapply(vals, function(x) {
    is.list(x) && is.null(oldClass(x)) && length(x) > 0L &&
      !is.null(tile_json_names(x)) &&
      all(vapply(x, function(e) {
        length(e) == 1L && is.null(attributes(e)) && !is.na(e) &&
          (is.character(e) || is.numeric(e) || is.logical(e))
      }, logical(1L)))
  }, logical(1L))
  out <- rep(NA_character_, length(vals))
  if (!any(ok)) {
    return(out)
  }
  flat <- unlist(vals[ok], recursive = FALSE, use.names = FALSE)
  keys <- tile_json_tokens(unlist(lapply(vals[ok], names),
                                  use.names = FALSE))
  value <- character(length(flat))
  for (test in list(is.character, is.numeric, is.logical)) {
    at <- vapply(flat, test, logical(1L))
    value[at] <- tile_json_tokens(unlist(flat[at], use.names = FALSE))
  }
  out[ok] <- tile_json_collapse(paste0(keys, ":", value), lengths(vals[ok]),
                                "{", "}")
  out
}

# One claim per column or row, `{column: [values]}` (table_structured_claim()):
# an object of string arrays. NULL, a row without a claim, is `null`.
tile_json_claims <- function(vals) {
  none <- vapply(vals, is.null, logical(1L))
  ok <- vapply(vals, function(x) {
    is.list(x) && is.null(oldClass(x)) && length(x) > 0L &&
      !is.null(tile_json_names(x))
  }, logical(1L))
  out <- rep(NA_character_, length(vals))
  out[none] <- "null"
  if (!any(ok)) {
    return(out)
  }
  flat <- unlist(vals[ok], recursive = FALSE, use.names = FALSE)
  arrays <- tile_json_arrays(flat, is.character)
  keys <- tile_json_tokens(unlist(lapply(vals[ok], names),
                                  use.names = FALSE))
  n <- lengths(vals[ok])
  body <- tile_json_collapse(paste0(keys, ":", arrays), n, "{", "}")
  # A claim with an array that does not fit does not fit.
  g <- rep.int(seq_along(n), n)
  body[unique(g[is.na(arrays)])] <- NA_character_
  out[ok] <- body
  out
}

# The mark of a tile column: its kind, and the hover card's description when
# it has one.
tile_json_marks <- function(vals) {
  out <- rep(NA_character_, length(vals))
  nms <- lapply(vals, names)
  bare <- vapply(nms, identical, logical(1L), "kind")
  full <- vapply(nms, identical, logical(1L), c("kind", "hover"))
  kind <- vapply(vals, function(x) identical(x$kind, "tile"), logical(1L))
  out[bare & kind] <- "{\"kind\":\"tile\"}"
  at <- which(full & kind)
  if (length(at)) {
    hover <- tile_json_records(lapply(vals[at], `[[`, "hover"))
    out[at] <- ifelse(is.na(hover), NA_character_,
                      paste0("{\"kind\":\"tile\",\"hover\":", hover, "}"))
  }
  out
}
