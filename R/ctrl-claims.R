# What a drill sends: filter conditions in the shape a value filter takes,
# `list(name=, table=, mode=, values=)`. Two builders:
#
# - drill_claim_columns() reads a subset that is already drilled. A dimension
#   claims only when the subset holds one value of it. It is exported for
#   hand-rolled senders in function blocks.
# - ctrl_claims() takes a block's own click (column -> values), subsets the
#   input on it and claims the set of values the clicked rows hold, group
#   definitions and the board's subject column included. The blocks' drill
#   senders use it; it falls back to drill_claim_columns() on a structured
#   table's identity columns.

#' The board's subject column
#'
#' The column a drill claims when the board says what a subject is: the
#' `subject` field of the "study_roles" board option, or `USUBJID` when the
#' option exists and the field is empty. Read by option id, so this package
#' does not depend on blockr.pharma. A board without the option has no
#' subjects and its senders claim columns.
#'
#' @param session Shiny session (defaults to the current reactive domain).
#' @return The column name, or `NULL` when the board declares no subjects.
#' @keywords internal
#' @export
ctrl_subject <- function(session = shiny::getDefaultReactiveDomain()) {

  if (is.null(session)) {
    return(NULL)
  }

  val <- tryCatch(
    blockr.core::get_board_option_or_null("study_roles", session),
    error = function(e) NULL
  )

  if (is.null(val)) {
    return(NULL)
  }

  col <- trimws(as.character(unlist(val$subject, use.names = FALSE)))
  col <- col[!is.na(col) & nzchar(col)]

  if (length(col) == 1L) col else "USUBJID"
}

#' Read filter conditions off a drilled annotated data frame
#'
#' Turns the drilled subset of an annotated (ARD-shaped) data frame into the
#' `columns` payload a value filter takes, ready for [ctrl_send()]. This is the
#' claim logic behind the drill senders.
#'
#' @section Two claim modes:
#' **Annotated (`columns = NULL`).** The data frame is read on its ARD identity
#' columns: `.variable` is the source column name, `.variable_level` the source
#' value on that row, and each `.group<k>` / `.group<k>_level` pair is an
#' enclosing grouping dimension. Dimensions come back outermost first, with the
#' `.variable` leaf last. A `.filter` column (a row defined by a predicate, see
#' [drill_source()]) counts as a leaf for the purpose of reading its enclosing
#' groups.
#'
#' **Source columns (`columns` given).** Chart drills carry no ARD
#' identity, so the caller names the candidate columns, e.g. `columns = "SEX"`,
#' and each is claimed from the subset.
#'
#' @section What counts as a claim:
#' A dimension becomes a filter condition only when the subset resolves it to
#' exactly one value. Clicking one bar leaves `SEX` single-valued, which is a
#' claim. An undrilled chart passes all its rows through, `SEX` holds both `F`
#' and `M`, and nothing is claimed: the caller's cue to [ctrl_clear()]. A brush
#' across several bars is not a claim either.
#'
#' @param data A data frame, typically the drilled output of a table or chart
#'   block. Anything that resolves no dimension yields an empty list.
#' @param table Character(1). Name of the table in the target's `dm` that the
#'   conditions apply to. `""` for a value filter fed a plain data frame.
#' @param mode Character(1). Filter mode for each condition, e.g. `"multi"`.
#' @param columns Character vector of source column names to claim, for
#'   drilled output that carries no ARD identity. `NULL` reads the ARD identity
#'   columns instead.
#'
#' @return A list of `list(name=, table=, mode=, values=)` entries, one per
#'   claimed dimension. Empty when nothing is claimed.
#'
#' @examples
#' # Annotated mode: a drilled summary table.
#' drilled <- data.frame(
#'   .variable = "SEX", .variable_level = "F", n = 143
#' )
#' drill_claim_columns(drilled, table = "adsl")
#'
#' # Source-column mode: a chart drilled to one bar.
#' drill_claim_columns(
#'   data.frame(USUBJID = c("01-001", "01-002"), SEX = c("F", "F")),
#'   table = "adsl",
#'   columns = "SEX"
#' )
#'
#' @export
drill_claim_columns <- function(data, table, mode = "multi", columns = NULL) {

  stopifnot(
    is.character(table), length(table) == 1L,
    is.character(mode), length(mode) == 1L,
    is.null(columns) || is.character(columns)
  )

  if (!is.data.frame(data)) {
    return(list())
  }

  nms <- names(data)

  if (length(columns)) {
    claims <- list()

    for (col in columns[columns %in% nms]) {
      value <- single_value(as.character(data[[col]]))
      if (!is.null(value)) {
        claims[[length(claims) + 1L]] <- claim_cond(col, value, table, mode)
      }
    }

    return(claims)
  }

  has_leaf <- all(c(".variable", ".variable_level") %in% nms)
  has_fil <- ".filter" %in% nms
  if (!has_leaf && !has_fil) {
    return(list())
  }

  var <- if (has_leaf) as.character(data$.variable) else character(nrow(data))
  lvl <- if (has_leaf) as.character(data$.variable_level) else
    character(nrow(data))
  fil <- if (has_fil) as.character(data$.filter) else character(nrow(data))

  # A mixed table fills the leaf columns on some rows and `.filter` on others,
  # so a row carries a claim if either is usable.
  leaf_ok <- has_leaf & !is.na(var) & !is.na(lvl) & nzchar(lvl)
  fil_ok <- has_fil & !is.na(fil) & nzchar(fil)
  keep <- leaf_ok | fil_ok

  if (!any(keep)) {
    return(list())
  }

  cols <- list()

  for (g in group_level_cols(nms)) {
    gname <- substr(g, 1L, nchar(g) - 6L)
    if (!gname %in% nms) {
      next
    }
    name <- single_value(as.character(data[[gname]])[keep])
    value <- single_value(as.character(data[[g]])[keep])
    if (!is.null(name) && !is.null(value)) {
      cols[[length(cols) + 1L]] <- claim_cond(name, value, table, mode)
    }
  }

  if (any(leaf_ok)) {
    name <- single_value(var[leaf_ok])
    value <- single_value(lvl[leaf_ok])

    if (!is.null(name) && !is.null(value)) {
      cols[[length(cols) + 1L]] <- claim_cond(name, value, table, mode)
    }
  }

  cols
}

# One filter condition. An empty `table` (a value filter on a plain data
# frame) leaves the `table` field out.
claim_cond <- function(name, values, table = "", mode = "multi") {
  if (nzchar(table %||% "")) {
    list(name = name, table = table, mode = mode, values = values)
  } else {
    list(name = name, mode = mode, values = values)
  }
}

# `.group<k>_level` columns, ordered by k.
group_level_cols <- function(nms) {
  gl <- nms[startsWith(nms, ".group") & endsWith(nms, "_level")]
  gl[order(as.integer(substr(gl, 7L, nchar(gl) - 6L)))]
}

# The value filter's tokens for missing and empty values (blockr.dm
# VALUE_FILTER_NA / VALUE_FILTER_EMPTY). A claim names a missing value with
# these, never with NA or the table's "(Missing)" level.
CTRL_NA <- "<NA>"
CTRL_EMPTY <- "<empty>"

# The elements of `x` selected by a claim's values, tokens included.
ctrl_claim_hit <- function(x, values) {
  values <- as.character(unlist(values))
  x <- as.character(x)
  hit <- x %in% setdiff(values, c(CTRL_NA, CTRL_EMPTY))
  if (CTRL_NA %in% values) hit <- hit | is.na(x)
  if (CTRL_EMPTY %in% values) hit <- hit | (!is.na(x) & x == "")
  hit
}

# One "multi" claim per named column: the values the drilled rows hold,
# missing and empty ones as tokens.
drilled_value_claims <- function(rows, table, columns) {
  claims <- list()
  for (col in intersect(columns, names(rows))) {
    x <- as.character(rows[[col]])
    v <- unique(x[!is.na(x) & nzchar(x)])
    v <- c(v, if (anyNA(x)) CTRL_NA, if (any(!is.na(x) & x == "")) CTRL_EMPTY)
    if (length(v)) {
      claims[[length(claims) + 1L]] <- claim_cond(col, v, table)
    }
  }
  claims
}

# The single distinct value of `x`, or NULL if it does not resolve to one.
single_value <- function(x) {
  u <- unique(x[!is.na(x) & nzchar(x)])
  if (length(u) == 1L) u else NULL
}

#' Claims from a block's own drill-filter state.
#'
#' `filters` is a named list, column -> drilled value(s), from the block's
#' filter reactives. The input is subset on those pairs and the claim is read
#' off the subset, so the one-value-per-dimension rule of
#' [drill_claim_columns()] and its un-drill propagation apply.
#'
#' All-dot filter columns (a structured table's `.variable`, `.group<k>`, ...)
#' read the ARD identity off the subset. Any other column is a real source
#' column and is the claim column.
#'
#' @section Hold versus clear:
#' The sender reads `list()` as "clear the target" and `NULL` as "keep what the
#' target has". `list()` is returned only when the user has no drill. `NULL` is
#' returned when the block cannot resolve its selection right now: there is no
#' data frame (under lazy evaluation the input is gated to `NULL` while the
#' block is off screen), the frame lacks a filtered column (it is being rebuilt
#' during a board update), or nothing in the subset resolves to a claim.
#' Clearing in those cases would widen the target to everything and re-narrow
#' it a moment later.
#'
#' @param data The block's (annotated) input, as a data frame.
#' @param table Name of the table the claim applies to, `""` when the target
#'   filters a plain data frame (or when the target resolves the table itself).
#' @param filters Named list, column -> drilled value(s); the columns AND
#'   together. An `NA` value (or a table's "(Missing)" level) is a missing
#'   one: it takes the rows where the column is missing or empty, and the
#'   claim names them with the value filter's tokens `"<NA>"` / `"<empty>"`.
#'   A value on a column carrying a group definition (see [expand_groups()])
#'   is a group name. It is matched against the group's members, and the claim
#'   is the set of source values the group holds (`mode = "multi"`).
#' @param subject The board's subject column ([ctrl_subject()]), or `NULL`.
#'   When given, the claim becomes the subjects behind the click, with the
#'   column clause kept as its `label`. A target re-applies a column claim to
#'   its own data, which may differ from the data this block counted (a flag
#'   filter or a worst-grade reduction in between), whereas subject ids
#'   resolved here are exact. A structured table resolves them through its
#'   stamped `source_data` ([drill_source()]). A frame that carries the
#'   subject column neither way keeps the column claim.
#' @return A list of filter conditions, `list()` when the user has no drill,
#'   `NULL` when the claim cannot be resolved right now (hold).
#' @keywords internal
#' @export
ctrl_claims <- function(data, table, filters, subject = NULL) {

  if (!is.data.frame(data)) {
    return(NULL)
  }

  if (!length(filters)) {
    return(list())
  }

  if (!all(names(filters) %in% names(data))) {
    return(NULL)
  }

  table <- table %||% ""

  # A click on a group of an overlap definition names the group, while an
  # unexpanded group column holds raw levels, so match the group's members.
  # An NA filter value takes the rows where the column is missing or empty.
  keep <- rep(TRUE, nrow(data))
  for (col in names(filters)) {
    x <- as.character(data[[col]])
    vals <- as.character(unlist(filters[[col]]))
    vals[vals %in% MISSING_LEVEL] <- NA_character_
    hit <- x %in% group_column_values(data[[col]], vals[!is.na(vals)])
    if (anyNA(vals)) {
      hit <- hit | is.na(x) | x == ""
    }
    keep <- keep & hit
  }

  cols <- names(filters)[!startsWith(names(filters), ".")]

  # A picker-made copy (`Color` <- `RACE`) carries its origin in
  # `blockr_source`; the claim names the original column, which the target
  # filter sees. A copy whose source is gone claims under its own name.
  claim_col <- vapply(cols, function(col) {
    src <- attr(data[[col]], "blockr_source", exact = TRUE)
    if (is.character(src) && length(src) == 1L && nzchar(src) &&
          src %in% names(data)) src else col
  }, character(1L), USE.NAMES = FALSE)

  grouped <- vapply(cols, function(col) has_group_def(data[[col]]), logical(1L))
  group_claims <- group_def_claims(data, keep, filters, cols[grouped],
                                  claim_col[grouped])

  # Plain columns claim the set of values the clicked rows hold: a click can
  # cover several (the patients under a bar segment). With no named column the
  # ARD identity columns speak. With only group columns, only they claim.
  plain_cols <- setdiff(unique(claim_col[!grouped]), names(group_claims))
  rows <- data[keep, , drop = FALSE]
  out <- if (length(plain_cols)) {
    drilled_value_claims(rows, table, plain_cols)
  } else if (!length(cols)) {
    drill_claim_columns(rows, table = table)
  } else {
    list()
  }

  if (length(group_claims)) {
    out <- c(out, unname(Map(function(name, values) {
      claim_cond(name, values, table)
    }, names(group_claims), unname(group_claims))))
    ord <- match(vapply(out, function(e) as.character(e$name), character(1L)),
                 unique(claim_col))
    out <- out[order(ord)]
  }

  # A structured table's "(Missing)" level names the missing value.
  out <- lapply(out, function(cl) {
    v <- as.character(unlist(cl$values))
    if (MISSING_LEVEL %in% v) {
      cl$values <- unique(replace(v, v == MISSING_LEVEL, CTRL_NA))
    }
    cl
  })

  # Subjects resolve even where no column claim does.
  if (length(subject)) {
    ids <- claim_subjects(data, keep, out, subject)
    if (length(ids)) {
      return(list(list(name = subject, mode = "multi", values = ids,
                       label = claim_label(out))))
    }
  }

  # The user has a selection, so an empty read means this block failed to name
  # it, which is a hold.
  if (!length(out)) {
    return(NULL)
  }

  out
}

# Claims for the drilled columns that carry a group definition, as a list
# named by claim column. A group can pool several raw levels, so its claim is
# a set: the source values of the clicked rows, or the group's members when
# no source column exists to claim.
group_def_claims <- function(data, keep, filters, cols, claim_col) {
  out <- list()
  for (i in seq_along(cols)) {
    col <- cols[[i]]
    to <- claim_col[[i]]
    vals <- if (identical(to, col)) {
      v <- as.character(unlist(filters[[col]]))
      group_column_values(data[[col]], v[!is.na(v)], origin = TRUE)
    } else {
      v <- unique(as.character(data[[to]][keep]))
      v <- v[!is.na(v) & nzchar(v)]
      m <- unlist(group_def_origin(data[[col]])$groups, use.names = FALSE)
      v[order(match(v, m), v)]
    }
    if (length(vals)) {
      out[[to]] <- vals
    }
  }
  out
}

# The subjects behind a click: off the clicked rows when the frame carries the
# subject column, else off the stamped `source_data` narrowed by the column
# claims (a structured table's rows are display rows). NULL when neither
# frame knows the subject.
#' @noRd
claim_subjects <- function(data, keep, claims, subject) {

  ids <- if (subject %in% names(data)) {
    data[[subject]][keep]
  } else {
    src <- attr(data, "source_data", exact = TRUE)
    names_ok <- vapply(claims, function(cl) cl$name %in% names(src),
                       logical(1L))
    if (!is.data.frame(src) || !subject %in% names(src) ||
          !length(claims) || !all(names_ok)) {
      return(NULL)
    }
    hit <- rep(TRUE, nrow(src))
    for (cl in claims) {
      hit <- hit & ctrl_claim_hit(src[[cl$name]], cl$values)
    }
    src[[subject]][hit]
  }

  ids <- unique(as.character(ids))
  ids[!is.na(ids) & nzchar(ids)]
}

# What the user clicked, in the words a filter trail uses:
# "AEBODSYS = Nausea; TRT = Placebo".
#' @noRd
claim_label <- function(claims) {
  parts <- vapply(claims, function(cl) {
    vals <- as.character(unlist(cl$values))
    vals[vals == CTRL_NA] <- "(missing)"
    vals[vals == CTRL_EMPTY] <- "(empty)"
    if (length(vals) > 6L) {
      vals <- c(vals[1:6], "…")
    }
    paste0(cl$name, " = ", paste(vals, collapse = ", "))
  }, character(1L))
  paste(parts, collapse = "; ")
}
