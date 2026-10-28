#' Mark what a column is FOR
#'
#' Attaches a kind to columns so a block's mapping controls offer the few
#' columns that make sense for a role instead of every column in the frame.
#'
#' A kind is a fact about the column, not about a chart. `TRT` is marked
#' `"group"` once and is then offered for colour, facet, chart group and a
#' table's column split.
#'
#' The four kinds:
#'
#' * `group` -- low-cardinality stratifiers (TRT, SEX, RACE, AETOXGR). Colour,
#'   facet and chart group all take them.
#' * `time` -- ordered axis columns (AVISIT, ADY, ASTDY).
#' * `value` -- numeric measures (AVAL, CHG, PCHG, AGE).
#' * `id` -- identifiers and item dimensions (USUBJID, AEDECOD, CMDECOD).
#'   Groupable, countable and drillable, never offered as a colour.
#'
#' **No mark, no restriction.** A frame where nothing is marked offers every
#' column to every role. As soon as ANY column carries a kind, roles that
#' declare which kinds they accept offer only matching columns (plus whatever
#' they are currently set to).
#'
#' @section Where to mark:
#' Once per data chain, at its head (the flatten or adapter block the view
#' hangs off), not per exhibit.
#'
#' @section What survives:
#' The marks ride on the frame, in a `blockr_kinds` attribute. A per-column
#' attribute is dropped by `left_join()`, `bind_rows()` and base `[` row
#' subsetting; the frame attribute survives those and the dplyr verbs. It is
#' lost by base `[` column subsetting (use `dplyr::select()`) and by anything
#' that rebuilds the frame, such as a pivot.
#'
#' Marks naming a column that is no longer there are ignored. A chain that
#' loses the attribute falls back to offering every column.
#'
#' @param data A data frame.
#' @param ... Named character vectors, one per kind: `group =`, `time =`,
#'   `value =`, `id =`. Columns not named keep whatever they carry. A column
#'   named under two kinds takes the last one, with a warning.
#' @param .clear Logical. Drop every existing mark first (default `FALSE`,
#'   so repeated calls accumulate).
#'
#' @examples
#' d <- mark_column_kinds(
#'   datasets::iris,
#'   group = "Species",
#'   value = c("Sepal.Length", "Sepal.Width")
#' )
#' column_kinds(d)
#'
#' @export
mark_column_kinds <- function(data, ..., .clear = FALSE) {
  stopifnot(is.data.frame(data))
  marks <- list(...)
  if (length(marks) && !all(nzchar(rlang::names2(marks)))) {
    stop("Every argument to `mark_column_kinds()` must be named for its kind ",
         "(one of ", paste(COLUMN_KINDS, collapse = ", "), ").", call. = FALSE)
  }
  unknown <- setdiff(names(marks), COLUMN_KINDS)
  if (length(unknown)) {
    stop("Unknown column kind(s): ", paste(unknown, collapse = ", "),
         ". Known kinds are ", paste(COLUMN_KINDS, collapse = ", "), ".",
         call. = FALSE)
  }

  cur <- if (isTRUE(.clear)) character() else column_kinds(data)
  seen <- character()
  for (kind in names(marks)) {
    cols <- as.character(marks[[kind]])
    missing <- setdiff(cols, names(data))

    if (length(missing)) {
      warning("mark_column_kinds(): no such column(s): ",
              paste(missing, collapse = ", "), call. = FALSE)
      cols <- intersect(cols, names(data))
    }
    dup <- intersect(cols, seen)
    if (length(dup)) {
      warning("mark_column_kinds(): column(s) marked twice, last wins: ",
              paste(dup, collapse = ", "), call. = FALSE)
    }
    cur[cols] <- kind
    seen <- c(seen, cols)
  }

  attr(data, "blockr_kinds") <- cur[order(names(cur))]

  data
}

#' @param x A data frame.
#' @return `column_kinds()` returns a named character vector, one entry per
#'   marked column that is still in the frame. Empty when nothing is marked.
#' @rdname mark_column_kinds
#' @export
column_kinds <- function(x) {
  stopifnot(is.data.frame(x))
  kinds <- attr(x, "blockr_kinds")
  if (is.null(kinds) || !length(kinds)) {
    return(stats::setNames(character(), character()))
  }
  kinds <- stats::setNames(as.character(kinds), rlang::names2(kinds))
  kinds <- kinds[nzchar(rlang::names2(kinds)) & kinds %in% COLUMN_KINDS]
  # Marks for columns a later block dropped are ignored: the head of a chain
  # cannot know what the middle of it selects away.
  kinds[intersect(names(kinds), names(x))]
}

# The vocabulary; see mark_column_kinds() for what each kind covers.
COLUMN_KINDS <- c("group", "time", "value", "id")
