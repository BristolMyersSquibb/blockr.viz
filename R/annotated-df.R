#' Coerce a table object into an annotated data frame
#'
#' An annotated data frame is a plain data frame whose reserved `.`-columns
#' describe a display table's structure and identity: `.label` (row stub), the
#' ARD-named identity pairs `.group<k>` / `.group<k>_level` (row-grouping
#' variable name / value) and `.variable` / `.variable_level` (leaf source
#' variable / value; an `NA` level marks a stat row), their optional `_label`
#' display companions, `.indent` (extra display depth) and `.strong` / `.emph`
#' (emphasis). Two-level column spanners are encoded in the column name as
#' `Top||Leaf`. The table-level attributes `label` (the title), `subtitle` and
#' `caption` carry display text; the table and chart blocks use them as the
#' auto tier of their `title` / `subtitle` / `caption` state and the Excel
#' export carries them. Section headers are synthesized by the renderer from
#' grouping-value runs and are never rows of the frame.
#'
#' It is the format [new_table_block()] consumes and [summary_table()]
#' produces. The full contract lives in the blockr.design spec
#' (`open/annotated-data-frame/format.md`).
#'
#' `as_annotated_df()` coerces a table-producing object (composer, gtsummary,
#' gt, rtables, ...) into that data frame. Producer packages register one S3
#' method each, e.g. blockr.sandbox provides the composer methods. A data frame
#' passes through untouched, so coercion is idempotent.
#'
#' @param x A table-producing object that has an `as_annotated_df` method.
#' @param ... Passed on to methods.
#'
#' @return An annotated data frame.
#' @seealso [new_table_block()], [summary_table()]
#' @export
as_annotated_df <- function(x, ...) {
  UseMethod("as_annotated_df")
}

#' @rdname as_annotated_df
#' @export
as_annotated_df.data.frame <- function(x, ...) {
  x
}

#' @rdname as_annotated_df
#' @export
as_annotated_df.default <- function(x, ...) {
  stop(
    "No as_annotated_df() method for <",
    paste(class(x), collapse = "/"),
    ">. Supply a data frame, or an object whose package registers an ",
    "as_annotated_df() method (e.g. a composer table via blockr.sandbox).",
    call. = FALSE
  )
}

# Whether `x` dispatches to a non-default as_annotated_df() method. Method
# lookup only, the coercion never runs.
has_annotated_df_method <- function(x) {
  found <- function(cls) {
    !is.null(utils::getS3method("as_annotated_df", cls, optional = TRUE))
  }
  any(vapply(class(x), found, logical(1L)))
}

# The input contract of the renderer blocks: a data frame or an object that
# coerces through as_annotated_df(). See dev/table-and-chart-architecture.md,
# "Shape contract".
can_coerce_annotated_df <- function(x) {
  is.data.frame(x) || has_annotated_df_method(x)
}

# The `dat_valid` body of the renderer blocks. A method that refuses a
# particular value errors at eval time instead.
validate_annotated_df_input <- function(data) {
  if (!can_coerce_annotated_df(data)) {
    stop(
      "Input must be a data frame (e.g. a summary_table() annotated frame) ",
      "or an object with an as_annotated_df() method (e.g. a composer ",
      "table); got <", paste(class(data), collapse = "/"), ">"
    )
  }
}

# The reserved annotation columns (see as_annotated_df()): row stub, hierarchy,
# identity, emphasis, the `.fmt` template with its `.digits` precision, and
# `.filter`, the predicate identity of a row that is not a level of anything
# (see drill_source()). Row-grouping axes are the pairs `.group<k>` /
# `.group<k>_level` (+ optional `_label`), matched by ANNOTATION_GROUP_RE.
# Other dot-prefixed names pass through.
ANNOTATION_COLS <- c(".label", ".indent", ".strong", ".emph", ".fmt",
                     ".digits",
                     ".variable", ".variable_level", ".variable_label",
                     ".filter")
ANNOTATION_GROUP_RE <- "^\\.group\\d+(_level|_label)?$"
# Per-cell paint: `.bg:<col>` / `.fg:<col>` hold one hex colour per row for
# data column `<col>`, NA where the cell is unpainted. They are columns so
# they survive the row and column slicing of the pptx pager.
ANNOTATION_PAINT_RE <- "^\\.(bg|fg):"

# The reserved annotation columns present in `data`. Renderers exclude them
# from the data cells and as_plain_df() strips them.
annotation_cols_in <- function(data) {
  names(data)[names(data) %in% ANNOTATION_COLS |
                grepl(ANNOTATION_GROUP_RE, names(data)) |
                grepl(ANNOTATION_PAINT_RE, names(data))]
}

#' The paint of one column of an annotated frame
#'
#' For exporters in other blockr packages. Returns `list(bg =, fg =)`, one
#' entry per row with `NA` where unpainted, or `NULL` when the column carries
#' no paint.
#' @keywords internal
#' @export
annotation_cell_paint <- function(data, col) {
  bg <- data[[paste0(".bg:", col)]]
  if (is.null(bg) || all(is.na(bg))) return(NULL)
  fg <- data[[paste0(".fg:", col)]] %||% rep(NA_character_, length(bg))
  list(bg = as.character(bg), fg = as.character(fg))
}

# The `.group<k>_level` columns in `data`, outermost first.
annotation_group_level_cols <- function(data) {
  cols <- grep("^\\.group(\\d+)_level$", names(data), value = TRUE)
  cols[order(as.integer(sub("^\\.group(\\d+)_level$", "\\1", cols)))]
}

#' Coerce a table object into a plain data frame
#'
#' [as_annotated_df()] followed by dropping the reserved `.`-annotation
#' columns (`.label`, `.indent`, `.strong`, `.emph`, `.fmt`, `.digits`,
#' `.variable*`, `.group<k>*`). The chart and tile blocks use it, so a chart of
#' a structured display table charts its data columns. Other dot-prefixed
#' columns pass through.
#'
#' @param x A data frame or an object with an [as_annotated_df()] method.
#' @param ... Passed on to [as_annotated_df()].
#'
#' @return A data frame without the reserved annotation columns.
#' @seealso [as_annotated_df()], [new_chart_block()], [new_table_block()]
#' @export
as_plain_df <- function(x, ...) {
  x <- as_annotated_df(x, ...)
  drop <- names(x) %in% annotation_cols_in(x)
  x[, !drop, drop = FALSE]
}

#' Resolve a drilled summary row back to the source rows behind it
#'
#' A drill on a structured (annotated) table yields a subset of the display
#' frame: the clicked row, not the records it summarises. The row carries the
#' machine claim (`.variable` / `.variable_level`, plus any enclosing
#' `.group<k>` pairs), so the records are recoverable from the frame the table
#' was computed from. `drill_source()` returns the source rows that match a
#' drilled annotated frame.
#'
#' The frame comes from `attr(x, "source_data")`, which a producer stamps when
#' it summarises (composer tables via the blockr.sandbox adapter). Pass
#' `source` to resolve against another frame.
#'
#' The claim rule is the one of the control channel ([ctrl_send()]): a
#' dimension becomes a condition only when the drilled subset resolves it to
#' exactly one value. An undrilled table claims nothing and `unclaimed` decides
#' what comes back.
#'
#' Two claim modes, as in [ctrl_send()]. A structured table carries ARD
#' identity columns and `columns` stays `NULL`. An aggregated table (one
#' display row standing for many records) has none: name its group columns in
#' `columns`. A frame with neither errors instead of reporting zero rows.
#'
#' # Rows defined by a predicate
#'
#' A display row such as "Any Serious AEs" is not a level of anything. A
#' producer writes the predicate it counted with on a **`.filter`** column and
#' this function applies it to the source. It composes with the pairs: a
#' predicate row inside a treatment column resolves to the rows matching the
#' predicate and in that arm. A frame carrying `.filter` needs no `.variable` /
#' `.variable_level`.
#'
#' The expression is the producer's own string, evaluated against `source`,
#' the frame it was computed on. It must yield a logical vector.
#'
#' @param x A drilled annotated data frame, or an object coercible via
#'   [as_annotated_df()].
#' @param source Data frame to resolve against. Default `NULL` = the input's
#'   `source_data` attribute.
#' @param columns Real source columns the claim is read from, for an
#'   aggregated table. Default `NULL` = read the ARD identity columns
#'   (`.variable` / `.variable_level`, `.group<k>` pairs) instead.
#' @param unclaimed What a frame that claims nothing resolves to: `"none"`
#'   (default), a zero-row frame, or `"all"`, the whole source. The table
#'   block passes `"all"` before any click, so a listing below a Table 1
#'   shows every row until one is picked.
#'
#' @return A subset of `source`: the rows the claim selects. When nothing is
#'   claimed, a zero-row frame or the whole source (`unclaimed`).
#' @seealso [as_annotated_df()], [new_table_block()] (`drill = "source"`)
#' @export
drill_source <- function(x, source = NULL, columns = NULL,
                         unclaimed = c("none", "all")) {
  unclaimed <- match.arg(unclaimed)
  x <- as_annotated_df(x)
  src <- source %||% attr(x, "source_data", exact = TRUE)

  # The `.filter` column makes a frame resolvable; a single value in it makes
  # this subset a claim.
  has_filter_col <- ".filter" %in% names(x)
  filter_claim <- if (has_filter_col) {
    single_value(as.character(x$.filter))
  } else {
    NULL
  }

  if (!length(columns) && !has_filter_col &&
        !all(c(".variable", ".variable_level") %in% names(x))) {
    stop(
      "Cannot resolve this drill to source rows: the input carries no ARD ",
      "identity columns (`.variable` / `.variable_level`) and no `.filter` ",
      "predicate, so there is no claim to read. For an aggregated table pass ",
      "`columns` (its group columns); a flat table needs no resolution at ",
      "all, its drilled rows ARE the source rows.",
      call. = FALSE
    )
  }

  if (is.null(src)) {
    stop(
      "No source data to resolve the drill against: the input carries no ",
      "`source_data` attribute and none was passed. The producer must stamp ",
      "the frame it summarised (for a composer table: ",
      "`attr(tbl, \"source_data\") <- data` before it leaves the block).",
      call. = FALSE
    )
  }
  if (!is.data.frame(src)) {
    stop("`source_data` must be a data frame, got <",
         paste(class(src), collapse = "/"), ">.", call. = FALSE)
  }

  claims <- drill_claim_columns(x, table = "", columns = columns)
  if (!length(claims) && is.null(filter_claim)) {
    return(if (identical(unclaimed, "all")) src else src[0, , drop = FALSE])
  }

  keep <- rep(TRUE, nrow(src))

  # A subset spanning several predicates claims nothing.
  if (!is.null(filter_claim)) {
    keep <- keep & eval_row_filter(filter_claim, src)
  }
  for (cl in claims) {
    # A column the source lacks is a producer mistake (the stamped frame was
    # too narrow). Zero rows would hide it.
    if (!cl$name %in% names(src)) {
      stop(
        "The drill claims `", cl$name, "`, which the source data does not ",
        "have. Stamp a frame that carries every column the table can claim ",
        "(here: ", paste(vapply(claims, `[[`, character(1L), "name"),
                          collapse = ", "), ").",
        call. = FALSE
      )
    }
    v <- src[[cl$name]]
    hit <- as.character(v) %in% as.character(cl$values)
    if (MISSING_LEVEL %in% cl$values) hit <- hit | is.na(v)
    keep <- keep & hit
  }

  out <- src[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Apply a display row's `.filter` predicate to the frame it was computed from.
# A predicate that does not evaluate is a producer bug and errors.
#' @noRd
eval_row_filter <- function(expr_txt, src) {
  parsed <- tryCatch(parse(text = expr_txt), error = function(e) NULL)
  if (is.null(parsed)) {
    stop("The drilled row's `.filter` is not parseable R: ", expr_txt,
         call. = FALSE)
  }
  ok <- tryCatch(
    eval(parsed[[1L]], envir = src, enclos = baseenv()),
    error = function(e) {
      stop("The drilled row's `.filter` (", expr_txt, ") could not be ",
           "evaluated against the source data: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  if (!is.logical(ok) || !(length(ok) %in% c(1L, nrow(src)))) {
    stop("The drilled row's `.filter` (", expr_txt, ") must give a logical ",
         "vector, got <", paste(class(ok), collapse = "/"), "> of length ",
         length(ok), ".", call. = FALSE)
  }
  # NA is not a match.
  ok <- rep_len(ok, nrow(src))
  !is.na(ok) & ok
}

# Rewrite an emitted `dplyr::filter(.(data), <cond>)` call so its data slot
# is coerced: `dplyr::filter(blockr.viz::as_plain_df(.(data)), <cond>)`. The
# chart and tile exprs use it only when the input is not a data frame.
wrap_plain_df_input <- function(ex) {
  ex[[2L]] <- as.call(list(quote(blockr.viz::as_plain_df), ex[[2L]]))
  ex
}

# ---------------------------------------------------------------------------
# Column identity (the column / cell drill)
# ---------------------------------------------------------------------------

#' Read a frame's column-axis identity.
#'
#' The row axis of an annotated frame is in the frame. The column axis is
#' stamped by a producer that knows it as an optional table-level attribute:
#'
#'   attr(data, "column_keys") <- list(
#'     "Placebo"    = list(list(column = "ARM", values = "Placebo")),
#'     "Placebo||F" = list(list(column = "ARM", values = "Placebo"),
#'                         list(column = "SEX", values = "F"))
#'   )
#'
#' keyed by the column path as the frame spells it, so a leaf header looks
#' itself up by its own name and a spanner by its prefix. A `values` vector of
#' length > 1 is a pooled column (`ARM %in% c(...)`).
#'
#' Without the attribute the renderer emits no column claims. Malformed
#' entries are dropped one at a time.
#' @noRd
annotated_column_keys <- function(data) {

  keys <- attr(data, "column_keys", exact = TRUE)

  if (!is.list(keys) || !length(keys) || is.null(names(keys))) {
    return(NULL)
  }

  ok <- function(entry) {
    is.list(entry) && length(entry) > 0L && all(vapply(entry, function(p) {
      is.list(p) && dd_is_string(p[["column"]]) &&
        length(p[["values"]]) > 0L && !anyNA(p[["values"]])
    }, logical(1L)))
  }

  keys <- keys[nzchar(names(keys)) & vapply(keys, ok, logical(1L))]

  if (!length(keys)) NULL else keys
}

#' Display text for the spanner header cells.
#'
#' A leaf column carries its header on `attr(col, "label")`. A spanner exists
#' only as a prefix in the names of the columns beneath it, so a producer that
#' knows more than the prefix stamps its text as an optional table-level
#' attribute:
#'
#'   attr(data, "spanner_labels") <- c(
#'     "Xanomeline" = "Xanomeline\nN = 168"
#'   )
#'
#' keyed by the prefix path exactly as `column_keys` is. A newline splits a
#' strong name line from a quiet sub-line. Without the attribute a group
#' header renders its prefix.
#' @noRd
annotated_spanner_labels <- function(data) {

  lbl <- attr(data, "spanner_labels", exact = TRUE)

  if (!length(lbl) || is.null(names(lbl))) {
    return(NULL)
  }

  lbl <- unlist(lapply(lbl, function(x) {
    if (is.character(x) && length(x) == 1L) x else NA_character_
  }))

  lbl <- lbl[nzchar(names(lbl)) & !is.na(lbl) & nzchar(lbl)]

  if (!length(lbl)) NULL else lbl
}

#' @noRd
dd_is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}
