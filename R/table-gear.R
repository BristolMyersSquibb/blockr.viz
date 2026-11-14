#' A table block's own gear
#'
#' A block built on the table (a swimmer, an AE timeline) shows its own
#' settings instead of the whole table's: its tabs, its sections, its rows.
#' Pass the result as `gear` to [new_table_block()]. Table settings the gear
#' does not name have no row but keep working: from code, and through a word
#' in the titles.
#'
#' A key in `tabs` is one of
#' * a setting of the table (`"by"`, `"sort_by"`, `"search"`, ...), or a
#'   shorthand: `"sort"` (sort_by, sort_dir), `"titles"` (title, subtitle,
#'   caption, and the "In the subtitle" ticks when `clauses` is given) or
#'   `"drill"` (drill, drill_target, drill_table);
#' * a value the prepare script declares (`doses <- factor(...)  #| select()`),
#'   drawn as an ordinary row that writes `script_values`;
#' * `"columns"`: a checklist of `columns`; with `pick_by`, a select of
#'   several picks instead, in the order picked (a drag reorders them), that
#'   also offers the prepared data's columns with one value per row;
#' * `"group"`: a pick among `groups` (or none) for the outer column of a
#'   nested `by`, which groups the rows;
#' * a key with an editor registered in the browser
#'   (`Blockr.viz.gear.editors[key]`), for a package's own row.
#'
#' A key that resolves to none of these has no row.
#'
#' @param tabs A named list, one element per tab in tray order. An element is
#'   a character vector of keys (one section, titled as the tab) or a named
#'   list of character vectors (sections and their keys).
#' @param columns A named list of the columns the checklist offers, in order:
#'   each a summary as in `summaries` of [new_table_block()], without `name`
#'   (the list name) and with an optional `on` (default `TRUE`), whether a new
#'   block shows it. The block shows the ticked ones, first, then its other
#'   summaries.
#' @param pick_by The column naming the table's outer rows (a patient id).
#'   Set, the `"columns"` row is a select of several picks: `columns` first,
#'   then every column of the prepared data with at most one value per value
#'   of `pick_by`, named by its label, each a `field` summary. A picked column
#'   the data no longer has is left out of the table. Columns whose name
#'   starts with a dot, and those of `by`, are not offered.
#' @param pick_skip Columns of the prepared data the `"columns"` row does
#'   not offer (the block's own: a swimlane's start and end).
#' @param sort_value The name of a value the prepare script declares, which
#'   the `"columns"` row sets to its first pick (the name in `columns`, else
#'   the data column's name; `""` with none), so the script can order the
#'   rows by the first column.
#' @param columns_note A line under the `"columns"` row.
#' @param groups The columns the rows may be grouped by, for the `"group"`
#'   row: a character vector, named by the labels the pick shows.
#' @param info A function of the block's input (a data frame or a dm),
#'   called again when it changes. What it returns goes to the browser as
#'   `derived.gear.info`, for a package's own rows (which tables and columns
#'   a block found). An error gives `NULL`.
#' @param open The tab the tray opens at when the prepare script fails (a
#'   table or column missing), or `NULL`.
#' @param deps HTML dependencies the block's page needs, the script of a
#'   package's own rows among them.
#' @param clauses A named character vector: per setting, the clause it adds to
#'   the subtitle when ticked "In the subtitle" (`c(doses = "doses shown as
#'   {@doses}")`), in sentence order.
#' @return A `viz_table_gear` object.
#' @examples
#' table_gear(
#'   tabs = list(
#'     Rows = c("by", "sort", "columns"),
#'     Titles = "titles"
#'   ),
#'   columns = list(
#'     Weeks = list(type = "simple", func = "max", col = "weeks"),
#'     Age = list(type = "field", col = "AGE", on = FALSE)
#'   )
#' )
#' @export
table_gear <- function(tabs, columns = list(), groups = character(),
                       clauses = character(), info = NULL, open = NULL,
                       deps = list(), pick_by = NULL, pick_skip = character(),
                       sort_value = NULL, columns_note = NULL) {
  stopifnot(
    is.list(tabs), length(tabs) > 0L, is_named(tabs),
    is.null(info) || is.function(info),
    is.null(open) || (is.character(open) && length(open) == 1L),
    is.list(columns), !length(columns) || is_named(columns),
    is.character(groups),
    is.character(clauses), !length(clauses) || is_named(clauses),
    is.null(pick_by) || (is.character(pick_by) && length(pick_by) == 1L),
    is.character(pick_skip),
    is.null(sort_value) ||
      (is.character(sort_value) && length(sort_value) == 1L),
    is.null(columns_note) ||
      (is.character(columns_note) && length(columns_note) == 1L)
  )
  tabs <- lapply(names(tabs), function(tab) {
    secs <- tabs[[tab]]
    if (is.character(secs)) secs <- stats::setNames(list(secs), tab)
    stopifnot(is.list(secs), is_named(secs),
              all(vapply(secs, is.character, logical(1L))))
    list(title = tab, sections = lapply(names(secs), function(s) {
      list(title = s, keys = table_gear_keys(secs[[s]], clauses))
    }))
  })
  columns <- lapply(names(columns), function(nm) {
    s <- columns[[nm]]
    stopifnot(is.list(s), is.character(s$type))
    on <- if (is.null(s$on)) TRUE else isTRUE(s$on)
    s$on <- NULL
    s$name <- nm
    list(name = nm, on = on, summary = s)
  })
  labels <- names(groups) %||% groups
  labels[!nzchar(labels)] <- groups[!nzchar(labels)]
  structure(list(tabs = tabs, columns = columns,
                 groups = list(values = unname(groups), labels = unname(labels)),
                 clauses = as.list(clauses), info = info, open = open,
                 deps = deps, pick_by = pick_by, pick_skip = pick_skip,
                 sort_value = sort_value,
                 columns_note = columns_note),
            class = "viz_table_gear")
}

# The shorthands, spelled out. "In the subtitle" ticks come with the titles
# when there are clauses to tick.
table_gear_keys <- function(keys, clauses) {
  out <- lapply(keys, function(k) {
    switch(
      k,
      sort = c("sort_by", "sort_dir"),
      titles = c("title", "subtitle", if (length(clauses)) "words", "caption"),
      drill = c("drill", "drill_target", "drill_table"),
      k
    )
  })
  unique(unlist(out, use.names = FALSE))
}

#' @export
print.viz_table_gear <- function(x, ...) {
  cat("<viz_table_gear>\n")
  for (t in x$tabs) {
    keys <- unlist(lapply(t$sections, `[[`, "keys"))
    cat("  ", t$title, ": ", paste(keys, collapse = ", "), "\n", sep = "")
  }
  if (length(x$columns)) {
    on <- vapply(x$columns, `[[`, logical(1L), "on")
    nm <- vapply(x$columns, `[[`, character(1L), "name")
    cat("  columns: ", paste0(nm, ifelse(on, "", " (off)"), collapse = ", "),
        "\n", sep = "")
  }
  invisible(x)
}

#' The summaries a new block with this gear shows
#'
#' The ticked `columns` of a [table_gear()], in order, then `other`.
#'
#' @param gear A [table_gear()].
#' @param other Summaries the checklist does not offer (the block's own
#'   column, a swimlane).
#' @return A list of summaries for `summaries` of [new_table_block()].
#' @export
table_gear_summaries <- function(gear, other = list()) {
  stopifnot(inherits(gear, "viz_table_gear"))
  on <- Filter(function(cl) cl$on, gear$columns)
  c(lapply(on, `[[`, "summary"), other)
}

# What the browser gets: the gear as built, under derived$gear, with what
# its `info` found in the input.
table_gear_wire <- function(gear, info = NULL, more = list()) {
  if (is.null(gear)) {
    return(NULL)
  }
  # Lists, not vectors: a section with one key, unboxed, would be a string.
  tabs <- lapply(gear$tabs, function(t) {
    t$sections <- lapply(t$sections, function(x) {
      x$keys <- as.list(x$keys)
      x
    })
    t
  })
  out <- list(
    open = gear$open,
    info = info,
    tabs = tabs,
    columns = lapply(gear$columns, function(cl) {
      list(name = cl$name, label = cl$summary$label %||% cl$name,
           summary = cl$summary)
    }),
    groups = lapply(gear$groups, as.list),
    clauses = gear$clauses,
    pick = if (!is.null(gear$pick_by)) {
      list(by = gear$pick_by, sort = gear$sort_value,
           note = gear$columns_note,
           more = more)
    }
  )
  # A NULL would go out as `{}`.
  out[!vapply(out, is.null, logical(1L))]
}

is_named <- function(x) {
  !is.null(names(x)) && all(nzchar(names(x)))
}

# The prepared data's columns with at most one value per value of
# `gear$pick_by`, as `{name, label}`: what the "columns" row offers after the
# gear's own columns. Not offered: dot columns, `pick_by` itself,
# `pick_skip`, list columns, and a column a gear column already shows as a
# field.
table_gear_more <- function(gear, data) {
  by <- gear$pick_by
  if (!is.data.frame(data) || !by %in% names(data)) {
    return(list())
  }
  own <- unlist(lapply(gear$columns, function(cl) {
    if (identical(cl$summary$type, "field")) cl$summary$col
  }))
  id <- as.character(data[[by]])
  nms <- setdiff(names(data), c(by, own, gear$pick_skip))
  nms <- nms[!startsWith(nms, ".")]
  keep <- vapply(nms, function(nm) {
    x <- data[[nm]]
    if (is.list(x)) return(FALSE)
    has <- !is.na(x) & !is.na(id)
    if (!any(has)) return(FALSE)
    u <- unique(data.frame(id = id[has], x = as.character(x[has]),
                           stringsAsFactors = FALSE))
    !anyDuplicated(u$id)
  }, logical(1L))
  lapply(nms[keep], function(nm) {
    lbl <- attr(data[[nm]], "label", exact = TRUE)
    label <- if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl)) {
      lbl
    } else {
      nm
    }
    list(name = nm, label = label)
  })
}
