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
#' * `"columns"`: a checklist of `columns`;
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
                       deps = list()) {
  stopifnot(
    is.list(tabs), length(tabs) > 0L, is_named(tabs),
    is.null(info) || is.function(info),
    is.null(open) || (is.character(open) && length(open) == 1L),
    is.list(columns), !length(columns) || is_named(columns),
    is.character(groups),
    is.character(clauses), !length(clauses) || is_named(clauses)
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
                 deps = deps),
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
table_gear_wire <- function(gear, info = NULL) {
  if (is.null(gear)) {
    return(NULL)
  }
  out <- list(
    open = gear$open,
    info = info,
    tabs = gear$tabs,
    columns = lapply(gear$columns, function(cl) {
      list(name = cl$name, label = cl$summary$label %||% cl$name,
           summary = cl$summary)
    }),
    groups = gear$groups,
    clauses = gear$clauses
  )
  # A NULL would go out as `{}`.
  out[!vapply(out, is.null, logical(1L))]
}

is_named <- function(x) {
  !is.null(names(x)) && all(nzchar(names(x)))
}
