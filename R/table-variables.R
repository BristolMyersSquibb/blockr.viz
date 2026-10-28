# The variables layout of the table block on the kit: one row per statistic or
# level of each variable, one column per level of `by` (a Table 1).

#' The variables layout on the kit
#' @param kit The kit (viz_block_server()).
#' @param src table_sources()'s list.
#' @return A part, as R/table-server.R describes it.
#' @noRd
table_variables_part <- function(kit, src) {
  st <- kit$state
  shown <- shiny::reactive({
    d <- src$rows()
    if (!is.data.frame(d)) {
      return(d)
    }
    table_model_variables(src$values(), d)
  })
  # The click filters the built table: the code that builds it takes the
  # place of the input in the filter.
  built_from <- function(ex) {
    d <- tryCatch(src$rows(), error = function(e) NULL)
    built <- table_variables_call(table_variables_cfg(table_state_values(st), d))
    if (is.null(built)) {
      return(ex)
    }
    built[[2L]] <- quote(.(data))
    dd_splice_slot(ex, built)
  }
  table_frame_part(kit, src, shown, layout = "variables", filter = built_from)
}

# A numeric variable is a `dist` summary, a categorical one a count bar split
# by its levels (`simple` with `color`).

# The variables a summaries list names, in order, without duplicates.
table_summaries_vars <- function(summaries) {
  out <- character()
  for (s in summaries %||% list()) {
    if (!is.list(s)) next
    v <- switch(
      as.character(s[["type"]] %||% "")[1L],
      dist = s[["col"]],
      field = s[["col"]],
      simple = s[["color"]] %||% s[["col"]],
      NULL
    )
    v <- as.character(unlist(v))
    if (length(v) && nzchar(v[1L])) out <- c(out, v[1L])
  }
  unique(out)
}

# Rewrite `summaries` so it names exactly `vars`: entries for variables that
# stay are kept as they are (whatever the groups layout drew them with),
# entries for dropped variables go, new variables get a default entry.
table_vars_to_summaries <- function(summaries, vars, data = NULL) {
  summaries <- summaries %||% list()
  keep <- list()
  for (s in summaries) {
    v <- table_summaries_vars(list(s))
    if (!length(v) || v %in% vars) keep[[length(keep) + 1L]] <- s
  }
  have <- table_summaries_vars(keep)
  for (v in setdiff(vars, have)) {
    num <- is.data.frame(data) && v %in% names(data) && is.numeric(data[[v]])
    keep[[length(keep) + 1L]] <- if (num) {
      list(type = "dist", col = v, show = "box")
    } else {
      list(type = "simple", func = "count", show = "bar", color = v,
           name = v)
    }
  }
  # Order as the widget lists the variables; variable-free entries first.
  ord <- vapply(keep, function(s) {
    v <- table_summaries_vars(list(s))
    if (length(v)) match(v, vars) else 0L
  }, integer(1L))
  keep[order(ord)]
}

# Entries made without the data (a constructor call or a restore) default to
# the categorical shape.
table_fix_var_types <- function(summaries, data) {
  if (!is.data.frame(data) || !length(summaries)) return(summaries)
  lapply(summaries, function(s) {
    v <- s[["color"]]
    auto <- is.list(s) && identical(s[["type"]], "simple") &&
      identical(s[["func"]], "count") && length(v) == 1L &&
      identical(s[["name"]], v) && is.null(s[["col"]])
    if (auto && v %in% names(data) && is.numeric(data[[v]])) {
      list(type = "dist", col = v, show = "box")
    } else {
      s
    }
  })
}

# summary_table() arguments from the layout's settings, or NULL while no
# variable is picked (the input then passes through unchanged).
table_variables_args <- function(cfg) {
  if (!length(cfg$vars)) return(NULL)
  args <- list(
    vars = cfg$vars,
    sections = cfg$sections %||% character(),
    by = cfg$by %||% character(),
    stats = cfg$stats %||% "mean_sd",
    add_overall = isTRUE(cfg$total),
    overall_label = cfg$total_label %||% "Total",
    indent_details = TRUE,
    nest_hierarchies = isTRUE(cfg$nest)
  )
  if (length(cfg$id) && nzchar(cfg$id)) args$subject_var <- cfg$id
  if (identical(cfg$missing, "level")) args$missing <- "level"
  args
}

# The call that builds the table from `data`, or NULL.
table_variables_call <- function(cfg) {
  args <- table_variables_args(cfg)
  if (is.null(args)) return(NULL)
  as.call(c(list(quote(blockr.viz::summary_table), quote(data)), args))
}

#' Nest a tidy frame into a finished table
#'
#' Turns a plain data frame into an annotated data frame whose rows sit under
#' collapsible headers, one per level of `by`: what the table block's records
#' layout draws when `by` is set. Groups keep the order in which they first
#' appear; a factor keeps its level order.
#'
#' @param data A data frame, e.g. a `dplyr::summarise()` result.
#' @param by One or two grouping columns, outer first.
#' @param rowname The column that labels each row. `NULL`: the first column
#'   that is not in `by`.
#' @param missing Rows whose `by` value is missing: `"drop"` (default) keeps
#'   them under a header without a value, `"level"` under a "(Missing)"
#'   header, which a drill can claim like any other group.
#' @param columns The columns shown beside the row names, in order. `NULL`
#'   (default): every other column.
#' @return An annotated data frame (`.group1`, `.group1_level`, ..., `.label`,
#'   then the remaining columns).
#' @examples
#' d <- data.frame(species = c("a", "a", "b"), island = c("x", "y", "x"),
#'                 n = 1:3)
#' table_nest(d, by = "species", rowname = "island")
#' @export
table_nest <- function(data, by, rowname = NULL, missing = "drop",
                       columns = NULL) {
  data <- as.data.frame(data)
  by <- intersect(as.character(unlist(by)), names(data))
  if (!length(by)) return(data)
  if (identical(chr_state(missing), "level")) {
    data <- table_missing_levels(data, by)
  }
  rest <- setdiff(names(data), by)
  rowname <- as.character(rowname %||% character())
  rowname <- if (length(rowname) && rowname[[1L]] %in% rest) {
    rowname[[1L]]
  } else {
    rest[[1L]]
  }
  pos <- function(x) {
    if (is.factor(x)) as.integer(x) else match(x, unique(x))
  }
  ord <- do.call(order, c(unname(lapply(by, function(b) pos(data[[b]]))),
                          list(seq_len(nrow(data)))))
  data <- data[ord, , drop = FALSE]
  out <- list()
  for (k in seq_along(by)) {
    out[[paste0(".group", k)]] <- rep(by[[k]], nrow(data))
    out[[paste0(".group", k, "_level")]] <- as.character(data[[by[[k]]]])
  }
  out[[".label"]] <- as.character(data[[rowname]])
  keep <- setdiff(rest, rowname)
  columns <- as.character(unlist(columns %||% character()))
  if (length(columns)) keep <- intersect(columns, keep)
  res <- as.data.frame(out, stringsAsFactors = FALSE)
  for (cl in keep) res[[cl]] <- data[[cl]]
  rownames(res) <- NULL
  # The input column behind `.label`, for a row drill on it.
  attr(res, "nest_rowname") <- rowname
  res
}

# The name of the level that missing values become with `missing = "level"`.
MISSING_LEVEL <- "(Missing)"

# Missing values of the given columns as a level of their own, "(Missing)":
# `missing = "level"` in every layout.
table_missing_levels <- function(data, cols) {
  cols <- intersect(unique(as.character(cols)), names(data))
  for (cl in cols) {
    x <- data[[cl]]
    if (!anyNA(x)) next
    if (is.factor(x)) {
      levels(x) <- c(levels(x), MISSING_LEVEL)
      x[is.na(x)] <- MISSING_LEVEL
    } else {
      x <- as.character(x)
      x[is.na(x)] <- MISSING_LEVEL
    }
    data[[cl]] <- x
  }
  data
}
