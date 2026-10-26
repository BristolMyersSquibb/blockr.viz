# The variables layout of the table block: one row per statistic or level of
# each summarised variable, one column per level of `by` (Table 1). The
# settings come from the summary table's widget (inst/js/summary-table-block.js),
# the numbers from summary_table(), and the drawing from the records engine's
# structured path (R/table-records-server.R). See dev/table-block-rewrite.md.
#
# The variables and the grouping are the block's `summaries` and `by`, shared
# with the groups layout: switching between the two transposes the table
# instead of starting it over. A numeric variable is a `dist` summary, a
# categorical one a count bar split by its levels (`simple` with `color`).

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
# Entries that name no variable (a plain row count) are kept.
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
# the categorical shape. Once the data is known, turn those that name a
# numeric column into `dist` summaries. Entries a user shaped are left alone.
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

# Replace every `.(data)` placeholder in a bquoted block expression with
# `repl`, so the records engine's drill filter applies to the built table.
table_sub_data <- function(expr, repl) {
  if (is.call(expr)) {
    if (identical(expr[[1L]], as.name(".")) && length(expr) == 2L &&
          identical(expr[[2L]], as.name("data"))) {
      return(repl)
    }
    for (i in seq_along(expr)[-1L]) {
      if (!is.null(expr[[i]])) expr[[i]] <- table_sub_data(expr[[i]], repl)
    }
  }
  expr
}

#' @noRd
table_variables_server <- function(id, data, shared,
                                   stats = "mean_sd",
                                   sections = character(),
                                   total = FALSE,
                                   total_label = "Total",
                                   nest = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_stats <- shiny::reactiveVal(
      tryCatch(normalize_summary_stats(stats %||% "mean_sd"),
               error = function(e) "mean_sd")
    )
    r_sections <- shiny::reactiveVal(chr_vec_state(sections) %||% character())
    r_total <- shiny::reactiveVal(isTRUE(total))
    r_total_label <- shiny::reactiveVal(total_label %||% "Total")
    r_nest <- shiny::reactiveVal(isTRUE(nest))

    cfg <- shiny::reactive(list(
      # A variable that is also a grouping column has nothing to summarise
      # per group (a count split by its own levels in the groups layout).
      vars = setdiff(table_summaries_vars(shared$summaries()),
                     shared$by() %||% character()),
      sections = r_sections(),
      by = shared$by() %||% character(),
      stats = r_stats(),
      total = r_total(),
      total_label = r_total_label(),
      nest = r_nest(),
      id = shared$id(),
      missing = if (!is.null(shared$missing)) shared$missing() else "level"
    ))

    # The widget's own state shape (the summary table block's).
    widget_state <- function(c) {
      list(
        vars = as.list(c$vars),
        sections = as.list(c$sections),
        by = as.list(c$by),
        stats = as.list(c$stats),
        add_overall = c$total,
        overall_label = c$total_label,
        indent_details = TRUE,
        nest_hierarchies = c$nest,
        id_var = c$id %||% ""
      )
    }

    self_write <- new.env(parent = emptyenv())
    self_write$active <- FALSE

    send_columns <- function(d) {
      all_cols <- names(d)
      num_cols <- all_cols[vapply(d, is.numeric, logical(1))]
      log_cols <- all_cols[vapply(d, is.logical, logical(1))]
      cat_cols <- setdiff(all_cols, c(num_cols, log_cols))
      col_opt <- function(col) {
        lbl <- attr(d[[col]], "label", exact = TRUE)
        if (!is.null(lbl) && is.character(lbl) && nzchar(lbl[1L])) {
          list(value = col, label = lbl[1L])
        } else {
          col
        }
      }
      session$sendCustomMessage("summary-table-columns", list(
        id = ns("summary_input"),
        var_cols = lapply(c(num_cols, log_cols, cat_cols), col_opt),
        cat_cols = lapply(cat_cols, col_opt)
      ))
    }

    shiny::observeEvent(data(), {
      d <- data()
      shiny::req(is.data.frame(d))
      sm <- shiny::isolate(shared$summaries())
      fixed <- table_fix_var_types(sm, d)
      if (!identical(fixed, sm)) shared$summaries(fixed)
      send_columns(d)
      session$sendCustomMessage("summary-table-update", list(
        id = ns("summary_input"),
        state = widget_state(shiny::isolate(cfg()))
      ))
    })

    # Widget -> state. The variables and the grouping land in the block's
    # shared `summaries` / `by` / `id`.
    shiny::observeEvent(input$summary_input, {
      self_write$active <- TRUE
      st <- input$summary_input
      d <- tryCatch(shiny::isolate(data()), error = function(e) NULL)
      vars <- as.character(unlist(st$vars %||% character()))
      sm <- table_vars_to_summaries(shiny::isolate(shared$summaries()),
                                    vars, d)
      if (!identical(sm, shiny::isolate(shared$summaries()))) {
        shared$summaries(sm)
      }
      by <- as.character(unlist(st$by %||% character()))
      if (!identical(by, shiny::isolate(shared$by()))) shared$by(by)
      stats_in <- intersect(names(SUMMARY_STATS_CATALOG),
                            as.character(unlist(st$stats %||% character())))
      r_stats(if (length(stats_in)) stats_in else "mean_sd")
      r_sections(as.character(unlist(st$sections %||% character())))
      r_total(isTRUE(st$add_overall))
      r_total_label(st$overall_label %||% "Total")
      r_nest(isTRUE(st$nest_hierarchies))
      id_v <- st$id_var
      id_v <- if (is.null(id_v) || !nzchar(id_v)) NULL else as.character(id_v)
      if (!identical(id_v, shiny::isolate(shared$id()))) shared$id(id_v)
    })

    # State -> widget, for changes that did not come from the widget (a
    # restore, external control, an edit in the groups layout).
    shiny::observeEvent(cfg(), {
      if (self_write$active) {
        self_write$active <- FALSE
      } else {
        session$sendCustomMessage("summary-table-update", list(
          id = ns("summary_input"),
          state = widget_state(cfg())
        ))
      }
    })

    list(
      config = cfg,
      state = list(
        stats = r_stats,
        sections = r_sections,
        total = r_total,
        total_label = r_total_label,
        nest = r_nest
      )
    )
  })
}

#' @noRd
table_variables_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    blockr.ui::controls_dep(),
    summary_table_block_dep(),
    shiny::div(
      class = "block-container",
      shiny::div(
        id = ns("summary_input"),
        class = "summary-table-block-container"
      )
    )
  )
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
#' @return An annotated data frame (`.group1`, `.group1_level`, ..., `.label`,
#'   then the remaining columns).
#' @examples
#' d <- data.frame(species = c("a", "a", "b"), island = c("x", "y", "x"),
#'                 n = 1:3)
#' table_nest(d, by = "species", rowname = "island")
#' @export
table_nest <- function(data, by, rowname = NULL, missing = "drop") {
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
  res <- as.data.frame(out, stringsAsFactors = FALSE)
  for (cl in keep) res[[cl]] <- data[[cl]]
  rownames(res) <- NULL
  # The input column behind `.label`, for a row drill on it.
  attr(res, "nest_rowname") <- rowname
  res
}

# The name of the level that missing values become with `missing = "level"`.
# A drill on it claims the rows whose value is missing (summarize_drill_expr(),
# drill_source()).
MISSING_LEVEL <- "(Missing)"

# Missing values of the given columns as a level of their own, "(Missing)":
# `missing = "level"` in every layout. Factors get the level appended; other
# columns turn character. Columns without a missing value stay as they are.
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

# The table's number format, the chart's set (chart_format_state() on the
# chart branch): "number" (the default, up to two decimals), "compact" (1.2K,
# 3.4M, 5.6B), "percent" (a fraction x100 with "%") or a whole number of
# decimals, as an integer from 0 to 6. A number outside 0 to 6 is clamped
# (an old `digits = 8` reads as 6); anything else reads as "number".
# An old `digits = d` ("up to d decimals") as a format: 2, the old default,
# is "number"; any other number is that many decimals, now printed exactly.
table_digits_format <- function(digits) {
  f <- table_format_state(digits)
  if (identical(f, 2L)) "number" else f
}

table_format_state <- function(format) {
  f <- unlist(format, use.names = FALSE)
  if (!length(f) || is.na(f[[1L]])) return("number")
  f <- f[[1L]]
  if (is.character(f) && f %in% c("number", "compact", "percent")) return(f)
  n <- suppressWarnings(as.numeric(f))
  if (length(n) == 1L && is.finite(n)) {
    return(as.integer(max(0, min(6, round(n)))))
  }
  "number"
}
