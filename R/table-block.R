# Whether a wide frame carries the dotted structure columns (`.group<k>*`,
# `.variable*`, `.label`, `.indent`, `.strong`) that summary_table() writes.
dt_is_structured <- function(data) {
  any(grepl("^\\.(group\\d+(_level|_label)?|variable(_level|_label)?|label|indent|strong)$",
            names(data)))
}

# Numeric cells of a structured frame as text in the number format, the unit
# in their header. Text cells (a Table 1) pass unchanged.
dt_structured_numbers <- function(data, data_cols, digits = "number",
                                  unit = NULL) {
  num <- data_cols[vapply(data[data_cols], is.numeric, logical(1L))]
  unit <- chr_state(trimws(as.character(unlist(unit))))
  for (cn in num) {
    x <- data[[cn]]
    lbl <- dt_col_label(x, cn) %||% cn
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    out[ok] <- dt_format_num(x[ok], digits)
    attributes(out) <- attributes(x)[setdiff(names(attributes(x)),
                                             c("class", "levels"))]
    if (!is.null(unit)) attr(out, "label") <- paste0(lbl, " (", unit, ")")
    data[[cn]] <- out
  }
  data
}

#' Cell-colour spec for a table
#'
#' How numeric cells are painted; the data is never changed. `"diverging"`
#' is a scale centred on 0 (a correlation matrix), `"sequential"` a low to
#' high ramp, `"bar"` an in-cell bar scaled per column on absolute values.
#'
#' @param type `"diverging"`, `"sequential"`, or `"bar"`.
#' @param domain `NULL` to infer from the data (symmetric around 0 for
#'   diverging), or `c(min, max)`. Ignored for `"bar"`.
#' @param palette `NULL` for the defaults, or colours: three for diverging
#'   (low, mid, high), two for sequential, one for the bar.
#' @param columns `NULL` for every numeric column, re-resolved on new data,
#'   or the columns to paint. A missing column is skipped.
#' @param source `NULL` to paint by the cell's own value, a column name to
#'   paint every shaded column by that column, or a template such as
#'   `"{col} (sev)"` for one companion per column. Source columns are hidden
#'   and must be numeric. Diverging and sequential only.
#' @return A cell-colour spec list.
#' @examples
#' drilldown_table_color("sequential", domain = c(0, 100))
#' @export
drilldown_table_color <- function(type = c("diverging", "sequential", "bar"),
                                   domain = NULL, palette = NULL,
                                   columns = NULL, source = NULL) {
  type <- match.arg(type)
  list(type = type, domain = domain, palette = palette, columns = columns,
       source = source)
}

# The input aggregated for display: one row per `group`, a column per metric
# of `summaries` (`list(func, cols)` each). Neither: the frame unchanged.
# `na_group = "drop"` removes rows with a missing key (NA or ""). Returns
# `list(data, group, metric_cols, aggregated)`.
dd_table_aggregate <- function(data, group, summaries,
                               na_group = c("level", "drop")) {
  na_group <- match.arg(na_group)
  group <- intersect(as.character(group), names(data))
  if (identical(na_group, "drop") && length(group)) {
    miss <- lapply(data[group], function(x) {
      x <- as.character(x)
      is.na(x) | !nzchar(x)
    })
    data <- data[!Reduce(`|`, miss), , drop = FALSE]
  }
  plan  <- dd_metric_plan(summaries, data)
  if (!length(group) && !length(plan)) {
    return(list(data = data, group = character(), metric_cols = character(),
                aggregated = FALSE))
  }
  # A group without a metric counts; a metric without a group is one row.
  if (!length(plan)) {
    plan <- list(list(name = "Count", expr = quote(dplyr::n()), label = NULL))
  }
  exprs <- stats::setNames(lapply(plan, `[[`, "expr"),
                           vapply(plan, `[[`, "", "name"))
  out <- if (length(group)) {
    g <- dplyr::group_by(data, dplyr::across(dplyr::all_of(group)))
    as.data.frame(dplyr::summarise(g, !!!exprs, .groups = "drop"),
                  check.names = FALSE)
  } else {
    as.data.frame(dplyr::summarise(data, !!!exprs), check.names = FALSE)
  }
  # summarise() drops labels: "Mean: Baseline BMI" under "BMIBL".
  for (p in plan) {
    if (!is.null(p$label) && p$name %in% names(out)) {
      attr(out[[p$name]], "label") <- p$label
    }
  }
  list(data = out, group = group,
       metric_cols = vapply(plan, `[[`, "", "name"), aggregated = TRUE)
}

# min/max of an all-NA group is NA, as in the JS, not Inf with a warning.
dd_agg_min <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) x[NA_integer_] else min(x, na.rm = TRUE)
}

dd_agg_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
}

# One `list(name, expr, label)` per metric: the header (stat-qualified when
# a column repeats), the summarise() call, the label ("Median: Baseline
# BMI"). Non-numeric or missing columns fall away.
dd_metric_plan <- function(summaries, data) {
  fn_calls <- list(mean = quote(mean), median = quote(stats::median),
                   sum = quote(sum), min = quote(dd_agg_min),
                   max = quote(dd_agg_max))
  orig_label <- function(col) {
    l <- attr(data[[col]], "label", exact = TRUE)
    if (is.character(l) && length(l) == 1L && nzchar(l)) l else col
  }
  # An environment, so uniq() and add() accumulate without `<<-` (lintr).
  acc <- new.env(parent = emptyenv())
  acc$used <- character()
  acc$plan <- list()
  uniq <- function(nm, tag) {
    if (!(nm %in% acc$used)) {
      acc$used <- c(acc$used, nm)
      return(nm)
    }
    nm2 <- paste0(nm, " (", tag, ")")
    while (nm2 %in% acc$used) nm2 <- paste0(nm2, " ")
    acc$used <- c(acc$used, nm2)
    nm2
  }
  add <- function(name, expr, label) {
    acc$plan[[length(acc$plan) + 1L]] <-
      list(name = name, expr = expr, label = label)
  }
  # A numeric aggregation without columns takes every numeric column no
  # other entry names. count_distinct needs its columns.
  claimed <- unique(unlist(lapply(
    summaries %||% list(),
    function(m) as.character(m$cols %||% character())
  )))
  all_num <- names(data)[vapply(data, is.numeric, logical(1))]
  for (m in summaries %||% list()) {
    fn   <- as.character(m$func %||% "count")[1L]
    raw  <- as.character(m$cols %||% character())
    cols <- intersect(raw, names(data))
    # Picked columns that are gone stay gone; only no pick widens.
    if (!length(raw) && fn %in% names(fn_calls)) {
      cols <- setdiff(all_num, claimed)
    }
    if (identical(fn, "count")) {
      add(uniq(AGG_WORDS[["count"]], "count"), quote(dplyr::n()),
          "Number of Observations")
    } else if (identical(fn, "count_distinct")) {
      for (col in cols) {
        add(uniq(col, "distinct"),
            bquote(dplyr::n_distinct(.data[[.(col)]], na.rm = TRUE),
                   list(col = col)),
            paste0(AGG_WORDS[["count_distinct"]], ": ", orig_label(col)))
      }
    } else if (!is.null(fn_calls[[fn]])) {
      fc <- fn_calls[[fn]]
      for (col in cols) {
        if (is.numeric(data[[col]])) {
          add(uniq(col, fn),
              bquote(.(fc)(.data[[.(col)]], na.rm = TRUE),
                     list(fc = fc, col = col)),
              paste0(AGG_WORDS[[fn]], ": ", orig_label(col)))
        }
      }
    }
  }
  acc$plan
}

# `summaries` from a list or a JSON string as `list(list(func, cols), ...)`.
dd_parse_summaries <- function(v) {
  if (is.null(v)) return(list())
  ms <- if (is.character(v) && length(v) == 1L) {
    tryCatch(jsonlite::fromJSON(v, simplifyVector = FALSE),
             error = function(e) list())
  } else if (is.list(v)) {
    v
  } else {
    list()
  }
  lapply(ms, function(m) {
    list(
      func = as.character(m$func %||% "count")[1L],
      cols = as.character(unlist(m$cols %||% character()))
    )
  })
}

# `.data[[c1]] == v1 & .data[[c2]] == v2 & ...` for a grouped row click.
dd_group_filter_call <- function(cols, vals) {
  conds <- Map(function(cl, vl) bquote(.data[[.(cl)]] == .(vl)),
               as.character(cols), as.character(vals))
  Reduce(function(a, b) bquote(.(a) & .(b)), conds)
}

# A nested records claim as a filter on the input: `.group<k>_level` stands
# for the k-th `by` column; a real column (the drill column) is kept.
table_nest_drill_expr <- function(by, gc, gv) {
  by <- as.character(unlist(by))
  gc <- as.character(unlist(gc))
  gv <- as.character(unlist(gv))
  if (!length(gc) || length(gc) != length(gv)) {
    return(summarize_drill_expr(list()))
  }
  k <- suppressWarnings(
    as.integer(sub("^\\.group([0-9]+)_level$", "\\1", gc))
  )
  grp <- !is.na(k) & k <= length(by)
  real <- is.na(k) & !startsWith(gc, ".")
  cols <- gc
  cols[grp] <- by[k[grp]]
  keep <- grp | real
  summarize_drill_expr(stats::setNames(as.list(gv[keep]), cols[keep]))
}

# A claim in the data's words, outermost first: `.variable` /
# `.variable_level` read "AEDECOD = ABDOMINAL PAIN", `.group<k>_level` reads
# "ARM = Placebo" given `group_names` (dt_group_names()).
dt_claim_label <- function(gc, gv, group_names = character()) {
  gc <- as.character(unlist(gc))
  gv <- as.character(unlist(gv))
  if (!length(gc) || length(gc) != length(gv)) {
    return(character())
  }
  glv <- grepl("^\\.group[0-9]+_level$", gc)
  vi <- match(".variable", gc)
  li <- match(".variable_level", gc)
  if (!is.na(vi) && !is.na(li)) {
    c(gv[glv], paste0(gv[vi], " = ", gv[li]))
  } else if (any(glv)) {
    k <- as.integer(sub("^\\.group([0-9]+)_level$", "\\1", gc[glv]))
    nm <- group_names[k]
    real <- !glv & !startsWith(gc, ".")
    c(ifelse(!is.na(nm) & nzchar(nm), paste0(nm, " = ", gv[glv]), gv[glv]),
      if (any(real)) paste0(gc[real], " = ", gv[real]))
  } else {
    paste0(gc, " = ", gv)
  }
}

# The column each `.group<k>` stands for, read off the `.group<k>` column.
dt_group_names <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(character())
  out <- character()
  k <- 1L
  while (paste0(".group", k) %in% names(data)) {
    v <- as.character(data[[paste0(".group", k)]])
    v <- v[!is.na(v) & nzchar(v)]
    out[[k]] <- if (length(v)) v[[1L]] else ""
    k <- k + 1L
  }
  out
}

# A column claim as `list(list(column, values))`, or NULL.
dd_col_keys_state <- function(x) {
  if (!is.list(x) || !length(x)) return(NULL)
  out <- lapply(x, function(k) {
    if (!is.list(k)) return(NULL)
    cl <- as.character(unlist(k[["column"]]))
    vl <- as.character(unlist(k[["values"]]))
    if (length(cl) != 1L || !nzchar(cl) || !length(vl)) return(NULL)
    list(column = cl, values = vl)
  })
  out <- Filter(Negate(is.null), out)
  if (length(out)) out
}

# "ARM = Low dose" for each column claim.
dd_col_keys_label <- function(ck) {
  ck <- dd_col_keys_state(ck)
  vapply(ck %||% list(), function(k) {
    paste0(k$column, " = ", paste(k$values, collapse = ", "))
  }, character(1L))
}

# `dplyr::filter(<ex>, ARM %in% "Low dose")` for a column claim.
dd_col_keys_filter <- function(ex, ck) {
  ck <- dd_col_keys_state(ck)
  if (!length(ck)) return(ex)
  f <- stats::setNames(lapply(ck, `[[`, "values"),
                       vapply(ck, `[[`, "", "column"))
  out <- summarize_drill_expr(f)
  out[[2L]] <- ex
  out
}

# Whether a download format's packages are installed: system.file(), since
# requireNamespace() would load officer and flextable (0.5 s, 53 MB) just to
# grey out a menu entry. A seam, so tests can mock it.
dt_pkg_installed <- function(...) {
  all(vapply(c(...), function(p) nzchar(system.file(package = p)), logical(1L)))
}

dt_has_openxlsx <- function() {
  dt_pkg_installed("openxlsx")
}

dt_has_officer <- function() {
  dt_pkg_installed("officer", "flextable")
}

# A new groups table's first column: a count drawn as a bar.
table_groups_seed <- function() {
  list(list(type = "simple", func = "count", show = "bar", name = "Count"))
}

# What the assistant reads about the table block.
table_guidance <- function() {
  paste0(
    "The table block draws every table. `layout` says what one row is:\n",
    "- \"records\": the input rows as they are -- listings, model output, ",
    "a correlation matrix, or a finished table built in R (a frame from ",
    "dplyr, summary_table(), gtsummary converted with as_annotated_df()). ",
    "`rowname` picks the stub column; `columns` EXACTLY the columns the ",
    "user names (\"Revenue and Profit\" -> columns = [\"Revenue\", ",
    "\"Profit\"]), empty for all. `by` nests the rows under one header per ",
    "level. `shadings` colours numeric cells (diverging for a correlation ",
    "matrix, sequential for magnitudes); `format` is \"number\", ",
    "\"compact\", \"percent\" or a number of decimals, `unit` a unit ",
    "for the headers.\n",
    "- \"groups\": one row per level of `by` (at most two, outer first; the ",
    "outer is an expandable parent), one column per entry of `summaries`. ",
    "Without `by` it is one row over all input rows: the headline numbers ",
    "of a dashboard, usually with look = \"cards\". ",
    "Use it when one row is one category: most frequent adverse events ",
    "(simple count_distinct of USUBJID, show = bar), duration by term ",
    "(dist, show = box), an episode swimlane per subject (spans), a lab ",
    "trajectory (series), baseline to peak (pair). `color` on a summary ",
    "splits its cell (severity); `facet` repeats it per level (treatment ",
    "arm) on one scale; both are per column, never table-wide.\n",
    "- \"variables\": a Table 1. One row per statistic or level of each ",
    "variable in `summaries` (a dist for a numeric variable, a simple count ",
    "split by color for a categorical one), one column per level of `by`. ",
    "`stats` picks the numeric statistics (mean_sd, median_q1_q3, n_pct, ",
    "min_max, ...), `total` adds a Total column, `sections` is an OUTER ",
    "column that contains the variables (SOC over preferred term: ",
    "sections = [\"AEBODSYS\"], the term as the variable).\n",
    "A heatmap is the groups layout with look = \"compact\": by = [arm, ",
    "subject], one simple count with show = \"tile\", facet = the term, ",
    "fill = the grade (painted by the worst level). look = \"cards\" draws ",
    "one card per summary.\n",
    "Counting: only when the user asks for subjects or patients rather ",
    "than events, never for one-row-per-subject data. Groups layout: a ",
    "simple count_distinct with col = USUBJID. Variables layout: ",
    "`id` = USUBJID. ",
    "`pct_of` = \"facet\" (default) takes a count's percentage over its ",
    "arm; \"total\" over the table. `missing` = \"level\" (default) shows ",
    "missing groups as \"(Missing)\"; \"drop\" leaves them out.\n",
    "`drill` makes a row click filter downstream blocks; `drill_target` ",
    "sends the click to a filter block instead. Do NOT set `top_n` for an ",
    "interactive board (the table scrolls); it is for report exhibits.\n",
    "Map common requests: \"demographics by arm\" -> layout = \"variables\", ",
    "by = [\"TRT01A\"], summaries = AGE (dist), SEX and RACE (simple count ",
    "split by color), total = TRUE. \"most frequent AEs per arm\" -> layout ",
    "= \"groups\", by = [\"AEDECOD\"], summaries = ",
    "[simple count_distinct of USUBJID, facet = \"TRT01A\"]. For vertical ",
    "columns, lines or two measures on axes, use the chart block."
  )
}

#' @rdname new_table_block
#' @export
new_table_block <- function(layout = "records",
                            # `...` early: later arguments match exactly, so
                            # an old `value` cannot match `value_labels`.
                            ...,
                            by = character(),
                            rowname = NULL,
                            columns = NULL,
                            summaries = list(),
                            id = NULL,
                            percent_of = "facet",
                            missing = "level",
                            color = "auto",
                            shadings = list(),
                            format = "number",
                            unit = "",
                            look = "table",
                            facet_layout = "by_summary",
                            sort_by = "value",
                            sort_dir = NULL,
                            top_n = NULL,
                            value_labels = TRUE,
                            column_axis = TRUE,
                            bar_width = "fit",
                            drill = NULL,
                            search = TRUE,
                            sortable = TRUE,
                            collapsible = TRUE,
                            download = TRUE,
                            stats = "mean_sd",
                            sections = character(),
                            total = FALSE,
                            total_label = "Total",
                            nest = FALSE,
                            title = NULL,
                            subtitle = NULL,
                            caption = getOption("blockr.viz.default_caption"),
                            drill_target = "",
                            drill_table = "",
                            script = NULL,
                            script_values = list(),
                            selection = NULL) {

  spec <- viz_block_spec("table")
  given <- mget(intersect(as.character(names(match.call())[-1L]),
                          names(spec$args)))
  dots <- list(...)
  # Old argument names; any other name is a block attribute (block_name,
  # blockr.dock's `visible`) that blockr.core keeps.
  old <- intersect(names(dots), legacy_arg_names("table"))
  args <- legacy_ctor_args("table", c(given, dots[old]))
  values <- table_normalize(viz_check(spec, args))
  # blockr.core reads the starting state from this frame.
  list2env(values, environment())

  res <- blockr.core::new_transform_block(
    server = function(id, data) table_server(id, data, values),
    ui = table_ui,
    dat_valid = validate_annotated_df_input,
    allow_empty_state = viz_empty_ok(spec),
    external_ctrl = viz_ctrl_args(spec),
    expr_type = "bquoted",
    class = "table_block",
    ...
  )
  # Old names must not be saved back as attributes.
  for (nm in old) attr(res, nm) <- NULL
  res
}

table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    table_view_dep(),
    viz_output(ns("table"), "table")
  )
}
