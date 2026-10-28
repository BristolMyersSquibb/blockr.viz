# ---------------------------------------------------------------------------
# Structured ("Table 1") rendering -- section nesting, indents, spanners.
# Reuses html_table()'s thead / tbody builders so the structure matches the
# static html_table renderer exactly; only the surrounding chrome (the
# table-block wrapper, gear, drill/colour attributes) differs.
# ---------------------------------------------------------------------------

#' Does this (already wide) frame carry the dotted-column structure?
#'
#' True when it has any row-side grouping / identity column (the ARD-named
#' `.group<k>*` / `.variable*` pairs) OR a `.label` stub OR an `.indent` /
#' `.strong` column (the positional fallback dialect) -- the signals
#' [summary_table()] and coercing producers emit and `html_table()` renders.
#' @noRd
dt_is_structured <- function(data) {
  any(grepl("^\\.(group\\d+(_level|_label)?|variable(_level|_label)?|label|indent|strong)$",
            names(data)))
}

#' Numeric cells of a structured frame, printed in the block's number format
#' with the unit in their header. A nested tidy frame (table_nest()) keeps
#' its numbers as numbers; a Table 1 or a converted finished table holds text
#' cells and passes unchanged.
#' @noRd
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
#' Describes how numeric cells are decorated. Three modes, all purely
#' presentational (the data is never changed):
#'
#' - `"diverging"` -- a value-to-background scale centred on 0 with a
#'   symmetric domain (correlation matrices: -1 white-at-0 +1).
#' - `"sequential"` -- a low-to-high background ramp (heatmaps).
#' - `"bar"` -- an in-cell horizontal data bar proportional to the value
#'   (e.g. "patients with most adverse events"), normalised per column on
#'   absolute magnitude and left-anchored.
#'
#' @param type `"diverging"`, `"sequential"`, or `"bar"`.
#' @param domain `NULL` to infer from the data, or `c(min, max)`.
#'   Ignored for `"bar"` (always per-column). For `"diverging"` the
#'   inferred domain is symmetric around 0.
#' @param palette `NULL` for type defaults, or a character vector of
#'   colors -- length 3 for diverging (low/mid/high), length 2 for
#'   sequential (low/high), length 1 for the bar fill.
#' @param columns `NULL`/empty to apply to **all** numeric columns (a
#'   rule, re-resolved against whatever data arrives -- survives upstream
#'   schema changes), or a character vector to restrict to those columns.
#'   A picked column that no longer exists is silently skipped.
#' @param source `NULL` (the paint reads the shaded column's own values), a
#'   column name (paint every shaded column by THAT column's row values), or
#'   a `{col}` template such as `"{col} (sev)"` (pair each shaded column
#'   with its own companion, re-resolved per render). The cell keeps
#'   displaying its own value while the background encodes the source's --
#'   the AE heatmap's "count shown, worst grade painted". Source columns are
#'   hidden from the display; sources must be numeric. Diverging/sequential
#'   only.
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

# --- internal helpers --------------------------------------------------

#' @noRd
dt_th <- function(name, idx, stub = FALSE, label = NULL, numeric = FALSE,
                  sortable = TRUE, extra = NULL) {
  # `blockr-sortable` is the hook wireSort() binds to; drop it (and the sort
  # arrow) when sorting is turned off so the column is inert.
  base <- if (stub) "blockr-stub-header" else "blockr-col-header leaf"
  cls  <- if (isTRUE(sortable)) paste(base, "blockr-sortable") else base
  # Align the whole header (name + sub-label + sort arrow) to the column's
  # data type, so a flat table reads like the structured one: numeric right,
  # text left. The explicit class wins over the inherited html-table delta.
  cls <- paste(cls, if (isTRUE(numeric)) "dt-col-num" else "dt-col-txt")
  name_span <- htmltools::tags$span(class = "blockr-col-name", name)
  sort_span <- if (isTRUE(sortable)) {
    htmltools::tags$span(class = "blockr-sort-icon")
  }
  if (!is.null(label)) {
    # Labelled column: name on top, then the subtext and the sort arrow
    # share the lower row (label left, arrow right) -- like the html
    # preview, so the arrow doesn't add a row of its own.
    htmltools::tags$th(
      class = cls,
      `data-col-index` = idx,
      name_span,
      htmltools::tags$div(
        class = "dt-th-subrow",
        htmltools::tags$span(class = "blockr-col-label", label),
        sort_span
      ),
      # `extra` is trailing header content the caller owns (the summarize
      # table's column axis). Inside the SAME cell rather than a second header
      # row, so it inherits the sticky behaviour and the column's padding, which
      # is what keeps a tick aligned with the mark below it.
      extra
    )
  } else {
    # Unlabelled column: name and sort arrow share the single row.
    htmltools::tags$th(
      class = cls,
      `data-col-index` = idx,
      htmltools::tags$div(
        class = "dt-th-namerow",
        name_span,
        sort_span
      ),
      extra
    )
  }
}

#' @noRd
dt_message_table <- function(msg = "No data") {
  htmltools::tags$table(
    class = "blockr-table",
    htmltools::tags$tbody(
      htmltools::tags$tr(htmltools::tags$td(class = "blockr-data", msg))
    )
  )
}

#' Aggregate the table's input FOR DISPLAY.
#'
#' When `group` names one or more (existing) columns, collapse the frame to one
#' row per group with a column per metric. `summaries` is a list, each element
#' `list(func, cols)`, so one table can carry several summaries at once (e.g.
#' mean(AGE) and sum(DOSE)). Same aggregation vocabulary the chart uses (shared
#' `AGG_FNS`). This is a display projection only; the block's data output stays
#' the raw input filtered by the click (see the block server's `expr`). No group
#' -> the frame is returned unchanged, so the renderer draws the raw table.
#'
#' `na_group` decides what happens to rows whose group key is missing.
#' `"level"` (the default, and the behaviour this has always had) gives them
#' their own cell -- R keys it `NA`, the JS twin keys it `""`, same rows and
#' same numbers. `"drop"` removes them: they are not a category. Missing means
#' `NA` *or* the empty string, matching the JS fold `String(row[g] ?? "")`, so
#' both engines drop the same rows and the golden cross-test still holds.
#'
#' @return `list(data, group, metric_cols)` -- the (aggregated or raw) frame,
#'   the resolved group columns, and the metric column names.
#' @noRd
dd_table_aggregate <- function(data, group, summaries,
                               na_group = c("level", "drop")) {
  na_group <- match.arg(na_group)
  group <- intersect(as.character(group), names(data))
  if (identical(na_group, "drop") && length(group)) {
    # Any missing key drops the row: with several group columns the cell has no
    # complete address, so there is no honest place to put it.
    miss <- lapply(data[group], function(x) {
      x <- as.character(x)
      is.na(x) | !nzchar(x)
    })
    data <- data[!Reduce(`|`, miss), , drop = FALSE]
  }
  plan  <- dd_metric_plan(summaries, data)
  # Aggregation is "active" when the block carries a group and/or a metric.
  # Neither -> the raw frame passes through unchanged (`aggregated = FALSE`).
  if (!length(group) && !length(plan)) {
    return(list(data = data, group = character(), metric_cols = character(),
                aggregated = FALSE))
  }
  # A grouped request with no usable metric still counts; a metric with no group
  # reduces the WHOLE frame to a single totals row (grand totals).
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
    # Grand totals: an ungrouped summarise -> exactly one row of metric values.
    as.data.frame(dplyr::summarise(data, !!!exprs), check.names = FALSE)
  }
  # Carry a friendly, stat-prefixed label onto each metric column -- the source
  # column's own label is lost in summarise -- so the header reads e.g. bold
  # "BMIBL" over "Mean: Baseline BMI (kg/m^2)" (dt_col_label surfaces it).
  for (p in plan) {
    if (!is.null(p$label) && p$name %in% names(out)) {
      attr(out[[p$name]], "label") <- p$label
    }
  }
  list(data = out, group = group,
       metric_cols = vapply(plan, `[[`, "", "name"), aggregated = TRUE)
}

#' min/max with an empty-group guard.
#'
#' `min(x, na.rm = TRUE)` on an all-NA (or empty) group warns and returns
#' `Inf` (`-Inf` for max) -- neither a real extremum nor the `NA` the rest of
#' the vocabulary yields (mean -> NaN, median -> NA). An empty group has no
#' extremum: return NA (of `x`'s type), matching the JS engine's null and
#' keeping a chart and its table twin on the same number. `na.rm` is accepted
#' (dd_metric_plan composes every numeric aggregation as `fn(x, na.rm = TRUE)`)
#' but missing values are always removed.
#' @noRd
dd_agg_min <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) x[NA_integer_] else min(x, na.rm = TRUE)
}

#' @noRd
dd_agg_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
}

#' Plan the metric columns for a summaries list.
#'
#' For each metric produces `list(name, expr, label)`: `name` is the bold header
#' (the source variable when free, else stat-qualified so it stays unique),
#' `expr` the summarise call, and `label` the grey subtext -- the source column's
#' own label prefixed with the stat, e.g. "Median: Baseline BMI (kg/m^2)". count
#' -> "Count" (no label); count_distinct / numeric aggregations -> one entry per
#' (numeric) column. Non-numeric or missing columns fall away.
#' @noRd
dd_metric_plan <- function(summaries, data) {
  # Words come from the shared AGG_WORDS (R/block-arguments.R), the R twin of
  # the JS AGG_WORDS -- one home per side, tied by the drift test.
  fn_calls <- list(mean = quote(mean), median = quote(stats::median),
                   sum = quote(sum), min = quote(dd_agg_min),
                   max = quote(dd_agg_max))
  orig_label <- function(col) {
    l <- attr(data[[col]], "label", exact = TRUE)
    if (is.character(l) && length(l) == 1L && nzchar(l)) l else col
  }
  # `uniq()` and `add()` accumulate across the whole loop below, so their state
  # is held in an environment and mutated with a plain `<-`: an env is the one
  # R value with reference semantics, which is what the accumulation needs.
  # Do not fold this back into locals + `<<-` -- that is the same write with a
  # less explicit target.
  acc <- new.env(parent = emptyenv())
  acc$used <- character()
  acc$plan <- list()
  uniq <- function(nm, tag) {
    if (!(nm %in% acc$used)) {
      acc$used <- c(acc$used, nm)
      return(nm)
    }
    # Same variable aggregated twice -> qualify the second so the name is unique.
    nm2 <- paste0(nm, " (", tag, ")")
    while (nm2 %in% acc$used) nm2 <- paste0(nm2, " ")
    acc$used <- c(acc$used, nm2)
    nm2
  }
  add <- function(name, expr, label) {
    acc$plan[[length(acc$plan) + 1L]] <-
      list(name = name, expr = expr, label = label)
  }
  # Default-function rule (override semantics): a NUMERIC aggregation whose
  # entry names no columns applies to ALL numeric columns EXCEPT those
  # explicitly claimed by other entries -- dplyr's
  # `summarise(across(where(is.numeric), fn), DOSE = sum(DOSE))` with the
  # explicit entry winning its column. A rule, not a snapshot: it re-resolves
  # against the current frame, so upstream schema changes self-heal (same
  # convention as the empty colour scope = "all numeric"). count needs no
  # columns; count_distinct stays explicit-only (a distinct count over every
  # numeric column is rarely meant).
  claimed <- unique(unlist(lapply(
    summaries %||% list(),
    function(m) as.character(m$cols %||% character())
  )))
  all_num <- names(data)[vapply(data, is.numeric, logical(1))]
  for (m in summaries %||% list()) {
    fn   <- as.character(m$func %||% "count")[1L]
    raw  <- as.character(m$cols %||% character())
    cols <- intersect(raw, names(data))
    # Expand only when NOTHING was picked -- picked-but-dropped-upstream
    # columns must stay a visible no-op, not silently widen to all numerics.
    if (!length(raw) && fn %in% names(fn_calls)) {
      cols <- setdiff(all_num, claimed)
    }
    if (identical(fn, "count")) {
      add(uniq(AGG_WORDS[["count"]], "count"), quote(dplyr::n()),
          "Number of Observations")
    } else if (identical(fn, "count_distinct")) {
      for (col in cols) {
        add(uniq(col, "distinct"),
            # na.rm: NA is missingness, not a distinct value -- and the JS
            # chart aggregation excludes null/NaN, so the same distinct count
            # must read the same number on a chart and its table twin.
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

#' Normalize the `summaries` list from a restored block state (an R list) or from
#' JS (a JSON string) to `list(list(func, cols), ...)`.
#' @noRd
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

#' Serialize the `summaries` list to the JSON the gear reads off `data-dt-summaries`
#' (`func` a scalar, `cols` always an array). JSON lives only at this edge.
#' @noRd
dd_summaries_json <- function(summaries) {
  if (!length(summaries)) return("[]")
  as.character(jsonlite::toJSON(
    lapply(summaries, function(m) {
      list(
        func = jsonlite::unbox(as.character(m$func %||% "count")[1L]),
        cols = as.character(m$cols %||% character())
      )
    }),
    auto_unbox = FALSE
  ))
}

#' Serialize the `shadings` list to the JSON the gear reads off
#' `data-dt-shadings` (`mode` a scalar, `cols` always an array).
#' @noRd
dd_shadings_json <- function(shadings) {
  if (!length(shadings)) return("[]")
  as.character(jsonlite::toJSON(
    lapply(shadings, function(s) {
      out <- list(
        mode = jsonlite::unbox(as.character(s$mode %||% "diverging")[1L]),
        cols = as.character(s$cols %||% character())
      )
      # `source` must ROUND-TRIP through the gear even though the gear never
      # edits it: JS echoes the whole list back on any rule edit, so a field
      # left out here would be silently dropped by the first unrelated
      # mode/cols change.
      if (!is.null(s$source)) {
        out$source <- jsonlite::unbox(as.character(s$source)[1L])
      }
      out
    }),
    auto_unbox = FALSE
  ))
}

#' Build the ANDed equality filter for a grouped-table row click.
#'
#' Returns a language object `.data[[c1]] == v1 & .data[[c2]] == v2 & ...` from
#' aligned column / value vectors (the clicked row's group keys), for splicing
#' into the block's `dplyr::filter()` expr so downstream gets that group's
#' member rows.
#' @noRd
dd_group_filter_call <- function(cols, vals) {
  conds <- Map(function(cl, vl) bquote(.data[[.(cl)]] == .(vl)),
               as.character(cols), as.character(vals))
  Reduce(function(a, b) bquote(.(a) & .(b)), conds)
}

#' A nested records table's drill as a filter on the input rows.
#'
#' table_nest() puts the k-th `by` column's value in `.group<k>_level`, so a
#' header click on `.group1_level = "Placebo"` filters `by[1] == "Placebo"`.
#' A row click adds its value in the drill column, which is a real column.
#' The "(Missing)" header (`missing = "level"`) claims the rows with no value.
#' @param by The `by` columns, outer first.
#' @param gc,gv The stored claim's columns and values.
#' @noRd
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
  # A row claims its groups and its value in the drill column, a real column.
  real <- is.na(k) & !startsWith(gc, ".")
  cols <- gc
  cols[grp] <- by[k[grp]]
  keep <- grp | real
  summarize_drill_expr(stats::setNames(as.list(gv[keep]), cols[keep]))
}

#' Say what a drill claim claims, in the source data's own words.
#'
#' Structured drill keys are identity columns, so the source terms are in the
#' VALUES: `.variable` holds the variable NAME and `.variable_level` its level,
#' which is what makes "AEDECOD = ABDOMINAL PAIN" out of a pair of dotted
#' columns. Enclosing `.group<k>_level` values come first as a breadcrumb. A
#' header click has group levels and no variable pair; an aggregated table
#' drills on real column names and falls through to `col = value`.
#'
#' Shared by the latched footer ("Filtered: ...") and the transient receipt
#' ("Drilled down to ..."), so one click cannot be described two ways.
#' @param gc,gv Aligned claim columns and values.
#' @param group_names The columns the `.group<k>` levels stand for, outer
#'   first (dt_group_names()). With them a header click reads "ARM = Placebo"
#'   rather than "Placebo".
#' @return A character vector of clause labels, outermost first.
#' @noRd
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

#' The column each `.group<k>` level of an annotated frame stands for, read
#' off the `.group<k>` column (table_nest() and summary_table() write the
#' column name there). Empty where the frame has none.
#' @noRd
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

#' The column half of a structured drill, as stored: a list of
#' `list(column = , values = )`, from the click or from a saved board (where
#' `values` may come back as a list). NULL when there is none.
#' @noRd
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

#' "ARM = Low dose" for each column claim.
#' @noRd
dd_col_keys_label <- function(ck) {
  ck <- dd_col_keys_state(ck)
  vapply(ck %||% list(), function(k) {
    paste0(k$column, " = ", paste(k$values, collapse = ", "))
  }, character(1L))
}

#' AND a column claim onto an expression that yields source rows:
#' `dplyr::filter(<ex>, ARM %in% "Low dose")`. "(Missing)" claims the rows
#' with no value (summarize_drill_expr()).
#' @noRd
dd_col_keys_filter <- function(ex, ck) {
  ck <- dd_col_keys_state(ck)
  if (!length(ck)) return(ex)
  f <- stats::setNames(lapply(ck, `[[`, "values"),
                       vapply(ck, `[[`, "", "column"))
  out <- summarize_drill_expr(f)
  out[[2L]] <- ex
  out
}

#' Serialize the block's active drill-filter state for the `data-dt-active`
#' table attribute (JSON lives only at this edge, like `summaries`). `active`
#' is `list(col, vals, gcols, gvals)` -- the filter reactiveVals as captured
#' at render time; returns `NULL` when no filter is active so the attribute
#' is simply absent.
#' @noRd
dd_active_filter_json <- function(active) {
  if (is.null(active)) return(NULL)
  gcols <- as.character(unlist(active$gcols %||% character()))
  gvals <- as.character(unlist(active$gvals %||% character()))
  ck <- dd_col_keys_state(active$ckeys)
  # The column half rides along: table.js marks that column.
  col_filters <- if (length(ck)) {
    lapply(ck, function(k) list(column = jsonlite::unbox(k$column),
                                values = k$values))
  }
  if (length(gcols) && length(gcols) == length(gvals)) {
    return(as.character(jsonlite::toJSON(
      c(list(filters = unname(Map(
        function(cl, vl) list(column = jsonlite::unbox(cl),
                              value = jsonlite::unbox(vl)),
        gcols, gvals
      ))), if (length(col_filters)) list(col_filters = col_filters))
    )))
  }
  if (length(col_filters)) {
    return(as.character(jsonlite::toJSON(
      list(filters = list(), col_filters = col_filters)
    )))
  }
  col  <- active$col
  vals <- as.character(unlist(active$vals %||% character()))
  if (is.null(col) || !length(vals)) return(NULL)
  as.character(jsonlite::toJSON(
    # auto_unbox = FALSE keeps the `values` vector an array even at length 1.
    list(column = jsonlite::unbox(as.character(col)[1L]), values = vals),
    auto_unbox = FALSE
  ))
}

#' Tag a `<table>` with the data-attributes the table JS reads off it. These
#' live on the table (not the container) so that re-rendering only the table on
#' a data/config change still carries current drill / colour / digits state to
#' the JS, while the chrome stays put.
#' @noRd
dt_table_attrs <- function(table_tag, onclick_col, onclick_idx,
                           digits, color = NULL, shadings = list(),
                           num_cols = NULL,
                           toggles = NULL, group_cols = NULL,
                           group = character(), summaries = list(),
                           active = NULL, gear_cols = NULL,
                           drill_mode = NULL) {
  on_off <- function(x) if (isTRUE(x)) "on" else "off"
  htmltools::tagAppendAttributes(
    table_tag,
    # The gear's pickable columns (raw input schema, dt_gear_cols_json) --
    # re-stamped on every table render so the popover re-reads CURRENT
    # columns when it opens.
    `data-dt-cols` = gear_cols,
    # Active drill filter at render time (JSON; NULL = none). table.js
    # matches rows by their data-raw values and re-applies the dt-row-active
    # highlight, so a restored board (or a config re-render) shows which row
    # drives the downstream filter.
    `data-dt-active` = dd_active_filter_json(active),
    # Current aggregation config, read back by the gear (group comma-joined,
    # summaries a JSON array of {func, cols} -- JSON lives only at this edge).
    `data-dt-group` = paste(group %||% character(), collapse = ","),
    `data-dt-summaries` = dd_summaries_json(summaries),
    # Grouped-table drill: the group columns whose per-row values a click ANDs
    # into a downstream filter (comma-joined; "" = not a grouped table).
    `data-dt-group-cols` = paste(group_cols %||% character(), collapse = ","),
    # The drill MODE for the picker-less variants (structured / aggregated),
    # where `drill` is not a column but a word: "auto" hands the clicked
    # display subset downstream, "source" the records behind it. Stamped so
    # the gear reads back what is actually set -- readGearState used to
    # report a flat "auto" for any structured drill, which would clobber the
    # mode on the next config write.
    `data-dt-drill-mode` = drill_mode,
    `data-dt-onclick-col` = onclick_col,
    `data-dt-onclick-idx` = if (!is.null(onclick_idx)) onclick_idx else NULL,
    # The number format and its unit (format / unit), read back by the gear.
    `data-dt-format` = as.character(digits),
    `data-dt-unit` = toggles$unit %||% "",
    # Categorical identity color ("Color by" -- row tint via the board scale
    # map; the chart's color aesthetic applied to rows) and the value-encoding
    # `shadings` rules ({mode, cols} JSON, only at this edge).
    `data-dt-color` = color %||% "",
    `data-dt-shadings` = dd_shadings_json(shadings),
    # Numeric columns the gear may offer for the shading scope pickers.
    `data-dt-num-cols` = paste(num_cols %||% character(), collapse = ","),
    # Display-option toggles the gear reads back (default ON, except export).
    `data-dt-sortable` = on_off(toggles$sortable %||% TRUE),
    `data-dt-collapsible` = on_off(toggles$collapsible %||% TRUE),
    `data-dt-search` = on_off(toggles$search %||% TRUE),
    `data-dt-download` = on_off(toggles$download %||% FALSE),
    `data-dt-wrap-titles` = on_off(toggles$wrap_titles %||% TRUE),
    # Rotated column titles (matrix tables): the attribute drives the CSS
    # (vertical value-column headers) exactly like wrap-titles above, and
    # the gear reads the current state off it.
    `data-dt-rotate-titles` = on_off(toggles$rotate_titles %||% FALSE)
  )
}

#' Is a download format's writer available? A seam (not an inline
#' requireNamespace) so tests can mock the package-missing state -- see the
#' dt_download renderUI, which shows the entry disabled with the reason on it
#' rather than hiding it, because a toggle the user just switched on that
#' renders NOTHING reads as broken.
#'
#' Each format needs one Suggests-level package: `openxlsx` writes the
#' spreadsheet, `flextable` (with `officer`) typesets the slide. HTML needs
#' none -- the renderer is the package's own.
#'
#' Asked with `system.file()` rather than `requireNamespace()`, because this
#' runs when the CONTROL renders, not when a download is clicked, and
#' requireNamespace() LOADS what it finds: turning the PowerPoint pill on would
#' otherwise pull officer + flextable into the process (measured: 0.49s and
#' 53MB, once per process) purely to decide whether to grey out a menu entry.
#' The writers keep their own requireNamespace() guard, so an installed but
#' unloadable package still fails at click time with a sentence that says so --
#' this probe answers "installed?", not "loadable?", which is the question the
#' menu is actually asking.
#' @noRd
dt_pkg_installed <- function(...) {
  all(vapply(c(...), function(p) nzchar(system.file(package = p)), logical(1L)))
}

#' @noRd
dt_has_openxlsx <- function() {
  dt_pkg_installed("openxlsx")
}

#' @noRd
dt_has_officer <- function() {
  dt_pkg_installed("officer", "flextable")
}

#' @noRd
drilldown_table_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    # Shared popover CSS (the dd-* section/row/segmented/add classes) lives in
    # chart.css; the table's gear popover reuses it. ONE definition
    # (chart-dep.R) so the cache-busting suffix cannot drift from the chart's.
    chart_css_dep(),
    # Shared aggregation vocabulary + gear engine (one dep, one version -- see
    # drilldown_shared_dep()). Before the table JS, which reads both globals.
    drilldown_shared_dep(),
    htmltools::htmlDependency(
      name = "blockr-viz-table",
      # Suffix bumped when the bundled table JS/CSS changes, to bust the
      # version-pinned asset cache (display-option gear toggles).
      version = paste0(utils::packageVersion("blockr.viz"), ".44"),
      src = system.file(package = "blockr.viz"),
      script = "js/table.js",
      stylesheet = "css/table.css"
    )
  )
})

# --- block -------------------------------------------------------------

#' The column a groups table starts with when it has none: a count of rows,
#' drawn as a bar. Without `by` that is one row, the number of input rows.
#' The gear opens it, so a new table shows what its first column computes.
#' @noRd
table_groups_seed <- function() {
  list(list(type = "simple", func = "count", show = "bar", name = "Count"))
}

#' Construction guidance for the drill-down table block
#' @noRd
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
                            # `...` this early on purpose: every argument
                            # after it matches by exact name only, so an old
                            # name such as `value` cannot partially match a
                            # new one such as `value_labels`.
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
  # blockr.core reads the starting state from this frame, by argument name.
  list2env(values, environment())

  res <- blockr.core::new_transform_block(
    server = function(id, data) table_server(id, data, values),
    ui = table_ui,
    # Contract check only (dispatch lookup): a composer table or anything
    # with an as_annotated_df() method connects directly.
    dat_valid = validate_annotated_df_input,
    allow_empty_state = viz_empty_ok(spec),
    external_ctrl = viz_ctrl_args(spec),
    expr_type = "bquoted",
    class = "table_block",
    ...
  )
  # blockr.core keeps `...` as attributes, and would save old names back into
  # every board.
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
