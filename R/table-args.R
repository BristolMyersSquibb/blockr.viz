# Old argument names of the table, summarize table and summary table blocks,
# translated to new_table_block()'s. One table serves code that calls the
# constructor with old names, boards saved before the rewrite
# (blockr_deser.table_block()) and the old constructors, which now forward
# (new_summarize_table_block()). See dev/table-block-rewrite.md.

# Old names, by the block they belong to. Names that exist in both the old
# and the new signature with the same meaning (rowname, shadings, drill,
# digits, title, ...) are not listed: they need no translation.
table_legacy_names <- function(from = c("table", "summarize", "summary")) {
  switch(
    match.arg(from),
    table = c(
      "value", "group", "cell_color", "row_color", "rotate_titles", "digits",
      "excel_download", "html_download", "pptx_download",
      "filter_type", "filter_column", "filter_values", "filter_range",
      "filter_group_cols", "filter_group_vals", "filter_spread_col",
      "filter_spread_from", "ctrl_target", "ctrl_table",
      # The matrix flag before `look` (feat/table-rewrite boards).
      "compact"
    ),
    summarize = c(
      "group", "value", "func", "id_var", "parent", "color", "bar_mode",
      "facet", "cols", "fields", "values", "ctrl_target", "ctrl_table",
      "filter_type", "filter_column", "filter_values",
      # The facet comparison went in August 2026 (87616f6); a board saved
      # before carries the slot, always empty.
      "compare"
    ),
    summary = c(
      "vars", "add_overall", "overall_label", "indent_details",
      "nest_hierarchies", "id_var"
    )
  )
}

# Is `summaries` the old table block's in-block aggregation, a list of
# list(func, cols), rather than the typed summary list (list(type, ...))?
table_is_agg_summaries <- function(summaries) {
  is.list(summaries) && length(summaries) > 0L &&
    all(vapply(summaries, function(s) {
      is.list(s) && is.null(s$type) && !is.null(s$func)
    }, logical(1L)))
}

# The old table's aggregation as typed summaries: one number column per
# function and column, as the old table drew them.
table_agg_to_summaries <- function(summaries) {
  out <- list()
  for (s in dd_parse_summaries(summaries)) {
    cols <- as.character(unlist(s$cols))
    if (!length(cols) || identical(s$func, "count")) cols <- NA_character_
    for (cl in cols) {
      out[[length(out) + 1L]] <- c(
        list(type = "simple", func = s$func),
        if (!is.na(cl)) list(col = cl),
        list(show = "number")
      )
    }
  }
  out
}

# The old summarize table's ranked bar (no `summaries`) as typed summaries:
# the measure as one bar column with its colour split and facet, plus the
# opt-in count column and the extra fields. A distinct count counted
# `id_var`, which is the summary's column now. The fields were drawn only
# beside the as-is measure (one underlying row per group); under any other
# measure the bar ignored them, so they map to nothing. The opt-in "%"
# column maps to nothing either: the bar's own label carries the percent.
table_ranked_to_summaries <- function(value, func, color, facet, cols, fields,
                                      id_var = NULL) {
  func <- func %||% "count"
  if (!func %in% c("count", "count_distinct", "sum", "mean", "median", "min",
                   "max", "identity")) {
    func <- "count"
  }
  value <- if (length(value) && !identical(value, ".count")) value
  col <- switch(func,
    count = NULL,
    count_distinct = id_var %||% value,
    value
  )
  bar <- c(
    list(type = "simple", func = func),
    if (length(col)) list(col = col),
    list(show = "bar"),
    if (length(color)) list(color = color),
    if (length(facet)) list(facet = facet)
  )
  extra_n <- if ("n" %in% cols && !identical(func, "count")) {
    list(list(type = "simple", func = "count", show = "number"))
  }
  extra_fields <- if (identical(func, "identity")) {
    lapply(as.character(fields %||% character()), function(f) {
      list(type = "field", col = f)
    })
  }
  c(list(bar), extra_n, extra_fields)
}

# Translate old names in `args` (a named list) to new_table_block()'s. `from`
# says which block the names come from; NULL guesses from the names (a saved
# new_table_block payload carries `layout`, an old one does not). Names that
# are already new pass through; old ones are removed.
table_legacy_args <- function(args, from = NULL) {
  if (!length(args)) return(args)
  nm <- names(args)
  if (is.null(from)) {
    if ("layout" %in% nm) return(args)
    from <- "table"
  }
  old <- table_legacy_names(from)
  if (!any(nm %in% c(old, "summaries"))) return(args)

  get <- function(x) args[[x]]
  drop <- function(out, x) out[setdiff(names(out), x)]
  out <- args

  if (identical(from, "summary")) {
    # The summary table (Table 1) block: its variables become summaries,
    # its column split stays `by`.
    out$layout <- "variables"
    out$summaries <- table_vars_to_summaries(
      list(), chr_vec_state(get("vars")) %||% character()
    )
    if (!is.null(get("add_overall"))) out$total <- isTRUE(get("add_overall"))
    if (!is.null(get("overall_label"))) out$total_label <- get("overall_label")
    if (!is.null(get("nest_hierarchies"))) {
      out$nest <- isTRUE(get("nest_hierarchies"))
    }
    if (is.null(out[["id"]]) && !is.null(get("id_var"))) out$id <- get("id_var")
    return(drop(out, old))
  }

  if (identical(from, "table")) {
    # LEGACY colour args: a cell_color spec is one shadings rule, row_color
    # is the row tint.
    if (!length(out[["shadings"]]) && !is.null(get("cell_color"))) {
      out$shadings <- dd_parse_shadings(get("cell_color"))
    }
    if (is.null(out[["color"]]) && !is.null(get("row_color"))) {
      out$color <- get("row_color")
    }
    # Downloads were three per-format toggles before they became one. A
    # board that stored them keeps its choice; one without them takes the
    # default (on).
    per_format <- c("excel_download", "html_download", "pptx_download")
    if (any(!vapply(per_format, function(x) is.null(get(x)), logical(1L)))) {
      out$download <- any(vapply(per_format, function(x) isTRUE(get(x)),
                                 logical(1L)))
    }
    # The matrix look was a flag (`rotate_titles`, then `compact`) before it
    # became one of the three looks.
    if (isTRUE(get("rotate_titles")) || isTRUE(get("compact"))) {
      out$look <- "compact"
    }
    # `digits` is the number format now.
    if (!is.null(get("digits"))) out$format <- table_digits_format(get("digits"))
    if (!is.null(get("value")) && is.null(out[["columns"]])) {
      out$columns <- get("value")
    }
    group <- chr_vec_state(get("group"))
    if (length(group)) {
      out$layout <- "groups"
      out$by <- group
      # An aggregation list becomes summaries; summaries already in the
      # groups layout's form are kept; none is one count.
      out$summaries <- if (table_is_agg_summaries(out[["summaries"]])) {
        table_agg_to_summaries(out[["summaries"]])
      } else if (is.list(out[["summaries"]]) && length(out$summaries)) {
        out$summaries
      } else {
        list(list(type = "simple", func = "count", show = "number"))
      }
    } else if (table_is_agg_summaries(out[["summaries"]])) {
      # Aggregation without a group: one grand-total row, which is the
      # groups layout without `by`.
      out$layout <- "groups"
      out$summaries <- table_agg_to_summaries(out[["summaries"]])
    }
    sel <- list(
      column = chr_state(get("filter_column")),
      values = null_state(get("filter_values")),
      group_cols = chr_vec_state(get("filter_group_cols")),
      group_vals = null_state(get("filter_group_vals"))
    )
  } else {
    # The summarize table. With summaries, `group` / `parent` are the
    # grouping of a board saved before `by` existed; the table-level colour
    # and facet fan down onto the summaries. Without summaries it is the
    # ranked bar, which is one bar summary now: `parent` the outer `by`,
    # `func` / `value` / `id_var` the summary's, `bar_mode` its `position`.
    summaries <- if (is.list(out[["summaries"]])) out[["summaries"]] else list()
    by <- chr_vec_state(out[["by"]])
    parent <- chr_state(get("parent"))
    group <- chr_state(get("group"))
    color <- chr_state(get("color"))
    facet <- chr_state(get("facet"))
    if (!length(by)) by <- unique(c(parent, group))
    if (length(summaries)) {
      if (!is.null(color) || !is.null(facet)) {
        # Old keys renamed, `scope` kept: a pooled row takes no facet.
        norm <- lane_norm_summaries(lapply(summaries, function(e) {
          if (!is.list(e)) return(e)
          legacy_map_values("summary", legacy_rename("summary", e))
        }))
        if (is.null(norm$err)) {
          mig <- lane_migrate_globals(norm, color, facet)
          for (i in seq_along(summaries)) {
            summaries[[i]]$color <- mig[[i]]$color
            summaries[[i]]$facet <- mig[[i]]$facet
          }
        }
      }
    } else {
      # Without a group the bar drew nothing; its measure now draws one
      # Total row.
      summaries <- table_ranked_to_summaries(
        chr_state(get("value")), chr_state(get("func")), color, facet,
        chr_vec_state(get("cols")), chr_vec_state(get("fields")),
        id_var = chr_state(get("id_var"))
      )
    }
    # The block-level split layout is each colour split's `position` now.
    # The old word ("stacked") is mapped by LEGACY_NAMES.
    pos <- chr_state(get("bar_mode"))
    if (!is.null(pos) && pos %in% LANE_POSITIONS) {
      summaries <- lapply(summaries, function(x) {
        if (is.list(x) && identical(x[["type"]], "simple") &&
              !is.null(x[["color"]]) && is.null(x[["position"]])) {
          x[["position"]] <- pos
        }
        x
      })
    }
    out$layout <- "groups"
    out$by <- by
    out$summaries <- summaries
    if (is.null(out[["id"]]) && !is.null(get("id_var"))) out$id <- get("id_var")
    if (!length(out[["script_values"]]) && length(get("values"))) {
      out$script_values <- get("values")
    }
    sel <- list(
      column = chr_vec_state(get("filter_column")),
      values = null_state(get("filter_values"))
    )
  }

  if (!nzchar(out[["drill_target"]] %||% "") && !is.null(get("ctrl_target"))) {
    out$drill_target <- get("ctrl_target")
  }
  if (!nzchar(out[["drill_table"]] %||% "") && !is.null(get("ctrl_table"))) {
    out$drill_table <- get("ctrl_table")
  }
  sel <- sel[!vapply(sel, function(x) is.null(x) || !length(x), logical(1L))]
  if (is.null(out[["selection"]]) && length(sel)) out$selection <- sel

  drop(out, old)
}

#' @rdname blockr_deser.chart_block
#' @export
blockr_deser.table_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))
  res <- legacy_deser("table_block", data)
  do.call(
    new_table_block,
    c(res$args, res$attrs,
      list(ctor = "new_table_block", ctor_pkg = "blockr.viz"))
  )
}

#' @rdname blockr_deser.chart_block
#' @export
blockr_deser.summarize_table_block <- function(x, data, ...) {
  stopifnot("payload" %in% names(data))
  saved <- data[["payload"]]
  is_attr <- !names(saved) %in% legacy_arg_names("summarize_table")
  payload <- table_legacy_args(saved[!is_attr], from = "summarize")
  payload$layout <- "groups"
  attrs <- saved[is_attr]
  args <- legacy_restore("table", payload,
                         version = data[["constructor"]][["version"]])
  do.call(
    new_table_block,
    c(args, attrs, list(ctor = "new_table_block", ctor_pkg = "blockr.viz"))
  )
}
