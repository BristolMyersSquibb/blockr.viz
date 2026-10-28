# LEGACY: old argument names of the table, summarize table and summary table
# blocks translated to new_table_block()'s, and the restores of the tile and
# heatmap blocks. Serves code that calls the constructor with old names,
# saved boards (blockr_deser.table_block()) and the old constructors, which
# forward (new_summarize_table_block()).

# Old names, by the block they belong to. Names that exist in both signatures
# with the same meaning (rowname, shadings, drill, title, ...) are not listed.
legacy_table_names <- function(from = c("table", "summarize", "summary")) {
  switch(
    match.arg(from),
    table = c(
      "value", "group", "cell_color", "row_color", "rotate_titles", "digits",
      "excel_download", "html_download", "pptx_download",
      "filter_type", "filter_column", "filter_values", "filter_range",
      "filter_group_cols", "filter_group_vals", "filter_spread_col",
      "filter_spread_from", "ctrl_target", "ctrl_table",
      # The matrix flag before `look`.
      "compact"
    ),
    summarize = c(
      "group", "value", "func", "id_var", "parent", "color", "bar_mode",
      "facet", "cols", "fields", "values", "ctrl_target", "ctrl_table",
      "filter_type", "filter_column", "filter_values",
      # A removed slot, always empty in saved boards.
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
legacy_is_agg_summaries <- function(summaries) {
  is.list(summaries) && length(summaries) > 0L &&
    all(vapply(summaries, function(s) {
      is.list(s) && is.null(s$type) && !is.null(s$func)
    }, logical(1L)))
}

# The old table's aggregation as typed summaries: one number column per
# function and column.
legacy_agg_to_summaries <- function(summaries) {
  out <- list()
  for (s in table_parse_summaries(summaries)) {
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
# `id_var`, which is the summary's column. The fields were drawn only beside
# the as-is measure, so under any other measure they map to nothing. The
# opt-in "%" column maps to nothing: the bar's own label carries the percent.
legacy_ranked_to_summaries <- function(value, func, color, facet, cols, fields,
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
legacy_table_args <- function(args, from = NULL) {
  if (!length(args)) return(args)
  nm <- names(args)
  if (is.null(from)) {
    if ("layout" %in% nm) return(args)
    from <- "table"
  }
  old <- legacy_table_names(from)
  if (!any(nm %in% c(old, "summaries"))) return(args)

  get <- function(x) args[[x]]
  drop <- function(out, x) out[setdiff(names(out), x)]
  out <- args

  if (identical(from, "summary")) {
    # Table 1 block: its variables become summaries, its split stays `by`.
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
    # A cell_color spec is one shadings rule, row_color the row tint.
    if (!length(out[["shadings"]]) && !is.null(get("cell_color"))) {
      out$shadings <- dd_parse_shadings(get("cell_color"))
    }
    if (is.null(out[["color"]]) && !is.null(get("row_color"))) {
      out$color <- get("row_color")
    }
    # Three per-format download toggles become one; a board without them
    # takes the default (on).
    per_format <- c("excel_download", "html_download", "pptx_download")
    if (any(!vapply(per_format, function(x) is.null(get(x)), logical(1L)))) {
      out$download <- any(vapply(per_format, function(x) isTRUE(get(x)),
                                 logical(1L)))
    }
    # The matrix flags (`rotate_titles`, `compact`) are the "compact" look.
    if (isTRUE(get("rotate_titles")) || isTRUE(get("compact"))) {
      out$look <- "compact"
    }
    # `digits` is the number format.
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
      out$summaries <- if (legacy_is_agg_summaries(out[["summaries"]])) {
        legacy_agg_to_summaries(out[["summaries"]])
      } else if (is.list(out[["summaries"]]) && length(out$summaries)) {
        out$summaries
      } else {
        list(list(type = "simple", func = "count", show = "number"))
      }
    } else if (legacy_is_agg_summaries(out[["summaries"]])) {
      # Aggregation without a group: one grand-total row, which is the
      # groups layout without `by`.
      out$layout <- "groups"
      out$summaries <- legacy_agg_to_summaries(out[["summaries"]])
    }
    sel <- list(
      column = chr_state(get("filter_column")),
      values = null_state(get("filter_values")),
      group_cols = chr_vec_state(get("filter_group_cols")),
      group_vals = null_state(get("filter_group_vals"))
    )
  } else {
    # The summarize table. With summaries, `group` / `parent` are the grouping
    # of a board saved before `by` existed, and the table-level colour and
    # facet fan down onto the summaries. Without summaries it is the ranked
    # bar, one bar summary: `parent` the outer `by`, `func` / `value` /
    # `id_var` the summary's, `bar_mode` its `position`.
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
        norm <- groups_norm_summaries(lapply(summaries, function(e) {
          if (!is.list(e)) return(e)
          legacy_map_values("summary", legacy_rename("summary", e))
        }))
        if (is.null(norm$err)) {
          mig <- groups_migrate_globals(norm, color, facet)
          for (i in seq_along(summaries)) {
            summaries[[i]]$color <- mig[[i]]$color
            summaries[[i]]$facet <- mig[[i]]$facet
          }
        }
      }
    } else {
      # Without a group the old bar drew nothing; the measure is one Total row.
      summaries <- legacy_ranked_to_summaries(
        chr_state(get("value")), chr_state(get("func")), color, facet,
        chr_vec_state(get("cols")), chr_vec_state(get("fields")),
        id_var = chr_state(get("id_var"))
      )
    }
    # The block-level split layout is each colour split's `position` now.
    # The old word ("stacked") is mapped by LEGACY_NAMES.
    pos <- chr_state(get("bar_mode"))
    if (!is.null(pos) && pos %in% GROUPS_POSITIONS) {
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

# Old table names, and an aggregation list where typed summaries go. A
# payload with `layout` can still carry old names, hence `from = "table"`.
legacy_table_before_030 <- function(state) {
  state <- legacy_table_args(state, from = "table")
  if (legacy_is_agg_summaries(state[["summaries"]])) {
    state$summaries <- legacy_agg_to_summaries(state$summaries)
  }
  # `values` was never a table argument; code passed it for `columns`.
  if (is.character(state[["values"]]) && is.null(state[["columns"]])) {
    state$columns <- state$values
  }
  state[["values"]] <- NULL
  state
}

# The table's old selections (`column`/`values`, `group_cols`/`group_vals`,
# `col_keys`) in the kit's shape, `filters` and `columns` (kit-input.R).
legacy_table_selection <- function(state) {
  s <- state[["selection"]]
  if (!is.list(s) || !length(s) || !is.null(s$filters) || !is.null(s$type)) {
    return(state)
  }
  gc <- as.character(unlist(s$group_cols))
  gv <- as.character(unlist(s$group_vals))
  filters <- if (length(gc) && length(gc) == length(gv)) {
    stats::setNames(as.list(gv), gc)
  } else {
    table_drill_filters(s$column, s$values)
  }
  ck <- table_col_keys_state(s$col_keys)
  columns <- if (length(ck)) {
    stats::setNames(lapply(ck, `[[`, "values"), vapply(ck, `[[`, "", "column"))
  }
  state["selection"] <- list(if (length(filters) || length(columns)) {
    list(type = "categorical", filters = filters %||% list(),
         columns = columns %||% list())
  })
  state
}

# 0.3.0's `drill = TRUE` (saved as "TRUE") is "auto"; FALSE is off.
legacy_table_drill <- function(x) {
  d <- x[["drill"]]
  if ((is.logical(d) || is.character(d)) && length(d) == 1L &&
        !is.na(d) && toupper(d) %in% c("TRUE", "FALSE")) {
    x["drill"] <- list(if (toupper(d) == "TRUE") "auto")
  }
  x
}

legacy_summary_entry <- function(e) {
  if (!is.list(e) || is.null(names(e))) {
    return(e)
  }
  e <- legacy_rename("summary", e)
  e <- legacy_map_values("summary", e)
  type <- e[["type"]] %||% "simple"
  if (identical(type, "sparkline")) {
    band <- as.character(unlist(e[["band"]]))
    if (length(band) == 2L) {
      e$ymin <- e$ymin %||% band[[1L]]
      e$ymax <- e$ymax %||% band[[2L]]
    }
    e[["band"]] <- NULL
    if (!is.null(e[["ref"]])) {
      ref <- as.character(unlist(e$ref))[[1L]]
      e$summary <- e$summary %||%
        (if (identical(ref, "median_iqr")) "median_q1_q3" else ref)
    }
  }
  if (identical(type, "pair") && !is.null(e[["ref"]])) {
    ref <- suppressWarnings(as.numeric(unlist(e$ref)))
    if (is.null(e[["x_lines"]]) && length(ref) && all(is.finite(ref))) {
      e$x_lines <- ref
    }
  }
  e[["ref"]] <- NULL
  # A reference range is text now, numbers or columns.
  for (k in intersect(c("ref_min", "ref_max"), names(e))) {
    if (is.numeric(e[[k]])) e[[k]] <- format(e[[k]], trim = TRUE)
  }
  if (identical(type, "dist") && !is.null(e[["style"]])) {
    style <- as.character(unlist(e$style))[[1L]]
    if (is.null(e[["show"]]) || !identical(e$show, "text")) {
      e$show <- if (identical(style, "box")) "box" else "pointrange"
    }
  }
  e[["style"]] <- NULL
  if (type %in% c("field", "sparkline", "spans", "pair", "expr")) {
    e[["show"]] <- NULL
  }
  legacy_drop("summary", e)
}

legacy_shading_rule <- function(r) {
  if (!is.list(r) || is.null(names(r))) {
    return(r)
  }
  legacy_drop("shading", legacy_rename("shading", r))
}

legacy_entries <- function(block, x) {
  if (!identical(block, "table")) {
    return(x)
  }
  if (is.list(x[["summaries"]])) {
    x$summaries <- lapply(x$summaries, legacy_summary_entry)
  }
  if (is.list(x[["shadings"]])) {
    x$shadings <- lapply(x$shadings, legacy_shading_rule)
  }
  x
}

# A tile that aggregated is groups in cards; a wide frame records in cards;
# the long frame plain records. What has no counterpart warns once.
legacy_restore_tile <- function(payload, label = "a tile") {
  p <- payload
  value <- chr_vec_state(p[["value"]]) %||% character()
  group <- chr_vec_state(p[["group"]] %||% p[["by"]]) %||% character()
  group <- group[nzchar(group)]
  name <- chr_state(p[["name"]])
  summaries <- p[["summaries"]]
  filter_column <- chr_vec_state(p[["filter_column"]] %||% p[["filter_col"]])
  filter_values <- null_state(p[["filter_values"]] %||% p[["filter_value"]])
  set <- function(nm) {
    keep <- vapply(nm, function(k) {
      v <- p[[k]]
      !is.null(v) && length(v) && !identical(v, "") &&
        !identical(v, "plain") && !identical(v, "number") &&
        !identical(v, "up") && !identical(v, "cards")
    }, logical(1L))
    nm[keep]
  }
  if (legacy_is_agg_summaries(summaries)) {
    args <- list(
      layout = "groups", by = group, look = "cards",
      summaries = legacy_agg_to_summaries(summaries),
      drill = if (isTRUE(p[["drill"]]) && length(group)) group[[1L]],
      drill_target = chr_state(p[["ctrl_target"]]) %||% "",
      drill_table = chr_state(p[["ctrl_table"]]) %||% "",
      selection = if (length(filter_column) && length(filter_values)) {
        list(column = filter_column, values = filter_values)
      }
    )
    lost <- set(c("format", "unit", "secondary", "caption", "overline"))
  } else if (is.null(name) && length(value)) {
    args <- list(layout = "records", look = "cards", columns = value,
                 rowname = if (length(group)) group[[1L]])
    lost <- set(c("format", "unit", "secondary", "style", "caption",
                  "overline", "drill"))
  } else {
    args <- list(
      layout = "records",
      rowname = name %||% (if (length(group)) group[[1L]]),
      columns = unique(c(group, value, chr_state(p[["secondary"]])))
    )
    lost <- c("the long tile frame",
              set(c("format", "unit", "style", "caption", "overline",
                    "drill")))
  }
  if (length(lost)) {
    warning("The tile block is gone: ", label, " restores as a table ",
            "block. Not carried over: ", paste(lost, collapse = ", "), ".",
            call. = FALSE)
  }
  legacy_restore("table", args, version = NULL)  # unset `color` is "auto"
}

# new_heatmap_block() (< 0.3.0): groups, compact, one tile count per `col`
# painted by the worst `color`; a row click drilled, so the drill stays on.
legacy_restore_heatmap <- function(payload, label = "a heatmap") {
  p <- payload
  row <- chr_state(p[["row"]])
  col <- chr_state(p[["col"]])
  fill <- chr_state(p[["color"]])
  group <- chr_state(p[["group"]])
  values <- if (is.list(p[["values"]])) p[["values"]] else list()
  if (!is.null(p[["top_n"]]) && is.null(values$top_n)) {
    values$top_n <- as.integer(unlist(p[["top_n"]]))[[1L]]
  }
  summary <- c(
    list(type = "simple", func = "count", show = "tile", name = "Events"),
    if (!is.null(col)) list(facet = col),
    if (!is.null(fill)) list(fill = fill)
  )
  slots <- c(row = "by", col = "facet", color = "fill")
  sentence <- function(tpl) legacy_slots(tpl, slots)
  args <- list(
    layout = "groups", look = "compact", by = c(group, row),
    summaries = list(summary),
    value_labels = !isFALSE(unlist(p[["cell_numbers"]])),
    drill = "auto",
    drill_target = chr_state(p[["ctrl_target"]]) %||% "auto",
    drill_table = chr_state(p[["ctrl_table"]]) %||% "",
    download = isTRUE(unlist(p[["download"]])),
    title = sentence(p[["title"]]), subtitle = sentence(p[["subtitle"]]),
    caption = sentence(p[["caption"]]),
    script = p[["script"]], script_values = values
  )
  warning("new_heatmap_block() is gone: ", label, " restores as a table ",
          "block (groups layout, compact look). See ?new_table_block.",
          call. = FALSE)
  legacy_restore("table", args, version = VIZ_REWRITE_VERSION)
}

# A summary table block, flat or in the interim single-`state` form, as the
# variables layout.
legacy_restore_summary <- function(payload, version = NULL) {
  if (is.list(payload[["state"]])) {
    payload <- c(payload[["state"]], payload[setdiff(names(payload), "state")])
  }
  args <- legacy_table_args(payload, from = "summary")
  legacy_restore("table", args, version = version)
}

LEGACY_TILE_ARGS <- c("value", "group", "by", "name", "summaries", "color", "layout",
               "overline", "caption", "secondary", "style", "good_when",
               "format", "unit", "drill", "filter_column", "filter_values",
               "filter_col", "filter_value", "ctrl_target", "ctrl_table",
               "measures", "showcase", "state")
