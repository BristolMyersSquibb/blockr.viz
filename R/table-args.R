# Old argument names of the table, summarize table and summary table blocks,
# translated to new_table_block()'s. One table serves code that calls the
# constructor with old names, boards saved before the rewrite
# (blockr_deser.table_block()) and the old constructors, which now forward
# (new_summarize_table_block()). See dev/table-block-rewrite.md.

# Arguments of new_table_block() that hold the block's settings. Everything
# else in `...` goes to blockr.core::new_transform_block() (block_name, ctor,
# ...), or is an old name translated here.
table_formal_names <- function() {
  setdiff(names(formals(new_table_block)), "...")
}

# Old names, by the block they belong to. Names that exist in both the old
# and the new signature with the same meaning (rowname, shadings, drill,
# digits, title, ...) are not listed: they need no translation.
table_legacy_names <- function(from = c("table", "summarize")) {
  switch(
    match.arg(from),
    table = c(
      "value", "group", "cell_color", "row_color", "rotate_titles",
      "excel_download", "html_download", "pptx_download",
      "filter_type", "filter_column", "filter_values", "filter_range",
      "filter_group_cols", "filter_group_vals", "filter_spread_col",
      "filter_spread_from", "ctrl_target", "ctrl_table"
    ),
    summarize = c(
      "group", "value", "func", "id_var", "parent", "color", "bar_mode",
      "facet", "cols", "fields", "values", "ctrl_target", "ctrl_table",
      "filter_type", "filter_column", "filter_values"
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

# The old summarize table's ranked-bar surface (no `summaries`) as typed
# summaries: the measure as one bar column with its colour split, plus the
# opt-in count column and the extra fields.
table_ranked_to_summaries <- function(value, func, color, facet, cols, fields) {
  func <- func %||% "count"
  bar <- c(
    list(type = "simple", func = func),
    if (!identical(func, "count") && length(value) &&
          !identical(value, ".count")) list(col = value),
    list(show = "bar"),
    if (length(color)) list(color = color),
    if (length(facet)) list(facet = facet)
  )
  extra_n <- if ("n" %in% cols && !identical(func, "count")) {
    list(list(type = "simple", func = "count", show = "number"))
  }
  extra_fields <- lapply(as.character(fields %||% character()), function(f) {
    list(type = "field", col = f)
  })
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

  if (identical(from, "table")) {
    # LEGACY colour args: a cell_color spec is one shadings rule, row_color
    # is the row tint.
    if (!length(out[["shadings"]]) && !is.null(get("cell_color"))) {
      out$shadings <- dd_parse_shadings(get("cell_color"))
    }
    if (is.null(out[["color"]]) && !is.null(get("row_color"))) {
      out$color <- get("row_color")
    }
    # Downloads were three per-format toggles before they became one.
    if (isTRUE(get("excel_download")) || isTRUE(get("html_download")) ||
          isTRUE(get("pptx_download"))) {
      out$download <- TRUE
    }
    if (isTRUE(get("rotate_titles"))) out$compact <- TRUE
    if (!is.null(get("value")) && is.null(out[["columns"]])) {
      out$columns <- get("value")
    }
    group <- chr_vec_state(get("group"))
    if (length(group)) {
      out$layout <- "groups"
      out$by <- group
      out$summaries <- if (table_is_agg_summaries(out[["summaries"]])) {
        table_agg_to_summaries(out[["summaries"]])
      } else {
        list(list(type = "simple", func = "count", show = "number"))
      }
    } else if (table_is_agg_summaries(out[["summaries"]])) {
      # Aggregation without a group: one grand-total row. The groups layout
      # needs a group, so the totals show as the input rows instead.
      out$summaries <- list()
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
    # ranked-bar surface, which is one bar summary now.
    summaries <- if (is.list(out[["summaries"]])) out[["summaries"]] else list()
    by <- chr_vec_state(out[["by"]])
    parent <- chr_state(get("parent"))
    group <- chr_state(get("group"))
    color <- chr_state(get("color"))
    facet <- chr_state(get("facet"))
    if (!length(by)) by <- unique(c(parent, group))
    if (length(summaries)) {
      if (!is.null(color) || !is.null(facet)) {
        norm <- lane_norm_summaries(summaries)
        if (is.null(norm$err)) {
          mig <- lane_migrate_globals(norm, color, facet)
          for (i in seq_along(summaries)) {
            summaries[[i]]$color <- mig[[i]]$color
            summaries[[i]]$facet <- mig[[i]]$facet
          }
        }
      }
    } else if (length(by)) {
      summaries <- table_ranked_to_summaries(
        chr_state(get("value")), chr_state(get("func")), color, facet,
        chr_vec_state(get("cols")), chr_vec_state(get("fields"))
      )
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

#' Legacy deserialization for the table block
#'
#' Restores boards saved before the table block's rewrite, whose state
#' carries the old argument names (`cell_color`, `value`, `group`, the
#' `filter_*` fields, ...). The payload is translated by
#' `table_legacy_args()`, the same table the constructor uses for old names
#' in code. Current payloads (they carry `layout`) pass straight through.
#'
#' @param x,data,... Passed through from [blockr.core::blockr_deser()].
#' @keywords internal
#' @export
blockr_deser.table_block <- function(x, data, ...) {
  stopifnot(all(c("constructor", "payload") %in% names(data)))
  payload <- table_legacy_args(data[["payload"]])
  ctor <- blockr.core::blockr_deser(data[["constructor"]])
  args <- c(
    payload,
    list(
      ctor = blockr.core::coal(
        blockr.core::ctor_name(ctor),
        blockr.core::ctor_fun(ctor)
      ),
      ctor_pkg = blockr.core::ctor_pkg(ctor)
    )
  )
  do.call(blockr.core::ctor_fun(ctor), args)
}

#' Legacy deserialization for the summarize table block
#'
#' A board saved with a summarize table restores as a table block in the
#' groups layout. Its class changes (`summarize_table_block` becomes
#' `table_block`), which blockr.core accepts since it checks only that the
#' result is a block (BristolMyersSquibb/blockr.core#379). The board saves
#' as a table block from then on.
#'
#' @inheritParams blockr_deser.table_block
#' @keywords internal
#' @export
blockr_deser.summarize_table_block <- function(x, data, ...) {
  stopifnot("payload" %in% names(data))
  payload <- table_legacy_args(data[["payload"]], from = "summarize")
  do.call(
    new_table_block,
    c(payload, list(ctor = "new_table_block", ctor_pkg = "blockr.viz"))
  )
}
