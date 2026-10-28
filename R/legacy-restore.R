# LEGACY: delete with this file and R/legacy.R.
#
# Restore -------------------------------------------------------------------
#
# legacy_restore() takes a saved state (a payload, or a constructor call's
# arguments) in any older vocabulary and returns the block's arguments under
# the current names, healed through the spec. In order:
#
#   1. names from before 0.3.0, through the reference translators that still
#      live in R/chart-args.R and R/table-args.R (chart_legacy_args(),
#      table_legacy_args(); they move here when those files go);
#   2. the renames and value maps of LEGACY_NAMES;
#   3. the rules a map cannot say (orientation-free ones only; see
#      legacy_values());
#   4. summary entries and shading rules, by their own rows;
#   5. title templates, so a re-saved board carries only new slot names;
#   6. names dropped on purpose, without a warning;
#   7. viz_heal(): any other unknown name or bad value warns and heals.

# LEGACY: delete with this file
# Boards saved by the rewritten blocks carry at least this version. It sets
# the one value whose meaning changed under the same name: the table's
# `color = NULL` meant "auto" before. The DESCRIPTION version has to reach it
# before the rewrite saves boards.
VIZ_REWRITE_VERSION <- "0.3.0.9000"

# LEGACY: delete with this file
#' Restore a saved block state under the current argument names
#'
#' @param block `"chart"` or `"table"`.
#' @param state A named list: a saved payload's state, or a call's arguments.
#' @param version The blockr.viz version that saved it, as text; `NULL` when
#'   unknown, which is read as older than the rewrite.
#' @return A named list with every argument of the block's spec.
#' @noRd
legacy_restore <- function(block, state, version = NULL) {
  stopifnot(block %in% c("chart", "table"), is.list(state))
  state <- as.list(state)
  state <- switch(
    block,
    chart = legacy_chart_selection(chart_legacy_args(state)),
    table = legacy_table_before_030(state)
  )
  state <- legacy_rename(block, state)
  state <- legacy_map_values(block, state)
  state <- legacy_values(block, state, version)
  if (identical(block, "table")) state <- legacy_table_selection(state)
  state <- legacy_entries(block, state)
  for (k in intersect(c("title", "subtitle", "caption"), names(state))) {
    state[k] <- list(legacy_template(block, state[[k]], state))
  }
  state <- legacy_drop(block, state)
  viz_heal(viz_block_spec(block), state)
}

# LEGACY: delete with this file
# The constructor's hook. Old names and old values in code are translated
# with one warning that names them. Values the spec refuses are left for
# viz_check(): in code they are errors, not healed.
legacy_ctor_args <- function(block, args) {
  named <- switch(
    block,
    chart = legacy_chart_selection(chart_legacy_args(args)),
    table = legacy_table_selection(legacy_table_before_030(args))
  )
  named <- legacy_rename(block, named)
  out <- legacy_drop(block, legacy_entries(block, legacy_map_values(block,
                                                                    named)))
  if (is.character(out[["script"]]) && length(out$script) > 1L) {
    out$script <- paste(out$script, collapse = "\n")
  }
  # The old names, and the current names given an old value. A name that is
  # neither is left to viz_check(), which says it is unknown.
  current <- names(viz_block_spec(block)$args)
  known <- LEGACY_NAMES$old[LEGACY_NAMES$block == block]
  renamed <- intersect(setdiff(names(args), current), known)
  old <- union(renamed, Filter(function(nm) {
    nm %in% names(args) && !identical(named[[nm]], out[[nm]]) &&
      !identical(nm, "script")
  }, names(out)))
  labels <- "direct_labels" %in% old
  old <- setdiff(old, "direct_labels")
  if (length(old)) {
    warning(
      "new_", block, "_block(): ", paste0("`", old, "`", collapse = ", "),
      if (length(old) > 1L) " are" else " is",
      " from blockr.viz < 0.3.0. See ?new_", block, "_block for the ",
      "current names.",
      call. = FALSE
    )
  }
  if (labels) {
    warning("new_", block, "_block(): `direct_labels = TRUE` is now ",
            "`legend = \"marks\"`.", call. = FALSE)
  }
  out
}

# LEGACY: delete with this file
# The chart's selection as the reference saved it, `{type, filters, range,
# point}`, in the shape the kit reads (kit-input.R): categorical, or a range;
# a point becomes a range with equal ends.
legacy_chart_selection <- function(args) {
  s <- args[["selection"]]
  if (!is.list(s) || !length(s)) {
    return(args)
  }
  type <- as.character(unlist(s$type))[1L] %|NA|% "categorical"
  old <- any(c("range", "point") %in% names(s)) ||
    (identical(type, "categorical") && !"columns" %in% names(s))
  if (!old) {
    return(args)
  }
  r <- s$range
  p <- s$point
  args["selection"] <- list(if (identical(type, "range") && !is.null(r)) {
    list(type = "range", x = r$x_col, y = r$y_col,
         x_range = as.numeric(unlist(r$x_range)),
         y_range = if (!is.null(r$y_range)) as.numeric(unlist(r$y_range)),
         filters = s$filters %||% list())
  } else if (identical(type, "point") && !is.null(p)) {
    list(type = "range", x = p$x_col, y = p$y_col,
         x_range = rep(as.numeric(p$x_val), 2L),
         y_range = rep(as.numeric(p$y_val), 2L), filters = list())
  } else if (length(s$filters)) {
    list(type = "categorical", filters = s$filters, columns = list())
  })
  args
}

# LEGACY: delete with this file
# The table payloads before the rewrite of the table block: old table
# names, an aggregation list where the typed summaries go.
legacy_table_before_030 <- function(state) {
  # `from = "table"`: a payload with `layout` can still carry old names
  # (`compact`, `digits`, the download toggles).
  state <- table_legacy_args(state, from = "table")
  if (table_is_agg_summaries(state[["summaries"]])) {
    state$summaries <- table_agg_to_summaries(state$summaries)
  }
  # `values` was never a table argument; code passed it for `columns`.
  if (is.character(state[["values"]]) && is.null(state[["columns"]])) {
    state$columns <- state$values
  }
  state[["values"]] <- NULL
  state
}

# LEGACY: delete with this file
# The plain rows of a block: renames without a rule, and the logical ones.
legacy_rows <- function(block, renamed = TRUE) {
  rows <- LEGACY_NAMES[LEGACY_NAMES$block == block & !is.na(LEGACY_NAMES$new) &
                         (is.na(LEGACY_NAMES$rule) |
                            LEGACY_NAMES$rule == "to logical"), ]
  if (renamed) rows[rows$old != rows$new, ] else rows
}

# LEGACY: delete with this file
# Old names become new ones; a value already under the new name wins.
legacy_rename <- function(block, x) {
  rows <- legacy_rows(block)
  for (i in seq_len(nrow(rows))) {
    old <- rows$old[[i]]
    new <- rows$new[[i]]
    if (!old %in% names(x)) next
    if (is.null(x[[new]])) x[new] <- list(x[[old]])
    x[[old]] <- NULL
  }
  x
}

# LEGACY: delete with this file
# Old values under the current names: "stacked" is "stack", "off" is "none".
legacy_map_values <- function(block, x) {
  rows <- legacy_rows(block, renamed = FALSE)
  rows <- rows[lengths(rows$values) > 0L, ]
  for (i in seq_len(nrow(rows))) {
    nm <- rows$new[[i]]
    v <- x[[nm]]
    if (!is.character(v) || !length(v)) next
    map <- rows$values[[i]]
    hit <- v %in% names(map)
    v[hit] <- unname(map[v[hit]])
    if (identical(rows$rule[[i]], "to logical")) {
      v <- as.logical(v)
    }
    x[nm] <- list(v)
  }
  x
}

# LEGACY: delete with this file
# Rules a value map cannot say.
legacy_values <- function(block, x, version) {
  # A script arrived as a vector of lines.
  if (is.character(x[["script"]]) && length(x$script) > 1L) {
    x$script <- paste(x$script, collapse = "\n")
  }
  if (identical(block, "chart")) {
    # The reference took several roles; the spec takes one.
    if (length(x[["percent_of"]]) > 1L) {
      x$percent_of <- x$percent_of[[1L]]
    }
    # The reference saved panel columns as text.
    fc <- x[["facet_cols"]]
    if (is.character(fc)) {
      n <- suppressWarnings(as.numeric(fc[[1L]]))
      x["facet_cols"] <- list(if (isTRUE(n == round(n) && n >= 1)) {
        as.integer(n)
      })
    }
  }
  if (identical(block, "table")) {
    if (legacy_before_rewrite(version)) {
      # An emptied slot from a pre-#144 paste (list()) is unset too.
      if (is.list(x[["color"]]) && !length(x$color)) x["color"] <- list(NULL)
      x["color"] <- list(if (is.null(x[["color"]])) {
        "auto"
      } else if (!identical(x[["color"]], "")) {
        x[["color"]]
      })
    }
    st <- x[["stats"]]
    if (is.character(st) || is.list(st)) {
      st <- as.character(unlist(st))
      st[st == "compact"] <- "mean_sd"
      if ("expanded" %in% st) {
        st <- unique(c(setdiff(st, "expanded"), "n", "mean", "sd", "median",
                       "q1_q3", "min_max"))
      }
      x$stats <- st
    }
  }
  x
}

# LEGACY: delete with this file
legacy_before_rewrite <- function(version) {
  v <- as.character(unlist(version))
  if (!length(v) || is.na(v[[1L]]) || !nzchar(v[[1L]])) {
    return(TRUE)
  }
  ok <- tryCatch(package_version(v[[1L]]), error = function(e) NULL)
  is.null(ok) || ok < package_version(VIZ_REWRITE_VERSION)
}

# LEGACY: delete with this file
# One summary entry, under the public keys.
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
  # A reference range was numbers or columns; it is text now, either way.
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
  # One mark each: the type fixes it.
  if (type %in% c("field", "sparkline", "spans", "pair", "expr")) {
    e[["show"]] <- NULL
  }
  dropped <- LEGACY_NAMES$old[LEGACY_NAMES$block == "summary" &
                                is.na(LEGACY_NAMES$new)]
  e[setdiff(names(e), dropped)]
}

# LEGACY: delete with this file
legacy_shading_rule <- function(r) {
  if (!is.list(r) || is.null(names(r))) {
    return(r)
  }
  r <- legacy_rename("shading", r)
  dropped <- LEGACY_NAMES$old[LEGACY_NAMES$block == "shading" &
                                is.na(LEGACY_NAMES$new)]
  r[setdiff(names(r), dropped)]
}

# LEGACY: delete with this file
# Names read and dropped on purpose: no warning for them.
# The table's summary entries and shading rules under their current keys.
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

legacy_drop <- function(block, x) {
  dropped <- LEGACY_NAMES$old[LEGACY_NAMES$block == block &
                                is.na(LEGACY_NAMES$new)]
  x[setdiff(names(x), dropped)]
}

# Title templates ------------------------------------------------------------

# LEGACY: delete with this file
# Slot names a saved template may carry, old name = new name. The chart's
# `group` and `value` depend on the orientation and are added per state.
legacy_slot_names <- function(block, state) {
  if (identical(block, "table")) {
    # `value`, `func`, `color`, `facet` and `fill` named the first summary's
    # keys in the groups layout; the spec has no slot for them, so they stay
    # and print nothing.
    return(c(group = "by", parent = "by", id_var = "id",
             pct_of = "percent_of", axis = "column_axis"))
  }
  rows <- legacy_rows("chart")
  out <- stats::setNames(rows$new, rows$old)
  horizontal <- legacy_chart_horizontal(state)
  c(out[!names(out) %in% c("group", "value", "metric")],
    group = if (horizontal) "y" else "x",
    value = if (horizontal) "x" else "y")
}

# LEGACY: delete with this file
# Whether an aggregated chart lies horizontally, as the reference read its
# `group` and `value` slots. Without data a numeric column cannot be told.
legacy_chart_horizontal <- function(state) {
  type <- state[["type"]] %||% "bar"
  if (!type %in% c("bar", "boxplot", "pointrange")) {
    return(FALSE)
  }
  orient <- state[["orientation"]] %||%
    chart_infer_orientation(type, state[["x"]], state[["y"]],
                            state[["func"]], function(col) NA)
  identical(orient, "horizontal")
}

# LEGACY: delete with this file
# `{@old}`, `{label(@old)}` and `{@old in a, b}` under the new names, with
# the values of an `in` list mapped as well.
legacy_template <- function(block, tpl, state) {
  if (!is.character(tpl) || length(tpl) != 1L || is.na(tpl) ||
        !grepl("@", tpl, fixed = TRUE)) {
    return(tpl)
  }
  names_map <- legacy_slot_names(block, state)
  for (old in names(names_map)) {
    tpl <- gsub(paste0("@(\\s*)", old, "\\b(?![.(])"),
                paste0("@\\1", names_map[[old]]), tpl, perl = TRUE)
  }
  rows <- legacy_rows(block, renamed = FALSE)
  rows <- rows[lengths(rows$values) > 0L & is.na(rows$rule), ]
  for (i in seq_len(nrow(rows))) {
    tpl <- legacy_template_values(tpl, rows$new[[i]], rows$values[[i]])
  }
  tpl
}

# LEGACY: delete with this file
legacy_template_values <- function(tpl, name, map) {
  re <- paste0("(@\\s*", name, "\\s+in\\s+)([^}]*)")
  m <- gregexpr(re, tpl, perl = TRUE)
  hits <- regmatches(tpl, m)[[1L]]
  for (h in hits) {
    parts <- regmatches(h, regexec(re, h, perl = TRUE))[[1L]]
    vals <- trimws(strsplit(parts[[3L]], ",", fixed = TRUE)[[1L]])
    vals[vals %in% names(map)] <- unname(map[vals[vals %in% names(map)]])
    tpl <- sub(h, paste0(parts[[2L]], paste(vals, collapse = ", ")), tpl,
               fixed = TRUE)
  }
  tpl
}

# Removed blocks ---------------------------------------------------------------
#
# The blocks that no longer exist restore as table blocks. Each returns the
# table's arguments under the current names; the deser methods pass them to
# new_table_block() once the constructors take the new names.

# LEGACY: delete with this file
#' A saved tile as a table
#'
#' A tile that aggregated in place becomes the groups layout in the cards
#' look; a wide tile frame the records layout in cards; the long tile frame
#' a plain records table. Settings the table has no counterpart for are
#' dropped with one warning.
#' @param payload The tile's saved payload.
#' @param label How the warning names the block.
#' @return The table's arguments, under the current names.
#' @noRd
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
  if (table_is_agg_summaries(summaries)) {
    args <- list(
      layout = "groups", by = group, look = "cards",
      summaries = table_agg_to_summaries(summaries),
      drill = if (isTRUE(p[["drill"]]) && length(group)) "auto",
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
  legacy_restore("table", args, version = VIZ_REWRITE_VERSION)
}

# LEGACY: delete with this file
#' A saved summary table block as a table in the variables layout
#' @param payload The block's saved payload, flat or in the interim
#'   single-`state` form.
#' @param version The version that saved it.
#' @return The table's arguments, under the current names.
#' @noRd
legacy_restore_summary <- function(payload, version = NULL) {
  if (is.list(payload[["state"]])) {
    payload <- c(payload[["state"]], payload[setdiff(names(payload), "state")])
  }
  args <- table_legacy_args(payload, from = "summary")
  legacy_restore("table", args, version = version)
}

# LEGACY: delete with this file
#' What a saved block restores to
#'
#' The body of the blockr_deser methods for chart, table, tile and summary
#' table blocks, which call the block's constructor with `args` and the
#' block attributes. The methods themselves are registered once the
#' constructors take the new names.
#' @param class The saved block's first class.
#' @param data The saved block: `payload` and `constructor`.
#' @return `list(block, args, attrs)`: the block it becomes, its arguments
#'   under the current names, and block attributes such as `block_name`.
#' @noRd
legacy_deser <- function(class, data) {
  payload <- data[["payload"]]
  version <- data[["constructor"]][["version"]]
  is_attr <- if (identical(class, "chart_block")) {
    !names(payload) %in% legacy_arg_names("chart")
  } else {
    names(payload) %in% c(names(formals(blockr.core::new_block)), "block_name")
  }
  attrs <- payload[is_attr]
  state <- payload[!is_attr]
  label <- chr_state(attrs[["block_name"]])
  args <- switch(
    class,
    chart_block = legacy_restore("chart", state, version),
    table_block = legacy_restore("table", state, version),
    tile_block = legacy_restore_tile(state, label %||% "a tile"),
    summary_table_block = legacy_restore_summary(state, version),
    stop("No restore for a saved `", class, "`.", call. = FALSE)
  )
  list(block = if (identical(class, "chart_block")) "chart" else "table",
       args = args, attrs = attrs)
}

# LEGACY: delete with this file
# The names a saved chart owns: its arguments and every old name. Anything
# else in a payload is a block attribute, such as blockr.dock's `visible`.
legacy_arg_names <- function(block) {
  unique(c(names(viz_block_spec(block)$args), CHART_LEGACY_ARGS,
           CHART_RETIRED_ARGS, LEGACY_NAMES$old[LEGACY_NAMES$block == block]))
}

# LEGACY: delete with this file
# The table's selection as the reference saved it, one shape per engine:
# `column` and `values` (a flat table, the groups layout), `group_cols` and
# `group_vals` (a structured or nested table), `col_keys` (the clicked
# column's identity). The kit holds one shape for all: `filters`, column to
# values, and `columns` (kit-input.R).
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
    summarize_drill_filters(s$column, s$values)
  }
  ck <- dd_col_keys_state(s$col_keys)
  columns <- if (length(ck)) {
    stats::setNames(lapply(ck, `[[`, "values"), vapply(ck, `[[`, "", "column"))
  }
  # The shape viz_read_selection() gives a click: `columns` always there.
  state["selection"] <- list(if (length(filters) || length(columns)) {
    list(type = "categorical", filters = filters %||% list(),
         columns = columns %||% list())
  })
  state
}
