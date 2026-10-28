# LEGACY: this file and R/legacy.R, R/legacy-deser.R go when boards from
# blockr.viz < 0.3.0.9000 need no longer restore.
#
# legacy_translate() maps a saved state or a call's arguments to the current
# names, in one order: the renames of LEGACY_NAMES and the translators a
# rename cannot say (legacy_chart_roles(), legacy_table_args()), the old
# selection shape, the value maps, the rules a map cannot say
# (legacy_values()), summary entries and shading rules, title templates,
# dropped names. A restore heals what is left (viz_heal()); the constructor
# warns about what it translated and leaves bad values to viz_check().

# Boards saved by the rewritten blocks carry at least this version. Before
# it, the table's `color = NULL` meant "auto".
VIZ_REWRITE_VERSION <- "0.3.0.9000"

# `version` NULL is read as older than the rewrite. Returns every argument
# of the block's spec.
legacy_restore <- function(block, state, version = NULL) {
  stopifnot(block %in% c("chart", "table"), is.list(state))
  viz_heal(viz_block_spec(block), legacy_translate(block, state, version)$out)
}

# Code speaks the current version: an unset `color` is off, not "auto".
legacy_ctor_args <- function(block, args) {
  tr <- legacy_translate(block, args, VIZ_REWRITE_VERSION)
  legacy_warn(block, args, tr$named, tr$out)
  tr$out
}

# `named` is the state after the renames and translators, `out` the result.
legacy_translate <- function(block, x, version) {
  named <- legacy_named(block, as.list(x))
  if (identical(block, "table")) named <- legacy_table_selection(named)
  out <- legacy_values(block, legacy_map_values(block, named), version)
  out <- legacy_entries(block, out)
  for (k in intersect(c("title", "subtitle", "caption"), names(out))) {
    out[k] <- list(legacy_template(block, out[[k]], out))
  }
  list(named = named, out = legacy_drop(block, out))
}

# One warning naming the old names in a call, and the current names given an
# old value; anything else is left to viz_check().
legacy_warn <- function(block, args, named, out) {
  current <- names(viz_block_spec(block)$args)
  known <- LEGACY_NAMES$old[LEGACY_NAMES$block == block]
  renamed <- intersect(setdiff(names(args), current), known)
  old <- union(renamed, Filter(function(nm) {
    nm %in% names(args) && !identical(named[[nm]], out[[nm]]) &&
      !identical(nm, "script")
  }, names(out)))
  labels <- "direct_labels" %in% old
  old <- setdiff(old, "direct_labels")
  old <- if (length(old)) paste0("`", old, "`") else character()
  kinds <- c(summaries = "summary", shadings = "shading")
  for (nf in names(kinds)) {
    notes <- legacy_entry_keys(nf, args[[nf]], kinds[[nf]])
    if (length(notes) && paste0("`", nf, "`") %in% old) {
      old <- c(setdiff(old, paste0("`", nf, "`")), notes)
    }
  }
  if (length(old)) {
    warning(
      "new_", block, "_block(): ", paste(old, collapse = ", "),
      if (length(old) > 1L) " are" else " is",
      " from an earlier blockr.viz. See ?new_", block, "_block for the ",
      "current names.",
      call. = FALSE
    )
  }
  if (labels) {
    warning("new_", block, "_block(): `direct_labels = TRUE` is now ",
            "`legend = \"marks\"`.", call. = FALSE)
  }
  invisible()
}

# The old names of a block: those LEGACY_NAMES renames or drops.
legacy_old_names <- function(block) {
  rows <- LEGACY_NAMES[LEGACY_NAMES$block == block, ]
  rows$old[is.na(rows$new) | rows$old != rows$new]
}

# The first step: the old translators, by block.
legacy_named <- function(block, x) {
  switch(
    block,
    # The chart's roles read the renamed `type`, `func` and `orientation`.
    chart = legacy_chart_selection(legacy_chart_roles(
      legacy_rename("chart", x),
      old = any(names(x) %in% legacy_old_names("chart"))
    )),
    # The table's translator reads the old names themselves.
    table = legacy_rename("table", legacy_table_before_030(x))
  )
}

# A script saved as a vector of lines.
legacy_script <- function(x) {
  if (is.character(x[["script"]]) && length(x$script) > 1L) {
    x$script <- paste(x$script, collapse = "\n")
  }
  x
}

# Renames without a rule, and the "to logical" ones.
legacy_rows <- function(block, renamed = TRUE) {
  rows <- LEGACY_NAMES[LEGACY_NAMES$block == block & !is.na(LEGACY_NAMES$new) &
                         (is.na(LEGACY_NAMES$rule) |
                            LEGACY_NAMES$rule == "to logical"), ]
  if (renamed) rows[rows$old != rows$new, ] else rows
}

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

# Old values under the current names ("stacked" is "stack").
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

# Rules a value map cannot say.
legacy_values <- function(block, x, version) {
  x <- legacy_script(x)
  # What the blocks before the rewrite saved in another form; code speaks the
  # current version, where such a value is an error.
  if (identical(block, "chart") && legacy_before_rewrite(version)) {
    if (length(x[["percent_of"]]) > 1L) {
      x$percent_of <- x$percent_of[[1L]]
    }
    fc <- x[["facet_cols"]]
    if (is.character(fc)) {
      n <- suppressWarnings(as.numeric(fc[[1L]]))
      x["facet_cols"] <- list(if (isTRUE(n == round(n) && n >= 1)) {
        as.integer(n)
      })
    }
  }
  if (identical(block, "table")) {
    x <- legacy_table_drill(x)
    if (legacy_before_rewrite(version)) {
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

# Old keys inside entries, as "`summaries[[1]]$style` (now `show`)".
legacy_entry_keys <- function(field, entries, kind) {
  rows <- LEGACY_NAMES[LEGACY_NAMES$block == kind &
                         (is.na(LEGACY_NAMES$new) |
                            LEGACY_NAMES$old != LEGACY_NAMES$new), ]
  if (!is.list(entries)) return(character())
  unlist(lapply(seq_along(entries), function(i) {
    e <- entries[[i]]
    hits <- intersect(names(e), rows$old)
    vapply(hits, function(k) {
      new <- rows$new[match(k, rows$old)]
      paste0("`", field, "[[", i, "]]$", k, "` (",
             if (is.na(new)) "dropped" else paste0("now `", new, "`"), ")")
    }, character(1L), USE.NAMES = FALSE)
  }))
}

legacy_before_rewrite <- function(version) {
  v <- as.character(unlist(version))
  if (!length(v) || is.na(v[[1L]]) || !nzchar(v[[1L]])) {
    return(TRUE)
  }
  ok <- tryCatch(package_version(v[[1L]]), error = function(e) NULL)
  is.null(ok) || ok < package_version(VIZ_REWRITE_VERSION)
}

# Names read and dropped on purpose, without a warning.
legacy_drop <- function(block, x) {
  dropped <- LEGACY_NAMES$old[LEGACY_NAMES$block == block &
                                is.na(LEGACY_NAMES$new)]
  x[setdiff(names(x), dropped)]
}

# Old slot name = new one. The chart's `group` and `value` follow the
# orientation. The table's `value`, `func`, `color`, `facet` and `fill`
# stay: they name the first summary's keys.
legacy_slot_names <- function(block, state) {
  if (identical(block, "table")) {
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

# `{@old}`, `{label(@old)}` and `{@old in a, b}` under the new names and
# values.
legacy_template <- function(block, tpl, state) {
  if (!is.character(tpl) || length(tpl) != 1L || is.na(tpl) ||
        !grepl("@", tpl, fixed = TRUE)) {
    return(tpl)
  }
  tpl <- legacy_slots(tpl, legacy_slot_names(block, state))
  rows <- legacy_rows(block, renamed = FALSE)
  rows <- rows[lengths(rows$values) > 0L & is.na(rows$rule), ]
  for (i in seq_len(nrow(rows))) {
    tpl <- legacy_template_values(tpl, rows$new[[i]], rows$values[[i]])
  }
  tpl
}

legacy_slots <- function(tpl, map) {
  if (!is.character(tpl) || length(tpl) != 1L || is.na(tpl)) return(tpl)
  for (old in names(map)) {
    tpl <- gsub(paste0("@(\\s*)", old, "\\b(?![.(])"),
                paste0("@\\1", map[[old]]), tpl, perl = TRUE)
  }
  tpl
}

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

# Removed blocks restore as tables; each returns the table's arguments.

# The body of the blockr_deser methods: `list(block, args, attrs)`. A name
# that is not one of the saved block's arguments is a block attribute
# (block_name, blockr.dock's `visible`).
legacy_deser <- function(class, data) {
  payload <- data[["payload"]]
  version <- data[["constructor"]][["version"]]
  is_attr <- !names(payload) %in% legacy_arg_names(class)
  attrs <- payload[is_attr]
  state <- payload[!is_attr]
  label <- chr_state(attrs[["block_name"]])
  args <- switch(
    class,
    chart_block = legacy_restore("chart", state, version),
    table_block = legacy_restore("table", state, version),
    tile_block = legacy_restore_tile(state, label %||% "a tile"),
    heatmap_block = legacy_restore_heatmap(state, label %||% "a heatmap"),
    summary_table_block = legacy_restore_summary(state, version),
    stop("No restore for a saved `", class, "`.", call. = FALSE)
  )
  list(block = if (identical(class, "chart_block")) "chart" else "table",
       args = args, attrs = attrs)
}

# The names a saved block of `block`'s class owns: its arguments and old
# names.
legacy_arg_names <- function(block) {
  block <- sub("_block$", "", block)
  table <- c(names(viz_block_spec("table")$args),
             LEGACY_NAMES$old[LEGACY_NAMES$block == "table"],
             legacy_table_names("table"))
  unique(switch(
    block,
    chart = c(names(viz_block_spec("chart")$args),
              LEGACY_CHART_RETIRED,
              LEGACY_NAMES$old[LEGACY_NAMES$block == "chart"]),
    table = table,
    summarize_table = c(table, legacy_table_names("summarize")),
    summary_table = c(table, legacy_table_names("summary"), "stats",
                      "sections", "by"),
    tile = LEGACY_TILE_ARGS,
    heatmap = c(LEGACY_NAMES$old[LEGACY_NAMES$block == "heatmap"],
                "download", "title", "subtitle", "caption", "script"),
    character()
  ))
}

