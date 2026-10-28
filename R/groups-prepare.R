# The groups engine's first step: the input as one row per group with every
# summary's numbers, and the column plan. groups_cells() then draws from it.

#' Prepare the table and its column plan. One row per level of `by` (outer first, at most two), one column per entry of
#' `summaries`; empty `summaries` counts rows (groups_seed()).
#' @return `list(err =)` when the config cannot be honored, else
#'   `list(rows =, plan =, ...)`.
#' @noRd
groups_prepare <- function(data, summaries = list(), by = NULL,
                              facet_layout = "by_summary", sort_by = "value",
                              sort_dir = "desc", top_n = NULL,
                              scale_map = NULL, subject = NULL,
                              pct_of = "facet", missing = "drop",
                              format = NULL, unit = NULL) {
  bad <- function(msg) list(err = msg)

  if (!is.data.frame(data)) return(bad("No data"))
  if (!nrow(data)) return(bad("No rows to display"))

  by <- as.character(unlist(by %||% character()))
  by <- by[!is.na(by) & nzchar(by)]
  if (!is.list(summaries) || !length(summaries)) {
    summaries <- groups_seed()
  }
  split_cols <- unlist(lapply(summaries, function(s) {
    if (is.list(s)) c(chr_state(s[["color"]]), chr_state(s[["facet"]]))
  }))

  # Missing values of a grouping, colour or facet column: no group of their
  # own ("drop"; counted in N, not drawn) or a "(Missing)" level ("level").
  if (identical(chr_state(missing), "level")) {
    data <- table_missing_levels(data, c(by, split_cols))
  }

  # One copy of each group's rows for every split column that carries a group
  # definition (expand_groups()). Each input row is tagged first so a count's
  # percent base counts it once.
  pooled <- intersect(c(by, split_cols), names(data))
  if (any(vapply(pooled, function(col) !is.null(group_def(data[[col]])),
                 logical(1L)))) {
    data[[GROUPS_ROW_ID]] <- seq_len(nrow(data))
  }
  data <- expand_role_groups(data, c(by, split_cols))

  prep <- groups_prepare_summaries(
    data, by, summaries,
    facet_layout = chr_state(facet_layout) %||% "by_summary",
    sort_by = sort_by, sort_dir = sort_dir, top_n = top_n,
    scale_map = scale_map, subject = subject, pct_of = pct_of
  )
  groups_number_format(prep, format, unit)
}

# The table's `format` and `unit` on a prepared groups table. NULL `format`
# keeps the engine's number formatting; else number cells, bar value labels and
# cards print through dt_format_num(). `unit` heads the columns with `unit_ok`
# after the summary's name.
#' @noRd
groups_number_format <- function(prep, format = NULL, unit = NULL) {
  if (!is.null(prep$err)) return(prep)
  if (!is.null(format)) prep$format <- table_format_state(format)
  unit <- chr_state(trimws(as.character(unlist(unit))))
  if (!is.null(unit)) {
    prep$unit <- unit
    with_unit <- function(x) paste0(x, " (", unit, ")")
    prep$plan <- lapply(prep$plan, function(p) {
      if (!isTRUE(p$unit_ok)) return(p)
      if (is.null(p$flevel)) {
        p$label <- with_unit(p$label)
      } else if (!is.null(p$sub_label) && !is.null(p$sname)) {
        p$sub_label <- sub(p$sname, with_unit(p$sname), p$sub_label,
                           fixed = TRUE)
      }
      p
    })
  }
  prep
}

# Drops facet columns with nothing to draw (the chart draws no panel for a level
# without data). Empty = every cell missing, or for a count (`zero_empty`) every
# cell zero. Checked over ALL rows, never the Top N cut. A table left with no
# column keeps them all.
#' @noRd
groups_drop_empty_facets <- function(plan, leaf, par_rows = NULL) {
  frames <- Filter(Negate(is.null), list(leaf, par_rows))
  empty <- vapply(plan, function(p) {
    if (is.null(p$flevel) || is.null(p$key)) return(FALSE)
    v <- unlist(lapply(frames, function(f) f[[p$key]]), use.names = FALSE)
    if (is.null(v)) return(FALSE)
    v <- suppressWarnings(as.numeric(v))
    all(is.na(v) | (isTRUE(p$zero_empty) & !is.na(v) & v == 0))
  }, logical(1L))
  if (all(empty)) return(plan)
  plan[!empty]
}

#' The generic preparer: group_by(by) plus the summaries list.
#' `by` is outer -> inner, at most two columns (the outer is the expandable
#' parent). Colour and facet are each summary's own mapping; a summary naming a
#' facet repeats per level, adjacent. `facet` / `color` are the legacy
#' table-level pair, fanned down onto the rows (groups_migrate_globals()).
#' @noRd
groups_prepare_summaries <- function(data, by, summaries,
                                   facet_layout = "by_summary",
                                   sort_by = "value", sort_dir = "desc",
                                   top_n = NULL, scale_map = NULL,
                                   subject = NULL, pct_of = "facet") {
  bad <- function(msg) list(err = msg)
  g <- groups_prep_groups(data, by)
  if (!is.null(g$err)) return(bad(g$err))
  data <- g$data
  keys <- g$keys

  summaries <- groups_prep_norm(summaries, data)
  if (!is.null(summaries$err)) return(bad(summaries$err))
  summaries <- groups_prep_mappings(summaries, data, g$drawn)
  if (!is.null(summaries$err)) return(bad(summaries$err))

  fi <- groups_facet_info(summaries, data)
  sk <- groups_prep_skeleton(data, keys, g$group, g$parent, summaries)
  if (!is.null(sk$err)) return(bad(sk$err))

  ctx <- list(
    data = data, keys = keys, parent = g$parent, all = g$all,
    # A count's percentage base: a facet copy's own rows, or the whole table.
    pct_total = identical(chr_state(pct_of), "total"),
    qualify = fi$qualify, n_faceted = fi$n_faceted, scale_map = scale_map,
    subject = subject
  )
  st <- groups_build_columns(summaries, sk$leaf, sk$par_rows, ctx)
  summaries <- st$summaries
  st <- groups_primary_values(st, ctx)
  leaf <- st$leaf
  par_rows <- st$par_rows
  plan <- groups_drop_empty_facets(st$plan, leaf, par_rows)
  facet_levels <- intersect(fi$levels,
                            unlist(lapply(plan, function(p) p$flevel)))

  srt <- groups_resolve_sort(sort_by, plan, data, leaf, par_rows, g$group,
                                g$parent)
  asm <- groups_assemble_rows(srt$leaf, srt$par_rows, g$parent, srt$key,
                                 sort_dir, top_n)
  rows <- asm$rows

  plan <- groups_summary_domains(plan, rows)
  fl <- groups_facet_layout(plan, facet_levels, facet_layout, data, fi$shared,
                          subject)

  cg <- groups_color_groups(summaries, data, g$drawn, scale_map)
  # Single-dimension fallback slots; a plan entry carries its own fills and
  # levels.
  lead_group <- if (length(cg)) cg[[1L]]
  series_lv <- lead_group$levels
  pal <- lead_group$palette %||% character()

  list(
    rows = rows, plan = fl$plan,
    layout = if (!length(fi$cols)) "simple" else "facet",
    bar_max = 0, bar_min = 0,
    group_label = groups_group_label(data, g$group, g$parent),
    series = series_lv, palette = pal, group = g$group, parent = g$parent,
    color = lead_group$column, color_groups = cg,
    folded = asm$folded, fold_max = asm$fold_max,
    facet_spans = fl$facet_spans,
    failed = unlist(lapply(fl$plan, function(p) p$failed)),
    all = g$all
  )
}

# The synthetic group of a groups table without `by`: one "Total" row over the
# whole input.
GROUPS_ALL_COL <- ".all"
GROUPS_ALL_LABEL <- "Total"

#' Add the one-level group of a groups table without `by`.
#' @noRd
groups_all_data <- function(data) {
  data[[GROUPS_ALL_COL]] <- rep(GROUPS_ALL_LABEL, nrow(data))
  data
}

#' The grouping columns: `by` cleaned and checked, the synthetic group when `by`
#' is empty, the parent / group split and the rows that draw.
#' @return `list(err =)` or `list(data, all, group, parent, keys, drawn)`.
#' @noRd
groups_prep_groups <- function(data, by) {
  by <- as.character(by %||% character())
  by <- by[nzchar(by)]
  # Without `by`: one row over all input rows, a synthetic group with no
  # header, sort or drill.
  all <- !length(by)
  if (all) {
    data <- groups_all_data(data)
    by <- GROUPS_ALL_COL
  }
  if (length(by) > 2L) {
    return(list(err = "Group by supports at most two columns (outer, inner)"))
  }
  miss <- setdiff(by, names(data))
  if (length(miss)) {
    return(list(err = paste0("Mapped column not in data: ",
                             paste0("\"", miss, "\"", collapse = ", "),
                             ". Re-pick it in the gear.")))
  }
  group <- by[[length(by)]]
  parent <- if (length(by) == 2L) by[[1L]]
  # An outer column with no values lists the inner flat. A prepare script turns
  # nesting off this way, by blanking the outer column behind a flag.
  if (!is.null(parent) && all(is.na(data[[parent]]))) parent <- NULL
  keys <- c(parent, group)
  # A missing group draws no row, so a colour level only such rows carry gets
  # no slot or legend key.
  drawn <- stats::complete.cases(data[keys])
  list(data = data, all = all, group = group, parent = parent, keys = keys,
       drawn = drawn)
}

# A mapped column name, or NULL when it is unset or not in the data.
groups_present <- function(col, data) {
  col <- chr_state(col)
  if (is.null(col) || !col %in% names(data)) NULL else col
}

#' Normalize the summaries, fan the legacy colour and facet down onto the rows
#' and check every mapped column exists.
#' @return the summaries or `list(err =)`.
#' @noRd
groups_prep_norm <- function(summaries, data) {
  summaries <- groups_norm_summaries(summaries, data)
  if (!is.null(summaries$err)) return(list(err = summaries$err))
  if (!length(summaries)) return(list(err = "Add at least one summary column"))
  msg <- groups_missing_columns(summaries, data)
  if (!is.null(msg)) return(list(err = msg))
  summaries
}

# The message for the first summary that maps a missing column, else NULL.
groups_missing_columns <- function(summaries, data) {
  for (s in summaries) {
    cols <- unlist(s[intersect(names(s), c("col", "x", "xend", "ymin", "ymax",
                                            "linetype", "fill"))])
    cols <- as.character(cols)
    if (identical(s$type, "pair")) {
      cols <- c(cols, Filter(is.character, list(s$ref_min, s$ref_max)))
      cols <- as.character(unlist(cols))
    }
    miss <- setdiff(cols[nzchar(cols)], names(data))
    if (length(miss)) {
      return(paste0(
        "Summary \"", s$name, "\": column not in data: ",
        paste0("\"", miss, "\"", collapse = ", "), "."
      ))
    }
  }
  NULL
}

#' Per-summary colour and facet, each optional. Levels come from the FULL data,
#' never a facet slice, so fills and formats agree across copies and rows.
#' @return the summaries or `list(err =)`.
#' @noRd
groups_prep_mappings <- function(summaries, data, drawn) {
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    # A custom row runs its function once over the whole data to learn its
    # shape. A failing function leaves its column empty, with the error.
    if (identical(s$type, "custom")) {
      cs <- groups_custom_setup(s, data)
      if (!is.null(cs$err)) {
        summaries[[i]] <- groups_failed_summary(s, cs$err)
        next
      }
      s <- cs$s
    }
    r <- groups_map_color(s, data, drawn)
    if (!is.null(r$err)) return(r)
    r <- groups_map_facet(r$s, data, drawn)
    if (!is.null(r$err)) return(r)
    summaries[[i]] <- groups_map_levels(r$s, data)
  }
  summaries
}

# Colour mapping: levels over the drawn rows. Returns `list(s =)` or
# `list(err =)`.
groups_map_color <- function(s, data, drawn) {
  cc <- groups_present(s$color, data)
  if (!is.null(cc) && groups_color_capable(s)) {
    lv <- groups_color_levels(data[[cc]], drawn)
    if (length(lv) > GROUPS_MAX_LEVELS) {
      return(list(err = paste0(
        "Summary \"", s$name, "\": colour column \"", cc, "\" has ",
        length(lv), " levels. Pick one with at most ", GROUPS_MAX_LEVELS,
        " -- a palette has about seven readable colours."
      )))
    }
    s$color <- cc
    # A lane mark splits its cell per level; the swimlane colours segments
    # and reads `color` in the plan.
    s$.color <- if (groups_takes_color(s)) cc
    s$.levels <- lv
  }
  list(s = s)
}

# Facet mapping: levels, capped, in row-count order for a tile. Returns
# `list(s =)` or `list(err =)`.
groups_map_facet <- function(s, data, drawn) {
  fc <- groups_present(s$facet, data)
  if (!is.null(fc)) {
    # One level is a filter (one arm left), not a mistake: draw once, labelled.
    lv <- groups_levels(data[[fc]])
    tile <- identical(s$show, "tile")
    cap <- if (tile) GROUPS_MAX_TILE_LEVELS else GROUPS_MAX_LEVELS
    if (length(lv) > cap) {
      return(list(err = paste0(
        "Summary \"", s$name, "\": facet column \"", fc, "\" has ",
        length(lv), " levels, so the column would repeat ", length(lv),
        " times. Pick one with at most ", cap,
        if (tile) ", or keep the top ones in the prepare script", "."
      )))
    }
    if (tile && identical(s$facet_order, "count")) {
      # Most rows first; ties keep the level order.
      n <- tabulate(match(as.character(data[[fc]][drawn]), lv),
                    nbins = length(lv))
      lv <- lv[order(-n, seq_along(lv))]
    }
    s$.facet <- fc
    s$.flevels <- lv
  }
  list(s = s)
}

# Levels read off the full data besides colour and facet: tile paint, pair
# dash, date axis.
groups_map_levels <- function(s, data) {
  # One cell's worst level is read against the whole column's scale.
  if (identical(s$show, "tile") && !is.null(groups_present(s$fill, data))) {
    s$.fill <- s$fill
    s$.fill_levels <- groups_ord_levels(data[[s$fill]])
  }
  # Dash levels come from the full data, so a slice missing a level does not
  # renumber which rows draw dashed.
  if (identical(s$type, "pair") && !is.null(groups_present(s$linetype, data))) {
    s$.dlevels <- groups_levels(data[[s$linetype]])
  }
  if (s$type %in% c("sparkline", "spans")) {
    s$.date <- inherits(data[[chr_state(s$x)]], "Date")
  }
  s
}

# With ONE facet column the level alone labels a copy and by_level can span
# groups. With SEVERAL, each copy names its column ("SEX: F"), by_level has
# nothing to span and the layout stays by_summary; the header keeps the summary
# name beside its N so two "Placebo / N = 86" columns can be told apart.
groups_facet_info <- function(summaries, data) {
  facet_cols <- unique(unlist(lapply(summaries, function(s) s$.facet)))
  shared_facet <- if (length(facet_cols) == 1L) facet_cols else NULL
  list(
    cols = facet_cols, shared = shared_facet,
    qualify = length(facet_cols) > 1L,
    n_faceted = sum(vapply(summaries, function(s) !is.null(s$.facet),
                           logical(1L))),
    levels = if (is.null(shared_facet)) {
      character()
    } else {
      groups_levels(data[[shared_facet]])
    }
  )
}

#' The leaf and parent skeletons: one row per group (and per parent) with label,
#' sort order and keys.
#' @return `list(leaf, par_rows)` or `list(err =)`.
#' @noRd
groups_prep_skeleton <- function(data, keys, group, parent, summaries) {
  # A missing grouping value (a population join appends one for a subject with
  # no record) is not a group: it counts toward N and draws no row. A blank
  # string is a value.
  skel <- unique(data[keys])
  skel <- skel[stats::complete.cases(skel), , drop = FALSE]
  # Nothing left to draw: no rows, or every row without a group.
  if (!nrow(skel)) return(list(err = "No rows to display"))
  skel <- skel[do.call(order, unname(as.list(skel))), , drop = FALSE]
  leaf <- data.frame(.label = as.character(skel[[group]]),
                     .parent = if (is.null(parent)) NA_character_ else
                       as.character(skel[[parent]]),
                     .parent_key = if (is.null(parent)) NA_character_ else
                       as.character(skel[[parent]]),
                     stringsAsFactors = FALSE)
  for (k in keys) leaf[[k]] <- skel[[k]]
  # The data's own order (factor levels, else first appearance), read only by
  # `sort_by = "data"`.
  leaf$.ord <- groups_data_ord(data[[group]], leaf$.label)
  show_parent_n <- !is.null(parent) && any(vapply(summaries, function(s) {
    identical(s$type, "spans") && identical(s$parent, FALSE)
  }, logical(1L)))
  par_rows <- NULL
  if (!is.null(parent)) {
    par_rows <- groups_parent_skeleton(data, skel, parent, show_parent_n)
  }
  list(leaf = leaf, par_rows = par_rows)
}

# Parent rows: one per level of the outer column, with N in the label when a
# section-style column asks for it.
groups_parent_skeleton <- function(data, skel, parent, show_parent_n) {
  pv <- groups_levels(data[[parent]])
  parent_n <- tabulate(match(as.character(skel[[parent]]), pv),
                       nbins = length(pv))
  pl <- if (show_parent_n) paste0(pv, " (N = ", parent_n, ")") else pv
  par_rows <- data.frame(.label = pl, .parent = NA_character_,
                         .parent_key = as.character(pv),
                         stringsAsFactors = FALSE)
  par_rows[[parent]] <- pv
  par_rows$.ord <- groups_data_ord(data[[parent]], pv)
  par_rows
}

#' `.v`: the FIRST summary's primary value, the sort key. A faceted column ranks
#' by its value over all its levels together (by its first level, a row empty
#' there would sink however large its other levels); one more builder pass gives
#' it and its columns are dropped.
#' @return `st` with `.v` set on `leaf` and `par_rows`.
#' @noRd
groups_primary_values <- function(st, ctx) {
  ps <- st$primary$s
  pooled <- length(ps$.flevels) && is.null(ps$.custom) &&
    is.null(ps$.failed) && !identical(ps$show, "tile")
  if (pooled) {
    # The colour split adds nothing to the pooled value, so it is left out.
    ps0 <- ps[setdiff(names(ps), c("color", ".color"))]
    st$leaf$.v <- groups_pool_value(st$leaf, ctx$keys, ps0, ctx)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- groups_pool_value(st$par_rows, ctx$parent, ps0, ctx,
                                        parent_row = TRUE)
    }
  } else {
    primary_col <- groups_primary_col(st$primary$s, st$primary$sid)
    st$leaf$.v <- groups_primary_value(st$leaf, st$primary$s, primary_col)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- groups_primary_value(st$par_rows, st$primary$s,
                                           primary_col)
    }
  }
  # A faceted tile column is a matrix row: its sort key is the row's total
  # across the copies (a subject's AE burden), not its first term's count.
  if (identical(ps$show, "tile") && length(ps$.flevels)) {
    stem <- sub("f[0-9]+$", "", st$primary$sid)
    vcols <- paste0(stem, "f", seq_along(ps$.flevels), "_v")
    st$leaf$.v <- groups_tile_row_total(st$leaf, vcols)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- groups_tile_row_total(st$par_rows, vcols)
    }
  }
  st
}

groups_pool_value <- function(t, tkeys, ps0, ctx, parent_row = FALSE) {
  sid0 <- ".sP"
  t0 <- groups_fill(t, tkeys, ctx$keys, ctx$data, ps0, sid0,
                  parent_row = parent_row)
  groups_primary_value(t0, ps0, groups_primary_col(ps0, sid0))
}

groups_tile_row_total <- function(t, vcols) {
  cc <- intersect(vcols, names(t))
  if (!length(cc)) return(t$.v)
  rowSums(as.matrix(t[cc]), na.rm = TRUE)
}

#' A custom summary whose function failed: a text column of dashes carrying the
#' error (`.failed`), reported through blockr's conditions. One column whatever
#' the summary asked for.
#' @noRd
groups_failed_summary <- function(s, msg) {
  list(type = "field", name = s$name, .failed = msg)
}

#' Fill parent rows with empty marks for section-style summaries
#' @noRd
groups_blank_summary <- function(target, s, sid) {
  n <- nrow(target)
  if (identical(s$type, "spans")) {
    target[[paste0(sid, "_segs")]] <- I(rep(list(list()), n))
    target[[paste0(sid, "_start")]] <- rep(NA_real_, n)
    target[[paste0(sid, "_nseg")]] <- rep(0L, n)
  }
  target
}

#' @noRd
groups_primary_col <- function(s, sid) {
  switch(s$type,
    simple = paste0(sid, "_v"),
    dist = paste0(sid, "_bc"),
    field = paste0(sid, "_t"),
    expr = paste0(sid, "_v"),
    sparkline = paste0(sid, "_last"),
    spans = paste0(sid, "_start"),
    pair = paste0(sid, "_to"),
    paste0(sid, "_v")
  )
}

#' @noRd
groups_primary_value <- function(target, s, col) {
  v <- target[[col]]
  if (is.null(v)) {
    # expr rows may have landed as text.
    v <- target[[sub("_v$", "_t", col)]]
  }
  if (is.null(v)) rep(NA_real_, nrow(target)) else v
}

#' Shared ordering, fold and row assembly: rank order, cap, nest.
#' @noRd
groups_assemble_rows <- function(leaf, par_rows, parent, sort_key, sort_dir,
                                    top_n) {
  ord <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    v <- if (identical(sort_key, ".label")) df$.label else df[[sort_key]]
    if (is.null(v)) v <- df$.v
    if (is.character(v)) {
      df[order(v, decreasing = identical(sort_dir, "desc")), , drop = FALSE]
    } else {
      df[order(v, decreasing = identical(sort_dir, "desc"), na.last = TRUE), ,
         drop = FALSE]
    }
  }

  folded <- 0L
  fold_max <- NA_real_
  if (is.null(parent)) {
    rows <- ord(leaf)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 && nrow(rows) > top_n) {
      rest <- rows[seq.int(top_n + 1L, nrow(rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      rows <- rows[seq_len(top_n), , drop = FALSE]
    }
    rows$.level <- 0L
    rows$.is_parent <- FALSE
  } else {
    par_rows <- ord(par_rows)
    if (!is.null(top_n) && is.finite(top_n) && top_n > 0 &&
          nrow(par_rows) > top_n) {
      rest <- par_rows[seq.int(top_n + 1L, nrow(par_rows)), , drop = FALSE]
      folded <- nrow(rest)
      fold_max <- suppressWarnings(max(rest$.v, na.rm = TRUE))
      par_rows <- par_rows[seq_len(top_n), , drop = FALSE]
    }
    par_rows$.level <- 0L
    par_rows$.is_parent <- TRUE
    leaf$.level <- 1L
    leaf$.is_parent <- FALSE
    # Renderer-internal columns are dot-prefixed; the key columns are not, and
    # rbind() must not demand them from both frames.
    keep <- grep("^\\.", names(leaf), value = TRUE)
    pieces <- list()
    for (i in seq_len(nrow(par_rows))) {
      p <- par_rows[i, , drop = FALSE]
      pkey <- if (!is.null(p$.parent_key)) {
        p$.parent_key[[1L]]
      } else if (!is.null(parent) && parent %in% names(p)) {
        as.character(p[[parent]][[1L]])
      } else {
        p$.label[[1L]]
      }
      kids <- ord(leaf[!is.na(leaf$.parent) & leaf$.parent == pkey, ,
                       drop = FALSE])
      pieces[[length(pieces) + 1L]] <-
        p[, intersect(keep, names(p)), drop = FALSE]
      if (nrow(kids)) {
        pieces[[length(pieces) + 1L]] <- kids[, intersect(keep, names(kids)),
                                              drop = FALSE]
      }
    }
    rows <- do.call(rbind, pieces)
  }
  list(rows = rows, folded = folded, fold_max = fold_max)
}

#' Match a stat column from `src` into `target` by `keys`; absent keys stay NA
#' (an absent facet cell draws nothing).
#' @noRd
groups_match_col <- function(target, src, keys, col) {
  as.numeric(groups_match_field(target, src, keys, col))
}

# A new groups table's first column: a count drawn as a bar.
groups_seed <- function() {
  list(list(type = "simple", func = "count", show = "bar", name = "Count"))
}
