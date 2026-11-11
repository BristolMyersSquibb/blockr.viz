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
                              format = NULL, unit = NULL,
                              levels_outer = FALSE, group_rows = "name") {
  bad <- function(msg) list(err = msg)

  if (!is.data.frame(data)) return(bad("No data"))
  if (!nrow(data)) return(bad("No rows to display"))

  by <- as.character(unlist(by %||% character()))
  by <- by[!is.na(by) & nzchar(by)]
  if (!is.list(summaries) || !length(summaries)) {
    summaries <- groups_seed()
  }
  # A grouping, colour or facet column with no values is unset. When every
  # `by` column is empty, the inner one stays: its rows have no group, so the
  # table is empty under missing = "drop" (a prepare script that filters by
  # blanking the column filtered everything), not one total row.
  by_set <- by
  by <- cols_with_values(data, by)
  if (!length(by) && length(by_set)) by <- by_set[[length(by_set)]]
  summaries <- lapply(summaries, function(s) {
    if (!is.list(s)) return(s)
    for (k in c("color", "facet")) {
      col <- chr_state(s[[k]])
      if (!is.null(col) && !length(cols_with_values(data, col))) s[[k]] <- NULL
    }
    s
  })
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
    scale_map = scale_map, subject = subject, pct_of = pct_of,
    levels_outer = levels_outer, group_rows = group_rows
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
                                   subject = NULL, pct_of = "facet",
                                   levels_outer = FALSE, group_rows = "name") {
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
  # The outer rows of a nested table are headings (name and row count, no
  # summaries) unless `group_rows = "summary"`. A matrix's outer rows are
  # headings already (groups_tile_paint()) and keep their bare name.
  heads <- !is.null(g$parent) &&
    !identical(chr_state(group_rows) %||% "name", "summary")
  sk <- groups_prep_skeleton(data, keys, g$group, g$parent)
  if (!is.null(sk$err)) return(bad(sk$err))

  build <- list(
    data = data, keys = keys, parent = g$parent, all = g$all,
    # A count's percentage base: a facet copy's own rows, or the whole table.
    pct_total = identical(chr_state(pct_of), "total"),
    qualify = fi$qualify, n_faceted = fi$n_faceted, scale_map = scale_map,
    subject = subject
  )
  summaries <- groups_custom_runs(summaries, build)
  st <- groups_build_columns(summaries, sk$leaf, sk$par_rows, build)
  summaries <- st$summaries
  st <- groups_primary_values(st, build)
  leaf <- st$leaf
  par_rows <- st$par_rows
  plan <- groups_drop_empty_facets(st$plan, leaf, par_rows)
  facet_levels <- intersect(fi$levels,
                            unlist(lapply(plan, function(p) p$flevel)))

  srt <- groups_resolve_sort(sort_by, plan, data, leaf, par_rows, g$group,
                                g$parent)
  # A matrix (compact look) keeps its groups in the outer column's level
  # order, as 0.2's heatmap block did; the rows inside follow `sort_by`.
  if (isTRUE(levels_outer) && !is.null(srt$par_rows)) {
    srt$par_rows$.lvl <- match(srt$par_rows$.parent_key,
                               groups_ord_levels(data[[g$parent]]))
  }
  asm <- groups_assemble_rows(srt$leaf, srt$par_rows, g$parent, srt$key,
                                 sort_dir, top_n)
  # Blanked after the sort, which still ranks the groups by their values,
  # and before the scales, which then fit the rows that draw.
  rows <- groups_blank_parents(asm$rows, summaries, all = heads)

  plan <- groups_summary_domains(plan, rows)
  fl <- groups_facet_layout(plan, facet_levels, facet_layout, data, fi$shared,
                          subject)

  cg <- groups_color_groups(summaries, data, g$drawn, scale_map)
  groups_prepared(rows, fl, fi, g, data, cg, asm, heads)
}

#' The prepared groups table, as groups_prepare_summaries() returns it.
#' @noRd
groups_prepared <- function(rows, fl, fi, g, data, cg, asm, heads) {
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
    # The row column alone, as a compact matrix's stub header names it.
    row_label = if (!isTRUE(g$all)) groups_col_name(data, g$group),
    series = series_lv, palette = pal, group = g$group, parent = g$parent,
    color = lead_group$column, color_groups = cg,
    tile_legend = groups_tile_legend(fl$plan),
    folded = asm$folded, fold_max = asm$fold_max,
    facet_spans = fl$facet_spans,
    failed = unlist(lapply(fl$plan, function(p) p$failed)),
    all = g$all, parent_heads = heads
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
                                            "linetype", "fill",
                                            GROUPS_SPANS_OVERLAYS))])
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
  if (identical(s$type, "spans")) s <- groups_spans_levels(s, data)
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
groups_prep_skeleton <- function(data, keys, group, parent) {
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
  par_rows <- NULL
  if (!is.null(parent)) {
    par_rows <- groups_parent_skeleton(data, skel, parent)
  }
  list(leaf = leaf, par_rows = par_rows)
}

# Parent rows: one per level of the outer column, labelled by the bare level.
# A heading shows the number of its rows after the name (cells_head_count()).
groups_parent_skeleton <- function(data, skel, parent) {
  pv <- groups_levels(data[[parent]])
  par_rows <- data.frame(.label = pv, .parent = NA_character_,
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
groups_primary_values <- function(st, build) {
  ps <- st$primary$s
  pooled <- length(ps$.flevels) && is.null(ps$.custom) &&
    is.null(ps$.failed) && !identical(ps$show, "tile")
  if (pooled) {
    # The colour split adds nothing to the pooled value, so it is left out.
    ps0 <- ps[setdiff(names(ps), c("color", ".color"))]
    st$leaf$.v <- groups_pool_value(st$leaf, build$keys, ps0, build)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- groups_pool_value(st$par_rows, build$parent, ps0, build,
                                        parent_row = TRUE)
    }
  } else {
    st$leaf$.v <- groups_primary_value(st$leaf, ps, st$primary$sid)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- groups_primary_value(st$par_rows, ps, st$primary$sid)
    }
  }
  # A faceted tile column is a matrix row: its sort key is the row's total
  # across the copies (a subject's AE burden), not its first term's count.
  if (identical(ps$show, "tile") && length(ps$.flevels)) {
    vcols <- groups_col(groups_copy_sid(st$primary$i, seq_along(ps$.flevels)),
                        "v")
    st$leaf$.v <- groups_tile_row_total(st$leaf, vcols)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- groups_tile_row_total(st$par_rows, vcols)
    }
  }
  st
}

# The summary over all rows, filled under its own id into a copy of the frame
# that is thrown away: only its primary value is kept.
groups_pool_value <- function(t, tkeys, ps0, build, parent_row = FALSE) {
  sid0 <- groups_copy_sid("P")
  t0 <- groups_fill(t, tkeys, build$keys, build$data, ps0, sid0,
                    parent_row = parent_row)
  groups_primary_value(t0, ps0, sid0)
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

#' Empty the summaries of the parent rows: every summary when the outer rows
#' are headings (`all`), else the identity ones, whose value on an outer row
#' would be one inner row's. A summary's columns are "<sid>_*" with sid
#' ".s<i>" or ".s<i>f<j>" (groups_copy_sid()).
#' @noRd
groups_blank_parents <- function(rows, summaries, all = FALSE) {
  par <- rows$.is_parent %in% TRUE
  if (!any(par)) return(rows)
  ids <- if (isTRUE(all)) {
    seq_along(summaries)
  } else {
    which(vapply(summaries, groups_is_identity, logical(1L)))
  }
  if (!length(ids)) return(rows)
  pat <- paste0("^\\.s(", paste(ids, collapse = "|"), ")(f[0-9]+)?_")
  for (nm in grep(pat, names(rows), value = TRUE)) {
    rows[[nm]] <- groups_blank_col(rows[[nm]], par, nm)
  }
  rows
}

# One frame column with the rows `at` emptied: no value, no text, no
# segments.
groups_blank_col <- function(x, at, nm) {
  if (is.list(x)) {
    x[at] <- rep(list(list()), sum(at))
  } else if (endsWith(nm, "_nseg")) {
    x[at] <- 0L
  } else if (is.character(x)) {
    x[at] <- ""
  } else {
    x[at] <- NA
  }
  x
}

#' A summary drawn as is (`func = "identity"`).
#' @noRd
groups_is_identity <- function(s) {
  is.list(s) && is.null(s$.custom) &&
    identical(s$type %||% "simple", "simple") &&
    identical(chr_state(s$func), "identity")
}

#' A summary copy's primary value per row: the column its sort and label
#' read. An expr summary that evaluated to text has only its text column.
#' @noRd
groups_primary_value <- function(target, s, sid) {
  part <- switch(s$type,
    dist = "bc",
    field = "t",
    sparkline = "last",
    spans = "start",
    pair = "to",
    "v"
  )
  target[[groups_col(sid, part)]] %||% target[[groups_col(sid, "t")]] %||%
    rep(NA_real_, nrow(target))
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
