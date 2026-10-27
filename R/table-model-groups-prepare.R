# Table model, groups layout: the preparer ---------------------------------------
#
# The entry point of the groups table's compute, lane_prepare_summaries(), and
# the stages it runs: the grouping columns, the normalized summaries and their
# colour and facet mappings, the leaf and parent skeletons, the sort key, and
# the shared row assembly (rank order, cap, nest). The columns themselves are
# built in table-model-groups-build.R and described in
# table-model-groups-plan.R.
#
# The rule for what belongs on this surface: one row is one category, and the
# mark is a horizontal glyph on a shared linear domain confined to a cell
# (_blockr.design/open/summarize-table/).

#' The generic preparer: group_by(by) plus the summaries list.
#'
#' `by` is outer -> inner; the renderer nests one level, so at most two
#' columns (the outer becomes the expandable parent).
#'
#' Colour and facet are the SUMMARY's mappings, not the table's: a column
#' carries a colour split, or repeats per level of a facet column, or
#' neither, and two columns may map different ones. A summary that names a
#' facet repeats per level (copies adjacent, by_summary layout); one that
#' does not renders once. Each plan entry carries its own domain, shared
#' across that summary's facet copies -- mixed marks with different units
#' must not share a scale.
#'
#' `facet` / `color` are the legacy table-level pair: they are fanned down
#' onto the rows (lane_migrate_globals()) and never read again.
#' @noRd
lane_prepare_summaries <- function(data, by, summaries, facet = NULL,
                                   facet_layout = "by_summary",
                                   color = NULL,
                                   sort_by = "value", sort_dir = "desc",
                                   top_n = NULL, scale_map = NULL,
                                   subject = NULL, pct_of = "facet") {
  bad <- function(msg) list(err = msg)
  g <- lane_prep_groups(data, by)
  if (!is.null(g$err)) return(bad(g$err))
  data <- g$data
  keys <- g$keys

  summaries <- lane_prep_norm(summaries, data, color, facet)
  if (!is.null(summaries$err)) return(bad(summaries$err))
  summaries <- lane_prep_mappings(summaries, data, g$drawn)
  if (!is.null(summaries$err)) return(bad(summaries$err))

  fi <- lane_facet_info(summaries, data)
  sk <- lane_prep_skeleton(data, keys, g$group, g$parent, summaries)
  if (!is.null(sk$err)) return(bad(sk$err))

  ctx <- list(
    data = data, keys = keys, parent = g$parent, all = g$all,
    # A count's percentage base: each facet copy's own rows ("facet", one
    # level's N) or the whole table ("total").
    pct_total = identical(summarize_chr1(pct_of), "total"),
    qualify = fi$qualify, n_faceted = fi$n_faceted, scale_map = scale_map,
    subject = subject
  )
  st <- lane_build_columns(summaries, sk$leaf, sk$par_rows, ctx)
  summaries <- st$summaries
  st <- lane_primary_values(st, ctx)
  leaf <- st$leaf
  par_rows <- st$par_rows
  plan <- summarize_drop_empty_facets(st$plan, leaf, par_rows)
  facet_levels <- intersect(fi$levels,
                            unlist(lapply(plan, function(p) p$flevel)))

  srt <- summarize_resolve_sort(sort_by, plan, data, leaf, par_rows, g$group,
                                g$parent)
  asm <- summarize_assemble_rows(srt$leaf, srt$par_rows, g$parent, srt$key,
                                 sort_dir, top_n)
  rows <- asm$rows

  # --- per-entry domains ------------------------------------------------------
  # Computed over the ASSEMBLED rows (parents included) and shared across a
  # summary's facet copies: one scale per summary, never one scale per table.
  plan <- lane_summary_domains(plan, rows)
  fl <- lane_facet_layout(plan, facet_levels, facet_layout, data, fi$shared,
                          subject)

  cg <- lane_color_groups(summaries, data, g$drawn, scale_map)
  # The first group also fills the single-dimension slots the rank path's
  # emitters fall back on (a plan entry carries its own fills and levels, so
  # these are a fallback, never the source of truth).
  lead_group <- if (length(cg)) cg[[1L]]
  series_lv <- lead_group$levels
  pal <- lead_group$palette %||% character()

  list(
    rows = rows, plan = fl$plan,
    layout = if (!length(fi$cols)) "simple" else "facet",
    mark = "summaries", bar_max = 0, bar_min = 0,
    group_label = summarize_group_label(data, g$group, g$parent),
    series = series_lv,
    palette = pal, facet_levels = facet_levels,
    denoms = c(all = summarize_n_rows(data)), group = g$group,
    parent = g$parent,
    color = lead_group$column, color_groups = cg,
    facet = fi$shared,
    folded = asm$folded, fold_max = asm$fold_max,
    n_total = if (is.null(g$parent)) nrow(leaf) else nrow(par_rows),
    note = NULL, pct_ok = FALSE, func = "identity",
    facet_spans = fl$facet_spans,
    failed = unlist(lapply(fl$plan, function(p) p$failed)),
    all = g$all
  )
}

# The synthetic group a groups table without `by` draws from: one row over
# the whole input. Its row reads "Total", the variables layout's word for the
# column over all rows (`total_label`).
GROUPS_ALL_COL <- ".all"
GROUPS_ALL_LABEL <- "Total"

#' Add the one-level group of a groups table without `by`.
#' @noRd
groups_all_data <- function(data) {
  data[[GROUPS_ALL_COL]] <- rep(GROUPS_ALL_LABEL, nrow(data))
  data
}

#' The grouping columns of a groups table: `by` cleaned and checked, the
#' synthetic one-level group when `by` is empty, the parent / group split and
#' the rows that draw. Returns `list(err =)` or `list(data, all, group,
#' parent, keys, drawn)`.
#' @noRd
lane_prep_groups <- function(data, by) {
  by <- as.character(by %||% character())
  by <- by[nzchar(by)]
  # Without `by` the table is one row over all input rows: a synthetic
  # one-level group (GROUPS_ALL_COL) that has no header, no sort and no
  # drill. `all` on the result tells the renderers.
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
  # An outer column with no value in it is a level with nothing to show, so
  # the table lists the inner column flat. This is how a prepare script turns
  # the nesting off without touching `by`: it blanks the outer column
  # (`AEBODSYS = if (soc) AEBODSYS else NA`) behind a flag.
  if (!is.null(parent) && all(is.na(data[[parent]]))) parent <- NULL
  keys <- c(parent, group)
  # The rows that draw: a missing group draws none (see the skeletons below),
  # so a colour level only such rows carry gets no slot and no legend key.
  drawn <- stats::complete.cases(data[keys])
  list(data = data, all = all, group = group, parent = parent, keys = keys,
       drawn = drawn)
}

# A mapped column name, or NULL when it is unset or not in the data.
lane_present <- function(col, data) {
  col <- summarize_chr1(col)
  if (is.null(col) || !col %in% names(data)) NULL else col
}

#' Normalize the summaries, fan the legacy table-level colour and facet down
#' onto the rows and check every mapped column exists. Returns the summaries
#' or `list(err =)`.
#' @noRd
lane_prep_norm <- function(summaries, data, color, facet) {
  summaries <- lane_norm_summaries(summaries, data)
  if (!is.null(summaries$err)) return(list(err = summaries$err))
  if (!length(summaries)) return(list(err = "Add at least one summary column"))
  summaries <- lane_migrate_globals(summaries, lane_present(color, data),
                                    lane_present(facet, data))
  msg <- lane_missing_columns(summaries, data)
  if (!is.null(msg)) return(list(err = msg))
  summaries
}

# Every summary's mapped columns must exist -- reported by row name. Returns
# the message for the first row that maps a missing column, else NULL.
lane_missing_columns <- function(summaries, data) {
  for (s in summaries) {
    cols <- unlist(s[intersect(names(s), c("col", "x", "xend", "band", "from",
                                            "to", "dash", "fill"))])
    cols <- as.character(cols)
    if (identical(s$type, "pair")) {
      cols <- c(cols, Filter(is.character, list(s$lo, s$hi)))
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

#' Per-summary colour and facet. Both optional, both the summary's own.
#' Levels come from the FULL data, never a facet slice: fills and formats
#' must agree across a summary's copies and across every row. Returns the
#' summaries or `list(err =)`.
#' @noRd
lane_prep_mappings <- function(summaries, data, drawn) {
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    # A custom row runs its function once over the whole data to learn what
    # it returns, then stands in as the preset row that draws that shape.
    # A function that fails here leaves its column empty, the error with it.
    if (identical(s$type, "custom")) {
      cs <- lane_custom_setup(s, data)
      if (!is.null(cs$err)) {
        summaries[[i]] <- lane_failed_summary(s, cs$err)
        next
      }
      s <- cs$s
    }
    r <- lane_map_color(s, data, drawn)
    if (!is.null(r$err)) return(r)
    r <- lane_map_facet(r$s, data, drawn)
    if (!is.null(r$err)) return(r)
    summaries[[i]] <- lane_map_levels(r$s, data)
  }
  summaries
}

# The colour mapping of one summary: its levels over the drawn rows. Returns
# `list(s =)` or `list(err =)`.
lane_map_color <- function(s, data, drawn) {
  cc <- lane_present(s$color, data)
  if (!is.null(cc) && lane_color_capable(s)) {
    lv <- summarize_color_levels(data[[cc]], drawn)
    if (length(lv) > LANE_MAX_LEVELS) {
      return(list(err = paste0(
        "Summary \"", s$name, "\": colour column \"", cc, "\" has ",
        length(lv), " levels. Pick one with at most ", LANE_MAX_LEVELS,
        " -- a palette has about seven readable colours."
      )))
    }
    s$color <- cc
    # A lane mark splits its cell (one glyph per level); the swimlane
    # colours its segments instead, and reads `color` in the plan.
    s$.color <- if (lane_takes_color(s)) cc
    s$.levels <- lv
  }
  list(s = s)
}

# The facet mapping of one summary: its levels, capped, in row-count order
# for a tile. Returns `list(s =)` or `list(err =)`.
lane_map_facet <- function(s, data, drawn) {
  fc <- lane_present(s$facet, data)
  if (!is.null(fc)) {
    # One level is a filter narrowing the data (one arm left), not a
    # mistake: the column draws once, labelled with that level.
    lv <- summarize_levels(data[[fc]])
    tile <- identical(s$show, "tile")
    cap <- if (tile) LANE_MAX_TILE_LEVELS else LANE_MAX_LEVELS
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

# The levels a summary reads off the full data besides colour and facet: a
# tile's paint, a pair's dash, a series' or spans' date axis.
lane_map_levels <- function(s, data) {
  # A tile's paint levels come from the full data too: the worst level of
  # one cell is read against the whole column's scale.
  if (identical(s$show, "tile") && !is.null(lane_present(s$fill, data))) {
    s$.fill <- s$fill
    s$.fill_levels <- lane_ord_levels(data[[s$fill]])
  }
  # The dash levels come from the full data, like colour: a facet slice
  # missing a level must not renumber which rows draw dashed.
  if (identical(s$type, "pair") && !is.null(lane_present(s$dash, data))) {
    s$.dlevels <- summarize_levels(data[[s$dash]])
  }
  if (s$type %in% c("series", "spans")) {
    s$.date <- inherits(data[[summarize_chr1(s$x)]], "Date")
  }
  s
}

# One facet column across the table is the ordinary case: the level alone
# labels a copy, and the by_level layout can span level groups across the
# summaries. With SEVERAL, adjacent column groups answer different
# questions, so each copy names its column ("SEX: F") and by_level has no
# shared groups to span -- the layout stays by_summary. Several faceted
# columns: each copy's header keeps its summary's name beside its N, or two
# "Placebo / N = 86" columns could not be told apart.
lane_facet_info <- function(summaries, data) {
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
      summarize_levels(data[[shared_facet]])
    }
  )
}

#' The leaf and parent skeletons: one row per group (and per parent) with its
#' label, sort order and keys. Returns `list(leaf, par_rows)` or `list(err =)`.
#' @noRd
lane_prep_skeleton <- function(data, keys, group, parent, summaries) {
  # A row whose grouping value is missing (NA) is not a group. It is what
  # blockr.pharma's population join appends for a subject with no record:
  # it counts toward a column's N (below, over the full slice) and draws no
  # row. A blank string is a value and keeps its row.
  skel <- unique(data[keys])
  skel <- skel[stats::complete.cases(skel), , drop = FALSE]
  # Nothing left to draw: no rows at all, or every row without a group (a
  # population join with no events in it, or a prepare script that cleared
  # every term).
  if (!nrow(skel)) return(list(err = "No rows to display"))
  skel <- skel[do.call(order, unname(as.list(skel))), , drop = FALSE]
  leaf <- data.frame(.label = as.character(skel[[group]]),
                     .parent = if (is.null(parent)) NA_character_ else
                       as.character(skel[[parent]]),
                     .parent_key = if (is.null(parent)) NA_character_ else
                       as.character(skel[[parent]]),
                     stringsAsFactors = FALSE)
  for (k in keys) leaf[[k]] <- skel[[k]]
  # The data's own order for the row labels (factor levels, else
  # first-appearance): `sort_by = "data"` reads it, everything else ignores
  # it. A visit-keyed table is unreadable alphabetically.
  leaf$.ord <- summarize_data_ord(data[[group]], leaf$.label)
  show_parent_n <- !is.null(parent) && any(vapply(summaries, function(s) {
    identical(s$type, "spans") && identical(s$parent, FALSE)
  }, logical(1L)))
  par_rows <- NULL
  if (!is.null(parent)) {
    par_rows <- lane_parent_skeleton(data, skel, parent, show_parent_n)
  }
  list(leaf = leaf, par_rows = par_rows)
}

# The parent rows of the skeleton: one per level of the outer column, with
# its N in the label when a section-style column asks for it.
lane_parent_skeleton <- function(data, skel, parent, show_parent_n) {
  pv <- summarize_levels(data[[parent]])
  parent_n <- tabulate(match(as.character(skel[[parent]]), pv),
                       nbins = length(pv))
  pl <- if (show_parent_n) paste0(pv, " (N = ", parent_n, ")") else pv
  par_rows <- data.frame(.label = pl, .parent = NA_character_,
                         .parent_key = as.character(pv),
                         stringsAsFactors = FALSE)
  par_rows[[parent]] <- pv
  par_rows$.ord <- summarize_data_ord(data[[parent]], pv)
  par_rows
}

#' `.v` = the FIRST summary's primary value (its sort key): rank order
#' follows the leading column unless sort_by picks another. A faceted
#' column ranks by its value over all its levels together, as the single
#' ranked bar did: by its first level, a row with nothing in that level
#' would sink however large its other levels are. One more pass of the
#' builder over all rows gives that value; its columns are dropped again.
#' @param st The build state of [lane_build_columns()].
#' @return `st` with `.v` set on `leaf` and `par_rows`.
#' @noRd
lane_primary_values <- function(st, ctx) {
  ps <- st$primary$s
  pooled <- length(ps$.flevels) && is.null(ps$.custom) &&
    is.null(ps$.failed) && !identical(ps$show, "tile")
  if (pooled) {
    # The colour split adds nothing to the pooled value, so it is left out.
    ps0 <- ps[setdiff(names(ps), c("color", ".color"))]
    st$leaf$.v <- lane_pool_value(st$leaf, ctx$keys, ps0, ctx)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- lane_pool_value(st$par_rows, ctx$parent, ps0, ctx,
                                        parent_row = TRUE)
    }
  } else {
    primary_col <- lane_primary_col(st$primary$s, st$primary$sid)
    st$leaf$.v <- lane_primary_value(st$leaf, st$primary$s, primary_col)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- lane_primary_value(st$par_rows, st$primary$s,
                                           primary_col)
    }
  }
  # A faceted tile column is a matrix row: its sort key is the row's total
  # across the copies (a subject's AE burden), not its first term's count.
  if (identical(ps$show, "tile") && length(ps$.flevels)) {
    stem <- sub("f[0-9]+$", "", st$primary$sid)
    vcols <- paste0(stem, "f", seq_along(ps$.flevels), "_v")
    st$leaf$.v <- lane_tile_row_total(st$leaf, vcols)
    if (!is.null(st$par_rows)) {
      st$par_rows$.v <- lane_tile_row_total(st$par_rows, vcols)
    }
  }
  st
}

# The pooled primary value of a frame: the first summary built over all rows.
lane_pool_value <- function(t, tkeys, ps0, ctx, parent_row = FALSE) {
  sid0 <- ".sP"
  t0 <- lane_fill(t, tkeys, ctx$keys, ctx$data, ps0, sid0,
                  parent_row = parent_row)
  lane_primary_value(t0, ps0, lane_primary_col(ps0, sid0))
}

# A tile row's total across its facet copies.
lane_tile_row_total <- function(t, vcols) {
  cc <- intersect(vcols, names(t))
  if (!length(cc)) return(t$.v)
  rowSums(as.matrix(t[cc]), na.rm = TRUE)
}

#' A custom summary whose function failed: a text column of dashes that
#' carries the error (`.failed`), which the block reports through blockr's
#' conditions. Unfaceted and uncoloured, so it is one column whatever the
#' summary asked for.
#' @noRd
lane_failed_summary <- function(s, msg) {
  list(type = "field", name = s$name, .failed = msg)
}

#' Fill parent rows with empty marks for section-style summaries
#' @noRd
lane_blank_summary <- function(target, s, sid) {
  n <- nrow(target)
  if (identical(s$type, "spans")) {
    target[[paste0(sid, "_segs")]] <- I(rep(list(list()), n))
    target[[paste0(sid, "_start")]] <- rep(NA_real_, n)
    target[[paste0(sid, "_nseg")]] <- rep(0L, n)
  }
  target
}

#' @noRd
lane_primary_col <- function(s, sid) {
  switch(s$type,
    simple = paste0(sid, "_v"),
    dist = paste0(sid, "_bc"),
    field = paste0(sid, "_t"),
    expr = paste0(sid, "_v"),
    series = paste0(sid, "_last"),
    spans = paste0(sid, "_start"),
    pair = paste0(sid, "_to"),
    paste0(sid, "_v")
  )
}

#' @noRd
lane_primary_value <- function(target, s, col) {
  v <- target[[col]]
  if (is.null(v)) {
    # expr rows may have landed as text.
    v <- target[[sub("_v$", "_t", col)]]
  }
  if (is.null(v)) rep(NA_real_, nrow(target)) else v
}

#' Shared ordering + fold + row assembly: the tail of summarize_prepare(), split
#' out so every mark reuses one definition of "rank order, cap, nest".
#' @noRd
summarize_assemble_rows <- function(leaf, par_rows, parent, sort_key, sort_dir,
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
    # Every renderer-internal column is dot-prefixed; the original data
    # columns (the group / parent keys) are not, and rbind() must not demand
    # them from both frames.
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

#' Match an arbitrary stat column from `src` into `target` by `keys`
#' (summarize_match generalized beyond `.v`; absent keys stay NA -- an absent
#' facet cell draws nothing).
#' @noRd
summarize_match_col <- function(target, src, keys, col) {
  tk <- do.call(paste, c(lapply(keys, function(k) as.character(target[[k]])),
                         list(sep = "\r")))
  sk <- do.call(paste, c(lapply(keys, function(k) as.character(src[[k]])),
                         list(sep = "\r")))
  as.numeric(src[[col]][match(tk, sk)])
}
