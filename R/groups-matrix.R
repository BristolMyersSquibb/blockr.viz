# A faceted summary built in one pass over the data. A facet repeats its
# summary once per level, and a matrix (a tile count faceted by a column of
# 200 terms) has as many copies as columns. groups_build_summary() slices the
# data, aggregates and matches once per copy; here the rows are tabulated once
# by row key and facet level, and every copy's columns are read off that
# table. The frames and the plan come out as groups_build_summary() builds
# them.

# The facet level of each data row, as a column of the aggregated frame.
GROUPS_FACET_IDX <- ".blockr_facet"

# A summary this file builds: a plain value per row (`simple`), faceted, with
# no colour split. A facet column with missing values keeps the per-copy
# path, whose slices carry those rows.
groups_matrix_ok <- function(s, build) {
  is.null(s$.failed) && is.null(s$.custom) &&
    identical(s$type, "simple") && length(s$.flevels) > 0L &&
    is.null(s$.color) && !is.null(s$.facet) &&
    !anyNA(build$data[[s$.facet]])
}

# groups_build_summary() for such a summary.
groups_build_matrix <- function(st, i, build) {
  s <- st$summaries[[i]]
  data <- build$data
  fl <- s$.flevels
  sids <- groups_copy_sid(i, seq_along(fl))
  fj <- match(as.character(data[[s$.facet]]), fl)

  st$leaf <- groups_matrix_fill(st$leaf, build$keys, data, s, sids, fj)
  if (!is.null(st$par_rows)) {
    st$par_rows <- groups_matrix_fill(st$par_rows, build$parent, data, s,
                                      sids, fj)
  }

  den <- groups_matrix_denom(s, build, fj)
  pop <- groups_matrix_distinct(groups_pop_column(data, build$subject), fj,
                                length(fl))
  # The paint is the same for every copy.
  if (identical(s$show, "tile") && !is.null(s$.fill)) {
    s$.fills <- unname(groups_tile_colors(build$scale_map, s$.fill,
                                          s$.fill_levels, data[[s$.fill]]))
    s$.fvar <- groups_col_name(data, s$.fill)
  }
  entries <- lapply(seq_along(fl), function(j) {
    cp <- list(suffix = sids[[j]], level = fl[[j]], fcol = s$.facet,
               qualify = build$qualify, named = build$n_faceted > 1L,
               den = if (!is.null(den)) den[[j]], pop = pop[[j]])
    groups_summary_plan(s, cp, data, build$scale_map)
  })
  st$plan <- c(st$plan, entries)
  if (is.null(st$primary)) {
    st$primary <- list(s = st$summaries[[i]], i = i, sid = sids[[1L]])
  }
  st
}

# The value and, for a painted tile, the worst `fill` level of every copy on
# one target frame (the leaf rows, or the parent rows): groups_fill_simple()
# for all the copies together.
groups_matrix_fill <- function(target, tkeys, data, s, sids, fj) {
  nf <- length(sids)
  f <- chr_state(s$func) %||% "count"
  col <- chr_state(s$col)
  tk <- groups_key_string(target, tkeys)

  data[[GROUPS_FACET_IDX]] <- fj
  agg <- groups_aggregate(data, c(tkeys, GROUPS_FACET_IDX), f, col, col)
  v <- groups_matrix_spread(groups_key_string(agg, tkeys),
                            agg[[GROUPS_FACET_IDX]], as.numeric(agg$.v),
                            tk, nf, NA_real_)
  cols <- lapply(seq_len(nf), function(j) v[, j])
  names(cols) <- groups_col(sids, "v")

  if (!is.null(s$.fill)) {
    w <- groups_matrix_worst(tk, groups_key_string(data, tkeys), fj, nf,
                             match(as.character(data[[s$.fill]]),
                                   s$.fill_levels))
    wcols <- lapply(seq_len(nf), function(j) w[, j])
    names(wcols) <- groups_col(sids, "w")
    # A copy's value, then its worst level, as groups_fill_simple() adds them.
    cols <- c(rbind(cols, wcols))
    names(cols) <- c(rbind(groups_col(sids, "v"), groups_col(sids, "w")))
  }
  groups_add_columns(target, cols)
}

# Values known by source key and facet level, as a matrix with one row per
# target key and one column per level; `empty` where the source has none.
# A target key reads the first source key equal to it, as
# groups_match_field() does.
groups_matrix_spread <- function(sk, fj, value, tk, nf, empty) {
  u <- unique(sk)
  m <- matrix(empty, nrow = length(u), ncol = nf)
  ok <- !is.na(fj)
  m[cbind(match(sk[ok], u), fj[ok])] <- value[ok]
  m[match(tk, u), , drop = FALSE]
}

# The worst `fill` level index per target row and facet level; NA where the
# cell has no row with a level. groups_tile_worst() for all the copies.
groups_matrix_worst <- function(tk, sk, fj, nf, idx) {
  ok <- which(!is.na(idx) & !is.na(fj))
  u <- unique(sk)
  cell <- match(sk[ok], u) + (fj[ok] - 1) * length(u)
  o <- order(cell, idx[ok])
  last <- !duplicated(cell[o], fromLast = TRUE)
  m <- matrix(NA_integer_, nrow = length(u), ncol = nf)
  m[cell[o][last]] <- idx[ok][o][last]
  out <- m[match(tk, u), , drop = FALSE]
  # groups_tile_worst() looks a row up by its key as a name, and an empty
  # name finds nothing.
  out[!nzchar(tk), ] <- NA_integer_
  out
}

# Each copy's denominator, as groups_copy_denom() gives it: the count's base
# over the copy's rows, or over the whole table. NULL when the summary has no
# percentage.
groups_matrix_denom <- function(s, build, fj) {
  func <- chr_state(s$func) %||% "count"
  same_n <- build$all && !build$pct_total
  if (same_n || !groups_has_pct(func)) {
    return(NULL)
  }
  nf <- length(s$.flevels)
  data <- build$data
  id_var <- chr_state(s$col)
  if (build$pct_total) {
    return(rep(groups_denom(data, func, id_var), nf))
  }
  x <- if (identical(func, "count_distinct") && !is.null(id_var) &&
             id_var %in% names(data)) {
    data[[id_var]]
  } else {
    data[[GROUPS_ROW_ID]]
  }
  groups_matrix_distinct(x, fj, nf)
}

# The column a facet level's N counts (groups_pop_n()): the subject, else the
# tagged input rows, else NULL for the rows themselves.
groups_pop_column <- function(data, subject) {
  subject <- chr_state(subject)
  if (!is.null(subject) && subject %in% names(data)) {
    data[[subject]]
  } else {
    data[[GROUPS_ROW_ID]]
  }
}

# Distinct values of `x` per facet level (a missing value is one value);
# rows per level when `x` is NULL.
groups_matrix_distinct <- function(x, fj, nf) {
  if (is.null(x)) {
    return(tabulate(fj, nbins = nf))
  }
  xi <- match(x, unique(x))
  tabulate(fj[!duplicated(cbind(fj, xi))], nbins = nf)
}

# Append named columns to a frame in one step.
groups_add_columns <- function(target, cols) {
  out <- unclass(target)
  at <- attributes(target)
  out[names(cols)] <- cols
  at$names <- names(out)
  attributes(out) <- at
  out
}
