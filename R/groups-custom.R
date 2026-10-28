# Custom summaries: a function of `d` (rows grouped by table row) returning a
# data frame with columns named `value`, `from`/`to`, `lo`/`q1`/`mid`/`q3`/`hi`
# or `text`. One other column is the split, a row per level. It runs once per
# level of `by` and facet level and must keep the grouping. Its shape picks the
# preset row that draws it; only the fill differs (groups_custom_fill()).

GROUPS_CUSTOM_ROLES <- c("value", "from", "to", "lo", "q1", "mid", "q3", "hi",
                       "text")

# What each mark reads; the first mark whose columns are all present is the
# default.
GROUPS_CUSTOM_NEEDS <- list(
  dumbbell = c("from", "to"),
  box = "mid",
  pointrange = "mid",
  bar = "value",
  number = "value",
  text = "text"
)

#' Parse custom code (a function or a bare body using `d`) into a function.
#' @noRd
groups_custom_fn <- function(code) {
  exprs <- tryCatch(rlang::parse_exprs(paste(code, collapse = "\n")),
                    error = function(e) e)
  if (inherits(exprs, "error")) {
    return(paste0("does not parse: ", conditionMessage(exprs)))
  }
  if (!length(exprs)) return("is empty")
  env <- new.env(parent = globalenv())
  body <- if (length(exprs) == 1L) exprs[[1L]] else as.call(c(quote(`{`), exprs))
  f <- tryCatch(eval(body, env), error = function(e) NULL)
  if (is.function(f)) return(f)
  fun <- function(d) NULL
  formals(fun) <- alist(d = )
  body(fun) <- body
  environment(fun) <- env
  fun
}

#' Run a custom function on one set of rows; a data frame, or an error.
#' @noRd
groups_custom_run <- function(fn, d) {
  r <- tryCatch(fn(d), error = function(e) e)
  if (inherits(r, "error")) return(r)
  if (is.atomic(r) && !is.null(names(r)) && length(r)) {
    r <- as.data.frame(as.list(r), check.names = FALSE)
  }
  if (!is.data.frame(r)) {
    return(simpleError("must return a data frame (or a named vector)"))
  }
  as.data.frame(r, check.names = FALSE)
}

#' Learn a custom row's shape and rewrite it as its preset row.
#' @return `list(s =)` or `list(err =)`.
#' @noRd
groups_custom_setup <- function(s, data) {
  who <- paste0("Summary \"", s$name, "\": ")
  fn <- groups_custom_fn(s$fn)
  if (is.character(fn)) return(list(err = paste0(who, "the function ", fn)))
  probe <- groups_custom_run(fn, data)
  if (inherits(probe, "error")) {
    return(list(err = paste0(who, "the function failed on the whole data: ",
                             conditionMessage(probe))))
  }
  cols <- names(probe)
  sh <- groups_custom_shape(s, cols, who)
  if (!is.null(sh$err)) return(sh)
  split <- sh$split
  mark <- sh$mark
  lv <- NULL
  if (!is.null(split)) {
    # Levels from the full data when the split is a data column, so colours
    # do not depend on which cells return it.
    lv <- groups_color_levels(if (split %in% names(data)) data[[split]] else
                              probe[[split]])
    if (length(lv) > GROUPS_MAX_LEVELS) {
      return(list(err = paste0(
        who, "the split column \"", split, "\" has ", length(lv),
        " levels. At most ", GROUPS_MAX_LEVELS, "."
      )))
    }
  }
  keep <- s[intersect(names(s), c("name", "facet", "scope", "fn"))]
  # `denom`: a column whose distinct values in the slice are the N, so a count
  # reads "54 (18%)".
  denom <- chr_state(s$id)
  if (!is.null(denom) && !denom %in% names(data)) {
    return(list(err = paste0(who, "the percent column \"", denom,
                             "\" is not in the data.")))
  }
  s2 <- c(groups_custom_preset(mark, cols, denom), keep)
  if (!is.null(split) && mark %in% c("bar", "dumbbell", "box",
                                     "pointrange")) {
    s2$.color <- split
    s2$.levels <- lv
  }
  s2$.custom <- list(fn = fn, mark = mark, split = split, cols = cols,
                     levels = lv, denom = denom)
  list(s = s2)
}

# Split column and mark from the function's columns: `list(split, mark)` or
# `list(err =)`.
groups_custom_shape <- function(s, cols, who) {
  split <- setdiff(cols, GROUPS_CUSTOM_ROLES)
  if (length(split) > 1L) {
    return(list(err = paste0(
      who, "the function returns ", length(split), " columns that are not ",
      "values (", paste(split, collapse = ", "), "). One is the split; name ",
      "the others value, from, to, lo, q1, mid, q3, hi or text."
    )))
  }
  split <- if (length(split)) split
  fits <- names(GROUPS_CUSTOM_NEEDS)[vapply(GROUPS_CUSTOM_NEEDS, function(n) {
    all(n %in% cols)
  }, logical(1L))]
  if (!length(fits)) {
    return(list(err = paste0(
      who, "the function returns ", paste(cols, collapse = ", "),
      "; no mark reads that. Return value, from and to, mid, or text."
    )))
  }
  mark <- chr_state(s$show)
  if (is.null(mark)) mark <- fits[[1L]]
  if (!mark %in% fits) {
    return(list(err = paste0(
      who, "the mark \"", mark, "\" reads ",
      paste(GROUPS_CUSTOM_NEEDS[[mark]], collapse = ", "),
      "; the function returns ", paste(cols, collapse = ", "), "."
    )))
  }
  list(split = split, mark = mark)
}

groups_custom_preset <- function(mark, cols, denom) {
  count <- if (is.null(denom)) list(func = "sum") else
    list(func = "count_distinct", col = denom)
  switch(mark,
    bar = c(list(type = "simple", show = "bar"), count),
    number = c(list(type = "simple", show = "number"), count),
    text = list(type = "expr", expr = "(custom)", show = "text"),
    dumbbell = list(type = "pair", show = "dumbbell", x = "from",
                    xend = "to", x_func = "identity", xend_func = "identity"),
    list(type = "dist", col = "mid",
         style = if (identical(mark, "box")) "box" else "dot",
         summary = if (all(c("q1", "q3") %in% cols)) "median_q1_q3" else "none",
         whiskers = if (all(c("lo", "hi") %in% cols)) "min_max" else "none",
         show = mark)
  )
}

#' Run every custom summary's function on each of its facet copies, grouped
#' by the leaf keys and, in a nested table, by the parent: everything the fill
#' reads (`s$.custom$cells[[<copy sid>]]`, `leaf` and `par`). A function that
#' fails on any of them makes the summary a failed one
#' (groups_failed_summary()), the only place a failure is detected after
#' groups_custom_setup().
#' @noRd
groups_custom_runs <- function(summaries, build) {
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    if (is.null(s$.custom)) next
    runs <- list()
    for (cp in groups_summary_copies(s, i, build)) {
      # The user's function sees the input's columns, not the row tag.
      slice <- cp$slice
      slice[[GROUPS_ROW_ID]] <- NULL
      # A frame, or the failure's message.
      failed <- function(x) if (!is.data.frame(x)) x$err
      one <- list(leaf = groups_custom_cells(slice, build$keys, s))
      err <- failed(one$leaf)
      if (!is.null(build$parent) && is.null(err)) {
        one$par <- groups_custom_cells(slice, build$parent, s)
        err <- failed(one$par)
      }
      if (!is.null(err)) break
      runs[[cp$suffix]] <- one
    }
    summaries[[i]] <- if (is.null(err)) {
      s$.custom$cells <- runs
      s
    } else {
      groups_failed_summary(s, err)
    }
  }
  summaries
}

#' The function's frame for every cell of `slice`, keyed by `tkeys`. `d`
#' arrives grouped by `tkeys`. A failure is `list(err =)`.
#' @noRd
groups_custom_cells <- function(slice, tkeys, s) {
  fail <- function(...) {
    list(err = paste0("Summary \"", s$name, "\": ", ...))
  }
  g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
  r <- groups_custom_run(s$.custom$fn, g)
  if (inherits(r, "error")) {
    return(fail("the function failed: ", conditionMessage(r)))
  }
  miss <- setdiff(tkeys, names(r))
  if (length(miss)) {
    return(fail(
      "the function dropped the grouping by ", paste(miss, collapse = ", "),
      ". Keep it: add groups with group_by(..., .add = TRUE) and summarise ",
      "with dplyr, not d$."
    ))
  }
  r
}

#' Fill a custom row's columns from its runs (groups_custom_runs()): the
#' `<sid>_*` columns its preset would write.
#' @noRd
groups_custom_fill <- function(target, tkeys, s, sid, parent_row = FALSE) {
  cu <- s$.custom
  cells <- cu$cells[[sid]][[if (parent_row) "par" else "leaf"]]
  split_on <- !is.null(s$.color)
  switch(cu$mark,
    bar = groups_cfill_bar(target, tkeys, cells, cu, sid, split_on),
    number = {
      target[[groups_col(sid, "v")]] <- groups_custom_per(
        target, cells, "value", groups_sum_na, tkeys
      )
      target
    },
    text = groups_cfill_text(target, tkeys, cells, sid),
    dumbbell = groups_cfill_dumbbell(target, tkeys, cells, cu, sid, split_on),
    groups_cfill_dist(target, tkeys, cells, cu, sid, split_on)
  )
}

groups_mean_na <- function(x) mean(as.numeric(x), na.rm = TRUE)

groups_sum_na <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

groups_custom_per <- function(target, rows, col, f, tkeys) {
  if (!nrow(rows) || is.null(rows[[col]])) return(rep(NA_real_, nrow(target)))
  g <- dplyr::group_by(rows, dplyr::across(dplyr::all_of(tkeys)))
  a <- as.data.frame(dplyr::summarise(g, .v = f(.data[[col]]),
                                      .groups = "drop"))
  a$.v <- as.numeric(a$.v)
  a$.v[!is.finite(a$.v)] <- NA_real_
  groups_match_col(target, a, tkeys, ".v")
}

groups_custom_level_rows <- function(cells, cu, j) {
  if (!nrow(cells)) return(cells)
  cells[as.character(cells[[cu$split]]) == cu$levels[[j]], , drop = FALSE]
}

groups_cfill_bar <- function(target, tkeys, cells, cu, sid, split_on) {
  target[[groups_col(sid, "v")]] <- groups_custom_per(target, cells, "value",
                                                 groups_sum_na, tkeys)
  if (split_on) {
    for (j in seq_along(cu$levels)) {
      target[[groups_split_col(sid, cu$levels[[j]])]] <- groups_custom_per(
        target, groups_custom_level_rows(cells, cu, j), "value", groups_sum_na,
        tkeys
      )
    }
  }
  target
}

groups_cfill_text <- function(target, tkeys, cells, sid) {
  if (nrow(cells)) {
    g <- dplyr::group_by(cells, dplyr::across(dplyr::all_of(tkeys)))
    a <- as.data.frame(dplyr::summarise(
      g, .t = groups_field_join(.data$text), .groups = "drop"
    ))
    m <- groups_match_field(target, a, tkeys, ".t")
  } else {
    m <- rep(NA_character_, nrow(target))
  }
  m[is.na(m)] <- ""
  target[[groups_col(sid, "t")]] <- m
  target
}

groups_cfill_ends <- function(target, rows, prefix, tkeys) {
  n <- nrow(target)
  target[[groups_col(prefix, "from")]] <- groups_custom_per(target, rows, "from",
                                                       groups_mean_na, tkeys)
  target[[groups_col(prefix, "to")]] <- groups_custom_per(target, rows, "to",
                                                     groups_mean_na, tkeys)
  target[[groups_col(prefix, "lo")]] <- rep(NA_real_, n)
  target[[groups_col(prefix, "hi")]] <- rep(NA_real_, n)
  target[[groups_col(prefix, "d")]] <- rep(NA_integer_, n)
  target
}

groups_cfill_dumbbell <- function(target, tkeys, cells, cu, sid, split_on) {
  # The pooled pair (sort key, value label) is the mean of the cell's rows.
  target <- groups_cfill_ends(target, cells, sid, tkeys)
  target[[groups_col(sid, "f")]] <- rep(NA_integer_, nrow(target))
  if (split_on) {
    for (j in seq_along(cu$levels)) {
      target <- groups_cfill_ends(target, groups_custom_level_rows(cells, cu, j),
                                groups_level_sid(sid, j), tkeys)
    }
  }
  target
}

groups_cfill_put <- function(target, rows, prefix, tkeys) {
  stats <- c(bc = "mid", bl = "q1", bh = "q3", wl = "lo", wh = "hi")
  for (k in names(stats)) {
    target[[groups_col(prefix, k)]] <- groups_custom_per(
      target, rows, stats[[k]], groups_mean_na, tkeys
    )
  }
  target
}

groups_cfill_dist <- function(target, tkeys, cells, cu, sid, split_on) {
  target <- groups_cfill_put(target, cells, sid, tkeys)
  if (split_on) {
    for (j in seq_along(cu$levels)) {
      target <- groups_cfill_put(target, groups_custom_level_rows(cells, cu, j),
                               groups_level_sid(sid, j), tkeys)
    }
  }
  target
}

#' Words for a custom row's plan entry; the preset's name statistics the
#' function did not compute.
#' @noRd
groups_custom_plan <- function(entry, s, cp) {
  cu <- s$.custom
  # A count keeps the preset's second header line (its N); the other marks'
  # would name statistics the function did not compute.
  if (!cu$mark %in% c("bar", "number")) {
    entry$sub_label <- if (!is.null(cp$level)) {
      if (is.null(cp$pop)) s$name else paste0(s$name, " \u00b7 N = ", cp$pop)
    }
  }
  if (cu$mark %in% c("box", "pointrange")) {
    entry$words <- list(
      center = "mid",
      range = if (!identical(s$summary, "none")) "q1\u2013q3",
      whisk = if (!identical(s$whiskers, "none")) "lo\u2013hi"
    )
  }
  if (identical(cu$mark, "dumbbell")) {
    entry$words <- list(from = "from \u25c7", to = "to \u25cf")
  }
  entry$stype <- "custom"
  entry
}
