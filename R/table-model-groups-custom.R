# Table model, groups layout: custom summaries ---------------------------------
#
# A custom row is a function of the rows, grouped by table row, that returns
# a small data frame. Its columns name what they are: `value`; `from`, `to`;
# `lo`, `q1`, `mid`, `q3`, `hi`; `text`. One more column, if there is one, is
# the split: the frame has a row per level of it, and each level draws its own
# mark (the colour split of the preset rows). The block runs the function once
# per level of `by` and per facet level, with `d` grouped by that level's
# columns. A function that keeps the grouping (dplyr verbs, `.add = TRUE`)
# computes every table row of the level in one call, and a nested table needs
# nothing more from it: a worst-grade count is right for the SOC and for its
# terms alike, because each level reduces its own rows.
#
# The function is read once over the whole data to learn its shape, and the
# row then stands in as the preset row that draws that shape (a sum bar, a
# pair, a distribution, an expr cell). Plan, sort, domains and every renderer
# are the presets'; only the fill differs (lane_custom_fill()).

LANE_CUSTOM_ROLES <- c("value", "from", "to", "lo", "q1", "mid", "q3", "hi",
                       "text")

# What each mark reads. The first mark whose columns are all present is the
# default.
LANE_CUSTOM_NEEDS <- list(
  dumbbell = c("from", "to"),
  box = "mid",
  pointrange = "mid",
  bar = "value",
  number = "value",
  text = "text"
)

#' Parse a custom row's code into a function of one argument. Takes a
#' function (`\(d) d |> ...`) or a bare body that uses `d`.
#' @noRd
lane_custom_fn <- function(code) {
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
  # A bare body: `d` is the rows, grouped by table row.
  fun <- function(d) NULL
  formals(fun) <- alist(d = )
  body(fun) <- body
  environment(fun) <- env
  fun
}

#' Run a custom function on one set of rows; a data frame, or an error.
#' @noRd
lane_custom_run <- function(fn, d) {
  r <- tryCatch(fn(d), error = function(e) e)
  if (inherits(r, "error")) return(r)
  if (is.atomic(r) && !is.null(names(r)) && length(r)) {
    # A named vector reads as one row.
    r <- as.data.frame(as.list(r), check.names = FALSE)
  }
  if (!is.data.frame(r)) {
    return(simpleError("must return a data frame (or a named vector)"))
  }
  as.data.frame(r, check.names = FALSE)
}

#' Learn a custom row's shape and rewrite it as the preset row that draws it.
#' Returns `list(s =)` or `list(err =)`.
#' @noRd
lane_custom_setup <- function(s, data) {
  who <- paste0("Summary \"", s$name, "\": ")
  fn <- lane_custom_fn(s$fn)
  if (is.character(fn)) return(list(err = paste0(who, "the function ", fn)))
  probe <- lane_custom_run(fn, data)
  if (inherits(probe, "error")) {
    return(list(err = paste0(who, "the function failed on the whole data: ",
                             conditionMessage(probe))))
  }
  cols <- names(probe)
  sh <- lane_custom_shape(s, cols, who)
  if (!is.null(sh$err)) return(sh)
  split <- sh$split
  mark <- sh$mark
  lv <- NULL
  if (!is.null(split)) {
    # Levels from the full data where the split is a data column, so a
    # level's colour does not depend on which cells happen to return it.
    lv <- summarize_color_levels(if (split %in% names(data)) data[[split]] else
                              probe[[split]])
    if (length(lv) > LANE_MAX_LEVELS) {
      return(list(err = paste0(
        who, "the split column \"", split, "\" has ", length(lv),
        " levels. At most ", LANE_MAX_LEVELS, "."
      )))
    }
  }
  keep <- s[intersect(names(s), c("name", "facet", "scope", "fn"))]
  # `denom`: a column whose distinct values in the cell's slice are the N. A
  # count then reads as the presets' counts do: "54 (18%)", N in the header,
  # a bar as long as its share. The value stays the function's.
  denom <- summarize_chr1(s$denom)
  if (!is.null(denom) && !denom %in% names(data)) {
    return(list(err = paste0(who, "the percent column \"", denom,
                             "\" is not in the data.")))
  }
  s2 <- c(lane_custom_preset(mark, cols, denom), keep)
  if (!is.null(split) && mark %in% c("bar", "dumbbell", "box",
                                     "pointrange")) {
    s2$.color <- split
    s2$.levels <- lv
  }
  s2$.custom <- list(fn = fn, mark = mark, split = split, cols = cols,
                     levels = lv, denom = denom)
  list(s = s2)
}

# The split column and the mark of a custom row, from the columns its
# function returns. Returns `list(split, mark)` or `list(err =)`.
lane_custom_shape <- function(s, cols, who) {
  split <- setdiff(cols, LANE_CUSTOM_ROLES)
  if (length(split) > 1L) {
    return(list(err = paste0(
      who, "the function returns ", length(split), " columns that are not ",
      "values (", paste(split, collapse = ", "), "). One is the split; name ",
      "the others value, from, to, lo, q1, mid, q3, hi or text."
    )))
  }
  split <- if (length(split)) split
  fits <- names(LANE_CUSTOM_NEEDS)[vapply(LANE_CUSTOM_NEEDS, function(n) {
    all(n %in% cols)
  }, logical(1L))]
  if (!length(fits)) {
    return(list(err = paste0(
      who, "the function returns ", paste(cols, collapse = ", "),
      "; no mark reads that. Return value, from and to, mid, or text."
    )))
  }
  mark <- summarize_chr1(s$show)
  if (is.null(mark)) mark <- fits[[1L]]
  if (!mark %in% fits) {
    return(list(err = paste0(
      who, "the mark \"", mark, "\" reads ",
      paste(LANE_CUSTOM_NEEDS[[mark]], collapse = ", "),
      "; the function returns ", paste(cols, collapse = ", "), "."
    )))
  }
  list(split = split, mark = mark)
}

# The preset row that draws a custom row's mark.
lane_custom_preset <- function(mark, cols, denom) {
  count <- if (is.null(denom)) list(func = "sum") else
    list(func = "count_distinct", col = denom)
  switch(mark,
    bar = c(list(type = "simple", show = "bar"), count),
    number = c(list(type = "simple", show = "number"), count),
    text = list(type = "expr", expr = "(custom)", show = "text"),
    dumbbell = list(type = "pair", show = "dumbbell", from = "from",
                    to = "to", from_func = "identity", to_func = "identity"),
    {
      inner <- if (all(c("q1", "q3") %in% cols)) "median_q1_q3" else "none"
      outer <- if (all(c("lo", "hi") %in% cols)) "min_max" else "none"
      list(type = "dist", col = "mid", style = if (identical(mark, "box"))
             "box" else "dot", inner = inner, outer = outer,
           stat = if (identical(inner, "none")) NULL else inner,
           whiskers = if (identical(outer, "none")) NULL else outer,
           show = mark)
    }
  )
}

#' The function's frames for every cell of `slice`, keyed by `tkeys`. One
#' call per level: `d` arrives grouped by `tkeys`, and the function keeps that
#' grouping, so its result carries the key columns that place each value.
#' A failure is a `lane_custom_error`, which lane_prepare_summaries() turns
#' into the table's error message.
#' @noRd
lane_custom_cells <- function(slice, tkeys, s) {
  fail <- function(...) {
    stop(errorCondition(paste0("Summary \"", s$name, "\": ", ...),
                        class = "lane_custom_error"))
  }
  g <- dplyr::group_by(slice, dplyr::across(dplyr::all_of(tkeys)))
  r <- lane_custom_run(s$.custom$fn, g)
  if (inherits(r, "error")) {
    fail("the function failed: ", conditionMessage(r))
  }
  miss <- setdiff(tkeys, names(r))
  if (length(miss)) {
    fail("the function dropped the grouping by ",
         paste(miss, collapse = ", "), ". Keep it: add groups with ",
         "group_by(..., .add = TRUE) and summarise with dplyr, not d$.")
  }
  r
}

#' Fill a custom row's columns: the same `<sid>_*` columns the preset it
#' stands in for would write, so the plan reads them unchanged.
#' @noRd
lane_custom_fill <- function(target, tkeys, slice, s, sid) {
  # The user's function sees the input's columns, not the row tag.
  slice[[SUMMARIZE_ROW_ID]] <- NULL
  cu <- s$.custom
  cells <- lane_custom_cells(slice, tkeys, s)
  split_on <- !is.null(s$.color)
  switch(cu$mark,
    bar = lane_cfill_bar(target, tkeys, cells, cu, sid, split_on),
    number = {
      target[[paste0(sid, "_v")]] <- lane_custom_per(
        target, cells, "value", lane_sum_na, tkeys
      )
      target
    },
    text = lane_cfill_text(target, tkeys, cells, sid),
    dumbbell = lane_cfill_dumbbell(target, tkeys, cells, cu, sid, split_on),
    lane_cfill_dist(target, tkeys, cells, cu, sid, split_on)
  )
}

lane_mean_na <- function(x) mean(as.numeric(x), na.rm = TRUE)

lane_sum_na <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

# One statistic of a custom column per target row.
lane_custom_per <- function(target, rows, col, f, tkeys) {
  if (!nrow(rows) || is.null(rows[[col]])) return(rep(NA_real_, nrow(target)))
  g <- dplyr::group_by(rows, dplyr::across(dplyr::all_of(tkeys)))
  a <- as.data.frame(dplyr::summarise(g, .v = f(.data[[col]]),
                                      .groups = "drop"))
  a$.v <- as.numeric(a$.v)
  a$.v[!is.finite(a$.v)] <- NA_real_
  summarize_match_col(target, a, tkeys, ".v")
}

# The function's frame rows of the j-th split level.
lane_custom_level_rows <- function(cells, cu, j) {
  if (!nrow(cells)) return(cells)
  cells[as.character(cells[[cu$split]]) == cu$levels[[j]], , drop = FALSE]
}

lane_cfill_bar <- function(target, tkeys, cells, cu, sid, split_on) {
  target[[paste0(sid, "_v")]] <- lane_custom_per(target, cells, "value",
                                                 lane_sum_na, tkeys)
  if (split_on) {
    for (j in seq_along(cu$levels)) {
      target[[paste0(sid, "_S_", cu$levels[[j]])]] <- lane_custom_per(
        target, lane_custom_level_rows(cells, cu, j), "value", lane_sum_na,
        tkeys
      )
    }
  }
  target
}

lane_cfill_text <- function(target, tkeys, cells, sid) {
  if (nrow(cells)) {
    g <- dplyr::group_by(cells, dplyr::across(dplyr::all_of(tkeys)))
    a <- as.data.frame(dplyr::summarise(
      g, .t = lane_field_join(.data$text), .groups = "drop"
    ))
    m <- summarize_match_field(target, a, tkeys, ".t")
  } else {
    m <- rep(NA_character_, nrow(target))
  }
  m[is.na(m)] <- ""
  target[[paste0(sid, "_t")]] <- m
  target
}

# The ends of one dumbbell under `prefix`.
lane_cfill_ends <- function(target, rows, prefix, tkeys) {
  n <- nrow(target)
  target[[paste0(prefix, "_from")]] <- lane_custom_per(target, rows, "from",
                                                       lane_mean_na, tkeys)
  target[[paste0(prefix, "_to")]] <- lane_custom_per(target, rows, "to",
                                                     lane_mean_na, tkeys)
  target[[paste0(prefix, "_lo")]] <- rep(NA_real_, n)
  target[[paste0(prefix, "_hi")]] <- rep(NA_real_, n)
  target[[paste0(prefix, "_d")]] <- rep(NA_integer_, n)
  target
}

lane_cfill_dumbbell <- function(target, tkeys, cells, cu, sid, split_on) {
  # The pooled pair (the sort key, and the value label where a row draws
  # one level) is the mean of the cell's rows.
  target <- lane_cfill_ends(target, cells, sid, tkeys)
  target[[paste0(sid, "_f")]] <- rep(NA_integer_, nrow(target))
  if (split_on) {
    for (j in seq_along(cu$levels)) {
      target <- lane_cfill_ends(target, lane_custom_level_rows(cells, cu, j),
                                paste0(sid, "_L", j), tkeys)
    }
  }
  target
}

# The statistics of one box or point range under `prefix`.
lane_cfill_put <- function(target, rows, prefix, tkeys) {
  stats <- c(bc = "mid", bl = "q1", bh = "q3", wl = "lo", wh = "hi")
  for (k in names(stats)) {
    target[[paste0(prefix, "_", k)]] <- lane_custom_per(
      target, rows, stats[[k]], lane_mean_na, tkeys
    )
  }
  target
}

lane_cfill_dist <- function(target, tkeys, cells, cu, sid, split_on) {
  target <- lane_cfill_put(target, cells, sid, tkeys)
  if (split_on) {
    for (j in seq_along(cu$levels)) {
      target <- lane_cfill_put(target, lane_custom_level_rows(cells, cu, j),
                               paste0(sid, "_L", j), tkeys)
    }
  }
  target
}

#' Words for a custom row's plan entry: the preset's describe the preset's
#' statistics ("Median", "Q1-Q3"), which a custom function did not compute.
#' @noRd
lane_custom_plan <- function(entry, s, cp) {
  cu <- s$.custom
  # A count keeps the preset's second header line (its N); the other marks'
  # preset lines name statistics the function did not compute.
  if (!cu$mark %in% c("bar", "number")) {
    entry[names(entry) == "sub_label"] <- NULL
    entry$sub_label <- if (!is.null(cp$level)) {
      if (is.null(cp$pop)) s$name else paste0(s$name, " \u00b7 N = ", cp$pop)
    }
  }
  if (cu$mark %in% c("box", "pointrange")) {
    entry$words <- list(
      center = "mid",
      range = if (!identical(s$inner, "none")) "q1\u2013q3",
      whisk = if (!identical(s$outer, "none")) "lo\u2013hi"
    )
  }
  if (identical(cu$mark, "dumbbell")) {
    entry$words <- list(from = "from \u25c7", to = "to \u25cf")
  }
  entry$stype <- "custom"
  entry
}
