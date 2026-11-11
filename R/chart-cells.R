# What an aggregated chart (bar, histogram, waterfall, pie, treemap, boxplot,
# point range) sends in place of its rows: one row per mark. The browser
# (inst/js/chart/model-aggregated.js) draws from it and aggregates nothing,
# so the payload grows with the marks, not with the input's rows.
#
# A cell is a combination of the facet columns, the category and the colour.
# It travels under those column names, as a row does, so what the browser
# does by key (the grid key, the row index, the click's claim, the legend)
# reads a cell as it reads a row. The cells come in the order of each
# cell's first row. Beside the keys:
#
#   .n          rows in the cell
#   .value      the aggregate; a count reads .n
#   .drop       with missing = "drop": a cell with a missing key. It is not
#               drawn, but "(n)" labels and tooltips still count its rows.
#   .sort       sort_by names a column: its smallest value in the cell
#   .tt.<col>   a tooltip field: the value in the cell's first row
#   .label      `label`: the cell's distinct values, two at most (two say
#               the rows differ)
#   .hl         `highlight` on a column that is no key: a row matches
#   <id>        `id` under show_n: the distinct ids in the cell (a list)
#   <drill>     a drill column that is no key: its distinct values (a list)
#
# A histogram's category is the bin column chart_hist_data() adds, and its
# value a count. A boxplot or point range has `.value`, the mean, for a sort
# by value, and what its mark draws (chart_dist_stats()):
#
#   .vn                 the values in the cell
#   .count              `id`: the distinct ids among them
#   .lo .center .hi     the `summary` statistic: the box or the interval
#   .wlo .whi           a boxplot's whiskers
#   .out                box_points = "outliers": the values beyond the
#                       whiskers (a list). The one column that can grow with
#                       the rows: every outlier is sent, as every one is drawn.
#
# The numbers are table_aggregate()'s and chart_dist_stats()'s, the engines
# of the chart's Summary table and its Excel download.

CHART_CELL_TYPES <- c(CHART_AGGREGATED_TYPES, "histogram")

# The arguments chart_cells() reads, or NULL for a chart that sends rows.
# The server keys its cache on these and on the columns they name.
chart_cells_args <- function(v, data) {
  type <- v$type %||% "bar"
  if (!type %in% CHART_CELL_TYPES || !is.data.frame(data) || !nrow(data)) {
    return(NULL)
  }
  hist <- identical(type, "histogram")
  cat <- chart_col_in(data, if (hist) chart_bin_col(v$x) else chart_category(v))
  if (!length(cat)) {
    return(NULL)
  }
  dist <- type %in% CHART_DISTRIBUTION_TYPES
  col <- function(x) {
    x <- as.character(unlist(x))
    x[nzchar(x) & x %in% names(data)]
  }
  value <- if (!hist) col(chart_value_col(v))
  if (dist && length(value) && !is.numeric(data[[value]])) {
    value <- character()
  }
  func <- if (!length(value)) "count" else if (dist) "mean" else v$func %||% "count"
  box <- identical(type, "boxplot")
  sort <- col(v$sort_by)
  counted <- isTRUE(v$show_n %in% c("axis", "facet", "both"))
  drill <- if (!identical(v$drill, "auto")) col(v$drill)
  hl <- v$highlight
  hl_col <- if (is.list(hl) && length(hl)) col(names(hl)[[1L]])
  list(
    facets = chart_facets(v, data), category = cat,
    color = chart_split_in(data, v$color),
    func = func, value = value,
    drop = identical(v$missing, "drop"),
    percent_of = v$percent_of,
    sort = if (length(sort)) sort[[1L]],
    tooltip = col(v$tooltip),
    label = col(v$label),
    id = if (counted) col(v$id),
    dist = if (dist && length(value)) {
      list(summary = v$summary %||% if (box) "median_q1_q3" else "mean_se",
           whiskers = if (box) v$whiskers %||% "tukey",
           points = box && identical(v$box_points, "outliers"),
           id = col(v$id))
    },
    drill = drill,
    hl_col = hl_col,
    hl_values = if (length(hl_col)) chart_labelled_values(hl[[1L]])
  )
}

# The columns of `data` the cells are computed from.
chart_cells_cols <- function(a) {
  unique(c(unlist(a[c("facets", "category", "color", "value", "sort",
                      "tooltip", "label", "id", "drill", "hl_col")],
                  use.names = FALSE), a$dist$id))
}

# The values of a highlight: plain, or list(value, label) entries.
chart_labelled_values <- function(x) {
  vals <- lapply(x, function(e) if (is.list(e)) e$value %||% e[[1L]] else e)
  vals <- as.character(unlist(vals))
  vals[!is.na(vals) & nzchar(vals)]
}

#' One row per mark of an aggregated chart
#'
#' @param data The rows the chart draws (`src$plain()`).
#' @param a The arguments, from chart_cells_args().
#' @return A data frame; `<id>`, `<drill>` and `.label` are list columns.
#' @noRd
chart_cells <- function(data, a) {
  keys <- unique(c(a$facets, a$category, a$color))
  n <- nrow(data)
  # A cell's code per key column: NA and "" are one level, as the browser
  # reads both as a missing key.
  codes <- lapply(data[keys], cell_codes)
  id <- cell_combine(codes)
  first <- which(!duplicated(id))
  g <- match(id, id[first])
  cells <- length(first)

  out <- data[first, keys, drop = FALSE]
  rownames(out) <- NULL
  out$.n <- tabulate(g, cells)
  if (!identical(a$func, "count")) {
    out$.value <- cell_values(data, g, cells, codes, a)
  }
  if (!is.null(a$dist)) {
    out <- cbind(out, cell_dist(data, g, cells, a))
  }
  if (a$drop) {
    missing <- Reduce(`|`, lapply(codes, function(k) attr(k, "missing")[k]))
    if (any(missing[first])) out$.drop <- missing[first]
  }
  if (length(a$sort)) {
    out$.sort <- cell_min(cell_number(data[[a$sort]]), g, cells)
  }
  for (f in a$tooltip) {
    out[[paste0(".tt.", f)]] <- data[[f]][first]
  }
  if (length(a$label)) {
    lab <- cell_text(data[[a$label]])
    lab[lab %in% c("", "NA")] <- NA
    out$.label <- cell_distinct(lab, g, cells, most = 2L)
  }
  if (length(a$hl_col) && !a$hl_col %in% keys && length(a$hl_values)) {
    hit <- cell_text(data[[a$hl_col]]) %in% a$hl_values
    out$.hl <- tabulate(g[hit], cells) > 0L
  }
  for (col in setdiff(unique(c(a$id, a$drill)), keys)) {
    x <- data[[col]]
    out[[col]] <- cell_distinct(if (is.numeric(x)) x else cell_text(x), g,
                                cells)
  }
  structure(out, blockr_cells = TRUE)
}

is_chart_cells <- function(df) isTRUE(attr(df, "blockr_cells"))

# The distinct values of a column the gear lists, as the browser listed them
# from the rows: in order of appearance, 500 at most.
chart_levels <- function(x) {
  lv <- unique(cell_text(x))
  utils::head(lv[!is.na(lv) & nzchar(lv)], 500L)
}

# The aggregate per cell. table_aggregate() groups by the cell index, so
# its rows are the cells in order.
cell_values <- function(data, g, cells, codes, a) {
  x <- data[[a$value]]
  if (identical(a$func, "identity")) {
    num <- cell_number(x, missing = NA_real_)
    ok <- !is.na(num)
    return(num[ok][match(seq_len(cells), g[ok])])
  }
  if (identical(a$func, "pct_distinct")) {
    role <- c(facet = list(a$facets), group = list(a$category),
              color = list(a$color))
    of <- as.character(a$percent_of %||% "facet")
    of[of == "category"] <- "group"
    pop <- cell_combine(codes[unlist(role[intersect(of, names(role))])],
                        length(g))
    pop <- match(pop, unique(pop))
    den <- cell_n_distinct(x, pop, max(pop))[pop[match(seq_len(cells), g)]]
    num <- cell_n_distinct(x, g, cells)
    return(ifelse(den > 0, num / den, NA_real_))
  }
  d <- data.frame(.cell = g)
  d[[a$value]] <- x
  agg <- table_aggregate(d, ".cell", chart_summaries(a$func, a$value, d))
  val <- agg$data[[a$value]]
  if (is.null(val)) {
    return(rep(NA_real_, cells))
  }
  val <- as.numeric(val)
  val[is.nan(val)] <- NA_real_
  val
}

# What a boxplot or point range draws per cell, as columns.
cell_dist <- function(data, g, cells, a) {
  x <- cell_number(data[[a$value]], missing = NA_real_)
  ok <- !is.na(x)
  rows <- split(which(ok), factor(g[ok], levels = seq_len(cells)))
  stats <- lapply(rows, function(i) {
    chart_dist_stats(x[i], a$dist$summary, a$dist$whiskers,
                     points = a$dist$points)
  })
  num <- function(nm) {
    vapply(stats, function(s) s[[nm]] %||% NA_real_, numeric(1L),
           USE.NAMES = FALSE)
  }
  out <- data.frame(.vn = as.integer(num("n")))
  if (length(a$dist$id)) {
    id <- data[[a$dist$id]]
    id[!ok] <- NA
    out$.count <- cell_n_distinct(id, g, cells)
  }
  for (nm in c("lo", "center", "hi", if (!is.null(a$dist$whiskers)) c("wlo", "whi"))) {
    out[[paste0(".", nm)]] <- num(nm)
  }
  if (a$dist$points) {
    out$.out <- unname(lapply(stats, function(s) s$out %||% numeric()))
  }
  out
}

# The `summaries` table_aggregate() takes for a chart's func and value.
chart_summaries <- function(func, value, data) {
  func <- func %||% "count"
  if (identical(func, "count") || !length(value) || !value %in% names(data)) {
    return(list(list(func = "count", cols = character())))
  }
  list(list(func = func, cols = value))
}

# Integer codes of a key column, with which codes stand for a missing key
# (NA or "") as attribute `missing`.
cell_codes <- function(x) {
  txt <- if (is.factor(x)) levels(x) else NULL
  code <- if (is.factor(x)) as.integer(x) else match(x, unique(x))
  if (is.null(txt)) {
    txt <- as.character(unique(x))
  }
  gone <- is.na(txt) | !nzchar(txt)
  # One code for every missing value.
  spare <- length(txt) + 1L
  code[is.na(code) | code %in% which(gone)] <- spare
  structure(code, missing = c(gone, TRUE), size = spare)
}

# One number per row from several code vectors; all ones without any.
cell_combine <- function(codes, n = length(codes[[1L]])) {
  id <- rep(1, n)
  size <- 1
  for (k in codes) {
    id <- id + (k - 1) * size
    size <- size * attr(k, "size")
  }
  id
}

# A column as the browser's Number() read it off a row: a missing value is
# 0 (`missing`), text that is no number is NA.
cell_number <- function(x, missing = 0) {
  num <- if (is.numeric(x) || is.logical(x)) {
    as.numeric(x)
  } else {
    suppressWarnings(as.numeric(as.character(x)))
  }
  num[is.na(x)] <- missing
  num
}

# A column as text, the way its values travel (wire_values()).
cell_text <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (inherits(x, "POSIXct")) {
    return(as.character(as.numeric(x) * 1000))
  }
  as.character(x)
}

# The smallest value per cell, NA for a cell without one.
cell_min <- function(x, g, cells) {
  o <- order(g, x)
  x[o][match(seq_len(cells), g[o])]
}

# The number of distinct non-missing values per group.
cell_n_distinct <- function(x, g, groups) {
  ok <- !is.na(x)
  code <- match(x[ok], unique(x[ok]))
  tabulate(g[ok][!duplicated(g[ok] + (code - 1) * groups)], groups)
}

# The distinct non-missing values per cell, in order of appearance, as a
# list; `most` cuts each to its first values.
cell_distinct <- function(x, g, cells, most = NULL) {
  ok <- !is.na(x)
  x <- x[ok]
  g <- g[ok]
  code <- match(x, unique(x))
  keep <- !duplicated(g + (code - 1) * cells)
  out <- unname(split(x[keep], factor(g[keep], levels = seq_len(cells))))
  if (!is.null(most)) {
    out <- lapply(out, utils::head, most)
  }
  out
}

# The cells as the JSON the payload carries: the plain columns as the rows'
# columns travel (wire_df_parts()), the list columns as arrays.
chart_cells_json <- function(cells) {
  lists <- vapply(cells, is.list, logical(1L))
  cols <- wire_df_parts(cells[!lists], warn = TRUE, meta = FALSE)$cols
  for (nm in names(cells)[lists]) {
    cols[[nm]] <- unname(cells[[nm]])
  }
  jsonlite::toJSON(cols[intersect(names(cells), names(cols))], na = "null",
                   null = "null", digits = NA)
}

# The numbers of one box or point range: N, the `inner` statistic (the box or
# the interval), with `outer` a boxplot's whiskers, and with `points` the
# values beyond them, in the order given. One implementation for what the
# chart draws (chart_cells()) and what its Summary table lists
# (chart_summary_dist()).
chart_dist_stats <- function(x, inner, outer = NULL, points = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  s <- summarize_stat(x, inner)
  w <- if (!is.null(outer)) summarize_stat(x, outer)
  out <- list(n = length(x))
  if (!is.null(outer)) out$wlo <- w$lo %||% NA_real_
  out$lo <- s$lo %||% NA_real_
  out$center <- s$center %||% NA_real_
  out$hi <- s$hi %||% NA_real_
  if (!is.null(outer)) out$whi <- w$hi %||% NA_real_
  if (points && !is.null(w)) out$out <- x[x < w$lo | x > w$hi]
  out
}
