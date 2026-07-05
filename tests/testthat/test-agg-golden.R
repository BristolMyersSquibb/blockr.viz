# Golden cross-test: the JS aggregation engine (Blockr.DrilldownAgg.aggregate,
# inst/js/drilldown-agg.js -- the chart's client-side path) and the R engine
# (dd_table_aggregate / dd_metric_plan, R/table-block.R -- the table + tile
# server-side path) MUST produce the same numbers for the same data. The
# name-drift tests (test-agg-fns-drift.R) only tie the vocabulary; this test
# ties the SEMANTICS by executing inst/js/drilldown-agg.js as shipped in plain
# node (fixtures/agg-golden-runner.js) and comparing cell by cell.
#
# Alignment contract (R is the source of truth):
#   * mean/median/min/max of a cell with no usable numeric value: null (JS) /
#     NA (R; mean gives NaN, which is.na covers; min/max go through
#     dd_agg_min/dd_agg_max so they yield NA instead of warning +/-Inf).
#   * sum of such a cell: 0 on both sides (sum(x, na.rm = TRUE)).
#   * count = rows in the cell; count_distinct = distinct non-missing values
#     (n_distinct(x, na.rm = TRUE) <-> JS Set over non-null values).
#   * NA group KEYS form their own cell on both sides. Labeling differs: JS
#     stringifies null to '' (String(row[g] ?? '')), R keeps the key NA --
#     same rows, same numbers, so the comparison folds NA -> ''. (Corollary:
#     a column containing BOTH NA and "" would fold into one JS cell but two
#     R cells; the fixture -- like real data -- avoids that ambiguity.)

# --- harness ----------------------------------------------------------------

# Called at the top of every test_that (skips must live inside a test).
skip_if_no_node <- function() {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node not available")
}

js_aggregate <- function(rows, config) {
  runner <- testthat::test_path("fixtures", "agg-golden-runner.js")
  engine <- system.file("js", "drilldown-agg.js", package = "blockr.viz")
  payload <- jsonlite::toJSON(
    list(rows = rows, config = config),
    dataframe = "rows", na = "null", auto_unbox = TRUE
  )
  out <- system2(
    Sys.which("node"), shQuote(c(runner, engine)),
    input = as.character(payload), stdout = TRUE, stderr = TRUE
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("agg-golden-runner.js failed: ", paste(out, collapse = "\n"))
  }
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = FALSE)
}

# The R engine, driven exactly as the table/tile drive it (summaries list ->
# dd_metric_plan -> dd_table_aggregate). A count summary rides along (except
# for func == "count" itself) so the JS per-cell `n` is cross-checked too.
r_aggregate <- function(df, group, func, value) {
  summaries <- if (identical(func, "count")) {
    list(list(func = "count", cols = character()))
  } else {
    list(
      list(func = func, cols = value),
      list(func = "count", cols = character())
    )
  }
  dd_table_aggregate(df, group, summaries)
}

# JS stringifies a missing group key to '' -- fold R's NA key to match.
key_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

expect_engines_agree <- function(df, func, value,
                                 group = NULL, color = NULL, facet = NULL) {
  cfg <- list(func = func, value = value)
  cfg$group <- group
  cfg$color <- color
  cfg$facet <- facet
  js <- js_aggregate(df, cfg)

  r_group <- c(facet, group, color)
  res <- r_aggregate(df, if (is.null(r_group)) character() else r_group,
                     func, value)
  rd <- res$data
  metric <- res$metric_cols[[1L]]

  # (facet, group, color) key per cell, with the JS placeholders for unmapped
  # roles ('Total' group, '__all__' color/facet) and NA keys folded to ''.
  r_keys <- paste(
    if (is.null(facet)) "__all__" else key_chr(rd[[facet]]),
    if (is.null(group)) "Total" else key_chr(rd[[group]]),
    if (is.null(color)) "__all__" else key_chr(rd[[color]]),
    sep = "|||"
  )
  js_keys <- vapply(
    js, function(r) paste(r$facet, r$group, r$color, sep = "|||"), ""
  )
  expect_setequal(js_keys, r_keys)

  for (i in seq_along(js)) {
    cell <- js[[i]]
    ri <- match(js_keys[[i]], r_keys)
    lbl <- sprintf("func=%s cell=%s", func, js_keys[[i]])
    r_val <- rd[[metric]][[ri]]
    if (is.null(cell$value)) {
      # JS null (no usable value) <-> R NA/NaN. Never a fabricated 0.
      expect_true(is.na(r_val), label = paste(lbl, "R value is NA"))
    } else {
      expect_false(is.na(r_val), label = paste(lbl, "R value is not NA"))
      expect_equal(as.numeric(cell$value), as.numeric(r_val),
                   tolerance = 1e-12, label = lbl)
    }
    if ("Count" %in% res$metric_cols) {
      expect_identical(as.integer(cell$n), as.integer(rd[["Count"]][[ri]]),
                       label = paste(lbl, "n"))
    } else {
      # func == "count": the value IS n.
      expect_identical(as.integer(cell$value), as.integer(cell$n),
                       label = paste(lbl, "count == n"))
    }
  }
  invisible(js)
}

# --- fixture ----------------------------------------------------------------
# Exercises: plain cells, cells whose value column is ALL NA, NA group keys
# (in group, color AND facet), NAs in the count_distinct id column, negative
# values, and non-integer means/medians.
agg_fixture <- function() {
  data.frame(
    f   = c("F1", "F1", "F1", "F1", "F2", "F2", "F2", "F2", "F1", "F2", NA, NA),
    g   = c("A", "A", "B", "B", "A", "B", "C", "C", NA, "A", "B", NA),
    c   = c("x", "y", "x", "y", "x", "x", "y", NA, "x", "y", "x", "y"),
    val = c(1.5, -2.25, 3, NA, -4.5, NA, NA, NA, 7.25, 0.75, -1, 2.5),
    id  = c("p1", "p1", "p2", NA, "p3", "p3", "p4", "p4", "p5", NA, "p6", "p6"),
    stringsAsFactors = FALSE
  )
}

# --- tests ------------------------------------------------------------------

test_that("golden: JS and R engines agree on facet x group x color", {
  skip_if_no_node()
  df <- agg_fixture()
  for (func in AGG_FNS) {
    value <- switch(func, count = ".count", count_distinct = "id", "val")
    expect_engines_agree(df, func, value,
                         group = "g", color = "c", facet = "f")
  }
})

test_that("golden: JS and R engines agree on a single grouping", {
  skip_if_no_node()
  df <- agg_fixture()
  for (func in AGG_FNS) {
    value <- switch(func, count = ".count", count_distinct = "id", "val")
    expect_engines_agree(df, func, value, group = "g")
  }
})

test_that("golden: JS and R engines agree on the ungrouped grand total", {
  skip_if_no_node()
  df <- agg_fixture()
  for (func in c("count", "count_distinct", "mean", "sum", "min")) {
    value <- switch(func, count = ".count", count_distinct = "id", "val")
    expect_engines_agree(df, func, value)
  }
})

test_that("golden: an all-NA cell is a null/NA gap, sum stays 0", {
  skip_if_no_node()
  df <- agg_fixture()
  # (f=F2, g=C) has no usable `val` at all.
  cell_of <- function(js, key) {
    js[[match(key, vapply(
      js, function(r) paste(r$facet, r$group, r$color, sep = "|||"), ""
    ))]]
  }
  for (func in c("mean", "median", "min", "max")) {
    js <- js_aggregate(
      df, list(func = func, value = "val", group = "g", facet = "f")
    )
    cell <- cell_of(js, "F2|||C|||__all__")
    expect_null(cell$value, label = paste(func, "of all-NA cell"))
    expect_identical(cell$n, 2L)
  }
  js <- js_aggregate(
    df, list(func = "sum", value = "val", group = "g", facet = "f")
  )
  expect_equal(cell_of(js, "F2|||C|||__all__")$value, 0)
})
