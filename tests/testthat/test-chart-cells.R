# chart_cells() (R/chart-cells.R) is what a bar, waterfall, pie or treemap
# draws from. The JS snapshot tests feed the chart through its twin,
# tests/js/cells.js, whose numbers are the engine's in inst/js/chart/agg.js.
# Here both get the same rows and settings and must give the same cells, in
# the same order.

cells_df <- function() {
  i <- seq_len(240L)
  d <- data.frame(
    term = c("Rash", "Nausea", "Headache", "Fatigue")[(i * 5L) %% 4L + 1L],
    sev = factor(c("MILD", "MOD", "SEV")[i %% 3L + 1L],
                 levels = c("MILD", "MOD", "SEV", "FATAL")),
    arm = c("A", "B")[(i %/% 7L) %% 2L + 1L],
    site = c("S1", "S2", "S3")[(i %/% 11L) %% 3L + 1L],
    id = sprintf("P%02d", (i * 7L) %% 23L),
    val = round(((i * 37L) %% 101L) / 7, 3),
    day = as.Date("2024-01-01") + (i * 3L) %% 40L,
    stringsAsFactors = FALSE
  )
  d$term[c(4L, 19L, 33L)] <- NA
  d$sev[c(7L, 40L)] <- NA
  d$arm[c(100L, 101L)] <- NA
  d$val[c(5L, 22L, 47L)] <- NA
  d$id[c(58L, 59L)] <- NA
  d
}

js_cells <- function(d, v) {
  twin <- normalizePath(testthat::test_path("..", "js", "cells.js"))
  code <- sprintf(paste0(
    "const {chartCells} = require(%s);",
    "let s = ''; process.stdin.on('data', (c) => s += c);",
    "process.stdin.on('end', () => { const p = JSON.parse(s);",
    "process.stdout.write(JSON.stringify(chartCells(p.data, p.config, []))); });"
  ), jsonlite::toJSON(twin, auto_unbox = TRUE))
  payload <- jsonlite::toJSON(
    list(data = jsonlite::fromJSON(viz_wire_df(d, meta = FALSE)$data,
                                   simplifyVector = FALSE),
         config = v),
    auto_unbox = TRUE, null = "null", digits = NA
  )
  out <- system2(Sys.which("node"), c("-e", shQuote(code)),
                 input = as.character(payload), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(out, "status"))) {
    stop("cells.js failed: ", paste(out, collapse = "\n"))
  }
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = FALSE)
}

r_cells <- function(d, v) {
  a <- chart_cells_args(v, d)
  cols <- jsonlite::fromJSON(chart_cells_json(chart_cells(d, a)),
                             simplifyVector = FALSE)
  # A dictionary column as its values, as the browser reads it.
  lapply(cols, function(col) {
    if (identical(col[["__enc__"]], "dict")) {
      lapply(col$codes, function(k) if (is.null(k)) NULL else col$levels[[k + 1L]])
    } else {
      col
    }
  })
}

cells_cases <- list(
  count = list(type = "bar", category = "term"),
  color = list(type = "bar", category = "term", color = "sev"),
  facet = list(type = "bar", category = "term", color = "sev", facet = "arm"),
  grid = list(type = "bar", category = "term", facet = "arm",
              facet_row = "site"),
  drop = list(type = "bar", category = "term", color = "sev", facet = "arm",
              missing = "drop"),
  mean = list(type = "bar", category = "term", color = "sev", func = "mean",
              value = "val"),
  median = list(type = "bar", category = "term", func = "median",
                value = "val"),
  sum = list(type = "waterfall", category = "term", func = "sum",
             value = "val"),
  min = list(type = "bar", category = "term", func = "min", value = "val"),
  max = list(type = "bar", category = "term", func = "max", value = "val"),
  identity = list(type = "bar", category = "term", func = "identity",
                  value = "val"),
  distinct = list(type = "pie", category = "term", func = "count_distinct",
                  value = "id"),
  pct = list(type = "bar", category = "term", color = "sev", facet = "arm",
             func = "pct_distinct", value = "id"),
  pct_category = list(type = "bar", category = "term", color = "sev",
                      func = "pct_distinct", value = "id",
                      percent_of = "category"),
  pct_total = list(type = "treemap", category = "term", func = "pct_distinct",
                   value = "id", percent_of = "total"),
  sort = list(type = "bar", category = "term", sort_by = "val"),
  tooltip = list(type = "bar", category = "term", color = "sev",
                 tooltip = c("id", "val", "day")),
  label = list(type = "bar", category = "term", color = "sev", label = "arm"),
  ids = list(type = "bar", category = "term", facet = "arm", id = "id",
             show_n = "both"),
  drill = list(type = "bar", category = "term", color = "sev", drill = "id"),
  highlight = list(type = "bar", category = "term",
                   highlight = list(id = list("P01", "P05")))
)

for (nm in names(cells_cases)) {
  local({
    nm <- nm
    test_that(paste("chart_cells() gives the cells of its JS twin:", nm), {
      skip_on_cran()
      skip_if(Sys.which("node") == "", "node not available")
      skip_if_not(file.exists(testthat::test_path("..", "js", "cells.js")),
                  "no JS tests here")
      d <- cells_df()
      v <- cells_cases[[nm]]
      expect_equal(r_cells(d, v), js_cells(d, v), tolerance = 1e-9)
    })
  })
}

test_that("a chart that draws rows, or has no category, has no cells", {
  d <- cells_df()
  expect_null(chart_cells_args(list(type = "scatter", x = "val", y = "val"), d))
  expect_null(chart_cells_args(list(type = "boxplot", category = "term",
                                    value = "val"), d))
  expect_null(chart_cells_args(list(type = "histogram", x = "val"), d))
  expect_null(chart_cells_args(list(type = "bar"), d))
  expect_null(chart_cells_args(list(type = "bar", category = "gone"), d))
})

test_that("the cells do not grow with the rows", {
  d <- cells_df()
  big <- d[rep(seq_len(nrow(d)), 50L), ]
  v <- list(type = "bar", category = "term", color = "sev")
  small <- chart_cells(d, chart_cells_args(v, d))
  many <- chart_cells(big, chart_cells_args(v, big))
  expect_identical(nrow(many), nrow(small))
  expect_identical(many$.n, small$.n * 50L)
})

test_that("NA and an empty string in a key are one cell", {
  d <- data.frame(g = c("a", NA, "", "a"), stringsAsFactors = FALSE)
  cl <- chart_cells(d, chart_cells_args(list(type = "bar", category = "g"), d))
  expect_identical(cl$.n, c(2L, 2L))
})
