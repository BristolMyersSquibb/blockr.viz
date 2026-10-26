# Writes table-records.json, what the records gear JS tests mount and feed
# (tests/js/table-gear.test.js): per case, the chrome the records engine
# renders once, the layout the block pushes and the table payloads its server
# pushes, in order, as a sequence of gear edits arrives.
#
# Run from the package root after changing the records engine, its payload
# builders or its chrome, then re-run `npm test`:
#
#   Rscript tests/js/fixtures/make-table-fixtures.R
#
# The payloads come from the table block's own server under
# shiny::testServer(), so the browser side sees what it sees live.

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

sizes <- data.frame(
  Species = c("setosa", "versicolor", "virginica", "hybrid"),
  Sepal = c(5.006, 5.936, 6.588, 6.1),
  Petal = c(1.462, 4.26, 5.552, 4.4),
  Size = c("small", "medium", "large", NA),
  stringsAsFactors = FALSE
)
adsl <- data.frame(
  USUBJID = sprintf("S%02d", 1:12),
  ARM = rep(c("A", "B"), 6),
  AGE = c(61, 70, 55, 80, 66, 72, 59, 63, 77, 69, 74, 58),
  SEX = rep(c("F", "M", "M"), 4),
  stringsAsFactors = FALSE
)

# Run the block's server on `data`, apply `edits` (each a list(param,
# value) as the records gear sends it), and return the chrome, the layout
# and every table payload pushed.
record <- function(blk, data, edits = list()) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- list()
      layout <- NULL
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "blockr-viz-table-data")) {
          sent[[length(sent) + 1L]] <<- message
        }
        if (identical(type, "blockr-viz-table-layout")) {
          layout <<- message
        }
        invisible(NULL)
      }
      session$flushReact()
      rec <- session$makeScope("expr")$makeScope("records")
      for (e in edits) {
        rec$setInputs(drilldown_table_block_action = list(
          action = "config", param = e$param, value = e$value
        ))
        session$flushReact()
      }
      id <- sent[[1L]]$id
      chrome <- dt_chrome(
        elem_id = id, structured = NULL, max_height = NULL,
        inner = htmltools::tags$div(class = "dt-table-slot"),
        search = TRUE,
        download_slot = htmltools::span(class = "fixture-download"),
        status_slot = htmltools::span(class = "fixture-status")
      )
      out <<- list(
        id = id,
        chrome = as.character(htmltools::renderTags(chrome)$html),
        layout = list(ids = as.list(layout$ids), layout = layout$layout),
        payloads = lapply(sent, function(m) {
          list(rev = m$rev, payload = m$payload)
        })
      )
    },
    args = list(x = blk, data = list(data = shiny::reactive(data)))
  )
  out
}

cases <- list(
  # A flat table: row names and columns, the colour section, the script.
  flat = record(
    new_table_block(rowname = "Species", drill = "Species"), sizes,
    edits = list(
      list(param = "columns", value = list("Petal")),
      list(param = "script",
           value = "dplyr::filter(data, Sepal > 5.5)")
    )
  ),
  # Nested by `by`: no "Finished table" badge, numbers, missing, collapse.
  nested = record(
    new_table_block(by = "Size", rowname = "Species"), sizes,
    edits = list(list(param = "format", value = "0"))
  ),
  # A finished input: shown as it came, with a badge and no row controls.
  finished = record(
    new_table_block(),
    summary_table(adsl, vars = c("AGE", "SEX"), by = "ARM")
  ),
  # A Table 1: missing and collapse, no numbers (its cells are text).
  variables = record(
    new_table_block(
      layout = "variables", by = "ARM",
      summaries = list(list(type = "dist", col = "AGE", show = "box"))
    ),
    adsl
  )
)

jsonlite::write_json(
  cases, "tests/js/fixtures/table-records.json",
  auto_unbox = TRUE, null = "null", pretty = TRUE
)
