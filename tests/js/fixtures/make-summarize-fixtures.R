# Writes summarize-table.json, what the summarize table JS tests mount and
# feed: per case, the chrome the block's UI renders once and the payloads its
# server pushes, in order, as a sequence of gear edits arrives.
#
# Run from the package root after changing the block's server, its payload
# builders or its chrome, then re-run `npm test`:
#
#   Rscript tests/js/fixtures/make-summarize-fixtures.R
#
# The payloads come from the block's own server under shiny::testServer(),
# so the browser side sees what it sees live.

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

# Small, arithmetic, two levels of nesting: four terms in two classes, two
# severities, two arms. Term 1 carries markup characters to pin escaping.
ae <- local({
  subj <- sprintf("S%03d", seq_len(30))
  rows <- do.call(rbind, lapply(seq_len(4), function(i) {
    n <- 20L - 4L * i
    data.frame(
      USUBJID = subj[seq_len(n)],
      TERM = if (i == 1L) "T <1> & co" else paste0("T", i),
      SOC = if (i <= 2L) "SOC A" else "SOC B",
      SEV = rep(c("MILD", "MODERATE"), length.out = n),
      AVAL = seq_len(n),
      stringsAsFactors = FALSE
    )
  }))
  rows$ARM <- factor(rep(c("Placebo", "Active"), length.out = nrow(rows)),
                     levels = c("Placebo", "Active"))
  rows$SEV <- factor(rows$SEV, levels = c("MILD", "MODERATE"))
  rows
})

count_bar <- list(type = "simple", func = "count", show = "bar")

# Run the block's server, apply `edits` (each a list(param, value) as the
# gear sends it), and return the chrome plus every payload pushed.
record <- function(blk, edits = list()) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "blockr-viz-summarize-data")) {
          sent[[length(sent) + 1L]] <<- message
        }
        invisible(NULL)
      }
      session$flushReact()
      expr <- session$makeScope("expr")
      for (e in edits) {
        expr$setInputs(summarize_table_block_action = list(
          action = "config", param = e$param, value = e$value
        ))
        session$flushReact()
      }
      id <- sent[[1L]]$id
      # The chrome as the block's one-shot render builds it, from the
      # constructor's settings.
      st <- blk
      chrome <- summarize_chrome_shell(
        max_height = attr(st, "fixture_max_height"),
        search = attr(st, "fixture_search") %||% TRUE,
        drill = attr(st, "fixture_drill"),
        elem_id = id,
        ctrl_target = attr(st, "fixture_ctrl_target") %||% "",
        download = htmltools::span(class = "fixture-download")
      )
      out <<- list(
        id = id,
        chrome = as.character(htmltools::renderTags(chrome)$html),
        payloads = lapply(sent, function(m) {
          list(rev = m$rev, payload = m$payload)
        })
      )
    },
    args = list(x = blk, data = list(data = shiny::reactive(ae)))
  )
  out
}

# The chrome arguments mirror the constructor's, so a case states them once.
block <- function(..., search = TRUE, drill = NULL, ctrl_target = "",
                  max_height = NULL) {
  b <- new_summarize_table_block(..., search = search, drill = drill,
                                 ctrl_target = ctrl_target,
                                 max_height = max_height)
  attr(b, "fixture_search") <- search
  attr(b, "fixture_drill") <- drill
  attr(b, "fixture_ctrl_target") <- ctrl_target
  attr(b, "fixture_max_height") <- max_height
  b
}

cases <- list(
  # One grouping, a count bar, row drill on the grouping column.
  flat = record(block(by = "TERM", summaries = list(count_bar),
                      drill = "TERM", title = "Terms")),
  # The server's order ascending, so a header click visibly reorders.
  ascending = record(block(by = "TERM", summaries = list(count_bar),
                           sort_dir = "asc")),
  # Two grouping columns nest: SOC rows with TERM rows under them.
  nested = record(block(by = c("SOC", "TERM"), summaries = list(count_bar))),
  # The same, drilled: a row click claims the row's path.
  nested_drill = record(block(by = c("SOC", "TERM"), summaries = list(count_bar),
                              drill = "TERM")),
  # A colour column: the legend band.
  color = record(block(by = "TERM", summaries = list(
    list(type = "simple", func = "count", show = "bar", color = "SEV")
  ))),
  # A height: the table scrolls in its own box.
  height = record(block(by = "TERM", summaries = list(count_bar),
                        max_height = "300px")),
  # The gear's container settings, edited in turn: search off, drill on,
  # send to filter on, send to filter off, drill off, search on.
  settings = record(
    block(by = "TERM", summaries = list(count_bar)),
    edits = list(
      list(param = "search", value = "off"),
      list(param = "drill", value = "TERM"),
      list(param = "ctrl_target", value = "auto"),
      list(param = "ctrl_target", value = ""),
      list(param = "drill", value = NULL),
      list(param = "search", value = "on")
    )
  )
)

path <- file.path("tests", "js", "fixtures", "summarize-table.json")
writeLines(
  as.character(jsonlite::toJSON(cases, auto_unbox = TRUE, null = "null",
                                pretty = TRUE)),
  path
)
message("wrote ", path, ": ",
        paste(names(cases), vapply(cases, function(x) length(x$payloads),
                                   integer(1)), sep = " x", collapse = ", "))
