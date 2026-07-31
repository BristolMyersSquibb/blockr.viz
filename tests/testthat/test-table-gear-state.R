# Gear-popover state transport: the <table> carries the CURRENT pickable
# columns (data-dt-cols, raw input schema) and config on every render, so the
# gear can re-read them at popover-open time (table.js readGearState) instead
# of keeping the snapshot parsed at chrome-init -- the chrome outlives table
# re-renders, and a config that changed server-side (restore race, AI /
# external_ctrl) must not be overwritten by a stale gear.

library(shiny)

render <- function(tag) as.character(htmltools::renderTags(tag)$html)

test_that("dt_gear_cols_json emits the raw schema; '[]' for structured", {
  df <- data.frame(region = c("N", "S"), revenue = c(1, 2),
                   stringsAsFactors = FALSE)
  cols <- jsonlite::fromJSON(dt_gear_cols_json(df), simplifyVector = FALSE)
  expect_equal(
    cols,
    list(
      list(name = "region", type = "categorical"),
      list(name = "revenue", type = "numeric")
    )
  )

  structured <- data.frame(
    .label = c("Mean", "SD"), Total = c("1.0", "0.5"), check.names = FALSE
  )
  expect_equal(dt_gear_cols_json(structured), "[]")

  expect_equal(dt_gear_cols_json(NULL), "[]")
})

test_that("data-dt-cols is stamped on flat, message and structured tables", {
  df <- data.frame(region = c("N", "S"), revenue = c(1, 2),
                   stringsAsFactors = FALSE)

  flat <- render(dt_table_tag(df))
  expect_true(grepl("data-dt-cols=", flat))
  expect_true(grepl("&quot;region&quot;", flat))

  # A vanished configured column renders the message table, but the gear must
  # still read the CURRENT input columns to let the user fix the config.
  msg <- render(dt_table_tag(df, value_cols = "gone"))
  expect_true(grepl("Mapped column not in data", msg))
  expect_true(grepl("data-dt-cols=", msg))
  expect_true(grepl("&quot;revenue&quot;", msg))

  structured <- data.frame(
    .label = c("Mean", "SD"), Total = c("1.0", "0.5"), check.names = FALSE
  )
  st <- render(dt_table_tag(structured))
  expect_true(grepl("data-dt-cols=\"\\[\\]\"", st))
})

test_that("aggregated display keeps the RAW input schema on data-dt-cols", {
  df  <- data.frame(region = c("N", "S", "N"), revenue = c(1, 2, 3),
                    stringsAsFactors = FALSE)
  blk <- new_table_block(group = "region",
                         summaries = list(list(func = "count",
                                               cols = list())))
  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    # The body ships as the data-push payload (not a renderUI output); the
    # gear attributes ride on the payload's <table> head.
    p <- jsonlite::fromJSON(r_body_payload(), simplifyVector = FALSE)
    expect_identical(p$kind, "flat")
    # The displayed frame is the aggregate: group + the "Count" metric column
    # (and 2 group rows, not 3 raw rows).
    expect_true(grepl("Count", p$head))
    expect_identical(p$n, 2L)
    # ... but the gear's pickable columns stay the raw input schema: revenue
    # (not displayed) is offered, the synthetic Count column is not.
    m <- regmatches(p$head, regexpr("data-dt-cols=\"[^\"]*\"", p$head))
    expect_length(m, 1L)
    expect_true(grepl("region", m))
    expect_true(grepl("revenue", m))
    expect_false(grepl("Count", m))
  })
})

test_that("a format whose writer is missing is left out of the menu", {
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block(download = TRUE)

  # Nobody asked for the spreadsheet specifically -- the download toggle did --
  # so an entry that could only explain why it does not work is noise. (It was
  # shown disabled while each format had its own pill: there, a toggle you had
  # just switched on that rendered nothing read as broken.)
  local_mocked_bindings(dt_has_openxlsx = function() FALSE)
  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    html <- as.character(output$dt_download$html)
    expect_false(grepl("Excel (.xlsx)", html, fixed = TRUE))
    expect_false(grepl("--off", html, fixed = TRUE))
    # HTML needs no package of its own, so the control never empties.
    expect_true(grepl("dl_html", html))
  })
})

test_that("downloads on renders every format the machine can write", {
  skip_if_not_installed("openxlsx")
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block(download = TRUE)

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    html <- as.character(output$dt_download$html)
    expect_true(grepl("shiny-download-link", html))
    expect_true(grepl("Excel (.xlsx)", html, fixed = TRUE))
    expect_true(grepl("Web page (.html)", html, fixed = TRUE))
  })
})

test_that("downloads off renders no control at all", {
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block()

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    expect_null(output$dt_download)
  })
})

test_that("one writable format renders a button, several render a menu", {
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)

  # A machine with neither writer can still hand back the page: one format,
  # so the toolbar keeps the direct link and the reader keeps one click.
  local_mocked_bindings(
    dt_has_openxlsx = function() FALSE,
    dt_has_officer  = function() FALSE
  )
  testServer(new_table_block(download = TRUE)$expr_server,
             args = list(data = reactive(df)), {
    session$flushReact()
    html <- as.character(output$dt_download$html)
    expect_false(grepl("blockr-dl-menu", html))
    expect_true(grepl("shiny-download-link", html))
    expect_true(grepl("dl_html", html))
  })
})

test_that("several formats render a menu in spec order", {
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)

  local_mocked_bindings(
    dt_has_openxlsx = function() TRUE,
    dt_has_officer  = function() TRUE
  )
  testServer(new_table_block(download = TRUE)$expr_server,
             args = list(data = reactive(df)), {
    session$flushReact()
    html <- as.character(output$dt_download$html)
    expect_true(grepl("<details class=\"blockr-dl-menu\"", html))
    expect_true(grepl("Excel (.xlsx)", html, fixed = TRUE))
    expect_true(grepl("Web page (.html)", html, fixed = TRUE))
    expect_true(grepl("PowerPoint (.pptx)", html, fixed = TRUE))
    # Menu order is the spec order, never the order writers were probed in.
    expect_lt(regexpr("Excel (.xlsx)", html, fixed = TRUE),
              regexpr("Web page (.html)", html, fixed = TRUE))
    expect_lt(regexpr("Web page (.html)", html, fixed = TRUE),
              regexpr("PowerPoint (.pptx)", html, fixed = TRUE))
  })
})

test_that("the html download writes a self-contained page", {
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block(download = TRUE, title = "Groups")

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    # testServer runs the handler and hands back the path it wrote to.
    html <- paste(readLines(output$dl_html, warn = FALSE), collapse = "\n")
    expect_match(html, "<h1>Groups</h1>", fixed = TRUE)
    expect_match(html, "<table", fixed = TRUE)
    expect_false(grepl("<script[^>]+src=", html))
  })
})

test_that("the download control does not re-render when the data changes", {
  # The control reads the download toggle and nothing else. If it read the data
  # it would rebuild -- and re-probe for the writers -- on every filter click,
  # which is the cost that would make turning downloads on something to think
  # twice about. Counted through the probe seam.
  calls <- 0L
  local_mocked_bindings(
    dt_has_officer = function() { calls <<- calls + 1L; TRUE }
  )

  rv <- reactiveVal(data.frame(grp = c("A", "B"), val = c(1, 2)))
  blk <- new_table_block(download = TRUE)

  testServer(blk$expr_server, args = list(data = rv), {
    session$flushReact()
    invisible(output$dt_download)
    first <- calls
    expect_gt(first, 0L)

    for (i in 1:3) {
      rv(data.frame(grp = c("A", "B"), val = c(i, i + 1)))
      session$flushReact()
      invisible(output$dt_download)
    }
    expect_identical(calls, first)
  })
})
