# The chart's data view (R/chart-data-view.R): the Summary frame per chart
# family, the Rows frame, what the payload carries while the view is open,
# a row click on Summary, and the Excel download's two sheets.

dv_df <- function() {
  data.frame(
    arm = rep(c("A", "B"), 30),
    visit = rep(c("V1", "V2", "V3"), each = 20),
    sex = rep(c("F", "M"), each = 2, length.out = 60),
    val = c(1:30, 31:60 * 2),
    day = rep(1:20, 3)
  )
}

stats_of <- function(s) s[!startsWith(names(s), ".")]

test_that("an aggregated chart's Summary is one row per mark, keys first", {
  d <- dv_df()
  s <- chart_summary(d, list(type = "bar", category = "visit", color = "arm",
                             func = "count"))
  expect_identical(nrow(s), 6L)
  expect_identical(names(stats_of(s)), "Count of rows")
  expect_identical(unique(s$.group1), "visit")
  expect_identical(unique(s$.variable), "arm")
  expect_identical(sum(s[["Count of rows"]]), 60L)
  # The frame the Excel download held before, with the gear's words.
  m <- chart_summary(d, list(type = "bar", category = "visit", func = "mean",
                             value = "val"))
  ref <- chart_xlsx_frame(d, "bar", group = "visit", func = "mean",
                          value = "val")
  expect_equal(m$.label, as.character(ref$visit), ignore_attr = TRUE)
  expect_equal(m[["Mean of val"]], ref[[2L]], ignore_attr = TRUE)
  # A facet is the outer row group.
  f <- chart_summary(d, list(type = "pie", category = "arm", facet = "sex"))
  expect_identical(unique(f$.group1), "sex")
  expect_identical(nrow(f), 4L)
})

test_that("a chart that draws rows has no Summary", {
  d <- dv_df()
  for (type in c("scatter", "line", "area")) {
    expect_null(chart_summary(d, list(type = type, x = "day", y = "val")))
  }
  expect_null(chart_summary(d[0, ], list(type = "bar", category = "arm")))
})

test_that("a histogram's Summary is one row per bin with its count", {
  d <- dv_df()
  v <- list(type = "histogram", x = "val", bins = 4L, color = "arm")
  plain <- chart_hist_data(d, chart_hist(v, d))
  s <- chart_summary(plain, v)
  expect_identical(unique(s$.group1), "val")
  expect_identical(unique(s$.variable), "arm")
  expect_identical(sum(s[["Count of rows"]]), 60L)
  expect_true(all(s$.group1_level %in% levels(plain$val_bin)))
})

test_that("a box's Summary holds the numbers the box draws", {
  d <- dv_df()
  v <- list(type = "boxplot", category = "visit", value = "val")
  s <- chart_summary(d, v)
  expect_identical(names(stats_of(s)),
                   c("N", "Whisker low", "Q1", "Median", "Q3", "Whisker high"))
  expect_equal(s$.label, c("V1", "V2", "V3"), ignore_attr = TRUE)
  x <- d$val[d$visit == "V2"]
  body <- summarize_stat(x, "median_q1_q3")
  fence <- summarize_stat(x, "tukey")
  expect_equal(unlist(stats_of(s)[2L, ], use.names = FALSE),
               c(20, fence$lo, body$lo, body$center, body$hi, fence$hi))
  # A point range: its interval, no whiskers.
  p <- chart_summary(d, list(type = "pointrange", category = "visit",
                             value = "val", summary = "mean_sd"))
  expect_identical(names(stats_of(p)), c("N", "Mean - SD", "Mean", "Mean + SD"))
})

test_that("a selection lights its Summary row, and a row click selects it", {
  d <- dv_df()
  v <- list(type = "bar", category = "visit", color = "arm", func = "count",
            selection = list(type = "categorical",
                             filters = list(visit = "V2", arm = "B")))
  s <- chart_summary(d, v)
  claim <- chart_selection_to_claim(v$selection, s, v)
  model <- interactive_table_payload(s, TRUE, NULL, TRUE, TRUE, FALSE)$data
  # The claim is the key of exactly one row.
  hits <- vapply(model$rows$keys, function(k) {
    setequal(names(k), names(claim$filters)) &&
      all(vapply(names(k), function(n) {
        identical(as.character(k[[n]]), unlist(claim$filters[[n]]))
      }, logical(1L)))
  }, logical(1L))
  expect_identical(sum(hits), 1L)
  # The row's key back as the chart's selection.
  sel <- chart_claim_to_selection(
    list(filters = list(.group1_level = "V2", .variable = "arm",
                        .variable_level = "B")), s, v
  )
  expect_identical(sel$filters, list(visit = "V2", arm = "B"))
  # With a drill column, the selection a click on that mark makes.
  v$drill <- "visit"
  sel <- chart_claim_to_selection(
    list(filters = list(.group1_level = "V2", .variable = "arm",
                        .variable_level = "B")), s, v
  )
  expect_identical(sel$filters, list(visit = "V2"))
  # A partial selection lights nothing.
  part <- list(type = "categorical", filters = list(visit = "V2"))
  expect_null(chart_selection_to_claim(part, s, v))
  # A histogram's bin goes back to the bin column.
  h <- list(type = "histogram", x = "val", bins = 4L)
  hs <- chart_summary(chart_hist_data(d, chart_hist(h, d)), h)
  hsel <- chart_claim_to_selection(
    list(filters = list(.variable_level = hs$.label[[1L]])), hs, h
  )
  expect_identical(names(hsel$filters), "val_bin")
})

test_that("Rows are the block's output: the input behind the selection", {
  d <- dv_df()
  ex <- quote(dplyr::filter(.(data), .data[["arm"]] == "B"))
  r <- chart_output_rows(d, ex)
  expect_identical(nrow(r), 30L)
  expect_identical(names(r), names(d))
  expect_identical(chart_output_rows(d, NULL), d)
  expect_null(chart_output_rows(d, quote(stop("no"))))
})

test_that("the data view message is checked on arrival", {
  expect_identical(viz_read_data_view(list(on = TRUE, tab = "rows", n = 3L)),
                   list(on = TRUE, tab = "rows", n = 3))
  expect_identical(viz_read_data_view(list(on = FALSE, tab = "x"))$tab,
                   "summary")
  expect_null(viz_read_data_view(list(on = "yes")))
  expect_null(viz_read_data_view(NULL))
})

# The block server; `code(session, expr)` runs inside it. The payloads the
# kit builds are recorded: after a click the kit renders twice, and the test
# session reports the second, identical render as cancelled.
dv_seen <- new.env(parent = emptyenv())

dv_run <- function(blk, code) {
  dat <- shiny::reactive(dv_df())
  real <- viz_with_shared
  local_mocked_bindings(viz_with_shared = function(kit, parts, view) {
    build <- real(kit, parts, view)
    function() {
      p <- build()
      dv_seen$p <- p
      p
    }
  })
  dv_seen$p <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      code(session, expr)
    },
    args = list(x = blk, data = list(data = dat))
  )
}

dv_open <- function(expr, session, tab = "summary", on = TRUE, n = 1L) {
  expr$setInputs(chart_data = viz_read_data_view(list(on = on, tab = tab,
                                                      n = n)))
  session$flushReact()
}

dv_derived <- function(expr) {
  tryCatch(expr$output$chart, error = function(e) NULL)
  dv_seen$p$derived$data_view
}

test_that("the payload carries the data view only while it is open", {
  blk <- new_chart_block(type = "bar", category = "visit", color = "arm")
  dv_run(blk, function(session, expr) {
    expect_null(dv_derived(expr))
    dv_open(expr, session)
    dv <- dv_derived(expr)
    expect_identical(dv$tab, "summary")
    expect_identical(dv[c("summary", "rows", "total")],
                     list(summary = 6L, rows = 60L, total = 60L))
    expect_match(dv$table$config, "\"layout\":\"records\"", fixed = TRUE)
    expect_identical(dv$table$data$n, 6L)
    # A chart without a drill ignores clicks, and so do its Summary rows.
    expect_null(dv$table$data$rows$keys)
    dv_open(expr, session, "rows")
    dv <- dv_derived(expr)
    expect_identical(dv$tab, "rows")
    expect_null(dv$table$data$rows$keys)
    dv_open(expr, session, on = FALSE)
    expect_null(dv_derived(expr))
  })
})

test_that("a click narrows Rows and lights its Summary row", {
  blk <- new_chart_block(type = "bar", category = "visit", color = "arm",
                         drill = "auto")
  dv_run(blk, function(session, expr) {
    dv_open(expr, session)
    before <- dv_derived(expr)$table
    expect_length(before$data$rows$keys, 6L)
    chart_input_click(expr, visit = "V2", arm = "B")
    session$flushReact()
    dv <- dv_derived(expr)
    expect_identical(dv$rows, 10L)
    expect_identical(dv$total, 60L)
    expect_match(dv$table$config, "\".variable_level\":[\"B\"]",
                 fixed = TRUE)
    # Same table, so the view keeps its sort and scroll, and its rows stay
    # where they are; a new state.
    expect_identical(dv$table$data_rev, before$data_rev)
    expect_gt(dv$table$state_rev, before$state_rev)
    expect_null(dv$table[["data"]])
    # A view that lacks them asks with a new count.
    dv_open(expr, session, n = 2L)
    expect_identical(dv_derived(expr)$table$data$n, 6L)
  })
})

test_that("a row click on Summary selects that mark", {
  blk <- new_chart_block(type = "bar", category = "visit", color = "arm",
                         drill = "auto")
  dv_run(blk, function(session, expr) {
    dv_open(expr, session)
    expr$setInputs(`chart-data_select` = viz_read_selection(list(
      selection = list(type = "categorical", filters = list(
        .group1_level = list("V3"), .variable = list("arm"),
        .variable_level = list("A")
      )),
      nonce = 1
    )))
    session$flushReact()
    expect_identical(session$returned$state$selection()$filters,
                     list(visit = "V3", arm = "A"))
    expect_identical(dv_derived(expr)$rows, 10L)
  })
})

test_that("a scatter's data view is its rows alone", {
  blk <- new_chart_block(type = "scatter", x = "day", y = "val")
  dv_run(blk, function(session, expr) {
    dv_open(expr, session, "summary")
    dv <- dv_derived(expr)
    expect_identical(dv$tab, "rows")
    expect_null(dv$summary)
    expect_identical(dv$rows, 60L)
  })
})

test_that("the Excel download writes Summary and Rows, exhibit_data() stays", {
  skip_if_not_installed("openxlsx")
  blk <- new_chart_block(type = "boxplot", category = "visit", value = "val",
                         drill = "auto")
  dv_run(blk, function(session, expr) {
    chart_input_click(expr, visit = "V1")
    session$flushReact()
    path <- expr$output$dl_xlsx
    expect_identical(openxlsx::getSheetNames(path), c("Summary", "Rows"))
    summ <- openxlsx::read.xlsx(path, "Summary", colNames = FALSE)
    expect_true(all(c("N", "Q1", "Median", "Q3") %in% unlist(summ)))
    rows <- openxlsx::read.xlsx(path, "Rows")
    expect_identical(nrow(rows), 20L)
    expect_identical(unique(rows$visit), "V1")
  })
  # The exhibit's data is the frame it was: a box plot's input rows.
  dv_run(blk, function(session, expr) {
    ex <- NULL
    real <- exhibit_footed
    local_mocked_bindings(exhibit_footed = function(x, session) {
      ex <<- x
      real(x, session)
    })
    expr$output$dl_xlsx
    expect_identical(exhibit_data(ex)[names(dv_df())], dv_df())
    expect_identical(nrow(exhibit_meta(ex)$data_view$summary), 3L)
  })
})

test_that("a chart without a Summary downloads one sheet, as before", {
  skip_if_not_installed("openxlsx")
  blk <- new_chart_block(type = "scatter", x = "day", y = "val")
  dv_run(blk, function(session, expr) {
    path <- expr$output$dl_xlsx
    expect_length(openxlsx::getSheetNames(path), 1L)
  })
})
