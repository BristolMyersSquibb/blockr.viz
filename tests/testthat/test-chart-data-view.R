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

test_that("in a panel grid the Summary and the download key on both facets", {
  d <- dv_df()
  v <- list(type = "bar", category = "visit", func = "mean", value = "val",
            facet = "arm", facet_row = "sex")
  s <- chart_summary(d, v)
  # One row per panel and bar: the rows' facet outermost, then the columns'.
  expect_identical(nrow(s), 12L)
  expect_identical(unique(s$.group1), "sex")
  expect_identical(unique(s$.group2), "arm")
  expect_identical(unique(s$.variable), "visit")
  x <- chart_xlsx_frame(d, "bar", group = "visit", facet = c("sex", "arm"),
                        func = "mean", value = "val")
  expect_identical(names(x)[1:3], c("sex", "arm", "visit"))
  expect_identical(nrow(x), 12L)
  # A histogram and a boxplot split the same way.
  hv <- list(type = "histogram", x = "val", bins = 4L, facet = "arm",
             facet_row = "sex")
  h <- chart_summary(chart_hist_data(d, chart_hist(hv, d)), hv)
  expect_identical(c(unique(h$.group1), unique(h$.group2)), c("sex", "arm"))
  b <- chart_summary(d, list(type = "boxplot", category = "visit",
                             value = "val", facet = "arm", facet_row = "sex"))
  expect_identical(nrow(b), 12L)
  # facet_row alone: one column of panels.
  r <- chart_summary(d, list(type = "bar", category = "visit",
                             facet_row = "sex"))
  expect_identical(unique(r$.group1), "sex")
  expect_identical(nrow(r), 6L)
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
  # The outer key alone lights its section header; the inner key alone
  # lights nothing.
  part <- list(type = "categorical", filters = list(visit = "V2"))
  expect_identical(chart_selection_to_claim(part, s, v)$filters,
                   list(.group1_level = list("V2")))
  inner <- list(type = "categorical", filters = list(arm = "B"))
  expect_null(chart_selection_to_claim(inner, s, v))
  # A histogram's bin goes back to the bin column.
  h <- list(type = "histogram", x = "val", bins = 4L)
  hs <- chart_summary(chart_hist_data(d, chart_hist(h, d)), h)
  hsel <- chart_claim_to_selection(
    list(filters = list(.variable_level = hs$.label[[1L]])), hs, h
  )
  expect_identical(names(hsel$filters), "val_bin")
  # One key, no row groups.
  v1 <- list(type = "bar", category = "visit", func = "count",
             selection = list(type = "categorical",
                              filters = list(visit = "V2")))
  s1 <- chart_summary(d, v1)
  expect_identical(chart_selection_to_claim(v1$selection, s1, v1)$filters,
                   list(.variable_level = list("V2"), .variable = list("visit")))
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
# kit builds are recorded, since the test session reports a render the kit
# does not send again as cancelled.
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
    expect_null(dv$table)
    dv_open(expr, session, on = FALSE)
    expect_null(dv_derived(expr))
  })
})

dv_rows_html <- function(expr) {
  out <- tryCatch(expr$output$`chart-data_rows`, error = function(e) NULL)
  out$html
}

test_that("Rows is blockr.ui's table preview, following the selection", {
  blk <- new_chart_block(type = "bar", category = "visit", color = "arm",
                         drill = "auto")
  dv_run(blk, function(session, expr) {
    dv_open(expr, session)
    expect_null(dv_rows_html(expr))
    dv_open(expr, session, "rows")
    html <- as.character(dv_rows_html(expr))
    expect_match(html, "blockr-table-container", fixed = TRUE)
    expect_match(html, "data-blockr-tooltip-overflow", fixed = TRUE)
    expect_match(html, "of 60", fixed = TRUE)
    chart_input_click(expr, visit = "V2", arm = "B")
    session$flushReact()
    expect_match(as.character(dv_rows_html(expr)), "of 10", fixed = TRUE)
    # Back to Summary: its table again, the selected mark lit.
    dv_open(expr, session, n = 2L)
    dv <- dv_derived(expr)
    expect_identical(dv$tab, "summary")
    expect_identical(dv$table$data$n, 6L)
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

dv_menu <- function(expr) {
  tryCatch(expr$output$chart, error = function(e) NULL)
  dv_seen$p$derived$downloads
}

# The counts R answers with when the browser reaches for the menu, in the
# order of the menu's entries `data`.
dv_counts <- function(session, expr, data) {
  sent <- NULL
  root <- session$rootScope()
  old <- root$sendCustomMessage
  root$sendCustomMessage <- function(type, message) {
    if (identical(type, "blockr-viz:counts")) sent <<- message$counts
  }
  on.exit(root$sendCustomMessage <- old)
  expr$setInputs(dl_counts = as.numeric(Sys.time()))
  session$flushReact()
  unname(unlist(sent[vapply(data, `[[`, "", "id")]))
}

test_that("the menu has a Chart and a Data section, without the old Excel", {
  skip_if_not_installed("openxlsx")
  blk <- new_chart_block(type = "bar", category = "visit", drill = "auto")
  dv_run(blk, function(session, expr) {
    chart_input_click(expr, visit = "V1")
    session$flushReact()
    menu <- dv_menu(expr)
    secs <- vapply(menu, `[[`, "", "section")
    expect_identical(unique(secs), c("Chart", "Data"))
    expect_false("Excel" %in% vapply(menu[secs == "Chart"], `[[`, "", "label"))
    data <- menu[secs == "Data"]
    expect_identical(vapply(data, `[[`, "", "label"),
                     rep(c("Summary", "Selected rows", "All rows"), each = 2L))
    expect_identical(vapply(data, `[[`, "", "ext"),
                     rep(c("xlsx", "csv"), 3L))
    # The entries carry no count: building the tables waits for the reader.
    expect_null(data[[1L]]$count)
    expect_identical(unique(vapply(data, `[[`, "", "ask")),
                     expr$ns("dl_counts"))
    expect_identical(dv_counts(session, expr, data),
                     rep(c("3 rows", "20 of 60", "60 rows"), each = 2L))
    expect_identical(nrow(utils::read.csv(expr$output$dl_data_all_csv)), 60L)
  })
})

test_that("the Summary downloads are flat: every key a column", {
  skip_if_not_installed("openxlsx")
  blk <- new_chart_block(type = "boxplot", category = "visit", value = "val",
                         color = "arm", title = "Values")
  dv_run(blk, function(session, expr) {
    csv <- utils::read.csv(expr$output$dl_data_summary_csv,
                           check.names = FALSE)
    expect_identical(names(csv)[1:3], c("visit", "arm", "N"))
    expect_identical(nrow(csv), 6L)
    expect_true(all(c("Q1", "Median", "Q3") %in% names(csv)))
    path <- expr$output$dl_data_summary_xlsx
    expect_identical(openxlsx::getSheetNames(path), "Summary")
    x <- openxlsx::read.xlsx(path, colNames = FALSE, skipEmptyRows = FALSE)
    expect_identical(x[[1L]][[1L]], "Values")
    expect_identical(unname(unlist(x[3L, 1:3])), c("visit", "arm", "N"))
  })
})

test_that("the Rows downloads are the block's output", {
  skip_if_not_installed("openxlsx")
  blk <- new_chart_block(type = "boxplot", category = "visit", value = "val",
                         drill = "auto")
  dv_run(blk, function(session, expr) {
    chart_input_click(expr, visit = "V1")
    session$flushReact()
    rows <- utils::read.csv(expr$output$dl_data_rows_csv)
    expect_identical(nrow(rows), 20L)
    expect_identical(unique(rows$visit), "V1")
    xl <- openxlsx::read.xlsx(expr$output$dl_data_rows_xlsx)
    expect_identical(nrow(xl), 20L)
    expect_identical(names(xl), names(dv_df()))
  })
})

test_that("a chart that draws rows offers Rows alone", {
  skip_if_not_installed("openxlsx")
  blk <- new_chart_block(type = "scatter", x = "day", y = "val")
  dv_run(blk, function(session, expr) {
    menu <- dv_menu(expr)
    data <- Filter(function(d) identical(d$section, "Data"), menu)
    expect_identical(vapply(data, `[[`, "", "label"), c("Rows", "Rows"))
    expect_identical(dv_counts(session, expr, data), rep("60 rows", 2L))
    expect_identical(nrow(utils::read.csv(expr$output$dl_data_rows_csv)), 60L)
  })
})

test_that("the menu is listed without building a table", {
  built <- 0L
  real <- chart_summary
  local_mocked_bindings(chart_summary = function(...) {
    built <<- built + 1L
    real(...)
  })
  blk <- new_chart_block(type = "boxplot", category = "visit", value = "val")
  dv_run(blk, function(session, expr) {
    data <- Filter(function(d) identical(d$section, "Data"), dv_menu(expr))
    expect_identical(unique(vapply(data, `[[`, "", "label")),
                     c("Summary", "Rows"))
    expect_identical(built, 0L)
    expect_identical(dv_counts(session, expr, data),
                     rep(c("3 rows", "60 rows"), each = 2L))
    expect_identical(built, 1L)
  })
})

test_that("chart_has_summary() agrees with chart_summary()", {
  d <- dv_df()
  cases <- list(
    list(type = "bar", category = "visit"),
    list(type = "bar", category = "nope"),
    list(type = "bar"),
    list(type = "pie", category = "arm"),
    list(type = "treemap", category = "arm", color = "sex"),
    list(type = "waterfall", category = "visit", value = "val", func = "sum"),
    list(type = "boxplot", category = "visit", value = "val"),
    list(type = "boxplot", category = "visit"),
    list(type = "pointrange", category = "visit", value = "val"),
    list(type = "pointrange", category = "visit", value = "nope"),
    list(type = "histogram", x = "val"),
    list(type = "scatter", x = "day", y = "val"),
    list(type = "line", x = "day", y = "val"),
    list(type = "gantt", category = "visit", x = "day", xend = "day")
  )
  for (v in cases) {
    data <- if (identical(v$type, "histogram")) {
      chart_hist_data(d, chart_hist(v, d))
    } else {
      d
    }
    expect_identical(chart_has_summary(data, v),
                     !is.null(chart_summary(data, v)),
                     info = paste(names(v), unlist(v), collapse = " "))
  }
  expect_false(chart_has_summary(d[0, ], cases[[1L]]))
  expect_false(chart_has_summary(NULL, cases[[1L]]))
})

test_that("chart_summary_flat() puts the keys back as columns", {
  d <- dv_df()
  s <- chart_summary(d, list(type = "bar", category = "visit", color = "arm",
                             facet = "sex"))
  f <- chart_summary_flat(s)
  expect_identical(names(f), c("sex", "visit", "arm", "Count of rows"))
  expect_identical(nrow(f), nrow(s))
  expect_identical(sum(f[["Count of rows"]]), 60L)
  one <- chart_summary_flat(chart_summary_annotate(
    data.frame(row.names = 1L), data.frame(n = 3L)
  ))
  expect_identical(one, data.frame(n = 3L))
})

test_that("with a drill target, the view keeps the chart's last click", {
  blk <- new_chart_block(type = "bar", category = "visit", color = "arm",
                         drill = "auto", drill_target = "vf1")
  dv_run(blk, function(session, expr) {
    install_ctrl_send(function(x) NULL, session = session)
    dv_open(expr, session, "rows")
    expect_identical(dv_derived(expr)$rows, 60L)
    chart_input_click(expr, visit = "V2", arm = "B", nonce = 1L)
    session$flushReact()
    # The output stays whole; the view narrows to what went to the target.
    expect_identical(nrow(session$returned$result()), 60L)
    expect_identical(dv_derived(expr)$rows, 10L)
    menu <- Filter(function(d) identical(d$section, "Data"), dv_menu(expr))
    expect_true("All rows" %in% vapply(menu, `[[`, "", "label"))
    dv_open(expr, session, "summary", n = 2L)
    expect_match(dv_derived(expr)$table$config,
                 "\".variable_level\":[\"B\"]", fixed = TRUE)
    # Another block sending to the target leaves the click in place.
    session$userData$blockr_ctrl_send("vf1", list(state = list()),
                                      author = "other")
    session$flushReact()
    dv_open(expr, session, "rows", n = 3L)
    expect_identical(dv_derived(expr)$rows, 10L)
  })
})

test_that("a clear with a drill target drops the view's selection", {
  blk <- new_chart_block(type = "bar", category = "visit", drill = "auto",
                         drill_target = "vf1")
  dv_run(blk, function(session, expr) {
    install_ctrl_send(function(x) NULL, session = session)
    dv_open(expr, session, "rows")
    chart_input_click(expr, visit = "V1", nonce = 1L)
    session$flushReact()
    expect_identical(dv_derived(expr)$rows, 20L)
    chart_input_click(expr, nonce = 2L)
    session$flushReact()
    expect_identical(dv_derived(expr)$rows, 60L)
  })
})

test_that("with drill off, a brush narrows the view and filters nothing", {
  blk <- new_chart_block(type = "scatter", x = "day", y = "val")
  dv_run(blk, function(session, expr) {
    dv_open(expr, session, "rows")
    expr$setInputs(chart_peek = viz_read_selection(list(
      selection = list(type = "range", x = "day", y = "val",
                       x_range = list(1, 5), y_range = list(0, 200)),
      nonce = 1
    )))
    session$flushReact()
    expect_identical(nrow(session$returned$result()), 60L)
    expect_identical(dv_derived(expr)$rows, 15L)
    expr$setInputs(chart_peek = viz_read_selection(list(selection = NULL,
                                                        nonce = 2)))
    session$flushReact()
    expect_identical(dv_derived(expr)$rows, 60L)
  })
})
