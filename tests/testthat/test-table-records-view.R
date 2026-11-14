# The table block's data view: records a prepare script hands along
# (viz_records()), narrowed to the click, drawn under the table and written
# by the download menu's Data section (R/table-records-view.R).

rv_df <- function() {
  data.frame(USUBJID = c("01", "02", "03"), ARM = c("A", "B", "A"))
}

# Two records per subject, by the script.
rv_script <- function(label = "\"Adverse events\"") {
  paste0(
    "blockr.viz::viz_records(data, data.frame(",
    "USUBJID = rep(data$USUBJID, each = 2L), ",
    "AESEQ = rep(1:2, nrow(data)), ",
    "AETERM = paste0(\"TERM \", seq_len(2L * nrow(data)))), ",
    label, ")"
  )
}

rv_seen <- new.env()

rv_run <- function(blk, code) {
  dat <- shiny::reactive(rv_df())
  real <- viz_with_shared
  local_mocked_bindings(viz_with_shared = function(kit, parts, view) {
    build <- real(kit, parts, view)
    function() {
      p <- build()
      rv_seen$p <- p
      p
    }
  })
  rv_seen$p <- NULL
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

rv_derived <- function(expr) {
  tryCatch(expr$output$table, error = function(e) NULL)
  rv_seen$p$derived
}

rv_open <- function(expr, session, n = 1L) {
  expr$setInputs(table_data = viz_read_data_view(list(on = TRUE,
                                                      tab = "records",
                                                      n = n)))
  session$flushReact()
}

rv_click <- function(expr, session, id) {
  expr$setInputs(table_select = viz_read_selection(list(
    selection = list(type = "categorical",
                     filters = list(USUBJID = list(id)), columns = list()),
    nonce = 1
  )))
  session$flushReact()
}

rv_block <- function(...) {
  new_table_block(layout = "groups", by = "USUBJID", drill = "auto",
                  script = rv_script(), ...)
}

test_that("viz_records() attaches the records with their name", {
  x <- viz_records(rv_df(), data.frame(USUBJID = "01"), "Adverse events")
  r <- records_of(x)
  expect_identical(r$label, "Adverse events")
  expect_identical(r$data$USUBJID, "01")
  expect_identical(records_of(viz_records(rv_df(), NULL)), NULL)
  expect_identical(records_of(viz_records(rv_df(), rv_df()))$label, "Records")
  expect_null(records_of(rv_df()))
  expect_error(viz_records(rv_df(), "x"))
})

test_that("the records narrow on the clicked columns they carry", {
  rec <- data.frame(USUBJID = c("01", "01", "02", NA), AETERM = c("a", "b", "c", "d"))
  out <- records_narrow(rec, list(USUBJID = "01", Row = "Diarrhoea"))
  expect_identical(out$AETERM, c("a", "b"))
  expect_identical(rownames(out), c("1", "2"))
  expect_identical(nrow(records_narrow(rec, list())), 4L)
  expect_identical(nrow(records_narrow(rec, list(Row = "x"))), 4L)
  expect_identical(records_narrow(rec, list(USUBJID = NA_character_))$AETERM,
                   "d")
  expect_identical(names(records_filters(rec, list(Row = "x", USUBJID = "1"))),
                   "USUBJID")
})

test_that("a table without records offers no data view", {
  blk <- new_table_block(layout = "groups", by = "USUBJID")
  rv_run(blk, function(session, expr) {
    d <- rv_derived(expr)
    expect_false(d$data_view_tool)
    rv_open(expr, session)
    expect_null(rv_derived(expr)[["data_view"]])
    secs <- vapply(rv_derived(expr)$downloads,
                   function(e) e$section %||% "", "")
    expect_identical(unique(secs), "")
  })
})

test_that("the records show under their name and narrow to the click", {
  rv_run(rv_block(), function(session, expr) {
    expect_true(rv_derived(expr)$data_view_tool)
    expect_null(rv_derived(expr)[["data_view"]])
    rv_open(expr, session)
    dv <- rv_derived(expr)[["data_view"]]
    expect_identical(dv$tab, "records")
    expect_identical(dv$label, "Adverse events")
    expect_identical(dv[c("rows", "total")], list(rows = 6L, total = 6L))
    expect_match(dv$table$config, "\"search\":true", fixed = TRUE)
    expect_match(dv$table$config, "\"sortable\":true", fixed = TRUE)
    expect_identical(dv$table$data$n, 6L)
    rv_click(expr, session, "02")
    dv <- rv_derived(expr)[["data_view"]]
    expect_identical(dv[c("rows", "total")], list(rows = 2L, total = 6L))
    expect_identical(dv$table$data$n, 2L)
    expect_match(rv_derived(expr)$receipt, "02", fixed = TRUE)
  })
})

test_that("the menu writes the records as CSV and Excel under their name", {
  skip_if_not_installed("openxlsx")
  rv_run(rv_block(title = "Patients"), function(session, expr) {
    menu <- rv_derived(expr)$downloads
    secs <- vapply(menu, `[[`, "", "section")
    expect_identical(unique(secs), c("Table", "Data"))
    data <- menu[secs == "Data"]
    expect_identical(vapply(data, `[[`, "", "label"),
                     rep("Adverse events", 2L))
    expect_identical(nrow(utils::read.csv(expr$output$dl_data_rows_csv)), 6L)
    rv_click(expr, session, "03")
    data <- Filter(function(e) identical(e$section, "Data"),
                   rv_derived(expr)$downloads)
    expect_identical(vapply(data, `[[`, "", "label"),
                     rep(c("Adverse events", "All adverse events"),
                         each = 2L))
    csv <- utils::read.csv(expr$output$dl_data_rows_csv,
                           colClasses = "character")
    expect_identical(names(csv), c("USUBJID", "AESEQ", "AETERM"))
    expect_identical(unique(csv$USUBJID), "03")
    expect_identical(nrow(utils::read.csv(expr$output$dl_data_all_csv)), 6L)
    xl <- openxlsx::read.xlsx(expr$output$dl_data_rows_xlsx, colNames = FALSE)
    expect_identical(xl[[1L]][[1L]], "Patients")
    expect_identical(sum(xl[[1L]] == "03", na.rm = TRUE), 2L)
  })
})

test_that("a click sent to a drill target narrows nothing", {
  rv_run(rv_block(drill_target = "other"), function(session, expr) {
    rv_open(expr, session)
    # No board, so no channel to send the click down.
    expect_warning(rv_click(expr, session, "02"), "no control channel")
    expect_identical(rv_derived(expr)[["data_view"]]$rows, 6L)
  })
})
