# Every drill path of the table block, clicked in a browser (apps/table-drill).
# Each source table feeds a listing; a click must mark what was clicked, say
# so in the footer and filter the listing to exactly the rows a subset in R
# gives. A second click clears it, a click on another key replaces it, and a
# saved board restores the selection and the filter.
#
# The clicks are real mouse events at the row, card or cell (DevTools
# protocol), not element.click(). The app loads blockr.viz from this source
# tree (BLOCKR_VIZ_SRC). Skipped without a headless browser and under
# R CMD check, like test-table-sweep.R. On the Mac, Chrome cannot start in
# Claude's sandbox: start blockr.dev/scripts/chrome-remote.sh outside it and
# source blockr.dev/scripts/use-remote-chrome.R first.

run_drill <-
  run_browser_tests() &&
  requireNamespace("blockr.dock", quietly = TRUE) &&
  requireNamespace("blockr.dm", quietly = TRUE) &&
  requireNamespace("pkgload", quietly = TRUE)

drill_app_dir <- test_path("apps", "table-drill")
source(file.path(drill_app_dir, "data.R"), local = TRUE)

drill_app <- function(name, restore = NULL) {
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(
    BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")),
    BLOCKR_DRILL_RESTORE = restore %||% ""
  )
  configure_chromote()
  app <- shinytest2::AppDriver$new(
    drill_app_dir, name = name, width = 1500, height = 1000,
    load_timeout = 180 * 1000, timeout = 60 * 1000
  )
  app$wait_for_idle()
  app$run_js(paste(readLines(file.path(drill_app_dir, "drill.js")),
                   collapse = "\n"))
  app
}

drill_result <- function(app, id) {
  app$get_values(export = "result")$export$result[[id]]
}

drill_state <- function(app, id) {
  s <- app$get_js(sprintf("drillProbe.state('%s')", id))
  list(active = as.character(unlist(s$active)),
       footer = paste(unlist(s$footer), collapse = " "),
       rows = s$rows %||% 0L, cells = s$cells %||% 0L)
}

# Show a view and wait until its source has rows and its listing a result.
drill_view <- function(app, view, out, src = view) {
  # A background tab of a shared Chrome stops painting.
  try(app$get_chromote_session()$Page$bringToFront(), silent = TRUE)
  app$get_js(sprintf("drillProbe.view('%s')", view), timeout = 60 * 1000)
  t0 <- Sys.time()
  repeat {
    app$wait_for_idle()
    if (drill_state(app, src)$rows > 0 &&
          is.data.frame(drill_result(app, out))) break
    if (difftime(Sys.time(), t0, units = "secs") > 60) {
      stop("view ", view, " did not render")
    }
    Sys.sleep(1)
  }
}

# A left click at the middle of the row, card or cell labelled `label`.
drill_click <- function(app, id, label, cell = NULL) {
  cj <- if (is.null(cell)) "null" else if (is.numeric(cell)) cell else
    sprintf("'%s'", cell)
  p <- app$get_js(sprintf("drillProbe.at('%s', '%s', %s)", id, label, cj))
  if (is.null(p)) stop("nothing to click: ", id, " / ", label)
  cdp <- app$get_chromote_session()
  for (type in c("mousePressed", "mouseReleased")) {
    cdp$Input$dispatchMouseEvent(type = type, x = p$x, y = p$y,
                                 button = "left", clickCount = 1)
  }
  Sys.sleep(0.5)
  app$wait_for_idle()
  Sys.sleep(0.5)
  app$wait_for_idle()
}

# The listing holds exactly these rows, with their columns.
same_rows <- function(out, want) {
  if (!is.data.frame(out) || !all(names(want) %in% names(out))) return(FALSE)
  cols <- names(want)
  key <- function(d) {
    sort(do.call(paste, c(lapply(d[cols], as.character), sep = "\r")))
  }
  nrow(out) == nrow(want) && identical(key(out), key(want))
}

adsl <- drill_adsl
adae <- drill_adae
na_sex <- is.na(adsl$SEX)

# One drill path: the view, the block, what is clicked first (`a`) and next
# (`b`), the rows each claims, and the footer each shows. `lit_a` / `lit_b`
# are the rows lit after each click when that is more than the clicked one:
# a records click lights every row the filter keeps. `cell` / `cell_b`
# say where in the row to click (a td index or a selector; NULL the label).
# A click on a cell claims its row and its column. `expand` opens a
# collapsed parent row first. `none` is the listing with no click.
drill_paths <- list(
  list(view = "rec", a = "S-003", b = "S-001",
       want_a = adsl[adsl$SEX %in% "M", ], foot_a = "SEX = M",
       lit_a = adsl$USUBJID[adsl$SEX %in% "M"],
       want_b = adsl[adsl$SEX %in% "F", ], foot_b = "SEX = F",
       lit_b = adsl$USUBJID[adsl$SEX %in% "F"]),
  # A header claims its group; a row its group and its value in the drill
  # column (drill = "SEX").
  list(view = "nest", a = "Low dose", b = "S-033",
       cell = ".blockr-section-value", cell_b = NULL,
       want_a = adsl[adsl$ARM == "Low dose", ], foot_a = "ARM = Low dose",
       want_b = adsl[adsl$ARM == "Low dose" & adsl$SEX %in% "M", ],
       foot_b = "ARM = Low dose, SEX = M",
       lit_b = adsl$USUBJID[adsl$ARM == "Low dose" & adsl$SEX %in% "M"]),
  list(view = "rcards", a = "Low dose", b = "High dose",
       none = drill_arm_frame,
       want_a = drill_arm_frame[2, ], foot_a = "Arm = Low dose",
       want_b = drill_arm_frame[3, ], foot_b = "Arm = High dose"),
  list(view = "grp", a = "Low dose", b = "Placebo",
       want_a = adsl[adsl$ARM == "Low dose", ], foot_a = "ARM = Low dose",
       want_b = adsl[adsl$ARM == "Placebo", ], foot_b = "ARM = Placebo"),
  list(view = "grpm", a = "(Missing)", b = "M",
       want_a = adsl[na_sex, ], foot_a = "SEX = (Missing)",
       want_b = adsl[adsl$SEX %in% "M", ], foot_b = "SEX = M"),
  list(view = "ae", a = "Headache", b = "Headache", cell_b = 2L,
       expand = "Nervous system", none = adae,
       want_a = adae[adae$AEDECOD == "Headache", ],
       foot_a = "AESOC = Nervous system, AEDECOD = Headache",
       want_b = adae[adae$AEDECOD == "Headache" & adae$ARM == "Low dose", ],
       foot_b = "AESOC = Nervous system, AEDECOD = Headache, ARM = Low dose"),
  list(view = "col", a = "Low dose", b = "Low dose",
       cell_b = '.blockr-summarize-fill[data-seg="2"]',
       want_a = adsl[adsl$ARM == "Low dose", ], foot_a = "ARM = Low dose",
       want_b = adsl[adsl$ARM == "Low dose" & adsl$SEX %in% "M", ],
       foot_b = "ARM = Low dose, SEX = M"),
  # Heatmap columns: Headache, Nausea, ... (most frequent first).
  list(view = "heat", a = "S-033", b = "S-031", cell_b = 2L, none = adae,
       want_a = adae[adae$USUBJID == "S-033", ],
       foot_a = "ARM = Low dose, USUBJID = S-033",
       want_b = adae[adae$USUBJID == "S-031" & adae$AEDECOD == "Nausea", ],
       foot_b = "ARM = Low dose, USUBJID = S-031, AEDECOD = Nausea"),
  list(view = "cards", a = "Low dose", b = "High dose",
       want_a = adsl[adsl$ARM == "Low dose", ], foot_a = "ARM = Low dose",
       want_b = adsl[adsl$ARM == "High dose", ], foot_b = "ARM = High dose"),
  # drill = "source" resolves to source rows; with no click, all of them.
  list(view = "t1", a = "F", b = "(Missing)", cell_b = 1L,
       want_a = adsl[adsl$SEX %in% "F", ], foot_a = "SEX = F",
       want_b = adsl[na_sex & adsl$ARM == "Placebo", ],
       foot_b = "SEX = (Missing), ARM = Placebo")
)
names(drill_paths) <- vapply(drill_paths, `[[`, "", "view")

saved_board <- NULL

if (run_drill) {
  app <- drill_app("table-drill")
  withr::defer(app$stop(), teardown_env())
}

for (p in drill_paths) local({
  p <- p
  test_that(paste("drill:", p$view), {
    skip_if_not(run_drill, "no headless browser, or under R CMD check")
    id <- p$view
    out <- paste0(id, "_out")
    none <- p$none %||% adsl
    drill_view(app, id, out)
    if (!is.null(p$expand)) drill_click(app, id, p$expand, ".blockr-indent-btn")

    expect_true(same_rows(drill_result(app, out), none), label = "no click")

    drill_click(app, id, p$a, p[["cell"]])
    s <- drill_state(app, id)
    expect_setequal(s$active, p$lit_a %||% p$a)
    expect_identical(s$footer, paste("Filtered:", p$foot_a))
    expect_true(same_rows(drill_result(app, out), p$want_a),
                label = paste("listing after a click on", p$a))

    # A second click on the selected row clears it.
    drill_click(app, id, p$a, p[["cell"]])
    s <- drill_state(app, id)
    expect_length(s$active, 0L)
    expect_identical(s$footer, "")
    expect_true(same_rows(drill_result(app, out), none),
                label = "listing after the second click")

    # A click on another key replaces the first.
    cell_b <- if ("cell_b" %in% names(p)) p$cell_b else p[["cell"]]
    drill_click(app, id, p$a, p[["cell"]])
    drill_click(app, id, p$b, cell_b)
    s <- drill_state(app, id)
    expect_setequal(s$active, p$lit_b %||% p$b)
    expect_identical(s$footer, paste("Filtered:", p$foot_b))
    expect_true(same_rows(drill_result(app, out), p$want_b),
                label = paste("listing after a click on", p$b))
    if (!is.null(cell_b)) expect_gt(s$cells, 0L)

    # The same row or cell again clears; once more sets it for the restore.
    drill_click(app, id, p$b, cell_b)
    s <- drill_state(app, id)
    expect_length(s$active, 0L)
    expect_identical(s$cells, 0L)
    expect_true(same_rows(drill_result(app, out), none),
                label = paste("listing after a second click on", p$b))
    drill_click(app, id, p$b, cell_b)
  })
})

test_that("drill: drill_target sends the click to a drill filter block", {
  skip_if_not(run_drill, "no headless browser, or under R CMD check")
  drill_view(app, "ctrl", "ctrl_out", src = "tgrp")
  expect_true(same_rows(drill_result(app, "ctrl_out"), adsl))
  drill_click(app, "tgrp", "Low dose")
  expect_true(same_rows(drill_result(app, "ctrl_out"),
                        adsl[adsl$ARM == "Low dose", ]))
  # Nothing latches at the sender.
  expect_length(drill_state(app, "tgrp")$active, 0L)
  drill_click(app, "trec", "S-003")
  expect_true(same_rows(drill_result(app, "ctrl_out"),
                        adsl[adsl$SEX %in% "M", ]))
})

test_that("drill: a saved board restores every selection and filter", {
  skip_if_not(run_drill, "no headless browser, or under R CMD check")
  json <- app$get_values(export = "saved")$export$saved
  f <- withr::local_tempfile(fileext = ".json")
  writeLines(json, f)
  app2 <- drill_app("table-drill-restore", restore = f)
  withr::defer(app2$stop())
  for (p in drill_paths) {
    id <- p$view
    out <- paste0(id, "_out")
    drill_view(app2, id, out)
    s <- drill_state(app2, id)
    expect_true(p$b %in% s$active, label = paste(id, "marks", p$b))
    expect_identical(s$footer, paste("Filtered:", p$foot_b),
                     label = paste(id, "footer"))
    expect_true(same_rows(drill_result(app2, out), p$want_b),
                label = paste(id, "listing after restore"))
    if (!is.null(p[["cell_b"]])) {
      expect_gt(s$cells, 0L, label = paste(id, "marked cell after restore"))
    }
  }
  # A click on the restored selection clears it.
  drill_view(app2, "grp", "grp_out")
  drill_click(app2, "grp", "Placebo")
  expect_true(same_rows(drill_result(app2, "grp_out"), adsl))
})
