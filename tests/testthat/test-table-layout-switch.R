# The table block's "Rows are" switch, clicked around in a browser on the
# sweep board (apps/table-sweep, probe in layout-switch.js). After every
# pick:
#
#   (a) the block shows the picked layout, after one click
#   (b) the open gear highlights the layout on screen, and it is the only
#       gear and the only open tray in the block; in the variables layout it
#       holds the Table 1 settings and no "Finished table" badge
#   (c) closing and reopening the gear shows the same (after every second
#       pick; the other picks go to the gear the switch left open)
#   (d) the block's state (`layout`) is the pick
#
# Each block starts in one layout (Listing: records, Groups: groups, Table 1:
# variables). It runs a fixed sequence, then for each layout a seeded walk of
# 20 picks starting from it. Skipped like test-table-sweep.R. Takes a few
# minutes.

run_layout_switch <-
  !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
  requireNamespace("shinytest2", quietly = TRUE) &&
  requireNamespace("chromote", quietly = TRUE) &&
  requireNamespace("blockr.dock", quietly = TRUE) &&
  requireNamespace("pkgload", quietly = TRUE) &&
  chromote_works()

test_that("one click on \"Rows are\" switches, and the gear follows", {
  skip_if_not(run_layout_switch, "no headless browser, or under R CMD check")
  Sys.setenv(NOT_CRAN = "true")
  withr::local_envvar(BLOCKR_VIZ_SRC = normalizePath(test_path("..", "..")))
  configure_chromote()

  app <- shinytest2::AppDriver$new(
    test_path("apps", "table-sweep"),
    name = "table-layout-switch", width = 1400, height = 1000,
    load_timeout = 120 * 1000, timeout = 60 * 1000
  )
  withr::defer(app$stop())
  app$wait_for_idle()

  for (f in c("sweep.js", "layout-switch.js")) {
    app$run_js(paste(readLines(test_path("apps", "table-sweep", f)),
                     collapse = "\n"))
  }

  js <- function(code) {
    jsonlite::fromJSON(app$get_js(
      paste0("(async () => JSON.stringify((await ", code, ") ?? null))()"),
      timeout = 60 * 1000
    ))
  }
  saved <- function(id) app$get_values(export = "layouts")$export$layouts[[id]]

  fails <- character()
  check <- function(id, step, pick, after, again) {
    want <- tolower(pick)
    bad <- c(
      if (!identical(after$shown, want)) {
        sprintf("(a) shows %s", after$shown %||% "nothing")
      },
      if (!identical(after$selected, after$shown)) {
        sprintf("(b) gear highlights %s", after$selected %||% "nothing")
      },
      if (after$gears != 1L || after$open != 1L) {
        sprintf("(b) %d gears, %d open trays", after$gears, after$open)
      },
      if (identical(after$shown, "variables") &&
            (!isTRUE(after$table1) || isTRUE(after$badge))) {
        "(b) variables gear lacks the Table 1 settings or shows the badge"
      },
      if (!is.null(again) && again$closedOpen != 0L) "(c) gear does not close",
      if (!is.null(again) &&
            (!identical(again$selected, after$shown) || again$open != 1L)) {
        sprintf("(c) reopened gear highlights %s",
                again$selected %||% "nothing")
      },
      if (!identical(saved(id), want)) {
        sprintf("(d) state is %s", saved(id) %||% "NULL")
      }
    )
    if (length(bad)) {
      fails <<- c(fails, sprintf("%s, %s, pick %s: %s", id, step, pick,
                                 paste(bad, collapse = "; ")))
    }
  }

  # `reopen`: after which picks to close and reopen the gear for (c). In
  # between, the next pick goes to the gear the switch left open, where a
  # stale highlight would swallow the click.
  run <- function(id, picks, label, reopen = function(i) i %% 2L == 0L) {
    for (i in seq_along(picks)) {
      after <- js(sprintf("blockrLayout.pick('%s', '%s')", id, picks[[i]]))
      again <- if (reopen(i)) js(sprintf("blockrLayout.reopen('%s')", id))
      if (nzchar(Sys.getenv("BLOCKR_LAYOUT_LOG"))) {
        message(sprintf(
          "%s %s #%d %s: shown %s, selected %s, gears %d, open %d; reopened %s; state %s",
          id, label, i, picks[[i]], after$shown %||% "-",
          after$selected %||% "-", after$gears, after$open,
          if (is.null(again)) "-" else again$selected %||% "none",
          saved(id) %||% "-"
        ))
      }
      check(id, sprintf("%s #%d", label, i), picks[[i]], after, again)
    }
  }

  layouts <- c("Records", "Groups", "Variables")
  # A walk: every pick is one of the two layouts not on screen.
  walk <- function(from, n) {
    out <- character(n)
    cur <- from
    for (i in seq_len(n)) {
      cur <- sample(setdiff(layouts, cur), 1L)
      out[i] <- cur
    }
    out
  }

  blocks <- list(
    list(id = "listing", view = "1. Records", tab = "Listing",
         start = "Records"),
    list(id = "groups", view = "2. Groups", tab = "Groups",
         start = "Groups"),
    list(id = "demog", view = "3. Variables", tab = NULL,
         start = "Variables")
  )

  set.seed(20261026)
  for (b in blocks) {
    js(sprintf("blockrLayout.show('%s', %s)", b$view,
               if (is.null(b$tab)) "null" else sprintf("'%s'", b$tab)))
    expect_identical(js(sprintf("blockrLayout.read('%s')", b$id))$shown,
                     tolower(b$start))
    expect_identical(saved(b$id), tolower(b$start))
    js(sprintf("blockrLayout.openGear('%s')", b$id))

    # The sequence that showed the bug, from the groups layout.
    run(b$id, "Groups", "to start")
    run(b$id, c("Variables", "Groups", "Records", "Variables", "Records",
                "Groups", "Variables"), "sequence", function(i) FALSE)

    for (from in layouts) {
      run(b$id, from, sprintf("to %s", from))
      run(b$id, walk(from, 20L), sprintf("walk from %s", from))
    }
  }

  expect(length(fails) == 0L,
         paste0(length(fails), " picks went wrong:\n",
                paste(fails, collapse = "\n")))
})
