# Display-option toggles for the table block: sortable / collapsible / search /
# excel_download. The gear popover sends them as `config` messages; the renderer
# (dt_table_tag / dt_chrome) honours them and mirrors their state onto the
# <table> as data-dt-* attrs so the gear can read them back.

library(shiny)

render <- function(tag) as.character(htmltools::renderTags(tag)$html)

# --- renderer: sortable ------------------------------------------------------

test_that("sortable gates the sort hooks on a flat table", {
  df <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)

  on  <- render(dt_table_tag(df, sortable = TRUE))
  off <- render(dt_table_tag(df, sortable = FALSE))

  expect_true(grepl("blockr-sortable", on))
  expect_true(grepl("blockr-sort-icon", on))
  expect_false(grepl("blockr-sortable", off))
  expect_false(grepl("blockr-sort-icon", off))

  # State is mirrored on the table for the gear to read back.
  expect_true(grepl("data-dt-sortable=\"on\"", on))
  expect_true(grepl("data-dt-sortable=\"off\"", off))
})

# --- renderer: collapsible (structured / indented frame) ---------------------

test_that("collapsible gates the chevron toggles on a structured table", {
  # A parent row over a deeper-indented child becomes an indent toggle.
  df <- data.frame(
    .label  = c("Parent", "Child"),
    .indent = c(0L, 1L),
    Total   = c("", "5"),
    check.names = FALSE
  )

  on  <- render(dt_table_tag(df, collapsible = TRUE))
  off <- render(dt_table_tag(df, collapsible = FALSE))

  expect_true(grepl("blockr-indent-btn", on))
  expect_true(grepl("blockr-indent-toggle", on))
  expect_false(grepl("blockr-indent-btn", off))
  expect_false(grepl("blockr-indent-toggle", off))

  expect_true(grepl("data-dt-collapsible=\"on\"", on))
  expect_true(grepl("data-dt-collapsible=\"off\"", off))
})

test_that("collapsible off makes section headers static (no button)", {
  df <- data.frame(
    .group1_level = c("GI", "GI"),
    .label     = c("Nausea", "Vomiting"),
    Total      = c("3", "4"),
    check.names = FALSE
  )

  on  <- render(dt_table_tag(df, collapsible = TRUE))
  off <- render(dt_table_tag(df, collapsible = FALSE))

  expect_true(grepl("blockr-section-btn\"", on))          # a <button>
  expect_false(grepl("blockr-section-btn\"", off))        # not the plain button
  expect_true(grepl("blockr-section-btn-static", off))    # a static <span>
})

# --- renderer: data-dt-* mirror for search / download ------------------------

test_that("search / download states are mirrored onto the table", {
  df <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  h  <- render(dt_table_tag(df, search = FALSE, download = TRUE))
  expect_true(grepl("data-dt-search=\"off\"", h))
  expect_true(grepl("data-dt-download=\"on\"", h))

  off <- render(dt_table_tag(df))
  expect_true(grepl("data-dt-download=\"off\"", off))
})

# --- chrome: search input toggle --------------------------------------------

test_that("dt_chrome includes the search input only when search is on", {
  inner <- htmltools::tags$div("x")
  on  <- render(dt_chrome("id", FALSE, "600px", inner, search = TRUE))
  off <- render(dt_chrome("id", FALSE, "600px", inner, search = FALSE))
  # Match the input element (type="search"), not the .blockr-search CSS rules.
  expect_true(grepl("type=\"search\"", on))
  expect_false(grepl("type=\"search\"", off))
})

# --- server: the gear config messages flip the toggle state ------------------

test_that("config toggle messages update state (on/off and logical back-compat)", {
  df  <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block(values = "val")

  cfg <- function(session, param, value) {
    session$setInputs(drilldown_table_block_action = list(
      action = "config", param = param, value = value
    ))
  }

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    # Defaults: display features on, download off.
    expect_true(session$returned$state$sortable())
    expect_true(session$returned$state$collapsible())
    expect_true(session$returned$state$search())
    expect_false(session$returned$state$download())

    # Segmented pills emit "on"/"off".
    cfg(session, "sortable", "off")
    expect_false(session$returned$state$sortable())
    cfg(session, "collapsible", "off")
    expect_false(session$returned$state$collapsible())
    cfg(session, "search", "off")
    expect_false(session$returned$state$search())
    cfg(session, "download", "on")
    expect_true(session$returned$state$download())

    cfg(session, "sortable", "on")
    expect_true(session$returned$state$sortable())

    # Restore / constructor path may pass a logical — accepted too.
    cfg(session, "search", TRUE)
    expect_true(session$returned$state$search())
    cfg(session, "download", FALSE)
    expect_false(session$returned$state$download())

    # The legacy per-format formals survive as serialized NULLs, so a board
    # saved now carries the one toggle and nothing else.
    expect_null(session$returned$state$excel_download())
    expect_null(session$returned$state$html_download())
    expect_null(session$returned$state$pptx_download())
  })
})

test_that("a board saved with any per-format toggle restores as download on", {
  df <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)

  # Called literally, never through do.call(): blockr.core reads the ctor out
  # of the calling expression (see resolve_ctor), so a constructed call is not
  # a block a board could reopen.
  testServer(new_table_block(excel_download = TRUE)$expr_server,
             args = list(data = reactive(df)), {
    expect_true(session$returned$state$download())
  })
  testServer(new_table_block(html_download = TRUE)$expr_server,
             args = list(data = reactive(df)), {
    expect_true(session$returned$state$download())
  })
  testServer(new_table_block(pptx_download = TRUE)$expr_server,
             args = list(data = reactive(df)), {
    expect_true(session$returned$state$download())
  })

  # ... and one saved before downloads existed stays off.
  testServer(new_table_block()$expr_server, args = list(data = reactive(df)), {
    expect_false(session$returned$state$download())
  })
})
