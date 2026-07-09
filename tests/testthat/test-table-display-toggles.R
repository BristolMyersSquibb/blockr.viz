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

# --- renderer: data-dt-* mirror for search / excel ---------------------------

test_that("search / excel states are mirrored onto the table", {
  df <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  h  <- render(dt_table_tag(df, search = FALSE, excel_download = TRUE))
  expect_true(grepl("data-dt-search=\"off\"", h))
  expect_true(grepl("data-dt-excel=\"on\"", h))
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
    # Defaults: display features on, export off.
    expect_true(session$returned$state$sortable())
    expect_true(session$returned$state$collapsible())
    expect_true(session$returned$state$search())
    expect_false(session$returned$state$excel_download())

    # Segmented pills emit "on"/"off".
    cfg(session, "sortable", "off")
    expect_false(session$returned$state$sortable())
    cfg(session, "collapsible", "off")
    expect_false(session$returned$state$collapsible())
    cfg(session, "search", "off")
    expect_false(session$returned$state$search())
    cfg(session, "excel_download", "on")
    expect_true(session$returned$state$excel_download())

    cfg(session, "sortable", "on")
    expect_true(session$returned$state$sortable())

    # Restore / constructor path may pass a logical — accepted too.
    cfg(session, "search", TRUE)
    expect_true(session$returned$state$search())
    cfg(session, "excel_download", FALSE)
    expect_false(session$returned$state$excel_download())
  })
})
