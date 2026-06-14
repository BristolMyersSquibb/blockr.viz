test_that("html_table() returns a tagList for a minimal flat input", {
  df <- tibble::tibble(
    .label  = c("n", "Mean (SD)"),
    KarXT   = c("100", "42.3 (5.1)"),
    Placebo = c("100", "43.1 (6.0)")
  )

  out <- html_table(df)
  expect_s3_class(out, "shiny.tag.list")

  html <- as.character(htmltools::tagList(out))
  expect_true(grepl("<table class=\"blockr-table\">", html, fixed = TRUE))
  expect_true(grepl("<thead>", html, fixed = TRUE))
  expect_true(grepl("<tbody>", html, fixed = TRUE))
  expect_true(grepl("blockr-html-table-container", html, fixed = TRUE))
  expect_false(grepl("class=\"blockr-section-header\"", html, fixed = TRUE))
})

test_that("html_table_delta_css() scope gates Table-1 typography to structured", {
  # The default scope keeps the body-cell rules on the generic container (used
  # by standalone html_table()).
  default_css <- html_table_delta_css()
  expect_true(grepl(
    ".blockr-html-table-container .blockr-table tbody td.blockr-data",
    default_css, fixed = TRUE
  ))

  # The drilldown block passes the structured-only scope so the medium-weight
  # cells cannot leak onto a sibling flat table (both share
  # `.blockr-html-table-container`, but only the structured one carries
  # `.drilldown-table-structured`).
  scoped_css <- html_table_delta_css(scope = ".drilldown-table-structured")
  expect_true(grepl(
    ".drilldown-table-structured .blockr-table tbody td.blockr-data",
    scoped_css, fixed = TRUE
  ))
  expect_false(grepl(
    ".blockr-html-table-container .blockr-table tbody td.blockr-data",
    scoped_css, fixed = TRUE
  ))
  # The generic table-body scope is gone, but the bare chrome rules (search
  # input) stay global so a flat table still gets a styled toolbar.
  expect_true(grepl("input.blockr-search", scoped_css, fixed = TRUE))
})

test_that("structured dt_chrome scopes the delta; flat dt_chrome omits it", {
  tbl <- htmltools::tags$table(class = "blockr-table")

  struct <- as.character(htmltools::tagList(
    dt_chrome("e", structured = TRUE, max_height = NULL, inner = tbl)
  ))
  expect_true(grepl("drilldown-table-structured", struct, fixed = TRUE))
  expect_true(grepl(
    ".drilldown-table-structured .blockr-table tbody td.blockr-data",
    struct, fixed = TRUE
  ))

  flat <- as.character(htmltools::tagList(
    dt_chrome("e", structured = FALSE, max_height = NULL, inner = tbl)
  ))
  expect_false(grepl("drilldown-table-structured", flat, fixed = TRUE))
  # The flat table never injects the Table-1 weight rule at all.
  expect_false(grepl("td.blockr-data", flat, fixed = TRUE))
})

test_that("html_table() emits section header rows for .section_1 boundaries", {
  df <- tibble::tibble(
    .section_1 = c("GI Disorders", "GI Disorders", "Nervous System", "Nervous System"),
    .label     = c("Nausea", "Vomiting", "Headache", "Dizziness"),
    KarXT      = c("12 (12%)", "9 (9%)", "18 (18%)", "10 (10%)"),
    Placebo    = c("8 (8%)", "5 (5%)", "15 (15%)", "7 (7%)")
  )
  attr(df$.section_1, "label") <- "Body System or Organ Class"

  html <- as.character(htmltools::tagList(html_table(df)))

  expect_equal(
    length(gregexpr("class=\"blockr-section-header\"", html, fixed = TRUE)[[1]]),
    2L
  )
  expect_true(grepl("GI Disorders", html, fixed = TRUE))
  expect_true(grepl("Nervous System", html, fixed = TRUE))
  expect_true(grepl("Body System or Organ Class", html, fixed = TRUE))
  # .section_1 values must not leak into data cells
  expect_false(grepl("<td class=\"blockr-data\">GI Disorders", html, fixed = TRUE))
})

test_that("html_table() nests sections for multiple .section_* columns", {
  df <- tibble::tibble(
    .section_1 = c("Cardiac", "Cardiac", "Nervous", "Nervous"),
    .section_2 = c("Arrhythmia", "Arrhythmia", "Headache", "Headache"),
    .label     = c("Mild", "Severe", "Mild", "Severe"),
    Total      = c("4 (4%)", "2 (2%)", "5 (5%)", "1 (1%)")
  )
  attr(df$.section_1, "label") <- "System"
  attr(df$.section_2, "label") <- "Subclass"

  html <- as.character(htmltools::tagList(html_table(df)))

  expect_true(grepl("data-level=\"1\"", html, fixed = TRUE))
  expect_true(grepl("data-level=\"2\"", html, fixed = TRUE))
  expect_true(grepl("level-1", html, fixed = TRUE))
  expect_true(grepl("level-2", html, fixed = TRUE))
})

test_that("html_table() builds two-level column spanners from pipe-delimited names", {
  df <- tibble::tibble(
    .label            = c("n", "Mean (SD)"),
    `KarXT|Week 2`    = c("196", "124.3 (52.2)"),
    `KarXT|Week 4`    = c("205", "128.4 (51.8)"),
    `Placebo|Week 2`  = c("198", "120.1 (51.1)"),
    `Placebo|Week 4`  = c("200", "121.2 (54.1)")
  )
  attr(df$`KarXT|Week 2`, "label")   <- "Week 2"
  attr(df$`KarXT|Week 4`, "label")   <- "Week 4"
  attr(df$`Placebo|Week 2`, "label") <- "Week 2"
  attr(df$`Placebo|Week 4`, "label") <- "Week 4"

  html <- as.character(htmltools::tagList(html_table(df)))

  expect_true(grepl("colspan=\"2\"", html, fixed = TRUE))
  expect_true(grepl(">KarXT<", html, fixed = TRUE))
  expect_true(grepl(">Placebo<", html, fixed = TRUE))
  expect_true(grepl("Week 2", html, fixed = TRUE))
  expect_true(grepl("Week 4", html, fixed = TRUE))
})

test_that("html_table() handles mixed-depth columns via rowspan", {
  df <- tibble::tibble(
    .label           = c("n"),
    Total            = c("400"),
    `KarXT|Week 2`   = c("196"),
    `KarXT|Week 4`   = c("205")
  )

  html <- as.character(htmltools::tagList(html_table(df)))

  # "Total" is depth 1 while KarXT cells are depth 2 → max_depth = 2 →
  # Total should get rowspan=2 so it sits against both header rows. The
  # <th rowspan="2"> wraps the name in a dt-th-namerow div / arm__name span,
  # so allow intervening open tags between the attribute and the text.
  expect_true(grepl(
    "rowspan=\"2\"[^>]*>([[:space:]]*<[^>]*>)*[[:space:]]*Total", html
  ))
})

test_that("html_table() renders leaf-level attr(col, 'label') as HTML", {
  df <- tibble::tibble(
    .label  = c("n"),
    KarXT   = c("210"),
    Placebo = c("208")
  )
  attr(df$KarXT,   "label") <- "<strong>KarXT</strong><br>N = 210"
  attr(df$Placebo, "label") <- "<strong>Placebo</strong><br>N = 208"

  html <- as.character(htmltools::tagList(html_table(df)))

  expect_true(grepl("<strong>KarXT</strong><br>N = 210", html, fixed = TRUE))
  expect_true(grepl("<strong>Placebo</strong><br>N = 208", html, fixed = TRUE))
})

test_that("html_table() hides internal dotted columns from data cells", {
  df <- tibble::tibble(
    .section_1 = c("A", "A"),
    .var       = c("AGE", "AGE"),
    .label     = c("n", "Mean"),
    Total      = c("100", "42.3")
  )

  html <- as.character(htmltools::tagList(html_table(df)))
  expect_false(grepl("<td class=\"blockr-data\">AGE", html, fixed = TRUE))
  expect_false(grepl("<td class=\"blockr-data\">A<", html, fixed = TRUE))
  expect_true(grepl("<td class=\"blockr-stub\">n<", html, fixed = TRUE))
})

test_that("html_table() renders NA section values as '(missing)'", {
  df <- tibble::tibble(
    .section_1 = c(NA, "Known"),
    .label     = c("Row 1", "Row 2"),
    Value      = c("1", "2")
  )
  attr(df$.section_1, "label") <- "System"

  html <- as.character(htmltools::tagList(html_table(df)))
  expect_true(grepl("(missing)", html, fixed = TRUE))
})

test_that("html_table() rejects legacy long-format input", {
  df <- tibble::tibble(
    label   = c("A", "B"),
    depth   = c(0L, 1L),
    col_var = c("x", "x"),
    value   = c("1", "2")
  )
  expect_error(html_table(df), "legacy long-format")
})

test_that("default_expanded = FALSE sets the data attribute on the wrapper", {
  df <- tibble::tibble(.label = "n", Total = "100")
  html_expanded   <- as.character(htmltools::tagList(html_table(df, default_expanded = TRUE)))
  html_collapsed  <- as.character(htmltools::tagList(html_table(df, default_expanded = FALSE)))
  expect_true(grepl("data-initial-expanded=\"1\"", html_expanded, fixed = TRUE))
  expect_true(grepl("data-initial-expanded=\"0\"", html_collapsed, fixed = TRUE))
})

test_that("html_table() round-trips a summary_table() output", {
  df <- data.frame(
    TRT02P = rep(c("KarXT", "Placebo"), each = 10),
    AGE    = c(40:49, 42:51),
    stringsAsFactors = FALSE
  )
  st <- summary_table(df, vars = "AGE", by = "TRT02P", stats = "expanded")

  expect_silent(tags <- html_table(st, title = "Demographics"))
  html <- as.character(htmltools::tagList(tags))
  expect_true(grepl("Demographics", html, fixed = TRUE))
  expect_true(grepl("blockr-html-table", html, fixed = TRUE))
})

test_that("leaf data column headers carry data-col-index and sortable class", {
  df <- tibble::tibble(
    .label  = c("n", "Mean"),
    KarXT   = c("100", "42.3"),
    Placebo = c("100", "43.1")
  )
  html <- as.character(htmltools::tagList(html_table(df)))
  # Two sortable leaves at indices 1 and 2 (stub is col 0, not sortable)
  expect_true(grepl("data-col-index=\"1\"", html, fixed = TRUE))
  expect_true(grepl("data-col-index=\"2\"", html, fixed = TRUE))
  expect_true(grepl("class=\"blockr-col-header leaf blockr-sortable\"", html, fixed = TRUE))
  # Stub header is NOT sortable by default
  expect_false(grepl("blockr-stub-header blockr-sortable", html, fixed = TRUE))
})

test_that("merged spanners across data columns are not sortable", {
  df <- tibble::tibble(
    .label           = c("n"),
    `KarXT|Week 2`   = c("1"),
    `KarXT|Week 4`   = c("2"),
    `Placebo|Week 2` = c("3"),
    `Placebo|Week 4` = c("4")
  )
  html <- as.character(htmltools::tagList(html_table(df)))
  # Each leaf with span=1 should get a sortable class — there are four of them.
  expect_equal(
    length(gregexpr("blockr-col-header leaf blockr-sortable", html, fixed = TRUE)[[1]]),
    4L
  )
  # The KarXT and Placebo group <th>s (colspan=2) must NOT be sortable.
  expect_false(grepl("class=\"blockr-col-header group blockr-sortable\"", html,
                     fixed = TRUE))
})

test_that("html_table() renders a search input and toolbar", {
  df <- tibble::tibble(.label = "n", Total = "1")
  html <- as.character(htmltools::tagList(html_table(df)))
  expect_true(grepl("class=\"blockr-html-table-header\"", html, fixed = TRUE))
  expect_true(grepl("class=\"blockr-search\"", html, fixed = TRUE))
  expect_true(grepl("type=\"search\"", html, fixed = TRUE))
})

test_that("html_table() places the table inside a scroll container with max-height", {
  df <- tibble::tibble(.label = "n", Total = "1")
  html <- as.character(htmltools::tagList(html_table(df, max_height = "400px")))
  expect_true(grepl("class=\"blockr-table-wrapper\"", html, fixed = TRUE))
  expect_true(grepl("max-height:400px", html, fixed = TRUE))
})

test_that("html_table() applies hidden .indent/.bold/.italic styling columns", {
  df <- tibble::tibble(
    .label  = c("Row 1", "Row 2", "Row 3"),
    .indent = c(0L, 1L, 2L),
    .bold   = c(TRUE, FALSE, FALSE),
    .italic = c(FALSE, TRUE, FALSE),
    Total   = c("a", "b", "c")
  )
  html <- as.character(htmltools::tagList(html_table(df)))
  # Stub padding is a 24px base (aligns indented rows with the section-header
  # label) + 16px per indent level: level-1 -> 40px, level-2 -> 56px.
  expect_true(grepl("padding-left:40px", html, fixed = TRUE))
  expect_true(grepl("padding-left:56px", html, fixed = TRUE))
  # Bold and italic classes on data rows
  expect_true(grepl("blockr-data-row blockr-bold", html, fixed = TRUE))
  expect_true(grepl("blockr-data-row blockr-italic", html, fixed = TRUE))
  # Styling columns are not rendered as data cells
  expect_false(grepl("<td class=\"blockr-data\">TRUE", html, fixed = TRUE))
  expect_false(grepl("<td class=\"blockr-data\">FALSE", html, fixed = TRUE))
  expect_false(grepl("<td class=\"blockr-data\">0", html, fixed = TRUE))
})

test_that("collapse JS recomputes visibility from ancestor state", {
  # Structural test: the emitted JS should use a recomputeCollapse function
  # (state-driven), not a one-shot toggle loop that forgets nested state.
  df <- tibble::tibble(.label = "n", Total = "1")
  html <- as.character(htmltools::tagList(html_table(df)))
  expect_true(grepl("recomputeCollapse", html, fixed = TRUE))
  expect_true(grepl("anyAncestorCollapsed", html, fixed = TRUE))
})
