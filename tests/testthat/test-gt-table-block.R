test_that("gt_table hides dotted styling columns and applies tab_style", {
  d <- tibble::tibble(
    .label   = c("Non-white", "American Indian", "Asian", "Black", "White"),
    .indent  = c(0L, 1L, 1L, 1L, 0L),
    .bold    = c(TRUE, FALSE, FALSE, FALSE, TRUE),
    .italic  = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    KarXT    = c("14", "2", "2", "10", "87")
  )

  tbl <- gt_table(d, title = "t")
  html <- as.character(gt::as_raw_html(tbl))

  # dotted columns hidden — no <th id="...indent..."> or similar
  expect_false(grepl(">\\.indent<", html))
  expect_false(grepl(">\\.bold<",   html))
  expect_false(grepl(">\\.italic<", html))

  # extract just the row-label stub cells
  stub_lines <- regmatches(html,
    gregexpr("<th[^>]*gt_stub[^>]*>[^<]+</th>", html))[[1]]
  expect_length(stub_lines, 5L)

  # row 1 (Non-white): bold, no indent
  expect_true(grepl("Non-white", stub_lines[1]))
  expect_true(grepl("font-weight: bold", stub_lines[1]))
  expect_false(grepl("text-indent: 16px", stub_lines[1]))

  # row 2 (American Indian): indented, not bold
  expect_true(grepl("American Indian", stub_lines[2]))
  expect_true(grepl("text-indent: 16px", stub_lines[2]))

  # row 3 (Asian): indented AND italic
  expect_true(grepl("Asian", stub_lines[3]))
  expect_true(grepl("text-indent: 16px", stub_lines[3]))
  expect_true(grepl("font-style: italic", stub_lines[3]))

  # row 5 (White): bold, no indent
  expect_true(grepl("White", stub_lines[5]))
  expect_true(grepl("font-weight: bold", stub_lines[5]))
  expect_false(grepl("text-indent: 16px", stub_lines[5]))
})

test_that("gt_table multi-level .indent scales by level * 16", {
  d <- tibble::tibble(
    .label  = c("top", "child", "grandchild"),
    .indent = c(0L, 1L, 2L),
    val     = c("a", "b", "c")
  )
  html <- as.character(gt::as_raw_html(gt_table(d)))

  stub_lines <- regmatches(html,
    gregexpr("<th[^>]*gt_stub[^>]*>[^<]+</th>", html))[[1]]
  expect_false(grepl("text-indent",       stub_lines[1]))
  expect_true( grepl("text-indent: 16px", stub_lines[2]))
  expect_true( grepl("text-indent: 32px", stub_lines[3]))
})

test_that("gt_table renders a tibble with only .indent (no .label/.section)", {
  d <- tibble::tibble(
    stub    = c("a", "b"),
    .indent = c(0L, 1L),
    val     = c("1", "2")
  )
  # No .label/.section — .indent alone is a no-op because there is no
  # stub column to indent, but the call must not error and the .indent
  # column must still be hidden from display.
  html <- as.character(gt::as_raw_html(gt_table(d)))
  expect_false(grepl(">\\.indent<", html))
  expect_true(grepl(">stub<", html))
})
