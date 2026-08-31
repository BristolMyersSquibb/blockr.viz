skip_if_not_installed("blockr.assistant")

chart_desc <- function(state, x = new_chart_block()) {
  paste(
    blockr.assistant::describe_block(
      x, board = new_board(), id = "ch", state = state
    ),
    collapse = "\n"
  )
}

test_that("the description names what is mapped, in chart vocabulary", {

  out <- chart_desc(
    list(chart_type = "bar", group = "AESOC", color = "ARM",
         value = "USUBJID", func = "count_distinct")
  )

  expect_match(out, "Chart type: bar", fixed = TRUE)
  expect_match(out, "group = AESOC", fixed = TRUE)
  expect_match(out, "color = ARM", fixed = TRUE)
  expect_match(out, "Aggregation: count_distinct of USUBJID", fixed = TRUE)
})

test_that("unset arguments are not reported", {

  out <- chart_desc(list(chart_type = "bar", group = "AESOC"))

  # The constructor has 56 arguments; the default method's str() dump lists
  # every one of them, almost all NULL. That noise is the thing this replaces.
  # They still appear by name in the modifiable-argument line, which is the
  # point of that line -- what must not appear is a VALUE for an unset one.
  expect_false(grepl("facet =", out, fixed = TRUE))
  expect_false(grepl("smoother =", out, fixed = TRUE))
  expect_false(grepl("Initial block state", out, fixed = TRUE))
})

test_that("an active drill filter is called out as an active filter", {

  out <- chart_desc(
    list(chart_type = "bar", group = "AESOC", filter_type = "categorical",
         filter_column = "AESOC", filter_values = c("INFECTIONS", "NERVOUS"))
  )

  expect_match(out, "Drill filter: ACTIVE", fixed = TRUE)
  expect_match(out, "filter_column = AESOC", fixed = TRUE)
  expect_match(out, "filter_values = INFECTIONS, NERVOUS", fixed = TRUE)
})

test_that("no filter reads as passing the input through", {

  out <- chart_desc(list(chart_type = "bar", group = "AESOC"))

  expect_match(out, "Drill filter: none active", fixed = TRUE)
})

test_that("a long filter selection is capped, with its true length", {

  out <- chart_desc(
    list(chart_type = "bar", filter_column = "SOC",
         filter_values = as.character(1:40))
  )

  expect_match(out, "(40 values)", fixed = TRUE)
})

test_that("a chart with nothing mapped says so", {

  out <- chart_desc(list(chart_type = "bar"))

  expect_match(out, "nothing mapped yet", fixed = TRUE)
})

test_that("the modifiable-argument line survives, summarised", {

  out <- chart_desc(list(chart_type = "bar", group = "AESOC"))

  # A chart declares ~49 of its arguments controllable. Spelling them out cost
  # more of the prompt cap than the whole rest of the description, so the line
  # states the count and points at the typed tool for the schema.
  expect_match(out, "Modifiable via modify_block: all", fixed = TRUE)
  expect_match(out, "modify_chart_block", fixed = TRUE)
  expect_lt(nchar(out), 900L)
})

test_that("aggregation is reported only for the families that aggregate", {

  agg <- chart_desc(
    list(chart_type = "bar", group = "AESOC", value = "AGE", func = "mean")
  )
  ind <- chart_desc(
    list(chart_type = "scatter", x = "XMAX", y = "BILI", func = "count",
         value = ".count")
  )

  expect_match(agg, "Aggregation: mean of AGE", fixed = TRUE)
  # A scatter does not aggregate; `func`/`value` sit at their defaults and
  # "count of row counts" on an eDish plot is worse than saying nothing.
  expect_false(grepl("Aggregation", ind, fixed = TRUE))
})

test_that("without live state it falls back to the default method", {

  brd <- new_board(blocks = c(ch = new_chart_block(group = "AESOC")))

  out <- paste(
    blockr.assistant::describe_block(
      board_blocks(brd)[["ch"]], board = brd, id = "ch"
    ),
    collapse = "\n"
  )

  expect_match(out, "Initial block state", fixed = TRUE)
})
