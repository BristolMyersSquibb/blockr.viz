wire_spec <- function() {
  viz_spec(
    "chart",
    x = viz_arg("column", null = "unset"),
    format = viz_arg("format", default = "number"),
    bins = viz_arg("integer", default = 30L),
    flag = viz_arg("bool", default = FALSE),
    tooltip = viz_arg("columns", null = "none"),
    y_lines = viz_arg("numbers", null = "none"),
    x_lines = viz_arg(type_values(of = "x"), null = "none"),
    highlight = viz_arg(type_record(column = "column",
                                    values = type_values(of = "column")),
                        null = "none"),
    x_ranges = viz_arg(type_records(name = "string", from = "string",
                                    to = "string"), null = "none"),
    script_values = viz_arg("map", default = list())
  )
}

wired <- function(values) {
  s <- wire_spec()
  v <- viz_check(s, values)
  jsonlite::fromJSON(viz_wire(s, v), simplifyVector = FALSE)
}

test_that("every spec key is on the wire, NULL as null", {
  w <- wired(list())
  expect_named(w, names(wire_spec()$args))
  expect_null(w$x)
  expect_null(w$tooltip)
  expect_identical(w$script_values, setNames(list(), character()))
  expect_identical(
    as.character(viz_wire(wire_spec(), viz_check(wire_spec(), list()))),
    paste0('{"x":null,"format":"number","bins":30,"flag":false,',
           '"tooltip":null,"y_lines":null,"x_lines":null,"highlight":null,',
           '"x_ranges":null,"script_values":{}}')
  )
})

test_that("vector types are arrays at length 1 and 2", {
  for (n in 1:2) {
    cols <- c("a", "b")[seq_len(n)]
    w <- wired(list(tooltip = cols, y_lines = c(Target = 4, 6)[seq_len(n)],
                    x_lines = cols))
    expect_length(w$tooltip, n)
    expect_type(w$tooltip, "list")
    expect_length(w$y_lines, n)
    expect_identical(w$y_lines[[1L]], list(value = 4L, label = "Target"))
    expect_length(w$x_lines, n)
    expect_null(w$x_lines[[1L]]$label)
  }
})

test_that("records and maps keep their shape", {
  w <- wired(list(
    highlight = list(column = "country", values = "CH"),
    x_ranges = list(list(name = "Recession", from = "2008-01-01",
                         to = "2009-06-30")),
    script_values = list(site = "A")
  ))
  expect_identical(w$highlight$column, "country")
  expect_identical(w$highlight$values, list(list(value = "CH", label = NULL)))
  expect_identical(w$x_ranges[[1L]]$name, "Recession")
  expect_identical(w$script_values, list(site = "A"))
})

test_that("what goes on the wire comes back through the reader", {
  s <- wire_spec()
  v <- viz_check(s, list(
    x = "day", format = 2L, tooltip = "a", y_lines = c(Target = 4),
    x_lines = c("2024-01-02"), highlight = list(column = "c", values = 1),
    x_ranges = list(list(name = "R", from = "1", to = "2"))
  ))
  back <- viz_read_set(s, list(
    rev = 0, values = jsonlite::fromJSON(viz_wire(s, v),
                                         simplifyVector = FALSE)
  ))
  expect_identical(back$rejected, character())
  expect_identical(back$values, v)
})

test_that("rows: classes, NA and meta", {
  df <- data.frame(
    s = c("a", NA, "b"),
    f = factor(c("lo", "hi", NA), levels = c("lo", "mid", "hi")),
    n = c(1.5, NA, Inf),
    b = c(TRUE, NA, FALSE),
    d = as.Date(c("2024-01-01", NA, "2024-01-03")),
    t = as.POSIXct(c("2024-01-01 12:00:00", NA, NA), tz = "Europe/Zurich"),
    u = as.difftime(c(1, 2, NA), units = "days"),
    stringsAsFactors = FALSE
  )
  attr(df$n, "label") <- "Amount"
  w <- viz_wire_df(df)
  rows <- jsonlite::fromJSON(w$data, simplifyVector = FALSE)
  expect_identical(rows$s, list("a", NULL, "b"))
  expect_identical(rows$f$`__enc__`, "dict")
  expect_identical(unlist(rows$f$levels), c("lo", "mid", "hi"))
  expect_identical(rows$f$codes, list(0L, 2L, NULL))
  expect_identical(rows$n, list(1.5, NULL, NULL))
  expect_identical(rows$b, list(TRUE, NULL, FALSE))
  expect_identical(rows$d, list("2024-01-01", NULL, "2024-01-03"))
  expect_equal(rows$t[[1L]], as.numeric(df$t[[1L]]) * 1000)
  expect_identical(rows$u, list(1L, 2L, NULL))

  meta <- stats::setNames(w$columns, vapply(w$columns, `[[`, "", "name"))
  expect_identical(meta$n$label, "Amount")
  expect_identical(meta$d$class, "date")
  expect_identical(meta$t$tz, "Europe/Zurich")
  expect_identical(meta$u$unit, "days")
  expect_identical(meta$f$n_unique, 3L)
})

test_that("rows: unknown classes are left out with a warning", {
  df <- data.frame(a = 1:2)
  df$l <- list(1, "x")
  expect_warning(w <- viz_wire_df(df), "Left out .*`l`")
  expect_named(jsonlite::fromJSON(w$data), "a")
})

test_that("rows: long repeated columns travel as a dictionary", {
  df <- data.frame(g = rep(c("x", "y"), 300), v = seq_len(600))
  rows <- jsonlite::fromJSON(viz_wire_df(df)$data, simplifyVector = FALSE)
  expect_identical(unlist(rows$g$levels), c("x", "y"))
  expect_length(rows$g$codes, 600L)
  expect_type(rows$v, "list")
  expect_length(rows$v, 600L)
})

test_that("rows: big 64-bit integers go as text", {
  skip_if_not_installed("bit64")
  df <- data.frame(i = bit64::as.integer64(c("1", "9007199254740993")))
  expect_warning(w <- viz_wire_df(df), "2\\^53")
  rows <- jsonlite::fromJSON(w$data)
  expect_identical(rows$i, c("1", "9007199254740993"))
})
