json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)

spec_set <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line")),
    color = viz_arg("column", null = "no color"),
    connect = viz_arg("enum", default = "straight",
                      values = c("straight", "monotone")),
    script = viz_arg("string", null = "none", ai = FALSE),
    script_values = viz_arg("map", default = list(), ai = FALSE)
  )
}

test_that("a settings message is read against the spec", {
  m <- viz_read_set(spec_set(), json(
    '{"rev": 4, "values": {"color": "cyl", "connect": "zigzag", "colour": 1}}'
  ))
  expect_identical(m$rev, 4L)
  expect_identical(m$values, list(color = "cyl"))
  expect_setequal(m$rejected, c("connect", "colour"))
})

test_that("a JSON null clears a nullable setting", {
  m <- viz_read_set(spec_set(), json('{"rev": 0, "values": {"color": null}}'))
  expect_true("color" %in% names(m$values))
  expect_null(m$values$color)
})

test_that("a malformed settings message reads as NULL", {
  s <- spec_set()
  expect_null(viz_read_set(s, NULL))
  expect_null(viz_read_set(s, json('{"values": {}}')))
  expect_null(viz_read_set(s, json('{"rev": -1, "values": {}}')))
  expect_null(viz_read_set(s, json('{"rev": 1, "values": [1, 2]}')))
  expect_null(viz_read_set(s, json('{"action": "config"}')))
})

test_that("selections have one shape", {
  sel <- viz_read_selection(json(
    '{"selection": {"type": "categorical",
      "filters": {"AESEV": ["MILD", null]}}, "nonce": 3}'
  ))
  expect_identical(sel$nonce, 3)
  expect_identical(sel$selection$filters, list(AESEV = c("MILD", NA)))
  expect_identical(sel$selection$columns, list())

  rng <- viz_read_selection(json(
    '{"selection": {"type": "range", "x": "AGE", "y": "WT",
      "x_range": [40, 60], "y_range": [70, 90],
      "filters": {"SEX": ["F"]}}, "nonce": 4}'
  ))
  expect_identical(rng$selection$x_range, c(40, 60))
  expect_identical(rng$selection$filters, list(SEX = "F"))

  line <- viz_read_selection(json(
    '{"selection": {"type": "range", "x": "day", "x_range": [1, 2]},
      "nonce": 5}'
  ))
  expect_null(line$selection$y_range)

  clear <- viz_read_selection(json('{"selection": null, "nonce": 6}'))
  expect_null(clear$selection)
  expect_identical(clear$nonce, 6)
  # The table's receipt clear sends Date.now().
  expect_silent(late <- viz_read_selection(
    json('{"selection": null, "nonce": 1793173863956}')
  ))
  expect_identical(late$nonce, 1793173863956)
})

test_that("a malformed selection reads as NULL", {
  bad <- c(
    '{"selection": {"type": "categorical", "filters": {}}, "nonce": 1}',
    '{"selection": {"type": "categorical", "filters": {"a": "x"}}, "nonce": 1}',
    '{"selection": {"type": "categorical", "filters": {"a": [[1]]}}, "nonce": 1}',
    '{"selection": {"type": "point"}, "nonce": 1}',
    '{"selection": {"type": "range", "x": "a", "x_range": [1]}, "nonce": 1}',
    '{"selection": {"type": "range", "x": "a", "x_range": [1, 2],
      "y_range": [1, 2]}, "nonce": 1}',
    '{"selection": null}'
  )
  for (b in bad) expect_null(viz_read_selection(json(b)), label = b)
})

test_that("a capture needs a PNG data URL and a size", {
  ok <- viz_read_capture(list(png = "data:image/png;base64,AAAA", width = 900,
                              height = 500))
  expect_identical(ok$width, 900)
  expect_null(viz_read_capture(list(png = "javascript:x", width = 1,
                                    height = 1)))
  expect_null(viz_read_capture(list(png = "data:image/png;base64,A",
                                    width = 0, height = 1)))
})

test_that("the input handlers are registered by type", {
  viz_input_handlers(list(spec_set()))
  h <- shiny:::inputHandlers$get("blockr.viz.chart")
  expect_identical(
    h(json('{"rev": 1, "values": {"type": "line"}}'), NULL, "x")$values,
    list(type = "line")
  )
  expect_null(shiny:::inputHandlers$get("blockr.viz.selection")(1, NULL, "x"))
})

test_that("the gates drop locked, script and stale settings", {
  revs <- new_viz_revs(list(color = NULL))
  msg <- viz_read_set(spec_set(), json(
    '{"rev": 0, "values": {"color": "cyl", "script": "system(1)",
      "script_values": {"a": 1}}}'
  ))
  # The script's controls' values pass; the settings observer checks them.
  expect_identical(viz_gate_set(msg, revs, locked = FALSE,
                                browser_script = FALSE),
                   list(color = "cyl", script_values = list(a = 1)))
  expect_named(viz_gate_set(msg, revs, locked = FALSE, browser_script = TRUE),
               c("color", "script", "script_values"))
  expect_identical(viz_gate_set(msg, revs, locked = TRUE), list())

  # color changed elsewhere after the message's revision 0
  msg$values$script_values <- NULL
  revs$track(list(color = "gear"))
  expect_length(viz_gate_set(msg, revs, locked = FALSE,
                             browser_script = FALSE), 0L)
  msg$rev <- revs$now()
  expect_identical(viz_gate_set(msg, revs, locked = FALSE,
                                browser_script = FALSE),
                   list(color = "cyl"))
})

test_that("the lock follows the blockr.locked option", {
  withr::local_options(blockr.locked = TRUE)
  expect_true(viz_locked())
  withr::local_options(blockr.locked = FALSE)
  expect_false(viz_locked())
})
