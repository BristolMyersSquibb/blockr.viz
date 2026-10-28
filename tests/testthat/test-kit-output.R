out_spec <- function() {
  viz_spec("chart",
           type = viz_arg("enum", default = "bar", values = c("bar", "line")),
           color = viz_arg("column", null = "no color"))
}

# A block server reduced to the payload: state, rows from a reactive frame,
# and the output. `park` makes the build stop on a silent req(), as a block
# the dock parked does.
out_server <- function(id, df, park, poke) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- out_spec()
    state <- viz_state(viz_check(spec, list()))
    revs <- new_viz_revs(viz_values(state))
    rows <- viz_rows()
    builds <- shiny::reactiveVal(0L)
    viz_payload_output(output, input, "chart", function() {
      poke()
      shiny::req(!park())
      builds(shiny::isolate(builds()) + 1L)
      w <- viz_wire_df(df())
      viz_payload(spec, viz_values(state), rows(w$data, w$columns), revs)
    })
    list(state = state, builds = builds)
  })
}

out_args <- function() {
  list(df = shiny::reactiveVal(data.frame(g = c("a", "b"), v = 1:2)),
       park = shiny::reactiveVal(FALSE), poke = shiny::reactiveVal(0L))
}

payload <- function(output) {
  jsonlite::fromJSON(jsonlite::toJSON(output$chart, json_verbatim = TRUE,
                                      auto_unbox = TRUE, null = "null"),
                     simplifyVector = FALSE)
}

test_that("the first payload carries settings and rows", {
  shiny::testServer(out_server, args = out_args(), {
    p <- payload(output)
    expect_identical(p$data_rev, 1L)
    expect_identical(p$config$type, "bar")
    expect_identical(p[["data"]]$g, list("a", "b"))
    expect_identical(p$columns[[1L]]$name, "g")
  })
})

test_that("a settings change goes out without the rows", {
  shiny::testServer(out_server, args = out_args(), {
    payload(output)
    session$returned$state$color("g")
    session$flushReact()
    p <- payload(output)
    expect_identical(p$config$color, "g")
    expect_null(p[["data"]])
    expect_identical(p$data_rev, 1L)
  })
})

test_that("new rows bump the data revision and go out", {
  a <- out_args()
  shiny::testServer(out_server, args = a, {
    payload(output)
    a$df(data.frame(g = "c", v = 3L))
    session$flushReact()
    p <- payload(output)
    expect_identical(p$data_rev, 2L)
    expect_identical(p[["data"]]$g, list("c"))
  })
})

test_that("an identical payload is not sent again", {
  a <- out_args()
  shiny::testServer(out_server, args = a, {
    payload(output)
    builds <- session$returned$builds()
    a$poke(1L)
    session$flushReact()
    expect_error(output$chart, class = "shiny.output.cancel")
    expect_identical(session$returned$builds(), builds + 1L)
  })
})

test_that("a parked block cancels the render instead of erroring", {
  a <- out_args()
  shiny::testServer(out_server, args = a, {
    payload(output)
    a$park(TRUE)
    session$flushReact()
    expect_error(output$chart, class = "shiny.output.cancel")
  })
})

test_that("_need makes the next render send the rows again", {
  shiny::testServer(out_server, args = out_args(), {
    payload(output)
    session$returned$state$color("g")
    session$flushReact()
    expect_null(payload(output)[["data"]])
    session$setInputs(chart_need = 1L)
    p <- payload(output)
    expect_identical(p[["data"]]$g, list("a", "b"))
  })
})

test_that("the rows' revision moves only when the rows do", {
  rows <- viz_rows()
  r1 <- rows(list(a = 1:3), columns = "a")
  expect_identical(r1$rev, 1L)
  expect_identical(rows(list(a = 1:3), columns = "a"), r1)
  expect_identical(rows(list(a = 4:6))$rev, 2L)
  # The first rows count even when they are empty.
  expect_identical(viz_rows()(structure(list(), names = character()))$rev,
                   1L)
})
