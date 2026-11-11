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
    # The columns did not change either: the browser keeps them.
    expect_false("columns" %in% names(p))
  })
})

test_that("in a browser session the payload is a message, the output a token", {
  local_mocked_bindings(viz_posts = function(session) TRUE)
  post_server <- function(id, df) {
    shiny::moduleServer(id, function(input, output, session) {
      spec <- out_spec()
      state <- viz_state(viz_check(spec, list()))
      revs <- new_viz_revs(viz_values(state))
      rows <- viz_rows()
      viz_payload_output(output, input, "chart", function() {
        w <- viz_wire_df(df())
        viz_payload(spec, viz_values(state), rows(w$data, w$columns), revs)
      }, session = session, el = session$ns("chart"))
      list(state = state)
    })
  }
  shiny::testServer(post_server, args = list(
    df = shiny::reactiveVal(data.frame(g = c("a", "b"), v = 1:2))
  ), {
    posts <- list()
    root <- session$rootScope()
    root$sendCustomMessage <- function(type, message) {
      if (identical(type, "blockr-viz:payload")) {
        posts[[length(posts) + 1L]] <<- message
      }
    }
    token <- output$chart
    expect_length(posts, 1L)
    expect_identical(posts[[1L]]$id, session$ns("chart"))
    expect_false(is.null(posts[[1L]]$payload[["data"]]))
    expect_identical(token$posted, 1L)
    session$returned$state$color("g")
    session$flushReact()
    token <- output$chart
    expect_length(posts, 2L)
    expect_null(posts[[2L]]$payload[["data"]])
    expect_match(posts[[2L]]$payload$config, "\"color\":[\"g\"]|\"color\":\"g\"")
    expect_identical(token$posted, 2L)
  })
})

test_that("a test session keeps the payload as the output's value", {
  expect_false(viz_posts(shiny::MockShinySession$new()))
  withr::local_options(blockr.viz.post_payload = FALSE)
  expect_false(viz_posts(list()))
})

test_that("columns go out again when they change or the browser asks", {
  shiny::testServer(out_server, args = out_args(), {
    payload(output)
    df(data.frame(g = c("a", "b"), v = 3:4))
    session$flushReact()
    expect_identical(payload(output)$columns[[1L]]$name, "g")
    session$returned$state$color("g")
    session$flushReact()
    expect_false("columns" %in% names(payload(output)))
    session$setInputs(chart_need = 2L)
    session$flushReact()
    p <- payload(output)
    expect_identical(p$columns[[1L]]$name, "g")
    expect_false(is.null(p[["data"]]))
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

test_that("an ask for the column counts is answered, and kept per frame", {
  n_counted <- 0L
  local_mocked_bindings(viz_column_counts = function(df) {
    n_counted <<- n_counted + 1L
    list(g = length(unique(df$g)), v = length(unique(df$v)))
  })
  srv <- function(id, df) {
    shiny::moduleServer(id, function(input, output, session) {
      viz_counts_observer(input, session, "chart", session$ns("chart"),
                          shiny::reactive(df()))
    })
  }
  df <- shiny::reactiveVal(data.frame(g = c("a", "b", "a"), v = 1:3))
  shiny::testServer(srv, args = list(df = df), {
    answers <- list()
    root <- session$rootScope()
    root$sendCustomMessage <- function(type, message) {
      if (identical(type, "blockr-viz:column-counts")) {
        answers[[length(answers) + 1L]] <<- message
      }
    }
    session$setInputs(chart_counts = 1)
    expect_length(answers, 1L)
    expect_identical(answers[[1L]]$id, session$ns("chart"))
    expect_identical(answers[[1L]]$counts, list(g = 2L, v = 3L))
    expect_identical(n_counted, 1L)

    # The same frame again: answered, not counted.
    session$setInputs(chart_counts = 2)
    expect_length(answers, 2L)
    expect_identical(answers[[2L]]$counts, list(g = 2L, v = 3L))
    expect_identical(n_counted, 1L)

    df(data.frame(g = c("a", "b", "c"), v = c(1L, 1L, 1L)))
    session$setInputs(chart_counts = 3)
    expect_identical(answers[[3L]]$counts, list(g = 3L, v = 1L))
    expect_identical(n_counted, 2L)
  })
})
