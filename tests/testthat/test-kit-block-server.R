# viz_block_server(): the skeleton every block view runs (R/kit-server.R).

kbs_spec <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line")),
    color = viz_arg("column", null = "no color"),
    orientation = viz_arg("enum", default = "horizontal",
                          values = c("horizontal", "vertical")),
    drill_target = viz_arg("string", default = ""),
    selection = viz_arg("map", null = "nothing selected", ai = FALSE,
                        ctrl = FALSE)
  )
}

kbs_msg <- function(values, rev = 0) {
  viz_read_set(kbs_spec(), list(rev = rev, values = values))
}

# A block on the kit: its payload is the color, its expr names the
# selection's first filter.
kbs_block <- function(id, data, init, log = new.env()) {
  viz_block_server(id, kbs_spec(), data, init, "chart", function(kit) {
    log$kit <- kit
    list(
      normalize = function(v) {
        if (identical(v$type, "line")) v$orientation <- "vertical"
        v
      },
      payload = function() list(color = kit$state$color(), data_rev = 1L),
      expr = shiny::reactive({
        sel <- kit$state$selection()
        if (is.null(sel)) quote(TRUE) else as.name(names(sel$filters)[[1L]])
      })
    )
  })
}

kbs_args <- function(log = new.env()) {
  list(data = shiny::reactive(data.frame(a = 1)),
       init = viz_check(kbs_spec(), list()), log = log)
}

test_that("settings go through normalize into the state", {
  shiny::testServer(kbs_block, args = kbs_args(), {
    session$setInputs(chart_set = kbs_msg(list(type = "line")))
    expect_identical(session$returned$state$type(), "line")
    expect_identical(session$returned$state$orientation(), "vertical")
  })
})

test_that("a click is held as the selection and reaches the expr", {
  shiny::testServer(kbs_block, args = kbs_args(), {
    sel <- list(type = "categorical", filters = list(cyl = "4"))
    session$setInputs(chart_select = list(selection = sel, nonce = 1))
    expect_identical(session$returned$state$selection(), sel)
    expect_identical(session$returned$expr(), as.name("cyl"))
  })
})

test_that("with a drill target a click is an event, not a selection", {
  log <- new.env()
  shiny::testServer(kbs_block, args = kbs_args(log), {
    session$setInputs(chart_set = kbs_msg(list(drill_target = "f1")))
    sel <- list(type = "categorical", filters = list(cyl = "4"))
    session$setInputs(chart_select = list(selection = sel, nonce = 2))
    expect_null(session$returned$state$selection())
    expect_identical(log$kit$claim()$nonce, 2)
  })
})

test_that("the payload goes out through the view's output", {
  shiny::testServer(kbs_block, args = kbs_args(), {
    session$setInputs(chart_set = kbs_msg(list(color = "cyl")))
    expect_identical(output$chart$color, "cyl")
  })
})

test_that("the kit hands setup what a block needs", {
  log <- new.env()
  shiny::testServer(kbs_block, args = kbs_args(log), {
    expect_identical(log$kit$el, session$ns("chart"))
    expect_true(is.function(log$kit$transient))
    expect_false(log$kit$transient())
    expect_null(log$kit$capture())
  })
})

test_that("a block without drill_target holds every click", {
  spec <- viz_spec("chart", selection = viz_arg("map", null = "none",
                                                ai = FALSE, ctrl = FALSE))
  blk <- function(id) {
    viz_block_server(id, spec, shiny::reactive(NULL), viz_check(spec, list()),
                     "chart", function(kit) {
      list(payload = function() NULL, expr = shiny::reactive(NULL))
    })
  }
  shiny::testServer(blk, {
    sel <- list(type = "categorical", filters = list(a = "1"))
    session$setInputs(chart_select = list(selection = sel, nonce = 1))
    expect_identical(session$returned$state$selection(), sel)
  })
})

# A block whose payload reads the data but does not change with it, so a
# data change renders an identical payload.
kbs_steady <- function(id, data, init, log = new.env()) {
  viz_block_server(id, kbs_spec(), data, init, "chart", function(kit) {
    log$kit <- kit
    list(
      payload = function() {
        kit$data()
        list(data_rev = 1L, data = list(a = 1), derived = list())
      },
      expr = shiny::reactive(quote(TRUE)),
      claims = shiny::reactive({
        c1 <- kit$claim()
        sel <- if (is.null(c1)) kit$state$selection() else c1$selection
        if (!is.null(sel)) sel$filters
      })
    )
  })
}

test_that("a render that draws nothing ends the busy cue", {
  cue <- new.env()
  cue$calls <- character()
  local_mocked_bindings(
    busy_cue_start = function(session, id, label = "") {
      cue$calls <- c(cue$calls, "start")
    },
    busy_cue_done = function(session, id) cue$calls <- c(cue$calls, "done")
  )
  d <- shiny::reactiveVal(data.frame(a = 1))
  shiny::testServer(kbs_steady, args = list(data = d,
                                            init = viz_check(kbs_spec(), list())), {
    expect_identical(output$chart$data_rev, 1L)
    expect_identical(cue$calls, "start")
    d(data.frame(a = 2))
    session$flushReact()
    try(output$chart, silent = TRUE)
    expect_identical(cue$calls, c("start", "start", "done"))
  })
})

test_that("<view>_need brings the rows back through the kit", {
  d <- shiny::reactiveVal(data.frame(a = 1))
  shiny::testServer(kbs_steady, args = list(data = d,
                                            init = viz_check(kbs_spec(), list())), {
    expect_identical(output$chart$data, list(a = 1))
    session$setInputs(chart_need = 1)
    expect_identical(output$chart$data, list(a = 1))
  })
})

test_that("the drill sender sends once per click, nothing for a restored selection", {
  sent <- new.env()
  sent$n <- 0L
  sel <- list(type = "categorical", filters = list(cyl = "4"))
  init <- viz_check(kbs_spec(), list(drill_target = "f1", selection = sel))
  shiny::testServer(kbs_steady, args = list(data = shiny::reactive(NULL),
                                            init = init), {
    session$userData$blockr_ctrl_send <- function(target, args, ...) {
      sent$n <- sent$n + 1L
      sent$target <- target
    }
    session$flushReact()
    expect_identical(sent$n, 0L)
    click <- list(type = "categorical", filters = list(cyl = "6"))
    session$setInputs(chart_select = list(selection = click, nonce = 1))
    expect_identical(sent$n, 1L)
    expect_identical(sent$target, "f1")
    session$setInputs(chart_select = list(selection = click, nonce = 1))
    expect_identical(sent$n, 1L)
    session$setInputs(chart_select = list(selection = click, nonce = 2))
    expect_identical(sent$n, 2L)
  })
})
