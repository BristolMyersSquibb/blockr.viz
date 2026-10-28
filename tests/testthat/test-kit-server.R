kit_spec <- function() {
  viz_spec(
    "chart",
    type = viz_arg("enum", default = "bar", values = c("bar", "line")),
    color = viz_arg("column", null = "no color"),
    orientation = viz_arg("enum", default = "horizontal",
                          values = c("horizontal", "vertical")),
    script = viz_arg("string", null = "none", ai = FALSE),
    selection = viz_arg("map", null = "nothing selected", ai = FALSE)
  )
}

# A block server reduced to the kit, plus two stand-ins: core's revert of a
# frozen block's external_ctrl field (`frozen`), and a downstream reactive
# that evaluates the script, recording each script it ran in `log`.
kit_server <- function(id, spec, frozen = FALSE, log = new.env()) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- viz_state(viz_check(spec, list()))
    revs <- new_viz_revs(viz_values(state))
    viz_rev_tracker(state, revs)
    normalize <- function(v) {
      if (identical(v$type, "line")) v$orientation <- "vertical"
      v
    }
    viz_settings_observer(input, "chart", spec, state, revs, normalize)
    claim <- shiny::reactiveVal(NULL)
    transient <- shiny::reactiveVal(FALSE)
    viz_selection_observer(input, "chart", state, revs, transient, claim)
    capture <- shiny::reactiveVal(NULL)
    viz_capture_observer(input, "chart", capture)
    if (frozen) {
      held <- shiny::isolate(state$script())
      shiny::observe({
        if (!identical(state$script(), held)) state$script(held)
      }, priority = Inf)
    }
    log$ran <- character()
    shiny::observe({
      s <- state$script()
      if (!is.null(s)) log$ran <- c(log$ran, s)
    })
    list(state = state, revs = revs, claim = claim, transient = transient,
         capture = capture)
  })
}

set_msg <- function(values, rev = 0) {
  viz_read_set(kit_spec(), list(rev = rev, values = values))
}

test_that("settings are gated, normalized and written", {
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    st <- session$returned$state
    session$setInputs(chart_set = set_msg(list(color = "cyl")))
    expect_identical(st$color(), "cyl")
    session$setInputs(chart_set = set_msg(list(type = "line")))
    expect_identical(st$orientation(), "vertical")
    session$setInputs(chart_set = set_msg(list(color = NULL)))
    expect_null(st$color())
  })
})

test_that("a list setting replaces the old value whole", {
  spec <- viz_spec(
    "chart",
    highlight = viz_arg(type_named(viz_arg("strings")), default = NULL,
                        null = "none"),
    rules = viz_arg(type_records(mode = viz_arg("string")), default = list(),
                    null = FALSE),
    script = viz_arg("string", null = "none", ai = FALSE)
  )
  shiny::testServer(kit_server, args = list(spec = spec), {
    st <- session$returned$state
    set <- function(values) {
      session$setInputs(chart_set = viz_read_set(spec, list(rev = 0,
                                                            values = values)))
    }
    set(list(highlight = list(country = list("CH"))))
    set(list(highlight = list(region = list("EU"))))
    expect_identical(st$highlight(), list(region = "EU"))
    set(list(rules = list(list(mode = "bar"))))
    expect_identical(st$rules(), list(list(mode = "bar")))
  })
})

test_that("a quick second edit of the same setting lands", {
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    st <- session$returned$state
    session$setInputs(chart_set = set_msg(list(color = "a")))
    session$setInputs(chart_set = set_msg(list(color = "b")))
    expect_identical(st$color(), "b")
  })
})

test_that("a change from elsewhere makes an older message stale", {
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    st <- session$returned$state
    revs <- session$returned$revs
    st$color("from_ctrl")
    session$flushReact()
    expect_identical(revs$now(), 1L)
    session$setInputs(chart_set = set_msg(list(color = "old", type = "line")))
    expect_identical(st$color(), "from_ctrl")
    expect_identical(st$type(), "line")
    session$setInputs(chart_set = set_msg(list(color = "new"),
                                          rev = revs$now()))
    expect_identical(st$color(), "new")
  })
})

test_that("a locked board takes no settings", {
  withr::local_options(blockr.locked = TRUE)
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    session$setInputs(chart_set = set_msg(list(color = "cyl")))
    expect_null(session$returned$state$color())
  })
})

test_that("a browser script is dropped unless the option allows it", {
  withr::local_options(blockr.viz.browser_script = NULL)
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    session$setInputs(chart_set = set_msg(list(script = "data")))
    expect_null(session$returned$state$script())
  })
})

test_that("a forged script on a frozen block never runs", {
  withr::local_options(blockr.viz.browser_script = TRUE)
  log <- new.env()
  shiny::testServer(kit_server, args = list(spec = kit_spec(), frozen = TRUE,
                                            log = log), {
    session$setInputs(chart_set = set_msg(list(script = "forged")))
    session$flushReact()
    expect_null(session$returned$state$script())
  })
  expect_false("forged" %in% log$ran)

  # the same script on a block that is not frozen does run, so the check
  # above can fail
  open <- new.env()
  shiny::testServer(kit_server, args = list(spec = kit_spec(), log = open), {
    session$setInputs(chart_set = set_msg(list(script = "forged")))
  })
  expect_true("forged" %in% open$ran)
})

test_that("a rejected key is logged once", {
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    msg <- viz_read_set(kit_spec(), list(rev = 0,
                                         values = list(colour_once = 1)))
    expect_warning(session$setInputs(chart_set = msg),
                   "refuses: `colour_once`")
    expect_no_warning(session$setInputs(chart_set = msg))
  })
})

test_that("a selection is held, or sent as an event in transient mode", {
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    st <- session$returned$state
    sel <- viz_read_selection(list(
      selection = list(type = "categorical", filters = list(g = list("a"))),
      nonce = 1
    ))
    session$setInputs(chart_select = sel)
    expect_identical(st$selection()$filters, list(g = "a"))
    session$setInputs(chart_select = list(selection = NULL, nonce = 2L))
    expect_null(st$selection())

    session$returned$transient(TRUE)
    session$setInputs(chart_select = sel)
    expect_null(st$selection())
    expect_identical(session$returned$claim()$nonce, 1)
    session$setInputs(chart_select = list(selection = NULL, nonce = 3L))
    expect_identical(session$returned$claim()$nonce, 1)
  })
})

test_that("the last capture is kept outside the state, decoded", {
  shiny::testServer(kit_server, args = list(spec = kit_spec()), {
    cap <- viz_read_capture(list(png = "data:image/png;base64,AAAA",
                                 width = 96, height = 48))
    session$setInputs(chart_capture = cap)
    pic <- session$returned$capture()
    expect_s3_class(pic, "chart_capture")
    expect_identical(pic$png, as.raw(c(0, 0, 0)))
    expect_identical(c(pic$width, pic$height), c(1, 0.5))
  })
})
