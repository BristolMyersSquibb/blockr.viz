# `ctrl_target = "auto"` is the whole configuration a drill sender needs: the
# board answers with its drill destination. Found by CLASS, and only when the
# answer is unambiguous.

# A board's candidate registry, as install_ctrl_send() installs it: a function
# of the class asked for.
fake_targets <- function(session, by_class) {
  session$userData$blockr_ctrl_targets <- function(class = "value_filter_block") {
    by_class[[class]] %||% character()
  }
}

test_that("auto takes the one drill filter block, over any value filters", {
  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        fake_targets(session, list(
          drill_filter_block = c(`Patient Drill` = "pt_drill"),
          value_filter_block = c(`Patient Drill` = "pt_drill",
                                 `Global` = "global_filter",
                                 `Lab` = "lab_filter")
        ))
        expect_identical(ctrl_auto_target(session = session), "pt_drill")
      })
    },
    NULL
  )
})

test_that("with no drill filter, auto takes a SOLE value filter", {
  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        fake_targets(session, list(
          value_filter_block = c(Cohort = "cohort_filter")
        ))
        expect_identical(ctrl_auto_target(session = session), "cohort_filter")

        # Several candidates of a class: no winner is guessed. Only the board
        # author knows which filter means "the cohort just drilled into".
        fake_targets(session, list(
          value_filter_block = c(Cohort = "cohort_filter", Lab = "lab_filter")
        ))
        expect_identical(ctrl_auto_target(session = session), "")

        # Two drill filters is the same story, and it does NOT fall back to
        # the value filter: a board carrying a drill filter has said where
        # drills go, and carrying two is a board to fix.
        fake_targets(session, list(
          drill_filter_block = c(A = "d1", B = "d2"),
          value_filter_block = c(A = "d1", B = "d2", Cohort = "cohort_filter")
        ))
        expect_identical(ctrl_auto_target(session = session), "")
      })
    },
    NULL
  )
})

test_that("no channel / no candidate resolves to nothing rather than erroring", {
  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        expect_identical(ctrl_auto_target(session = session), "")
      })
    },
    NULL
  )
})

test_that("the sender resolves auto, and clears the block it actually sent to", {
  sent <- list()
  cleared <- character()

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        fake_targets(session, list(
          drill_filter_block = c(`Patient Drill` = "pt_drill")
        ))
        session$userData$blockr_ctrl_send <- function(target, args,
                                                      author = NULL) {
          sent[[length(sent) + 1L]] <<- target
        }
        session$userData$blockr_ctrl_clear <- function(target, args,
                                                       author = NULL) {
          cleared <<- c(cleared, target)
          TRUE
        }

        r_target <- shiny::reactiveVal("auto")
        r_claims <- shiny::reactive(
          list(list(table = "adsl", column = "SEX", values = list("F")))
        )

        dd_ctrl_sender(r_target, r_claims, shiny::reactive(FALSE),
                       session = session)

        session$flushReact()
        expect_identical(sent, list("pt_drill"))

        # Un-targeting releases the RESOLVED block, not the word "auto".
        r_target("")
        session$flushReact()
        expect_identical(cleared, "pt_drill")
      })
    },
    NULL
  )
})

test_that("the gear's picker offers auto first, labelled with what it found", {
  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        fake_targets(session, list(
          drill_filter_block = c(`Patient Drill` = "pt_drill"),
          value_filter_block = c(`Patient Drill` = "pt_drill",
                                 `Global Population Filter` = "global_filter")
        ))
        choices <- dd_ctrl_choices(session = session)
        session$flushReact()

        expect_identical(
          choices(),
          c(`Automatic (Patient Drill)` = "auto",
            `Patient Drill` = "pt_drill",
            `Global Population Filter` = "global_filter")
        )

        # A board with nothing to send to still offers the entry, and says so:
        # the alternative is a picker that looks broken.
        fake_targets(session, list())
        expect_identical(
          dd_ctrl_auto_choice(session = session),
          c(`Automatic (no filter block found)` = "auto")
        )
      })
    },
    NULL
  )
})
