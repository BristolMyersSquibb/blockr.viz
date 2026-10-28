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

        ctrl_sender(r_target, r_claims, shiny::reactive(FALSE),
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
        choices <- ctrl_choices(session = session)
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
          ctrl_auto_choice(session = session),
          c(`Automatic (no filter block found)` = "auto")
        )
      })
    },
    NULL
  )
})

test_that("the table's payload lists the board's filter blocks to send to", {
  blk <- new_table_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      fake_targets(session, list(
        drill_filter_block = c(`Patient Drill` = "pt_drill"),
        value_filter_block = c(`Patient Drill` = "pt_drill",
                               Cohort = "cohort_filter")
      ))
      session$flushReact()
      expr <- session$makeScope("expr")
      p <- expr$output$table
      expect_identical(
        p$derived$ctrl_choices,
        list(list(value = "auto", label = "Automatic (Patient Drill)"),
             list(value = "pt_drill", label = "Patient Drill"),
             list(value = "cohort_filter", label = "Cohort"))
      )
    },
    args = list(x = blk, data = list(data = shiny::reactive(mtcars)))
  )
})

test_that("install_ctrl_send() wires send, clear and the board's targets", {
  dataset <- blockr.core::new_dataset_block("iris", block_name = "Cohort")
  filt <- function(nm = NULL) {
    b <- blockr.core::new_dataset_block("mtcars", block_name = nm)
    class(b) <- c("value_filter_block", class(b))
    b
  }
  board <- shiny::reactiveValues(board = blockr.core::new_board(
    blocks = blockr.core::as_blocks(list(f1 = filt("Global"), f2 = filt(),
                                         d = dataset))
  ))
  updates <- list()
  session <- shiny::MockShinySession$new()
  send <- install_ctrl_send(function(x) updates[[length(updates) + 1L]] <<- x,
                            board = board, session = session)
  expect_identical(send, session$userData$blockr_ctrl_send)

  targets <- shiny::isolate(session$userData$blockr_ctrl_targets())
  expect_identical(targets, c(Global = "f1", Dataset = "f2"))
  expect_identical(
    shiny::isolate(session$userData$blockr_ctrl_targets("drill_filter_block")),
    character()
  )

  expect_true(ctrl_send("f1", column = "Species", session = session) %||% TRUE)
  expect_identical(updates[[1L]],
                   list(blocks = list(mod = list(f1 = list(column = "Species")))))
  # Only the block that sent may clear, and only once.
  clear <- session$userData$blockr_ctrl_clear
  expect_false(clear("f1", list(column = NULL), author = "someone else"))
  expect_true(clear("f1", list(column = NULL), author = ctrl_author(session)))
  expect_false(clear("f1", list(column = NULL), author = ctrl_author(session)))
  expect_length(updates, 2L)

  # Without a board there is nothing to send to.
  bare <- shiny::MockShinySession$new()
  install_ctrl_send(function(x) NULL, session = bare)
  expect_identical(bare$userData$blockr_ctrl_targets(), character())
})
