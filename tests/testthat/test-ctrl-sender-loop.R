test_that("an unchanged claim is not re-sent, so the sender cannot loop", {
  sends <- 0

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        r_data <- shiny::reactiveVal(1)

        # A ctrl_send() is a board update, and a board update re-evaluates every
        # block -- including the sender. Model that: sending bumps the data the
        # claim is derived from.
        session$userData$blockr_ctrl_send <- function(target, args,
                                                      author = NULL) {
          sends <<- sends + 1
          if (sends > 20) {
            stop("runaway: ctrl_send looped ", sends, " times")
          }
          shiny::isolate(r_data(r_data() + 1))
        }

        r_target <- shiny::reactiveVal("vf")

        # A plain reactive over the data, with a CONSTANT value: the drill never
        # changes, but the reactive invalidates on every re-evaluation.
        r_claims <- shiny::reactive({
          r_data()
          list(list(table = "adsl", column = "SEX", values = list("M")))
        })

        # not pristine: the user drilled, so this claim is theirs to push
        dd_ctrl_sender(r_target, r_claims, shiny::reactive(FALSE),
                       session = session)
      })
    },
    {
      expect_no_error({
        for (i in 1:15) session$flushReact()
      })
      expect_identical(sends, 1)
    }
  )
})

test_that("a restored claim is not pushed at startup", {
  sends <- 0

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        session$userData$blockr_ctrl_send <- function(target, args,
                                                      author = NULL) {
          sends <<- sends + 1
        }

        # the board came back with a drill already set on this block
        column <- "SEX"
        values <- list("M")
        r_column <- shiny::reactiveVal(column)
        r_values <- shiny::reactiveVal(values)

        r_claims <- shiny::reactive({
          list(list(table = "adsl", column = r_column(),
                    values = r_values()))
        })

        dd_ctrl_sender(
          shiny::reactiveVal("vf"), r_claims,
          dd_ctrl_pristine(r_column, r_values, column, values),
          session = session
        )

        session$flushReact()
        # the target restored its own filter from the same board: nothing owed
        expect_identical(sends, 0)

        # now the user actually drills -> that one must go out
        r_values(list("F"))
        session$flushReact()
      })
    },
    {
      expect_identical(sends, 1)
    }
  )
})

test_that("clearing back to the constructor's drill still notifies the target", {
  # the latch: pristine must not come back TRUE, or the clear is swallowed and
  # the target stays stuck on a claim the block no longer makes
  cleared <- 0

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        session$userData$blockr_ctrl_send <- function(target, args,
                                                      author = NULL) {
          invisible(TRUE)
        }
        session$userData$blockr_ctrl_clear <- function(target, args,
                                                       author = NULL) {
          cleared <<- cleared + 1
          TRUE
        }

        column <- "SEX"
        values <- list("M")
        r_column <- shiny::reactiveVal(column)
        r_values <- shiny::reactiveVal(values)

        r_claims <- shiny::reactive({
          vals <- r_values()
          if (!length(vals)) {
            return(list())
          }
          list(list(table = "adsl", column = r_column(), values = vals))
        })

        dd_ctrl_sender(
          shiny::reactiveVal("vf"), r_claims,
          dd_ctrl_pristine(r_column, r_values, column, values),
          session = session
        )

        session$flushReact()
        r_values(list("F"))     # drill: latches touched
        session$flushReact()
        r_values(values)        # back to exactly the constructor's value
        session$flushReact()
        r_values(list())        # un-drill entirely
        session$flushReact()
      })
    },
    {
      expect_identical(cleared, 1)
    }
  )
})

test_that("a claim that really changes still sends", {
  sends <- list()

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        session$userData$blockr_ctrl_send <- function(target, args,
                                                      author = NULL) {
          sends[[length(sends) + 1L]] <<- args
        }

        r_target <- shiny::reactiveVal("vf")
        r_vals <- shiny::reactiveVal("M")
        r_claims <- shiny::reactive({
          list(list(table = "adsl", column = "SEX", values = list(r_vals())))
        })

        dd_ctrl_sender(r_target, r_claims, shiny::reactive(FALSE),
                       session = session)

        session$flushReact()
        r_vals("F")
        session$flushReact()
      })
    },
    {
      expect_length(sends, 2L)
    }
  )
})
