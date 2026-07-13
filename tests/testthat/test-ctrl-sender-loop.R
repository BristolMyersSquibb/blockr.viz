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

        dd_ctrl_sender(r_target, r_claims, session = session)
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

        dd_ctrl_sender(r_target, r_claims, session = session)

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
