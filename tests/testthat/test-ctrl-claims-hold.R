test_that("dd_ctrl_claims distinguishes gated input from an evaluated no-claim", {
  # NULL data = the block's input is gated (off screen under lazy eval): no
  # opinion. An evaluated frame with no drill = list(), the un-drill shape.
  expect_null(dd_ctrl_claims(NULL, "adsl", list(SEX = "F")))
  expect_identical(
    dd_ctrl_claims(data.frame(SEX = c("F", "M")), "adsl", list()),
    list()
  )
})

test_that("a gated input HOLDS: no clear on hide, no re-send on return", {
  # The bug this pins down: drill a chart, switch views. The hidden sender's
  # input gates to NULL, its claims reactive went empty, and the old sender
  # read that as an un-drill -- clearing the cohort filter the user was about
  # to consume, then re-sending the claim on the way back. Both transitions
  # must be no-ops.
  sends <- 0
  clears <- 0

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        session$userData$blockr_ctrl_send <- function(target, args,
                                                      author = NULL) {
          sends <<- sends + 1
        }
        session$userData$blockr_ctrl_clear <- function(target, args,
                                                       author = NULL) {
          clears <<- clears + 1
          TRUE
        }

        # data() as the block sees it: a frame while on screen, NULL gated
        r_data <- shiny::reactiveVal(data.frame(SEX = c("F", "M")))
        r_vals <- shiny::reactiveVal(NULL)

        r_claims <- shiny::reactive({
          filters <- if (length(r_vals())) list(SEX = r_vals()) else list()
          dd_ctrl_claims(r_data(), "adsl", filters)
        })

        dd_ctrl_sender(
          shiny::reactiveVal("vf"), r_claims,
          dd_ctrl_pristine(function() list(r_vals()), list(NULL)),
          session = session
        )

        session$flushReact()

        r_vals("F")             # the user drills -> one send
        session$flushReact()
        expect_identical(sends, 1)

        r_data(NULL)            # view switch: input gated -> HOLD
        session$flushReact()
        expect_identical(clears, 0)

        r_data(data.frame(SEX = c("F", "M")))  # back on screen: same claim
        session$flushReact()
        expect_identical(sends, 1)

        r_vals(NULL)            # a real un-drill, on evaluated data -> clear
        session$flushReact()
      })
    },
    {
      expect_identical(sends, 1)
      expect_identical(clears, 1)
    }
  )
})

test_that("a drill naming several values claims them as a set", {
  d <- data.frame(
    USUBJID = c("S1", "S1", "S2", "S3"),
    AETERM = c("Rash", "Rash", "Rash", "Nausea"),
    AESEV = c("MILD", "MILD", "MILD", "SEVERE")
  )
  # The patients under a bar segment: a click with an explicit drill column
  # sends their ids.
  claims <- dd_ctrl_claims(d, "", list(USUBJID = c("S1", "S2")))
  expect_length(claims, 1L)
  expect_equal(claims[[1L]]$name, "USUBJID")
  expect_equal(claims[[1L]]$mode, "multi")
  expect_setequal(claims[[1L]]$values, c("S1", "S2"))

  # One value claims as before.
  one <- dd_ctrl_claims(d, "adsl", list(USUBJID = "S3"))
  expect_equal(one[[1L]]$values, "S3")
  expect_equal(one[[1L]]$table, "adsl")

  # Several columns: each claims what the rows under all of them hold.
  both <- dd_ctrl_claims(d, "", list(AETERM = "Rash", AESEV = "MILD"))
  expect_equal(vapply(both, `[[`, "", "name"), c("AETERM", "AESEV"))
})
