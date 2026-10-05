# A drill claims the SUBJECTS behind the click when the board declares what a
# subject is (the "study_roles" board option). The target then receives
# exactly the patients this block counted, whatever ran upstream of it: a
# column claim re-applied to the target's own data loses every step between
# the data model and the sender (the AE flag filter: 69 patients in the
# table, 70 in the profile).

ae <- data.frame(
  USUBJID = c("01", "01", "02", "03", "04"),
  AEBODSYS = c("GI", "Skin", "GI", "GI", "Skin"),
  TRTEMFL = c(TRUE, TRUE, TRUE, FALSE, TRUE)
)

test_that("a click claims the subjects in the clicked rows, labelled", {
  flagged <- ae[ae$TRTEMFL, ]
  out <- dd_ctrl_claims(flagged, "", list(AEBODSYS = "GI"),
                        subject = "USUBJID")
  expect_identical(out, list(list(
    name = "USUBJID", mode = "multi", values = c("01", "02"),
    label = "AEBODSYS = GI"
  )))
})

test_that("no subject column: the column claim, as before", {
  out <- dd_ctrl_claims(ae, "", list(AEBODSYS = "GI"))
  expect_identical(out[[1L]]$name, "AEBODSYS")

  # declared, but this frame does not carry it
  out <- dd_ctrl_claims(ae[, -1L], "", list(AEBODSYS = "GI"),
                        subject = "USUBJID")
  expect_identical(out[[1L]]$name, "AEBODSYS")
})

test_that("a structured table resolves through its stamped source_data", {
  ard <- data.frame(.variable = "AEBODSYS", .variable_level = c("GI", "Skin"),
                    n = c(2, 2))
  attr(ard, "source_data") <- ae[ae$TRTEMFL, ]
  out <- dd_ctrl_claims(ard, "", list(.variable_level = "GI"),
                        subject = "USUBJID")
  expect_identical(out[[1L]]$values, c("01", "02"))
  expect_identical(out[[1L]]$label, "AEBODSYS = GI")
})

test_that("the un-drill and the hold are untouched", {
  expect_identical(dd_ctrl_claims(ae, "", list(), subject = "USUBJID"),
                   list())
  expect_null(dd_ctrl_claims(NULL, "", list(AEBODSYS = "GI"),
                             subject = "USUBJID"))
})

test_that("the subject column comes from the study_roles board option", {
  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        # no option: a board without subjects
        expect_null(dd_ctrl_subject(session))

        opt <- shiny::reactiveVal(list(arm = "TRT", subject = ""))
        session$userData$board_options <- list(study_roles = opt)
        # declared nothing: the convention
        expect_identical(shiny::isolate(dd_ctrl_subject(session)), "USUBJID")

        opt(list(subject = "SUBJID"))
        expect_identical(shiny::isolate(dd_ctrl_subject(session)), "SUBJID")
      })
    },
    NULL
  )
  expect_null(dd_ctrl_subject(NULL))
})

test_that("one click sends once, however often its ids are re-read", {
  sends <- list()

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        session$userData$blockr_ctrl_send <- function(target, args,
                                                      author = NULL) {
          sends[[length(sends) + 1L]] <<- args
        }

        # The sender's input changes under the same click (a global filter),
        # so the id set it reads changes too.
        r_ids <- shiny::reactiveVal(c("01", "02"))
        r_nonce <- shiny::reactiveVal(1)
        r_claims <- shiny::reactive(
          list(list(name = "USUBJID", mode = "multi", values = r_ids()))
        )

        dd_ctrl_sender(shiny::reactiveVal("pt_drill"), r_claims,
                       shiny::reactive(FALSE), session = session,
                       r_nonce = r_nonce)

        session$userData$set_ids <- function(x) r_ids(x)
        session$userData$click <- function() r_nonce(r_nonce() + 1)
      })
    },
    {
      session$flushReact()
      expect_length(sends, 1L)

      session$userData$set_ids("01")
      session$flushReact()
      expect_length(sends, 1L)

      # a new click sends what the data says now
      session$userData$click()
      session$flushReact()
      expect_length(sends, 2L)
      expect_identical(sends[[2L]]$state$columns[[1L]]$values, "01")
    }
  )
})
