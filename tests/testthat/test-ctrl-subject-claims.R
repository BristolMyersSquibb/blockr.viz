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
  out <- ctrl_claims(flagged, "", list(AEBODSYS = "GI"),
                        subject = "USUBJID")
  expect_identical(out, list(list(
    name = "USUBJID", mode = "multi", values = c("01", "02"),
    label = "AEBODSYS = GI"
  )))
})

test_that("no subject column: the column claim, as before", {
  out <- ctrl_claims(ae, "", list(AEBODSYS = "GI"))
  expect_identical(out[[1L]]$name, "AEBODSYS")

  # declared, but this frame does not carry it
  out <- ctrl_claims(ae[, -1L], "", list(AEBODSYS = "GI"),
                        subject = "USUBJID")
  expect_identical(out[[1L]]$name, "AEBODSYS")
})

test_that("a click on a short subject id claims the subject column", {
  # `short` is one-to-one with the subject column `subj`; a chart grouped by
  # it sends the clicked bar's short id. The target joins on `subj`.
  d <- data.frame(
    subj = c("01-1015", "01-1015", "01-1023", "02-1023"),
    short = c("1015", "1015", "1023-a", "1023-b"),
    arm = c("A", "A", "B", "B")
  )
  out <- ctrl_claims(d, "", list(short = "1015"), subject = "subj")
  expect_identical(out, list(list(
    name = "subj", mode = "multi", values = "01-1015", label = "short = 1015"
  )))

  # a many-to-one column claims every subject under the click
  out <- ctrl_claims(d, "", list(arm = "B"), subject = "subj")
  expect_identical(out[[1L]]$values, c("01-1023", "02-1023"))
  expect_identical(out[[1L]]$label, "arm = B")

  # no declared subject: the clicked column, as before
  out <- ctrl_claims(d, "", list(short = "1015"))
  expect_identical(out[[1L]]$name, "short")
})

test_that("a structured table resolves through its stamped source_data", {
  ard <- data.frame(.variable = "AEBODSYS", .variable_level = c("GI", "Skin"),
                    n = c(2, 2))
  attr(ard, "source_data") <- ae[ae$TRTEMFL, ]
  out <- ctrl_claims(ard, "", list(.variable_level = "GI"),
                        subject = "USUBJID")
  expect_identical(out[[1L]]$values, c("01", "02"))
  expect_identical(out[[1L]]$label, "AEBODSYS = GI")
})

test_that("the un-drill and the hold are untouched", {
  expect_identical(ctrl_claims(ae, "", list(), subject = "USUBJID"),
                   list())
  expect_null(ctrl_claims(NULL, "", list(AEBODSYS = "GI"),
                             subject = "USUBJID"))
})

test_that("the subject column comes from the study_roles board option", {
  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        # no option: a board without subjects
        expect_null(ctrl_subject(session))

        opt <- shiny::reactiveVal(list(arm = "TRT", subject = ""))
        session$userData$board_options <- list(study_roles = opt)
        # declared nothing: no subjects, this package knows no convention
        expect_null(shiny::isolate(ctrl_subject(session)))

        opt(list(subject = "SUBJID"))
        expect_identical(shiny::isolate(ctrl_subject(session)), "SUBJID")
      })
    },
    NULL
  )
  expect_null(ctrl_subject(NULL))
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

        ctrl_sender(shiny::reactiveVal("pt_drill"), r_claims,
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
