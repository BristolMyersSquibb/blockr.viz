# An outer `by` column with no value draws no level: the table is flat.

ep_data <- function(soc) {
  data.frame(
    USUBJID = c("1", "2", "3", "4"),
    AEBODSYS = if (soc) c("SKIN", "SKIN", "GI", "GI") else NA_character_,
    AEDECOD = c("RASH", "ITCH", "NAUSEA", "NAUSEA"),
    stringsAsFactors = FALSE
  )
}

ep_prep <- function(d) {
  lane_prepare_summaries(
    d, by = c("AEBODSYS", "AEDECOD"),
    summaries = list(list(type = "simple", name = "n",
                          func = "count_distinct", col = "USUBJID",
                          show = "number"))
  )
}

test_that("a filled outer column nests", {
  p <- ep_prep(ep_data(TRUE))
  expect_identical(p$parent, "AEBODSYS")
  expect_true(any(p$rows$.is_parent))
})

test_that("an all-NA outer column lists the inner column flat", {
  p <- ep_prep(ep_data(FALSE))
  expect_null(p$parent)
  expect_false(any(p$rows$.is_parent))
  expect_setequal(p$rows$.label, c("RASH", "ITCH", "NAUSEA"))
})
