# A display row that is not a level of anything.
#
# "Any Serious AEs" stands for a CONDITION, not for a value of a column, so a
# producer writing it has no variable/level pair to offer. It does have the
# predicate it counted with, and that resolves the row exactly: applying the
# same expression to the same frame returns the rows behind the number, rather
# than reconstructing them from a label.
#
# Without this, such a row claims its own LABEL as a column name -- there is no
# column called "Any Serious AEs" -- and the drill silently returns nothing.

src <- data.frame(
  USUBJID   = sprintf("S%d", 1:6),
  TRT01A    = rep(c("A", "B"), 3),
  TMPFL_SER = c("Y", "N", "Y", "N", "N", "Y"),
  stringsAsFactors = FALSE
)

drilled <- function(filter, ...) {
  d <- data.frame(.filter = filter, n = 1L, stringsAsFactors = FALSE, ...)
  attr(d, "source_data") <- src
  d
}

test_that("a predicate row resolves to the rows it counted", {
  out <- drill_source(drilled("TMPFL_SER == 'Y'"))
  expect_equal(nrow(out), 3L)
  expect_setequal(out$USUBJID, c("S1", "S3", "S6"))
})

test_that("the predicate composes with a column claim, not replaces it", {
  # Clicking that row inside arm A means both conditions: the rows matching
  # the predicate AND in that arm. Either alone is the wrong cohort.
  d <- drilled("TMPFL_SER == 'Y'", .group1 = "TRT01A", .group1_level = "A")
  out <- drill_source(d)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$USUBJID, c("S1", "S3"))
  expect_true(all(out$TRT01A == "A"))
})

test_that("several predicates in the subset claim nothing", {
  # The same rule the rest of the machinery uses: one value is a decision,
  # many is not. An undrilled table must not hand back its whole source.
  d <- data.frame(.filter = c("TMPFL_SER == 'Y'", "TMPFL_SER == 'N'"),
                  stringsAsFactors = FALSE)
  attr(d, "source_data") <- src
  expect_equal(nrow(drill_source(d)), 0L)
})

test_that("a frame with neither identity nor predicate still errors", {
  d <- data.frame(n = 1L)
  attr(d, "source_data") <- src
  expect_error(drill_source(d), "no ARD identity columns")
})

test_that("a broken predicate errors rather than claiming nobody", {
  # Silently returning zero rows would read as "no records match", which is
  # exactly the failure this path exists to prevent.
  expect_error(drill_source(drilled("TMPFL_SER ==")), "not parseable")
  expect_error(drill_source(drilled("NO_SUCH_COLUMN == 'Y'")),
               "could not be evaluated")
  expect_error(drill_source(drilled("USUBJID")), "must give a logical")
})

test_that("an NA in the predicate is not a match", {
  s2 <- src
  s2$TMPFL_SER[1] <- NA
  d <- drilled("TMPFL_SER == 'Y'")
  attr(d, "source_data") <- s2
  out <- drill_source(d)
  expect_equal(nrow(out), 2L)
  expect_false("S1" %in% out$USUBJID)
})

test_that("a variable/level row is unaffected", {
  d <- data.frame(.variable = "TMPFL_SER", .variable_level = "Y",
                  stringsAsFactors = FALSE)
  attr(d, "source_data") <- src
  expect_equal(nrow(drill_source(d)), 3L)
})
