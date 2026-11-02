# A grouping, colour or facet column that is NA in every row counts as unset,
# as if it were absent: upstream switches a dimension off by filling its
# column with NA (cols_with_values()). `missing` does not change that.

ec_data <- function() {
  data.frame(
    USUBJID = c("1", "2", "3", "4"),
    AEBODSYS = c("SKIN", "SKIN", "GI", "GI"),
    AEDECOD = c("RASH", "ITCH", "NAUSEA", "NAUSEA"),
    AGE = c(30, 40, 50, 60),
    SUB = NA_character_,
    stringsAsFactors = FALSE
  )
}

ec_prep <- function(d = ec_data(), by, missing = "drop", ...) {
  groups_prepare(
    d, by = by, missing = missing,
    summaries = list(list(type = "simple", name = "n",
                          func = "count_distinct", col = "USUBJID",
                          show = "number", ...))
  )
}

test_that("a filled outer column nests", {
  p <- ec_prep(by = c("AEBODSYS", "AEDECOD"))
  expect_identical(p$parent, "AEBODSYS")
  expect_true(any(p$rows$.is_parent))
})

test_that("an all-NA outer column lists the inner column flat", {
  for (m in c("drop", "level")) {
    p <- ec_prep(by = c("SUB", "AEDECOD"), missing = m)
    expect_null(p$parent)
    expect_false(any(p$rows$.is_parent))
    expect_setequal(p$rows$.label, c("RASH", "ITCH", "NAUSEA"))
  }
})

test_that("an all-NA inner column leaves the outer column alone", {
  for (m in c("drop", "level")) {
    p <- ec_prep(by = c("AEBODSYS", "SUB"), missing = m)
    expect_null(p$parent)
    expect_setequal(p$rows$.label, c("SKIN", "GI"))
  }
})

test_that("an all-NA only column draws the total row", {
  for (m in c("drop", "level")) {
    p <- ec_prep(by = "SUB", missing = m)
    expect_null(p$err)
    expect_identical(nrow(p$rows), 1L)
  }
})

test_that("an all-NA facet or colour column splits nothing", {
  plain <- ec_prep(by = "AEDECOD")
  for (m in c("drop", "level")) {
    facet <- ec_prep(by = "AEDECOD", missing = m, facet = "SUB")
    expect_null(facet$err)
    expect_identical(facet$rows$.label, plain$rows$.label)
    expect_identical(vapply(facet$plan, `[[`, "", "label"),
                     vapply(plain$plan, `[[`, "", "label"))
    color <- ec_prep(by = "AEDECOD", missing = m, color = "SUB")
    expect_null(color$err)
    expect_identical(color$rows$.label, plain$rows$.label)
  }
})

test_that("records and variables layouts ignore an all-NA by column", {
  d <- ec_data()
  for (m in c("drop", "level")) {
    expect_identical(table_nest(d, by = "SUB", missing = m), d)
    expect_identical(
      names(summary_table(d, vars = "AGE", by = "SUB", missing = m)),
      names(summary_table(d, vars = "AGE", missing = m))
    )
  }
})
