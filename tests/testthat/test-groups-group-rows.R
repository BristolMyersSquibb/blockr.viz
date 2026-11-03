# The outer rows of a nested groups table (`group_rows`): by default a heading
# with the group's name and N, groups open; "summary" draws every summary on
# the group's line. An identity summary never draws on an outer row.

gr_data <- function() {
  data.frame(
    ARM = rep(c("Placebo", "Active"), each = 6),
    USUBJID = rep(sprintf("S%02d", 1:4), each = 3),
    AGE = rep(c(70, 40, 55, 65), each = 3),
    AVAL = c(1, 5, 3, 2, 8, 4, 9, 1, 2, 6, 6, 7),
    VISIT = rep(c("V1", "V2", "V3"), 4),
    stringsAsFactors = FALSE
  )
}

gr_summaries <- function() {
  list(
    list(type = "simple", name = "Age", func = "identity", col = "AGE",
         show = "bar"),
    list(type = "simple", name = "Peak", func = "max", col = "AVAL",
         show = "bar"),
    list(type = "dist", name = "AVAL", col = "AVAL"),
    list(type = "spans", name = "Days", x = "AVAL", xend = "AVAL"),
    list(type = "field", name = "Visits", col = "VISIT")
  )
}

gr_parents <- function(p) p$rows[p$rows$.is_parent %in% TRUE, , drop = FALSE]

test_that("the outer rows are headings by default: bare name, no summaries", {
  p <- groups_prepare(gr_data(), by = c("ARM", "USUBJID"),
                      summaries = gr_summaries())
  par <- gr_parents(p)
  expect_setequal(par$.label, c("Placebo", "Active"))
  expect_true(all(is.na(par$.s1_v)) && all(is.na(par$.s2_v)))
  expect_true(all(is.na(par$.s3_bc)))
  expect_identical(par$.s4_nseg, c(0L, 0L))
  expect_identical(par$.s5_t, c("", ""))
  # The inner rows keep theirs, and the groups open.
  leaf <- p$rows[!p$rows$.is_parent, ]
  expect_false(anyNA(leaf$.s2_v))
  m <- groups_cells(p)
  expect_true(m$expanded)
  par_i <- which(m$parent_row)
  for (c in m$cols) {
    d <- c$disp[par_i]
    expect_true(all(is.na(d) | !nzchar(d)))
  }
})

test_that("group_rows summary draws the summaries, never an identity one", {
  p <- groups_prepare(gr_data(), by = c("ARM", "USUBJID"),
                      summaries = gr_summaries(), group_rows = "summary")
  par <- gr_parents(p)
  expect_setequal(par$.label, c("Placebo", "Active"))
  expect_true(all(is.na(par$.s1_v)))
  expect_setequal(par$.s2_v, c(8, 9))
  expect_false(anyNA(par$.s3_bc))
  expect_true(all(par$.s4_nseg > 0L))
  expect_false(groups_cells(p)$expanded)
})

test_that("the groups still sort by their values when the rows are headings", {
  S <- list(list(type = "simple", name = "Peak", func = "max", col = "AVAL"))
  lab <- function(gr) {
    p <- groups_prepare(gr_data(), by = c("ARM", "USUBJID"), summaries = S,
                        group_rows = gr)
    gr_parents(p)$.label
  }
  expect_identical(lab("name"), lab("summary"))
  expect_identical(lab("name"), c("Active", "Placebo"))
})

test_that("a table without nesting has no outer rows to change", {
  p <- groups_prepare(gr_data(), by = "USUBJID", summaries = gr_summaries())
  expect_false(isTRUE(p$parent_heads))
  expect_false(anyNA(p$rows$.s1_v))
})

test_that("the compact look keeps its outer rows' bare names", {
  m <- table_model_groups(
    list(layout = "groups", by = c("ARM", "USUBJID"), look = "compact",
         summaries = list(list(type = "simple", func = "count",
                               show = "tile", facet = "VISIT"))),
    gr_data()
  )
  expect_setequal(m$prep$rows$.label[m$prep$rows$.is_parent],
                  c("Placebo", "Active"))
})

test_that("the block reads group_rows; code and new boards default to name", {
  b <- new_table_block(layout = "groups", by = c("ARM", "USUBJID"),
                       summaries = gr_summaries())
  st <- blockr.core::blockr_ser(b)$payload
  expect_identical(st$group_rows, "name")
  st$block_name <- NULL
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  m <- table_model_groups(st, gr_data())
  expect_true(m$prep$parent_heads)
  st$group_rows <- "summary"
  expect_false(table_model_groups(st, gr_data())$prep$parent_heads)
  expect_error(new_table_block(layout = "groups", group_rows = "both"))
})

test_that("a restored 0.2 nested table keeps its summaries on the outer rows", {
  saved <- blockr.core:::read_json(
    test_path("fixtures", "summarize-block-0.2.200.json")
  )
  b <- suppressWarnings(blockr.core::blockr_deser(saved))
  expect_identical(blockr.core::blockr_ser(b)$payload$group_rows, "summary")
  # Saved by the rewrite: the state says what it is, else the default.
  st <- legacy_restore("table", list(layout = "groups", by = c("a", "b")),
                       version = "0.3.0.9023")
  expect_identical(st$group_rows, "name")
  # Not nested before the rewrite: nothing to keep.
  st <- legacy_restore("table", list(layout = "groups", by = "b"),
                       version = "0.2.200")
  expect_identical(st$group_rows, "name")
})
