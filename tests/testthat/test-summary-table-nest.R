# summary_table() with nested hierarchies (the table block's `nest = TRUE`
# in the variables layout) and a logical variable.

nest_df <- function() {
  data.frame(
    ARM = rep(c("A", "B"), each = 4),
    SOC = c("Card", "Card", "Skin", "Skin", "Card", "Skin", "Skin", "Skin"),
    TERM = c("MI", "AF", "Rash", "Itch", "MI", "Rash", "Rash", "Itch"),
    FLAG = c(TRUE, FALSE, NA, TRUE, TRUE, TRUE, FALSE, FALSE),
    ID = paste0("S", c(1, 2, 3, 4, 5, 6, 6, 7))
  )
}

test_that("a nested pair puts each term under its organ class", {
  r <- as.data.frame(summary_table(
    nest_df(), vars = c("SOC", "TERM", "FLAG"), by = "ARM",
    nest_hierarchies = TRUE, add_overall = TRUE, subject_var = "ID"
  ))
  expect_identical(r$.label,
                   c("Card", "AF", "MI", "Skin", "Itch", "Rash", "FLAG"))
  expect_identical(r$.variable,
                   c("SOC", "TERM", "TERM", "SOC", "TERM", "TERM", "FLAG"))
  expect_identical(r$.indent, c(1L, 2L, 2L, 1L, 2L, 2L, 1L))
  # Counts are subjects: S6 has two Rash rows in B and counts once.
  expect_identical(as.vector(r$B), c("1 (33.3%)", NA, "1 (33.3%)", "2 (66.7%)",
                          "1 (33.3%)", "1 (33.3%)", "2 (66.7%)"))
  expect_identical(as.vector(r$Total)[1:4], c("3 (42.9%)", "1 (14.3%)", "2 (28.6%)",
                                   "4 (57.1%)"))
})

test_that("a nested pair repeats per section", {
  r <- as.data.frame(summary_table(
    nest_df(), vars = c("SOC", "TERM"), sections = "ARM",
    nest_hierarchies = TRUE
  ))
  expect_identical(as.vector(r$.group1_level), rep(c("A", "B"), c(6L, 5L)))
  expect_identical(r$.indent[1:3], c(0L, 1L, 1L))
  expect_identical(as.vector(r$Overall)[r$.label == "Rash"], c("1 (25.0%)", "2 (50.0%)"))
})

test_that("terms that sit under two classes do not nest", {
  d <- nest_df()
  d$TERM[d$SOC == "Skin"][1L] <- "MI"
  r <- as.data.frame(summary_table(d, vars = c("SOC", "TERM"), by = "ARM",
                                   nest_hierarchies = TRUE))
  expect_true(all(r$.indent == 1L))
  expect_setequal(unique(r$.variable), c("SOC", "TERM"))
})
