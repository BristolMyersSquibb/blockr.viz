# A column's variable label under its name in the table headers, when the
# column has missing values and `missing = "level"` turns them into a
# "(Missing)" level. table_missing_levels() made such a column character with
# as.character(), which drops the label: the groups stub read "CMDECOD" alone
# where 0.2.200 drew "Standardized Medication Name" under it, and a Table 1
# section read "CMDECOD" instead of its label.

tml_data <- function() {
  d <- data.frame(
    USUBJID = sprintf("S%02d", c(1:6, 1:4)),
    CMCLAS = rep(c("ANALGESICS", "ANTIBIOTICS"), 5),
    CMDECOD = c("IBUPROFEN", NA, "AMOXICILLIN", "IBUPROFEN", NA,
                "PARACETAMOL", "IBUPROFEN", "AMOXICILLIN", "PARACETAMOL",
                "IBUPROFEN"),
    TRT01A = rep(c("Placebo", "Drug"), each = 5),
    stringsAsFactors = FALSE
  )
  attr(d$CMDECOD, "label") <- "Standardized Medication Name"
  attr(d$CMCLAS, "label") <- "Medication Class"
  d
}

tml_stub <- function(by, data = tml_data(), missing = "level") {
  s <- list(list(type = "simple", func = "count_distinct", col = "USUBJID",
                 show = "bar"))
  b <- new_table_block(layout = "groups", by = by, summaries = s,
                       missing = missing)
  st <- blockr.core::blockr_ser(b)$payload
  st$block_name <- NULL
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  m <- table_model_groups(st, data)
  expect_null(m$prep$err)
  table_groups_to_model(m)$head[[1L]][[1L]]
}

test_that("a missing level keeps the column's variable label", {
  d <- tml_data()
  d$f <- factor(d$CMDECOD)
  attr(d$f, "label") <- "As factor"
  out <- table_missing_levels(d, c("CMDECOD", "f"))
  expect_true("(Missing)" %in% out$CMDECOD)
  expect_identical(attr(out$CMDECOD, "label"), "Standardized Medication Name")
  expect_identical(attr(out$f, "label"), "As factor")
})

test_that("the groups stub keeps its label line with a (Missing) group", {
  for (miss in c("level", "drop")) {
    h <- tml_stub("CMDECOD", missing = miss)
    expect_identical(h$text, "CMDECOD")
    expect_identical(h$sub, "Standardized Medication Name")
  }
  h <- tml_stub(c("CMCLAS", "CMDECOD"))
  expect_identical(h$text, "CMCLAS / CMDECOD")
  expect_identical(h$sub, "Medication Class / Standardized Medication Name")
})

test_that("a Table 1 section keeps its label with a (Missing) level", {
  x <- summary_table(tml_data(), vars = c("CMCLAS", "CMDECOD"),
                     by = "TRT01A", missing = "level")
  lab <- unique(x$.variable_label[x$.variable %in% "CMDECOD"])
  expect_identical(lab, "Standardized Medication Name")
  expect_true("(Missing)" %in% x$.label)
})
