test_that("dictionary encoding round-trips every column type exactly", {
  set.seed(1)
  n <- 2000L
  df <- data.frame(
    chr = sample(c("aaaa", "bbbb", NA), n, TRUE),
    fct = factor(sample(c("x", "y"), n, TRUE)),
    num = round(rnorm(n), 2),
    int = sample(1:12, n, TRUE),
    lgl = sample(c(TRUE, FALSE, NA), n, TRUE),
    dt  = as.Date("2020-01-01") + sample(300, n, TRUE),
    uid = paste0("id", seq_len(n)),
    stringsAsFactors = FALSE
  )

  # The payload as it was written before the encoding, which is the contract
  # the client reads: whatever a value looked like on the wire then, it has to
  # look like that now.
  plain <- jsonlite::fromJSON(
    jsonlite::toJSON(df, dataframe = "columns", digits = NA)
  )
  encoded <- jsonlite::fromJSON(chart_data_json(df))

  decode <- function(col) {
    if (is.list(col) && !is.null(col$`__enc__`)) col$levels[col$codes + 1] else col
  }
  for (nm in names(plain)) {
    expect_identical(decode(encoded[[nm]]), plain[[nm]], info = nm)
  }
})

test_that("the guards keep the encoding off columns it would not help", {
  n <- 2000L
  # near-unique: one level per row plus one code per row is strictly worse
  expect_false(chart_col_dict_worth_it(paste0("id", seq_len(n))))
  # too few rows for the levels array to pay for itself
  expect_false(chart_col_dict_worth_it(rep("a", 100L)))
  # more distinct values than the code width can pay back
  expect_false(
    chart_col_dict_worth_it(as.character(sample(1e5, 5e4L)), max_levels = 4096L)
  )
  # the case it exists for
  expect_true(chart_col_dict_worth_it(rep(c("Placebo", "Active"), n / 2L)))
  # a list column has no round trip through unique(), so it is left alone
  expect_false(chart_col_dict_worth_it(rep(list(1:2), n)))
})

test_that("an encoded column is smaller than the column it replaces", {
  n <- 5000L
  wide <- rep(c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo"),
              length.out = n)
  df <- data.frame(TRT = wide, stringsAsFactors = FALSE)
  before <- nchar(jsonlite::toJSON(df, dataframe = "columns", digits = NA))
  after <- nchar(chart_data_json(df))
  expect_lt(after, before / 5)
})
