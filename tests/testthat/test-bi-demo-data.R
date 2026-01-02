test_that("bi_demo_data returns a tibble", {
  demo <- bi_demo_data()
  expect_s3_class(demo, "tbl_df")
})

test_that("bi_demo_data has expected columns", {
  demo <- bi_demo_data()

  expected_cols <- c(
    "Region", "Country", "Category", "Channel",
    "Year", "Quarter", "Revenue", "Quantity", "Profit", "Transactions"
  )

  expect_equal(names(demo), expected_cols)
})

test_that("bi_demo_data has expected dimensions", {
  demo <- bi_demo_data()

  # Check dimension values
  expect_equal(
    sort(unique(demo$Region)),
    sort(c("Western Europe", "Southern Europe", "Northern Europe", "Central Europe"))
  )

  expect_equal(
    sort(unique(demo$Category)),
    sort(c("Electronics", "Clothing", "Food & Beverage", "Home & Garden"))
  )

  expect_equal(
    sort(unique(demo$Channel)),
    sort(c("Online", "Retail", "Wholesale"))
  )
})

test_that("bi_demo_data has expected row count", {
  demo <- bi_demo_data()

  # 10 countries * 4 categories * 3 channels * 3 years * 4 quarters = 1440
  expect_equal(nrow(demo), 1440)
})

test_that("bi_demo_data measures are numeric", {
  demo <- bi_demo_data()

  expect_type(demo$Revenue, "double")
  expect_type(demo$Quantity, "integer")
  expect_type(demo$Profit, "double")
  expect_type(demo$Transactions, "integer")
})

test_that("bi_demo_data measures are positive", {
  demo <- bi_demo_data()

  expect_true(all(demo$Revenue > 0))
  expect_true(all(demo$Quantity > 0))
  expect_true(all(demo$Profit > 0))
  expect_true(all(demo$Transactions > 0))
})

test_that("bi_demo_data is reproducible (uses set.seed)", {
  demo1 <- bi_demo_data()
  demo2 <- bi_demo_data()

  expect_equal(demo1$Revenue, demo2$Revenue)
  expect_equal(demo1$Profit, demo2$Profit)
})

test_that("bi_demo_data country-region mapping is correct", {
  demo <- bi_demo_data()

  # Check some mappings
  germany_regions <- unique(demo$Region[demo$Country == "Germany"])
  expect_equal(germany_regions, "Western Europe")

  italy_regions <- unique(demo$Region[demo$Country == "Italy"])
  expect_equal(italy_regions, "Southern Europe")

  uk_regions <- unique(demo$Region[demo$Country == "United Kingdom"])
  expect_equal(uk_regions, "Northern Europe")

 poland_regions <- unique(demo$Region[demo$Country == "Poland"])
  expect_equal(poland_regions, "Central Europe")
})
