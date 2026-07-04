# Auto-coercion of table-producing inputs in the table block: objects with an
# as_annotated_df() method (composer tables et al.) connect directly, no
# explicit coercion block in between. A minimal producer class stands in for
# blockr.sandbox's composer methods.

library(shiny)

new_fake_composed <- function(df) {
  structure(list(df = df), class = "fake_composed")
}

# Register the method the way a producer package would (exportS3Method);
# registerS3method targets the generic's namespace method table, which is
# also where has_annotated_df_method() looks.
registerS3method(
  "as_annotated_df", "fake_composed",
  function(x, ...) x$df,
  envir = environment(as_annotated_df)
)

fake_df <- data.frame(
  .label = c("Age", "Sex"),
  Value  = c("64 (12)", "52 (48%)"),
  stringsAsFactors = FALSE
)

# Same helper as test-table-block-server.R: bind `data` and unwrap `.()`.
eval_block_expr <- function(ex, data) {
  e <- new.env(parent = globalenv())
  e$data <- data
  e$. <- function(x) x
  eval(ex, envir = e)
}

test_that("as_annotated_df passes data frames through untouched", {
  df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  expect_identical(as_annotated_df(df), df)
})

test_that("as_annotated_df default method errors informatively", {
  expect_error(
    as_annotated_df(structure(list(), class = "no_such_table")),
    "No as_annotated_df\\(\\) method for <no_such_table>"
  )
})

test_that("has_annotated_df_method reflects real (non-default) dispatch", {
  expect_true(has_annotated_df_method(data.frame(a = 1)))
  expect_true(has_annotated_df_method(new_fake_composed(fake_df)))
  expect_false(has_annotated_df_method(structure(list(), class = "no_such_table")))
  expect_false(has_annotated_df_method(1:3))
})

test_that("table block dat_valid accepts data frames and coercible objects", {
  blk <- new_table_block()
  expect_no_error(
    blockr.core::validate_data_inputs(blk, list(data = fake_df))
  )
  expect_no_error(
    blockr.core::validate_data_inputs(blk, list(data = new_fake_composed(fake_df)))
  )
  expect_error(
    blockr.core::validate_data_inputs(
      blk, list(data = structure(list(), class = "no_such_table"))
    ),
    "as_annotated_df\\(\\) method .*got <no_such_table>"
  )
})

test_that("block expr coerces a table-producing input to the annotated df", {
  blk <- new_table_block()
  obj <- new_fake_composed(fake_df)

  testServer(blk$expr_server, args = list(data = reactive(obj)), {
    ex <- session$returned$expr()
    # The emitted expression stands alone: self-qualified coercion call.
    expect_match(paste(deparse(ex), collapse = " "),
                 "blockr.viz::as_annotated_df", fixed = TRUE)
    out <- eval_block_expr(ex, obj)
    expect_s3_class(out, "data.frame")
    expect_equal(out$.label, fake_df$.label)
  })
})

test_that("block expr is unchanged-in-effect for plain data frames", {
  df <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_table_block()

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    out <- eval_block_expr(session$returned$expr(), df)
    expect_equal(out, dplyr::filter(df, TRUE))
  })
})

test_that("drill filter applies to the coerced frame", {
  blk <- new_table_block(drill = ".label")
  obj <- new_fake_composed(fake_df)

  testServer(blk$expr_server, args = list(data = reactive(obj)), {
    session$setInputs(drilldown_table_block_action = list(
      action = "filter", column = ".label", values = list("Age")
    ))
    out <- eval_block_expr(session$returned$expr(), obj)
    expect_equal(nrow(out), 1L)
    expect_equal(out$.label, "Age")
  })
})
