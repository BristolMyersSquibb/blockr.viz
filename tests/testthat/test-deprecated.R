# Deprecated constructors (R/deprecated.R): a warning when code calls them, a
# toast when a saved board restores them, and the result is a table block.

dep_toasts <- function(expr) {
  shown <- character()
  testthat::local_mocked_bindings(
    get_session = function() list(),
    notify = function(...) {
      shown <<- c(shown, paste0(...))
      invisible()
    },
    .package = "blockr.core"
  )
  force(expr)
  shown
}

test_that("code that calls a deprecated constructor gets a warning", {
  expect_warning(b <- new_summarize_table_block(by = "Species"),
                 class = "deprecatedWarning")
  expect_s3_class(b, "table_block")
  expect_warning(new_summarize_table_block(),
                 "Use new_table_block\\(layout = \"groups\"\\)")
  expect_warning(b <- new_summary_table_block(by = "Species"),
                 class = "deprecatedWarning")
  expect_s3_class(b, "table_block")
})

test_that("code does not toast, a restore does not warn", {
  expect_length(dep_toasts(suppressWarnings(new_summarize_table_block())), 0L)
  expect_length(dep_toasts(suppressWarnings(new_summary_table_block())), 0L)
  expect_no_warning(
    shown <- dep_toasts(new_summarize_table_block(ctor = "x"))
  )
  expect_length(shown, 1L)
})

test_that("a restored summarize table toasts and saves as a table block", {
  saved <- blockr.core:::read_json(
    test_path("fixtures", "summarize-block-0.2.200.json")
  )
  shown <- dep_toasts(b <- withCallingHandlers(
    blockr.core::blockr_deser(saved),
    deprecatedWarning = function(w) stop("restore warned")
  ))
  expect_length(shown, 1L)
  expect_match(shown, "Save the board")
  expect_s3_class(b, "table_block")
  expect_identical(blockr.core::blockr_ser(b)$constructor$constructor,
                   "new_table_block")
})

test_that("outside a session a restore shows no toast", {
  expect_null(blockr.core::get_session())
  expect_silent(viz_deprecated_toast())
})
