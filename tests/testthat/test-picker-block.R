# Browser-free tests for the picker block's server logic. The picker stores a
# curated set of column choices per picker; the regression these guard against
# is the one that made a saved board come back with an empty "Columns offered"
# field: a transient data frame lacking the picker's columns (an upstream block
# still restoring its own state) must NOT wipe the authored definition.

library(shiny)

# Evaluate the block's bquote()'d expression with `data` bound, unwrapping the
# `.()` placeholder the board would otherwise substitute.
eval_picker_expr <- function(ex, data) {
  e <- new.env(parent = globalenv())
  e$data <- data
  e$. <- function(x) x
  eval(ex, envir = e)
}

test_that("normalize_pickers keeps choices absent from a transient frame", {
  # Structural normalization only -- no pruning against data columns.
  pks <- normalize_pickers(list(
    list(into = "value", choices = c("N", "Pct"), selected = "N",
         multiple = FALSE)
  ))
  expect_equal(pks[[1]]$choices, c("N", "Pct"))
  expect_equal(pks[[1]]$selected, "N")
})

test_that("a data frame missing the picker columns does not wipe the state", {
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = c("N", "Pct"), selected = "N",
           multiple = FALSE)
    ))
  )

  data <- reactiveVal(
    data.frame(grp = c("a", "b"), other = 1:2, stringsAsFactors = FALSE)
  )
  final <- data.frame(
    grp = c("a", "b"), N = 1:2, Pct = c(0.5, 0.5), stringsAsFactors = FALSE
  )

  testServer(blk$expr_server, args = list(data = data), {
    session$flushReact()

    # Transient frame lacks N/Pct: the picker is skipped, not emptied.
    st <- session$returned$state$state()$pickers
    expect_equal(st[[1]]$choices, c("N", "Pct"))
    expect_equal(st[[1]]$selected, "N")
    out <- eval_picker_expr(session$returned$expr(), data())
    expect_false("value" %in% names(out))

    # Real columns arrive: choices intact and the pick self-heals.
    data(final)
    session$flushReact()
    st <- session$returned$state$state()$pickers
    expect_equal(st[[1]]$choices, c("N", "Pct"))
    expect_equal(st[[1]]$selected, "N")
    out <- eval_picker_expr(session$returned$expr(), final)
    expect_true("value" %in% names(out))
    expect_equal(as.integer(out$value), final$N)
  })
})

test_that("single picker copies the column and carries its label", {
  df <- datasets::iris
  attr(df$Sepal.Width, "label") <- "Sepal width"

  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
           selected = "Sepal.Width", multiple = FALSE)
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    out <- eval_picker_expr(session$returned$expr(), df)
    expect_equal(as.numeric(out$value), as.numeric(df$Sepal.Width))
    expect_equal(attr(out$value, "label"), "Sepal width")
  })
})

test_that("multiple picker pivots the picks long into into + into_measure", {
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
           selected = c("Sepal.Length", "Sepal.Width"), multiple = TRUE)
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(datasets::iris)), {
    session$flushReact()
    out <- eval_picker_expr(session$returned$expr(), datasets::iris)
    expect_equal(nrow(out), 2L * nrow(datasets::iris))
    expect_true(all(c("value", "value_measure") %in% names(out)))
    expect_setequal(
      levels(out$value_measure), c("Sepal.Length", "Sepal.Width")
    )
  })
})

test_that("a JS-sent selection updates the stored state", {
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
           selected = "Sepal.Length", multiple = FALSE)
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(datasets::iris)), {
    session$flushReact()
    session$setInputs(pickers = jsonlite::toJSON(
      list(list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
                selected = "Sepal.Width", multiple = FALSE)),
      auto_unbox = TRUE
    ))
    session$flushReact()
    st <- session$returned$state$state()$pickers
    expect_equal(st[[1]]$selected, "Sepal.Width")
  })
})
