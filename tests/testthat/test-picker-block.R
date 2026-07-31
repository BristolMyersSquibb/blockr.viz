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
    expect_equal(attr(out$value, "blockr_source"), "Sepal.Width")
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

test_that("the state slot is externally controllable", {
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
           selected = "Sepal.Length", multiple = FALSE)
    ))
  )

  expect_setequal(blockr.core::external_ctrl_vars(blk), c("state", "block_name"))

  testServer(blk$expr_server, args = list(data = reactive(datasets::iris)), {
    session$flushReact()
    ctrl <- session$returned$state$state
    # blockr.core writes the controlled value straight into the reactiveVal,
    # unnormalized -- here without `optional` and with a list-shaped pick.
    ctrl(list(pickers = list(
      list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
           selected = list("Sepal.Width"), multiple = FALSE)
    )))
    session$flushReact()

    out <- eval_picker_expr(session$returned$expr(), datasets::iris)
    expect_equal(as.numeric(out$value), datasets::iris$Sepal.Width)
    # ... and the stored state comes back canonical.
    st <- ctrl()$pickers
    expect_equal(st[[1]]$selected, "Sepal.Width")
    expect_false(st[[1]]$optional)
  })
})

test_that("an external payload patches the pick, keeping the offer list", {
  # The realistic send: only `into` + `selected`, one of two pickers.
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "x", choices = c("Sepal.Length", "Sepal.Width"),
           selected = "Sepal.Length", multiple = FALSE),
      list(into = "y", choices = c("Petal.Length", "Petal.Width"),
           selected = "Petal.Length", multiple = FALSE)
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(datasets::iris)), {
    session$flushReact()
    ctrl <- session$returned$state$state
    ctrl(list(pickers = list(list(into = "y", selected = "Petal.Width"))))
    session$flushReact()

    pks <- ctrl()$pickers
    expect_length(pks, 2L)
    # Untouched picker survives, patched picker keeps its curated offer list.
    expect_equal(pks[[1]]$selected, "Sepal.Length")
    expect_equal(pks[[2]]$choices, c("Petal.Length", "Petal.Width"))
    expect_equal(pks[[2]]$selected, "Petal.Width")

    out <- eval_picker_expr(session$returned$expr(), datasets::iris)
    expect_equal(as.numeric(out$y), datasets::iris$Petal.Width)
  })
})

test_that("an unmatched into adds a picker only when it offers something", {
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "y", choices = c("Petal.Length", "Petal.Width"),
           selected = "Petal.Length", multiple = FALSE)
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(datasets::iris)), {
    session$flushReact()
    ctrl <- session$returned$state$state

    # A typo'd `into` would otherwise leave an inert control on a curated
    # board, permanently, with nothing for the viewer to pick.
    ctrl(list(pickers = list(list(into = "y_value", selected = "Petal.Width"))))
    session$flushReact()
    expect_length(ctrl()$pickers, 1L)
    expect_equal(ctrl()$pickers[[1]]$selected, "Petal.Length")

    # Bringing an offer list is a real request for a second picker.
    ctrl(list(pickers = list(
      list(into = "x", choices = c("Sepal.Length", "Sepal.Width"),
           selected = "Sepal.Width")
    )))
    session$flushReact()
    pks <- ctrl()$pickers
    expect_length(pks, 2L)
    expect_equal(pks[[2]]$into, "x")

    out <- eval_picker_expr(session$returned$expr(), datasets::iris)
    expect_equal(as.numeric(out$x), datasets::iris$Sepal.Width)
    expect_equal(as.numeric(out$y), datasets::iris$Petal.Length)
  })
})

test_that("a payload that is not a picker list leaves the block standing", {
  # core's default `ctrl_block_ui()` is a text field per controllable input,
  # so submitting it writes a bare string into `state`. The repair observer
  # runs outside the ctrl plugin's try()/rollback, so reaching into that used
  # to kill the block's session; it must be a no-op instead.
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
           selected = "Sepal.Length", multiple = FALSE)
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(datasets::iris)), {
    session$flushReact()
    ctrl <- session$returned$state$state

    for (junk in list("Sepal.Width", list(), list(pickers = "Sepal.Width"),
                      list(pickers = list("Sepal.Width")))) {
      ctrl(junk)
      session$flushReact()
      pks <- ctrl()$pickers
      expect_length(pks, 1L)
      expect_equal(pks[[1]]$choices, c("Sepal.Length", "Sepal.Width"))
      expect_equal(pks[[1]]$selected, "Sepal.Length")
    }

    out <- eval_picker_expr(session$returned$expr(), datasets::iris)
    expect_equal(as.numeric(out$value), datasets::iris$Sepal.Length)
  })
})

test_that("clearing the offer list from the client still goes inert", {
  # The patch is for EXTERNAL writes only: the builder emptying "Columns
  # offered" must not have the cleared list patched back in.
  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
           selected = "Sepal.Length", multiple = FALSE)
    ))
  )

  testServer(blk$expr_server, args = list(data = reactive(datasets::iris)), {
    session$flushReact()
    session$setInputs(pickers = jsonlite::toJSON(
      list(list(into = "value", choices = character(),
                selected = character(), multiple = FALSE)),
      auto_unbox = TRUE
    ))
    session$flushReact()

    st <- session$returned$state$state()$pickers
    expect_length(st[[1]]$choices, 0L)
    out <- eval_picker_expr(session$returned$expr(), datasets::iris)
    expect_null(out$value)
  })
})

test_that("chained pickers keep the ORIGINAL source column in blockr_source", {
  # Upstream picker already copied Sepal.Width into `mid`; a second picker
  # copying `mid` must claim Sepal.Width, not the intermediate copy (which
  # the source table does not have -- the drill send would silently no-op).
  df <- datasets::iris
  df$mid <- df$Sepal.Width
  attr(df$mid, "blockr_source") <- "Sepal.Width"

  blk <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = "mid", selected = "mid",
           multiple = FALSE)
    ))
  )
  testServer(blk$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    out <- eval_picker_expr(session$returned$expr(), df)
    expect_equal(attr(out$value, "blockr_source"), "Sepal.Width")
  })

  # Same through the multiple/pivot path with a single pick (the only case
  # where the pivot records provenance).
  blk2 <- new_picker_block(
    state = list(pickers = list(
      list(into = "value", choices = "mid", selected = "mid",
           multiple = TRUE)
    ))
  )
  testServer(blk2$expr_server, args = list(data = reactive(df)), {
    session$flushReact()
    out <- eval_picker_expr(session$returned$expr(), df)
    expect_equal(attr(out$value, "blockr_source"), "Sepal.Width")
  })
})

test_that("the expr carries the data SLOT, not a free `data` symbol", {

  # expr_type = "bquoted" means blockr.core substitutes `.()` placeholders
  # and nothing else. A free `data` resolves at runtime (the block's eval env
  # binds it) so the app looks fine, while the EXPORTED code carries the bare
  # name and falls through to utils::data: the block evaluates to a FUNCTION
  # and every dependent fails on an object of class "function".
  df <- datasets::iris
  pks <- list(
    list(into = "value", choices = c("Sepal.Length", "Sepal.Width"),
         selected = "Sepal.Width", multiple = FALSE)
  )

  for (ex in list(make_picker_expr(pks), make_picker_expr(list()))) {

    # What export_code() does: substitute the slot with the upstream id.
    sub <- do.call(bquote, list(ex, list(data = as.name("upstream"))))
    e <- new.env(parent = globalenv())
    e$upstream <- df
    out <- eval(sub, e)
    expect_s3_class(out, "data.frame")

    # And unsubstituted it must FAIL, not quietly pick up utils::data.
    expect_error(eval(ex, new.env(parent = globalenv())))
  }
})
