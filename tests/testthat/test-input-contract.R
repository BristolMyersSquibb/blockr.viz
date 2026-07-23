# The unified input contract across the renderer blocks (chart / table /
# tile / gt): ONE definition of "acceptable input" (a data frame or an
# object with a real as_annotated_df() method) behind every block's
# dat_valid, and as_plain_df() -- coerce + drop the reserved annotation
# columns -- behind the chart / tile, whose renderers chart a structured
# frame's DATA columns. Plain-data-frame inputs keep their exact
# pre-contract emitted expr (byte-identical), asserted by goldens below.

library(shiny)

new_contract_composed <- function(df) {
  structure(list(df = df), class = "contract_composed")
}

# Register the method the way a producer package would (exportS3Method);
# registerS3method targets the generic's namespace method table, which is
# also where has_annotated_df_method() looks.
registerS3method(
  "as_annotated_df", "contract_composed",
  function(x, ...) x$df,
  envir = environment(as_annotated_df)
)

# An annotated frame with every reserved column class: stub / hierarchy /
# emphasis / format template + precision / section, plus two data columns
# and a non-reserved dotted column that must survive as_plain_df().
ann_df <- data.frame(
  .label     = c("Mean", "SD"),
  .indent    = c(0L, 1L),
  .strong    = c(TRUE, FALSE),
  .emph      = c(FALSE, TRUE),
  .fmt       = c("{n}", "{n}"),
  .digits    = c(1L, 1L),
  .group1_level = c("Stats", "Stats"),
  .keepme    = c("u", "v"),
  grp        = c("A", "B"),
  n          = c(10, 2),
  stringsAsFactors = FALSE
)

garbage <- structure(list(), class = "no_such_table")

# Same helper as test-table-block-server.R: bind `data` and unwrap `.()`.
eval_block_expr <- function(ex, data) {
  e <- new.env(parent = globalenv())
  e$data <- data
  e$. <- function(x) x
  eval(ex, envir = e)
}

test_that("can_coerce_annotated_df is the one contract predicate", {
  expect_true(can_coerce_annotated_df(data.frame(a = 1)))
  expect_true(can_coerce_annotated_df(tibble::tibble(a = 1)))
  expect_true(can_coerce_annotated_df(ann_df))
  expect_true(can_coerce_annotated_df(new_contract_composed(ann_df)))
  expect_false(can_coerce_annotated_df(garbage))
  expect_false(can_coerce_annotated_df(1:3))
})

test_that("validate_annotated_df_input errors are user-level", {
  expect_no_error(validate_annotated_df_input(data.frame(a = 1)))
  expect_no_error(validate_annotated_df_input(new_contract_composed(ann_df)))
  err <- tryCatch(validate_annotated_df_input(garbage), error = identity)
  expect_match(
    conditionMessage(err),
    "Input must be a data frame .*as_annotated_df\\(\\) method .*got <no_such_table>"
  )
  # Says what the block expects, never internal column names.
  expect_match(conditionMessage(err), "summary_table\\(\\)")
  expect_no_match(conditionMessage(err), "depth|col_var|Missing required")
})

test_that("as_plain_df drops the reserved annotation columns only", {
  out <- as_plain_df(ann_df)
  expect_identical(names(out), c(".keepme", "grp", "n"))
  # Coercible objects go through as_annotated_df() first.
  out2 <- as_plain_df(new_contract_composed(ann_df))
  expect_identical(out2, out)
  # Plain frames without reserved columns pass through untouched.
  df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  expect_identical(as_plain_df(df), df)
  expect_error(as_plain_df(garbage), "No as_annotated_df\\(\\) method")
})

test_that("every renderer block's dat_valid enforces the shared contract", {
  blocks <- list(
    chart = new_chart_block(),
    table = new_table_block(),
    tile  = new_tile_block(),
    gt    = new_gt_table_block()
  )
  plain <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  for (blk in blocks) {
    expect_no_error(blockr.core::validate_data_inputs(blk, list(data = plain)))
    expect_no_error(
      blockr.core::validate_data_inputs(blk, list(data = tibble::tibble(a = 1)))
    )
    expect_no_error(blockr.core::validate_data_inputs(blk, list(data = ann_df)))
    expect_no_error(
      blockr.core::validate_data_inputs(
        blk, list(data = new_contract_composed(ann_df))
      )
    )
    expect_error(
      blockr.core::validate_data_inputs(blk, list(data = garbage)),
      "as_annotated_df\\(\\) method .*got <no_such_table>"
    )
  }
})

test_that("gt block accepts a tidy .fmt frame without .label", {
  tidy <- data.frame(
    .var   = c("AGE", "AGE"),
    .fmt   = c("{mean} ({sd})", "{mean} ({sd})"),
    .group = c("Placebo", "Drug"),
    mean   = c(45.2, 47.8),
    sd     = c(8.1, 9.3),
    stringsAsFactors = FALSE
  )
  blk <- new_gt_table_block()
  expect_no_error(blockr.core::validate_data_inputs(blk, list(data = tidy)))
  # And the renderer actually handles it (fmt_to_wide spread).
  tbl <- gt_table(tidy)
  expect_s3_class(tbl, "gt_tbl")
  html <- as.character(gt::as_raw_html(tbl))
  expect_match(html, "45.20 (8.10)", fixed = TRUE)
})

test_that("gt_table coerces table-producing objects on entry", {
  tbl <- gt_table(new_contract_composed(ann_df))
  expect_s3_class(tbl, "gt_tbl")
})

# --- chart: byte-identical expr for plain frames, coercion otherwise -------

test_that("chart expr is byte-identical for plain data frames", {
  df <- data.frame(grp = c("A", "B"), val = c(1, 2), stringsAsFactors = FALSE)
  blk <- new_chart_block(chart_type = "bar", group = "grp")

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    # Golden: the exact pre-contract emitted code, no coercion wrapper.
    expect_identical(
      deparse(session$returned$expr()),
      "dplyr::filter(.(data), TRUE)"
    )
    expect_equal(eval_block_expr(session$returned$expr(), df), df)

    # A click filter stays unwrapped too.
    session$setInputs(drilldown_block_action = list(
      action = "filter", filter_type = "categorical",
      column = "grp", values = list("A")
    ))
    expect_identical(
      deparse(session$returned$expr()),
      "dplyr::filter(.(data), .data[[\"grp\"]] == \"A\")"
    )
  })
})

test_that("chart expr coerces a table-producing input to the plain df", {
  blk <- new_chart_block(chart_type = "bar", group = "grp", value = "n",
                         func = "sum")
  obj <- new_contract_composed(ann_df)

  testServer(blk$expr_server, args = list(data = reactive(obj)), {
    ex <- session$returned$expr()
    # The emitted expression stands alone: self-qualified coercion call.
    expect_match(paste(deparse(ex), collapse = " "),
                 "blockr.viz::as_plain_df", fixed = TRUE)
    out <- eval_block_expr(ex, obj)
    expect_s3_class(out, "data.frame")
    expect_identical(names(out), c(".keepme", "grp", "n"))
  })
})

test_that("chart mapped to a coercion-dropped column still emits a valid expr", {
  blk <- new_chart_block(chart_type = "bar", group = ".label")
  obj <- new_contract_composed(ann_df)
  testServer(blk$expr_server, args = list(data = reactive(obj)), {
    # `.label` is an annotation column, dropped by the coercion -- so the
    # chart is mapped to a column the plain frame does not carry. That is a
    # PRESENTATION problem, surfaced by the JS renderer's in-canvas message,
    # never an expr-level failure (the expr-level aesthetic guard used to
    # validate() here and leaked its message into the dock header). The
    # emitted expr is only the click/brush filter: still valid, passes the
    # data through. See test-chart-block.R for the plain-df counterpart.
    ex <- session$returned$expr()
    expect_match(paste(deparse(ex), collapse = " "), "dplyr::filter",
                 fixed = TRUE)
    out <- eval_block_expr(ex, obj)
    expect_s3_class(out, "data.frame")
  })
})

# --- tile: same two behaviors ----------------------------------------------

test_that("tile expr is byte-identical for plain data frames", {
  df <- data.frame(region = c("A", "B"), revenue = c(1, 2),
                   stringsAsFactors = FALSE)
  blk <- new_tile_block(value = "revenue")

  testServer(blk$expr_server, args = list(data = reactive(df)), {
    expect_identical(
      deparse(session$returned$expr()),
      "dplyr::filter(.(data), TRUE)"
    )
    expect_equal(eval_block_expr(session$returned$expr(), df), df)

    session$setInputs(tile_block_action = list(
      action = "filter", column = "region", values = list("A")
    ))
    expect_identical(
      deparse(session$returned$expr()),
      "dplyr::filter(.(data), .data[[\"region\"]] == \"A\")"
    )
  })
})

test_that("tile expr coerces a table-producing input to the plain df", {
  blk <- new_tile_block(value = "n")
  obj <- new_contract_composed(ann_df)

  testServer(blk$expr_server, args = list(data = reactive(obj)), {
    ex <- session$returned$expr()
    expect_match(paste(deparse(ex), collapse = " "),
                 "blockr.viz::as_plain_df", fixed = TRUE)
    out <- eval_block_expr(ex, obj)
    expect_s3_class(out, "data.frame")
    expect_identical(names(out), c(".keepme", "grp", "n"))
  })
})
