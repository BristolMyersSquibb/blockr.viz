# Guard the AI-facing `*_arguments()` descriptors against drift from the
# block constructors. Each descriptor must only advertise arguments that
# actually exist on the constructor, and its `examples` attribute must only
# carry keys that exist in the descriptor itself.

ctor_formals <- function(ctor) {
  setdiff(names(formals(ctor)), "...")
}

# summary_table + gt_table are pure descriptors whose surface must match the
# constructor EXACTLY (this caught gt_table advertising a nonexistent
# `indent_stat` and omitting `na_rep`).
test_that("gt_table_arguments() names match new_gt_table_block() formals exactly", {
  ctor <- ctor_formals(new_gt_table_block)
  args <- gt_table_arguments()
  ex   <- attr(args, "examples")

  expect_setequal(names(args), ctor)
  expect_setequal(names(ex), ctor)
  # ordering should be stable too, so the prompt reads sensibly
  expect_identical(names(args), names(ex))
})

test_that("summary_table_arguments() names match new_summary_table_block() formals exactly", {
  ctor <- ctor_formals(new_summary_table_block)
  args <- summary_table_arguments()
  ex   <- attr(args, "examples")

  expect_setequal(names(args), ctor)
  expect_setequal(names(ex), ctor)
  expect_identical(names(args), names(ex))
})

# The remaining blocks deliberately hide some constructor params from the AI
# surface (runtime-transport reactiveVals such as the filter_* fields). The
# invariant there is weaker but still important: no descriptor may advertise a
# phantom argument, and no example may reference a phantom descriptor key.
test_that("every *_arguments() descriptor is consistent with its constructor", {
  pairs <- list(
    list(args = summary_table_arguments, ctor = new_summary_table_block),
    list(args = gt_table_arguments,      ctor = new_gt_table_block),
    list(args = tile_arguments,          ctor = new_tile_block),
    list(args = chart_arguments,         ctor = new_chart_block),
    list(args = table_arguments,         ctor = new_table_block)
  )

  for (p in pairs) {
    args <- p$args()
    ctor <- ctor_formals(p$ctor)
    ex   <- attr(args, "examples")

    # no phantom arguments advertised to the assistant
    expect_true(all(names(args) %in% ctor))
    # examples never reference a key the descriptor doesn't define
    expect_true(all(names(ex) %in% names(args)))
    # descriptor entries are non-empty strings
    expect_true(all(nzchar(unlist(args))))
  }
})
