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
# invariant on the *constructor* side is therefore weaker (descriptor args are
# a subset of the ctor formals), but the *example* must still cover EVERY arg
# the assistant is shown. A subset check (`names(ex) %in% names(args)`) was too
# weak: chart_arguments() advertised 23 args but its example carried only 13,
# and the gap passed silently. blockr.assistant surfaces the example as the
# canonical arg shape, so any advertised arg missing from the example is one
# the model has no value to copy -- it then invents a name (drilldown,
# direction, x_col) that the ctor's `...` swallows, producing a misconfigured
# block with no error. Require the example to setequal the advertised args.
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
    # the example must cover EVERY advertised arg (no missing, no phantom),
    # so the assistant always has a value to mirror for each one
    expect_setequal(names(ex), names(args))
    # descriptor entries are non-empty strings
    expect_true(all(nzchar(unlist(args))))
  }
})
