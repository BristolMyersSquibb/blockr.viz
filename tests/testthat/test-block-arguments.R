# Guard the AI-facing `*_arguments()` descriptors against drift from the
# block constructors. Each descriptor (now a `new_block_args()` structure)
# must only advertise arguments that actually exist on the constructor, and
# every advertised arg must carry a description and an example slot.

ctor_formals <- function(ctor) {
  setdiff(names(formals(ctor)), "...")
}

arg_descriptions <- function(args) {
  vapply(args, blockr.core::block_arg_description, character(1))
}

# summary_table + gt_table are pure descriptors whose surface must match the
# constructor EXACTLY (this caught gt_table advertising a nonexistent
# `indent_stat` and omitting `na_rep`).
test_that("gt_table_arguments() names match new_gt_table_block() formals exactly", {
  ctor <- ctor_formals(new_gt_table_block)
  args <- gt_table_arguments()

  expect_setequal(names(args), ctor)
})

test_that("summary_table_arguments() names match new_summary_table_block() formals exactly", {
  ctor <- ctor_formals(new_summary_table_block)
  args <- summary_table_arguments()

  expect_setequal(names(args), ctor)
})

# The remaining blocks deliberately hide some constructor params from the AI
# surface (runtime-transport reactiveVals such as the filter_* fields). The
# invariant on the *constructor* side is therefore weaker (descriptor args are
# a subset of the ctor formals). Each advertised arg carries an example slot
# in the `new_block_args()` structure, so the assistant always has a value to
# mirror; we still require every descriptor to have a non-empty description.
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

    # new_block_args() structure keyed by arg name
    expect_s3_class(args, "block_args")
    # no phantom arguments advertised to the assistant
    expect_true(all(names(args) %in% ctor))
    # every advertised arg carries a worked example slot (possibly NULL)
    for (nm in names(args)) {
      expect_true("example" %in% names(args[[nm]]))
    }
    # descriptor entries are non-empty strings
    expect_true(all(nzchar(arg_descriptions(args))))
  }
})
