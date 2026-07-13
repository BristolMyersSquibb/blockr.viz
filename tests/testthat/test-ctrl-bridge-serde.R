test_that("the ctrl bridge extension survives a board round-trip", {
  skip_if_not_installed("blockr.dock")

  ext <- new_ctrl_bridge_extension()
  expect_s3_class(ext, "ctrl_bridge_extension")

  # Dock records an extension by constructor name + package and rebuilds it by
  # calling that constructor back with `ctor`/`pkg`. A constructor without dots
  # errors with "unused arguments", blockr.session swallows it, and the saved
  # board comes back blank.
  out <- blockr.core::blockr_deser(blockr.core::blockr_ser(ext, data = list()))

  expect_s3_class(out, "ctrl_bridge_extension")
  expect_s3_class(out, "dock_extension")
})

test_that("new_ctrl_bridge_extension() accepts the ctor/pkg dock passes on restore", {
  skip_if_not_installed("blockr.dock")

  expect_no_error(
    new_ctrl_bridge_extension(
      ctor = "new_ctrl_bridge_extension",
      pkg = "blockr.viz"
    )
  )
})
