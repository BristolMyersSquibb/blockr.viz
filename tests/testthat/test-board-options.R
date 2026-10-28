# Each board option this package defines survives a save and a restore: its
# recorded constructor is the exported option function, not a helper.

test_that("the viz board options serialize and restore", {
  opts <- list(
    new_exhibit_font_option(), new_download_footer_option(),
    new_drilldown_theme_option()
  )
  for (o in opts) {
    ser <- blockr.core::blockr_ser(o)
    back <- blockr.core::blockr_deser(ser)
    expect_s3_class(back, class(o)[[1L]])
    expect_identical(blockr.core::board_option_id(back),
                     blockr.core::board_option_id(o))
    expect_identical(blockr.core::board_option_default(back),
                     blockr.core::board_option_default(o))
  }
  ser <- blockr.core::blockr_ser(new_exhibit_font_option(value = 11))
  expect_identical(ser$constructor$constructor, "new_exhibit_font_option")
})
