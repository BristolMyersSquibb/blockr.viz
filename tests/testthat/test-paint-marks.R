
test_that("an inverted tag paints white on the event ink", {
  gl <- list(px = 1 / 96, family = "sans")
  fills <- function(e) {
    g <- paint_event(e, 0, 0, gl)
    c(g[[1L]]$gp$fill, g[[2L]]$gp$col)
  }
  expect_identical(fills(list(0, 6L, "AE")), c("#ffffff", PAINT_EV_INK))
  expect_identical(fills(list(0, 6L, "WD", 1L)), c(PAINT_EV_INK, "#ffffff"))
})
