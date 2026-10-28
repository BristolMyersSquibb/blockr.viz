# The constructors' shared pieces: the legacy hook for old names in code
# (legacy_ctor_args()) and the block attributes read off a spec
# (viz_empty_ok(), viz_ctrl_args()).

test_that("old chart names in code are translated, with one warning", {
  expect_warning(
    out <- legacy_ctor_args("chart", list(
      chart_type = "bar", group = "cat", bar_mode = "grouped",
      y_scale = "log", pct_of = "group"
    )),
    "`chart_type`, `group`, `bar_mode`, `y_scale`, `pct_of` are from"
  )
  expect_identical(out$type, "bar")
  expect_identical(out$position, "dodge")
  expect_identical(out$y_transform, "log10")
  expect_identical(out$percent_of, "category")
  expect_false(any(c("chart_type", "group", "bar_mode", "y_scale",
                     "pct_of") %in% names(out)))
})

test_that("an old value under a current name is translated and named", {
  expect_warning(
    out <- legacy_ctor_args("chart", list(connect = "step-start",
                                          show_n = "off")),
    "`connect`, `show_n` are from"
  )
  expect_identical(out$connect, "step_start")
  expect_identical(out$show_n, "none")
})

test_that("current names pass silently, bad values are left for the spec", {
  expect_silent(out <- legacy_ctor_args("chart", list(
    type = "bar", x = "cat", facet_cols = "junk", script = c("a <- 1", "data")
  )))
  expect_identical(out$facet_cols, "junk")
  expect_identical(out$script, "a <- 1\ndata")
  expect_error(viz_check(viz_block_spec("chart"), out), "facet_cols")
})

test_that("direct_labels gets its own warning, after the names", {
  w <- character()
  withCallingHandlers(
    out <- legacy_ctor_args("chart", list(lo = "a", hi = "b",
                                          direct_labels = TRUE)),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 2L)
  expect_match(w[[1L]], "`lo`, `hi` are from")
  expect_match(w[[2L]], "direct_labels = TRUE` is now `legend = \"marks\"`")
  expect_identical(out$legend, "marks")
  expect_identical(out[c("ymin", "ymax")], list(ymin = "a", ymax = "b"))
})

test_that("a chart selection of the reference becomes the kit's shape", {
  sel <- function(s) legacy_chart_selection(list(selection = s))$selection
  expect_identical(
    sel(list(type = "categorical", filters = list(cat = "a"), range = NULL,
             point = NULL)),
    list(type = "categorical", filters = list(cat = "a"), columns = list())
  )
  expect_identical(
    sel(list(type = "point", filters = NULL, range = NULL,
             point = list(x_col = "x", y_col = "y", x_val = 2, y_val = 3))),
    list(type = "range", x = "x", y = "y", x_range = c(2, 2),
         y_range = c(3, 3), filters = list())
  )
  r <- sel(list(type = "range", filters = list(fac = "lo"), point = NULL,
                range = list(x_col = "x", y_col = NULL, x_range = list(1, 4))))
  expect_identical(r$x_range, c(1, 4))
  expect_null(r$y_range)
  expect_identical(r$filters, list(fac = "lo"))
  kit <- list(type = "categorical", filters = list(a = "x"), columns = list())
  expect_identical(sel(kit), kit)
  expect_null(sel(NULL))
})

test_that("old table names in code are translated, with one warning", {
  expect_warning(
    out <- legacy_ctor_args("table", list(axis = TRUE)),
    "`axis` is from"
  )
  expect_identical(out$column_axis, TRUE)
  expect_silent(legacy_ctor_args("table", list(layout = "records")))
})

test_that("the empty-able and the controllable arguments come from the spec", {
  spec <- viz_block_spec("chart")
  empty <- viz_empty_ok(spec)
  expect_true(all(c("x", "y", "unit", "drill_target", "script_values",
                    "selection") %in% empty))
  expect_false(any(c("type", "func", "position", "download") %in% empty))
  ctrl <- viz_ctrl_args(spec)
  expect_true(all(c("type", "x", "title", "script") %in% ctrl))
  expect_false(any(c("download", "selection") %in% ctrl))
  expect_true(all(viz_empty_ok(viz_block_spec("table")) %in%
                    names(viz_block_spec("table")$args)))
})

test_that("a chart keeps block attributes it does not own through a restore", {
  blk <- new_chart_block(type = "bar", x = "a", visible = "outputs")
  expect_identical(attr(blk, "visible"), "outputs")
  expect_silent(out <- blockr.core::blockr_deser(blockr.core::blockr_ser(blk)))
  expect_identical(attr(out, "visible"), "outputs")
  expect_warning(old <- new_chart_block(chart_type = "line"), "`chart_type`")
  expect_null(attr(old, "chart_type"))
  expect_warning(
    out <- blockr.core::blockr_deser(
      structure(list(), class = "chart_block"),
      list(payload = list(type = "bar", x = "a", smooth = TRUE,
                          visible = "outputs"),
           constructor = list(version = "0.2.9"))
    ),
    "smooth"
  )
  expect_null(attr(out, "smooth"))
  expect_identical(attr(out, "visible"), "outputs")
})

test_that("the table's pct_of reads as percent_of, in code and in a saved board", {
  expect_warning(
    out <- legacy_ctor_args("table", list(pct_of = "total")),
    "`pct_of` is from"
  )
  expect_identical(out$percent_of, "total")
  st <- legacy_restore("table", list(layout = "groups", pct_of = "total"))
  expect_identical(st$percent_of, "total")
  expect_false("pct_of" %in% names(st))
})

test_that("the old summary table constructor is the variables layout", {
  b <- new_summary_table_block(by = "ARM")
  expect_identical(blockr.core::blockr_ser(b)$payload$layout, "variables")
})
