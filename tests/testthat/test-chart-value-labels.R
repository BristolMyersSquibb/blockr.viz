# Bar value labels (`value_labels`, #42): the argument, its state round-trip,
# and the two R renderers (static_chart() and chart_expr()) that must draw
# what the canvas draws.

vl_data <- function() {
  data.frame(
    g = c("a", "a", "b", "b", "c"),
    k = c("x", "y", "x", "y", "x"),
    v = c(3, 2, 5, -4, 1234.5678),
    stringsAsFactors = FALSE
  )
}

vl_state <- function(blk) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <<- session$returned$state$value_labels()
    },
    args = list(x = blk, data = list(data = vl_data))
  )
  out
}

text_layers <- function(p) {
  Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
}

test_that("value_labels is off by default and stored as a logical", {
  expect_false(eval(formals(new_chart_block)$value_labels))
  expect_false(vl_state(new_chart_block(group = "g")))
  expect_true(vl_state(new_chart_block(group = "g", value_labels = TRUE)))
  # The gear's transport strings restore too.
  expect_true(vl_state(new_chart_block(group = "g", value_labels = "on")))
  expect_false(vl_state(new_chart_block(group = "g", value_labels = "off")))
})

test_that("value_labels is registered with the other chart args", {
  expect_true("value_labels" %in% names(chart_arguments()))
  blk <- new_chart_block(group = "g")
  expect_true("value_labels" %in% blockr.core::external_ctrl_vars(blk))
})

test_that("value_labels round-trips through serialization", {
  blk <- new_chart_block(group = "g", value = "v", func = "sum",
                         value_labels = TRUE)
  ser <- blockr.core::blockr_ser(blk)
  expect_true(ser[["payload"]]$value_labels)
  back <- blockr.core::blockr_deser(ser)
  expect_true(vl_state(back))

  off <- blockr.core::blockr_ser(new_chart_block(group = "g"))
  expect_false(off[["payload"]]$value_labels)
})

test_that("the gear switch reaches state and the browser payload", {
  blk <- new_chart_block(group = "g", value = "v", func = "sum")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_false(session$returned$state$value_labels())
      expr_scope <- session$makeScope("expr")
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", value_labels = "on"
      ))
      session$flushReact()
      expect_true(session$returned$state$value_labels())
      expr_scope$setInputs(drilldown_block_action = list(
        action = "config", value_labels = "off"
      ))
      session$flushReact()
      expect_false(session$returned$state$value_labels())
    },
    args = list(x = blk, data = list(data = vl_data))
  )
})

test_that("static_chart() draws value labels only when asked", {
  d <- vl_data()
  expect_length(text_layers(static_chart(d, group = "g", value = "v",
                                         func = "sum")), 0)

  p <- static_chart(d, group = "g", value = "v", func = "sum",
                    value_labels = TRUE)
  expect_length(text_layers(p), 1)
  ld <- ggplot2::layer_data(p, 2)
  # Sums per group: a = 5, b = 1, c = 1234.5678 -> ddNum formatting.
  expect_setequal(ld$label, c("5", "1", "1,234.57"))
})

test_that("static_chart() labels a stack once, with its total", {
  d <- vl_data()
  p <- static_chart(d, group = "g", color = "k", value = "v", func = "sum",
                    value_labels = TRUE)
  ld <- ggplot2::layer_data(p, 2)
  expect_equal(nrow(ld), 3L)
  expect_setequal(ld$label, c("5", "1", "1,234.57"))
})

test_that("static_chart() labels every grouped bar and percent shares", {
  d <- vl_data()
  p <- static_chart(d, group = "g", color = "k", value = "v", func = "sum",
                    bar_mode = "grouped", value_labels = TRUE)
  ld <- ggplot2::layer_data(p, 2)
  expect_equal(nrow(ld), 5L)
  # A negative bar's label sits left of its end.
  expect_equal(ld$hjust[ld$label == "-4"], 1.15)

  d2 <- data.frame(g = c("a", "a", "b", "b"), k = c("x", "y", "x", "y"))
  p2 <- static_chart(d2, group = "g", color = "k", bar_mode = "percent",
                     value_labels = TRUE)
  expect_setequal(ggplot2::layer_data(p2, 2)$label, "50%")
})

test_that("chart_expr() emits the labels and the code runs", {
  d <- vl_data()
  off <- chart_expr("d", group = "g", value = "v", func = "sum")
  expect_no_match(chart_code(off), "geom_text")

  for (mode in c("stacked", "grouped", "percent")) {
    ex <- chart_expr("d", group = "g", color = "k", value = "v", func = "sum",
                     bar_mode = mode, value_labels = TRUE, qualify = TRUE)
    expect_match(chart_code(ex), "geom_text", fixed = TRUE)
    p <- eval(ex, list(d = d), baseenv())
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
    expect_length(text_layers(p), 1)
  }

  ex <- chart_expr("d", group = "g", value = "v", func = "sum",
                   value_labels = TRUE, qualify = TRUE)
  p <- eval(ex, list(d = d), baseenv())
  expect_setequal(ggplot2::layer_data(p, 2)$label, c("5", "1", "1,234.57"))
})

test_that("the report and download paths carry value_labels", {
  blk <- new_chart_block(group = "g", value = "v", func = "sum",
                         value_labels = TRUE)
  expect_true(chart_report_state(blk)$value_labels)
  expect_match(chart_code(report_call(blk, "d")), "geom_text", fixed = TRUE)

  old <- options(blockr.viz.report_style = "static")
  on.exit(options(old))
  txt <- paste(deparse(report_call(blk, "d")), collapse = " ")
  expect_match(txt, "value_labels = TRUE", fixed = TRUE)
  txt_off <- paste(deparse(report_call(new_chart_block(group = "g"), "d")),
                   collapse = " ")
  expect_no_match(txt_off, "value_labels")
})
