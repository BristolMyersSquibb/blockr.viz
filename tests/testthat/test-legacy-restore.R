# legacy_restore() and legacy_deser() (R/legacy.R): saved states in older
# vocabularies come out under the current names and pass the spec.

restored_ok <- function(block, args) {
  expect_silent(viz_check(viz_block_spec(block), args))
  invisible(args)
}

test_that("a chart saved before 0.3.0 restores under the new names", {
  st <- legacy_restore("chart", list(
    chart_type = "bar", group = "class", value = "hwy", agg_fn = "mean",
    bar_mode = "grouped", tt_fields = "drv", count_on = "axis",
    na_group = "drop", ctrl_target = "f1", values = list(k = 1)
  ))
  restored_ok("chart", st)
  expect_identical(st$type, "bar")
  expect_identical(st$func, "mean")
  expect_identical(st$position, "dodge")
  expect_identical(st$tooltip, "drv")
  expect_identical(st$show_n, "axis")
  expect_identical(st$missing, "drop")
  expect_identical(st$drill_target, "f1")
  expect_identical(st$script_values, list(k = 1))
  # An unset orientation drew a bar horizontally: the group is on y.
  expect_identical(st$y, "class")
  expect_identical(st$x, "hwy")
})

test_that("a chart saved by the reference restores under the new names", {
  st <- legacy_restore("chart", list(
    type = "scatter", x = "a", y = "b", pct_of = c("color", "facet"),
    ref_lo = "lo", ref_hi = "hi", y_scale = "log", x_scale = "linear",
    show_n = "off", connect = "step-start", facet_cols = "2",
    box_points = "all", value_labels = "on", identity_line = "off",
    script = c("k <- 1", "data")
  ), version = "0.3.0")
  restored_ok("chart", st)
  expect_identical(st$percent_of, "color")
  expect_identical(st$ref_min, "lo")
  expect_identical(st$ref_max, "hi")
  expect_identical(st$y_transform, "log10")
  expect_identical(st$x_transform, "identity")
  expect_identical(st$show_n, "none")
  expect_identical(st$connect, "step_start")
  expect_identical(st$facet_cols, 2L)
  expect_identical(st$box_points, "none")
  expect_true(st$value_labels)
  expect_false(st$identity_line)
  expect_identical(st$script, "k <- 1\ndata")
  expect_false(any(c("pct_of", "ref_lo", "y_scale") %in% names(st)))
})

test_that("title templates come out with the new slot names", {
  st <- legacy_restore("chart", list(
    chart_type = "bar", group = "class", value = "hwy", agg_fn = "mean",
    subtitle = "{@chart_type}: {label(@group)} by {@bar_mode in stacked, grouped}",
    caption = "{@value_lines} and {@pct_of} [{@y_scale in linear, log}]"
  ))
  expect_identical(st$subtitle,
                   "{@type}: {label(@y)} by {@position in stack, dodge}")
  expect_identical(st$caption,
                   "{@y_lines} and {@percent_of} [{@y_transform in identity, log10}]")
  tb <- legacy_restore("table", list(
    layout = "groups", title = "{@group} within {@parent}, {@id_var}"
  ))
  expect_identical(tb$title, "{@by} within {@parent}, {@id}")
})

test_that("names dropped on purpose go without a warning", {
  expect_silent(st <- legacy_restore("chart", list(func_toggle = TRUE)))
  expect_false("func_toggle" %in% names(st))
  expect_silent(tb <- legacy_restore("table", list(layout = "records",
                                                   max_height = "600px")))
  expect_false("max_height" %in% names(tb))
})

test_that("an unknown name or a bad value warns and heals", {
  expect_warning(st <- legacy_restore("chart", list(colour = "x")), "colour")
  expect_false("colour" %in% names(st))
  expect_warning(st <- legacy_restore("chart", list(legend = "left")),
                 "legend")
  expect_identical(st$legend, "bottom")
})

test_that("a table saved by the reference restores under the new names", {
  st <- legacy_restore("table", list(
    layout = "groups", pct_of = "total", axis = FALSE, sort_by = "label",
    color = NULL, stats = list("compact"), script = c("a <- 1", "data")
  ), version = "0.3.0")
  restored_ok("table", st)
  expect_identical(st$percent_of, "total")
  expect_false(st$column_axis)
  expect_identical(st$sort_by, "alpha")
  expect_identical(st$color, "auto")
  expect_identical(st$stats, "mean_sd")
  expect_identical(st$script, "a <- 1\ndata")
})

test_that("the table's color = NULL means auto only before the rewrite", {
  expect_identical(legacy_restore("table", list(color = NULL),
                                  version = "0.2.9")$color, "auto")
  expect_null(legacy_restore("table", list(color = ""),
                             version = "0.3.0")$color)
  expect_null(legacy_restore("table", list(color = NULL),
                             version = VIZ_REWRITE_VERSION)$color)
  expect_identical(legacy_restore("table", list(color = "SEX"),
                                  version = "0.3.0")$color, "SEX")
})

test_that("old table names and the expanded stats preset translate", {
  st <- legacy_restore("table", list(
    value = c("a", "b"), group = "g", digits = 2, ctrl_target = "f",
    stats = "expanded"
  ))
  restored_ok("table", st)
  expect_identical(st$columns, c("a", "b"))
  expect_identical(st$by, "g")
  expect_identical(st$drill_target, "f")
  expect_setequal(st$stats, c("n", "mean", "sd", "median", "q1_q3",
                              "min_max"))
  expect_identical(legacy_restore("table", list(values = c("x", "y")))$columns,
                   c("x", "y"))
})

test_that("summary entries and shading rules take the new keys", {
  st <- legacy_restore("table", list(
    layout = "groups", by = "arm",
    summaries = list(
      list(type = "series", x = "t", col = "v", band = c("lo", "hi"),
           ref = "median_iqr", show = "sparkline"),
      list(type = "pair", from = "a", to = "b", to_func = "max", lo = "1",
           hi = "upper", ref = 5, dash = "arm"),
      list(type = "dist", col = "AGE", style = "box", inner = "mean_sd",
           outer = "p5_p95"),
      list(type = "simple", func = "count", show = "bar",
           position = "grouped", color = "arm", scope = "cell",
           facet_order = "count"),
      list(type = "spans", x = "start", xend = "end", fields = "term",
           parent = FALSE),
      list(type = "custom", fn = "\\(d) data.frame(value = 1)",
           denom = "USUBJID")
    ),
    shadings = list(list(type = "sequential", cols = list("a"),
                         palette = c("#fff", "#000"), domain = c(0, 1)))
  ), version = "0.3.0")
  restored_ok("table", st)
  s <- st$summaries
  expect_identical(s[[1L]]$type, "sparkline")
  expect_identical(c(s[[1L]]$ymin, s[[1L]]$ymax), c("lo", "hi"))
  expect_identical(s[[1L]]$summary, "median_q1_q3")
  expect_null(s[[1L]]$show)
  expect_identical(c(s[[2L]]$x, s[[2L]]$xend, s[[2L]]$xend_func),
                   c("a", "b", "max"))
  expect_identical(c(s[[2L]]$ref_min, s[[2L]]$ref_max), c("1", "upper"))
  expect_equal(s[[2L]]$x_lines, 5)
  expect_identical(s[[2L]]$linetype, "arm")
  expect_identical(s[[3L]]$show, "box")
  expect_identical(c(s[[3L]]$summary, s[[3L]]$whiskers), c("mean_sd", "p5_p95"))
  expect_null(s[[3L]]$style)
  expect_identical(s[[4L]]$position, "dodge")
  expect_false(any(c("scope", "facet_order") %in% names(s[[4L]])))
  expect_identical(s[[5L]]$tooltip, "term")
  expect_false("parent" %in% names(s[[5L]]))
  expect_identical(s[[6L]]$id, "USUBJID")
  sh <- st$shadings[[1L]]
  expect_identical(sh$mode, "sequential")
  expect_identical(sh$columns, "a")
  expect_equal(sh$domain, c(0, 1))
  expect_false(any(c("palette", "source", "cols") %in% names(sh)))
})

# The tile board saved by blockr.viz 0.2.223 (main before the tile went).
tile_payloads <- function() {
  ser <- blockr.core:::read_json(test_path("fixtures", "tile-board-main.json"))
  ser$payload$blocks$payload
}

test_that("every tile of a saved board restores as a table", {
  blocks <- tile_payloads()
  expect_length(blocks, 4L)
  for (id in names(blocks)) {
    r <- suppressWarnings(legacy_deser("tile_block", blocks[[id]]))
    expect_identical(r$block, "table", info = id)
    restored_ok("table", r$args)
  }
})

test_that("a grouped tile restores as grouped cards with its drill", {
  r <- legacy_deser("tile_block", tile_payloads()[["by_region"]])
  expect_identical(r$args$layout, "groups")
  expect_identical(r$args$look, "cards")
  expect_identical(r$args$by, "region")
  expect_identical(vapply(r$args$summaries, `[[`, "", "func"),
                   c("sum", "count"))
  expect_identical(r$args$drill, "region")
  expect_identical(r$args$selection$filters, list(region = "North"))
  expect_identical(r$attrs$block_name, "Revenue by region")
})

test_that("a tile's lost settings are named in one warning", {
  expect_warning(legacy_deser("tile_block", tile_payloads()[["wide"]]),
                 "unit")
})

# blockr.viz's own heatmap block, saved by 0.2.223 with its defaults
# (_scratch/viz-cran-review/integration/make-legacy.R, the old-heatmap board).
heatmap_saved <- function() {
  blockr.core:::read_json(test_path("fixtures", "heatmap-block-0.2.223.json"))
}

test_that("a saved heatmap restores as a compact groups table", {
  expect_warning(r <- legacy_deser("heatmap_block", heatmap_saved()),
                 "new_heatmap_block\\(\\) is gone: Old: heatmap")
  expect_identical(r$block, "table")
  expect_identical(r$attrs$block_name, "Old: heatmap")
  restored_ok("table", r$args)
  expect_identical(r$args$layout, "groups")
  expect_identical(r$args$look, "compact")
  s <- r$args$summaries[[1L]]
  expect_identical(s[c("type", "func", "show")],
                   list(type = "simple", func = "count", show = "tile"))
  expect_identical(r$args$drill, "auto")
  expect_identical(r$args$drill_target, "auto")
})

test_that("a heatmap's settings land on the table and its tile summary", {
  p <- list(row = "USUBJID", col = "AEDECOD", color = "AETOXGR",
            group = "TRT01A", top_n = 10, cell_numbers = FALSE,
            download = TRUE, ctrl_target = "f1", max_height = 400,
            subtitle = "Top {@top_n} {label(@col)}, by {label(@color)}",
            script = "top_n <- 25\ndata", values = list())
  expect_warning(st <- legacy_restore_heatmap(p, "AEs"), "AEs restores")
  restored_ok("table", st)
  expect_identical(st$by, c("TRT01A", "USUBJID"))
  s <- st$summaries[[1L]]
  expect_identical(s$facet, "AEDECOD")
  expect_identical(s$fill, "AETOXGR")
  expect_false(st$value_labels)
  expect_true(st$download)
  expect_identical(st$drill_target, "f1")
  expect_equal(st$script_values, list(top_n = 10))
  expect_identical(st$subtitle,
                   "Top {@top_n} {label(@facet)}, by {label(@fill)}")
  expect_false("max_height" %in% names(st))
})

test_that("a restored heatmap draws one row per subject", {
  ae <- data.frame(
    TRT01A = c("A", "A", "A", "B"), USUBJID = c("1", "1", "2", "3"),
    AEDECOD = c("x", "y", "x", "x"), AETOXGR = c("1", "3", "2", "1")
  )
  st <- suppressWarnings(legacy_restore_heatmap(list(
    row = "USUBJID", col = "AEDECOD", color = "AETOXGR", group = "TRT01A"
  )))
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  m <- table_model_groups(st, ae)
  expect_null(m$prep$err)
  v <- table_groups_to_model(m)
  expect_identical(as.character(v$cols[[1L]]$disp),
                   c("A", "1", "2", "B", "3"))
})

test_that("a saved heatmap board entry restores as a table block", {
  b <- suppressWarnings(blockr.core::blockr_deser(heatmap_saved()))
  expect_s3_class(b, "table_block")
})

test_that("another package's heatmap restores through its constructor", {
  data <- heatmap_saved()
  data$constructor$package <- "otherpkg"
  expect_error(blockr.core::blockr_deser(data), "otherpkg")
})

test_that("a summary table block restores in the variables layout", {
  data <- list(
    payload = list(
      state = list(vars = c("AGE", "SEX"), by = "ARM", add_overall = TRUE,
                   overall_label = "All", id_var = "USUBJID",
                   nest_hierarchies = TRUE, indent_details = TRUE,
                   stats = list("compact")),
      block_name = "Demographics"
    ),
    constructor = list(version = "0.2.200")
  )
  r <- legacy_deser("summary_table_block", data)
  restored_ok("table", r$args)
  expect_identical(r$block, "table")
  expect_identical(r$args$layout, "variables")
  expect_identical(r$args$by, "ARM")
  expect_true(r$args$total)
  expect_identical(r$args$total_label, "All")
  expect_identical(r$args$id, "USUBJID")
  expect_true(r$args$nest)
  expect_identical(r$args$stats, "mean_sd")
  expect_length(r$args$summaries, 2L)
  expect_identical(r$attrs$block_name, "Demographics")
})

test_that("a saved chart keeps its block attributes apart", {
  data <- list(
    payload = list(chart_type = "line", x = "t", y = "v",
                   block_name = "Trend"),
    constructor = list(version = "0.2.100")
  )
  r <- legacy_deser("chart_block", data)
  restored_ok("chart", r$args)
  expect_identical(r$block, "chart")
  expect_identical(r$args$type, "line")
  expect_identical(r$attrs, list(block_name = "Trend"))
})

test_that("blocks dropped for good have no restore", {
  expect_error(legacy_deser("gt_table_block", list(payload = list())),
               "No restore")
  expect_error(legacy_deser("summarize_table_block", list(payload = list())),
               "No restore")
})

test_that("an old table with group and typed summaries keeps its summaries", {
  s <- list(
    list(type = "simple", func = "count", show = "bar", name = "Subjects"),
    list(type = "dist", col = "AGE", style = "box", name = "Age")
  )
  r <- legacy_restore("table", list(group = "ARM", summaries = s))
  expect_identical(r$layout, "groups")
  expect_identical(r$by, "ARM")
  expect_identical(vapply(r$summaries, `[[`, "", "name"), c("Subjects", "Age"))
  expect_identical(r$summaries[[2L]]$show, "box")
})

test_that("a 0.3.0 summary key reads in code as in a saved board", {
  make <- function() {
    new_table_block(
      layout = "groups", by = "ARM",
      summaries = list(list(type = "dist", col = "AGE", style = "box"))
    )
  }
  expect_warning(make(), "`summaries[[1]]$style` (now `show`)", fixed = TRUE)
  b <- suppressWarnings(make())
  s <- blockr.core::blockr_ser(b)$payload$summaries[[1L]]
  expect_null(s$style)
  expect_identical(s$show, "box")
})

test_that("0.3.0's drill = TRUE turns the drill on, in code and on a board", {
  expect_warning(
    b <- new_table_block(layout = "groups", by = "ARM", drill = TRUE),
    "`drill` is from an earlier blockr.viz", fixed = TRUE
  )
  expect_identical(blockr.core::blockr_ser(b)$payload$drill, "auto")
  st <- legacy_restore("table", list(layout = "groups", by = "ARM",
                                     drill = "TRUE"), "0.3.0")
  restored_ok("table", st)
  expect_identical(st$drill, "auto")
  st <- legacy_restore("table", list(layout = "groups", drill = FALSE),
                       "0.3.0")
  expect_null(st$drill)
})

test_that("a block attribute such as blockr.dock's `visible` passes through", {
  vis <- list("table", "gear")
  r <- expect_silent(legacy_deser("table_block", list(
    payload = list(layout = "groups", by = "ARM", visible = vis,
                   block_name = "T"),
    constructor = list(version = VIZ_REWRITE_VERSION)
  )))
  expect_identical(r$attrs$visible, vis)
  expect_false("visible" %in% names(r$args))
  r <- suppressWarnings(legacy_deser("heatmap_block", list(
    payload = c(heatmap_saved()$payload, list(visible = vis)),
    constructor = list(version = "0.2.223")
  )))
  expect_identical(r$attrs$visible, vis)
  b <- expect_silent(blockr.core::blockr_deser(list(
    object = c("table_block", "transform_block", "block"),
    payload = list(layout = "groups", by = "ARM", visible = vis),
    constructor = blockr.core::blockr_ser(new_table_block())$constructor
  )))
  expect_identical(attr(b, "visible"), vis)
})

test_that("an unknown name is an error in code, on both blocks (row 28)", {
  expect_error(new_table_block(colour = "auto"),
               "unknown argument `colour`. Did you mean `color`?",
               fixed = TRUE)
  expect_error(new_chart_block(colour = "a"), "unknown argument `colour`",
               fixed = TRUE)
  expect_error(new_table_block(visibl = list("table")),
               "unknown argument `visibl`", fixed = TRUE)
  # blockr.core's own block arguments and blockr.dock's `visible` pass.
  expect_identical(attr(new_table_block(block_name = "T"), "block_name"),
                   "T")
})
