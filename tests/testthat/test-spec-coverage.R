# The specs against the reference constructors (feat/viz-0.3.0 @ 8651d92):
# every public argument is in the spec, renamed in LEGACY_NAMES, or a
# documented drop, and every default is the reference default unless it is
# a documented change.

# Formals of the reference constructors, without `...`, as deparsed defaults.
REFERENCE_FORMALS <- list(
  chart = c(
    type = '"bar"', x = "NULL", y = "NULL", func = '"count"', color = "NULL",
    facet = "NULL", series = "NULL", xend = "NULL", label = "NULL",
    orientation = "NULL", position = '"stack"', sort_by = "NULL",
    sort_dir = "NULL", value_labels = "FALSE", format = '"number"',
    unit = '""', y_scale = '"linear"', x_scale = '"linear"',
    tooltip = "NULL", drill = "NULL", line_width = "1", point_size = "1",
    connect = '"straight"', y_lines = "NULL", x_lines = "NULL",
    ymin = "NULL", ymax = "NULL", linetype = "NULL", x_ranges = "NULL",
    legend = '"bottom"', highlight = "NULL", smoother = '"none"',
    identity_line = "FALSE", box_points = '"none"', summary = "NULL",
    whiskers = "NULL", connect_centers = "FALSE",
    band_window = '"adaptive"', band_size = "45", band_min_n = "12",
    id = "NULL", ref_lo = "NULL", ref_hi = "NULL",
    waterfall_totals = "NULL", bins = "30", show_n = '"off"',
    missing = '"level"', pct_of = '"facet"', facet_scales = '"fixed"',
    facet_cols = 'getOption("blockr.viz.facet_cols")', title = "NULL",
    subtitle = "NULL", caption = 'getOption("blockr.viz.default_caption")',
    download = "TRUE", drill_target = '""', drill_table = '""',
    script = "NULL", script_values = "list()", selection = "NULL"
  ),
  table = c(
    layout = '"records"', by = "character()", rowname = "NULL",
    columns = "NULL", summaries = "list()", id = "NULL",
    pct_of = '"facet"', missing = '"level"', color = "NULL",
    shadings = "list()", format = '"number"', unit = '""',
    look = '"table"', facet_layout = '"by_summary"', sort_by = '"value"',
    sort_dir = '"desc"', top_n = "NULL", value_labels = "TRUE",
    axis = "TRUE", bar_width = '"fit"', drill = "NULL",
    drill_target = '""', drill_table = '""', search = "TRUE",
    sortable = "TRUE", collapsible = "TRUE", max_height = "NULL",
    download = "TRUE", title = "NULL", subtitle = "NULL",
    caption = 'getOption("blockr.viz.default_caption")',
    stats = '"mean_sd"', sections = "character()", total = "FALSE",
    total_label = '"Total"', nest = "FALSE", script = "NULL",
    script_values = "list()", selection = "NULL"
  )
)

# Reference arguments the rewrite drops (spec-table.md section 4).
DROPPED_ARGS <- list(chart = character(), table = "max_height")

# The aggregated types map `category` and `value`, as before 0.3.0, and the
# value-axis lines are `value_lines` again (behaviour-changes.md row 32).
# The reference's names were never released, so LEGACY_NAMES has no row.
ADDED_ARGS <- list(chart = c("category", "value"), table = character())
REFERENCE_RENAMES <- list(chart = c(y_lines = "value_lines"),
                          table = character())

# Defaults the rewrite changes on purpose (spec-table.md section 1).
CHANGED_DEFAULTS <- list(chart = character(), table = c("color", "sort_dir"))

block_spec <- function(block) viz_block_spec(block)

# Every spec in the package: the two blocks, the summary types, the shading
# rule.
all_specs <- function() {
  c(list(viz_block_spec("chart"), viz_block_spec("table"),
         table_shading_spec()),
    unname(table_summary_specs()))
}

# Where a reference argument lives now: its own name, or the LEGACY_NAMES
# rename for it. NA for a documented drop.
current_name <- function(block, old) {
  spec <- block_spec(block)
  if (old %in% names(spec$args)) {
    return(old)
  }
  if (old %in% names(REFERENCE_RENAMES[[block]])) {
    return(REFERENCE_RENAMES[[block]][[old]])
  }
  hit <- LEGACY_NAMES[LEGACY_NAMES$block == block & LEGACY_NAMES$old == old, ]
  if (!nrow(hit)) {
    return(NULL)
  }
  hit$new[[1L]]
}

# A reference default carried into the new names: an option default stays
# an option name, a value goes through the LEGACY_NAMES value map.
reference_default <- function(block, old, text) {
  if (grepl("^getOption\\(", text)) {
    return(list(option = eval(parse(text = sub("^getOption", "c", text)))))
  }
  value <- eval(parse(text = text), baseenv())
  hit <- LEGACY_NAMES[LEGACY_NAMES$block == block & LEGACY_NAMES$old == old, ]
  map <- if (nrow(hit)) hit$values[[1L]] else character()
  if (is.character(value) && length(value) == 1L && value %in% names(map)) {
    value <- unname(map[[value]])
  }
  value
}

spec_default_for_test <- function(arg) {
  d <- arg$default
  if (inherits(d, "viz_opt")) list(option = d$name) else d
}

for (block in names(REFERENCE_FORMALS)) {

  test_that(paste("the", block, "spec covers every reference argument"), {
    spec <- block_spec(block)
    for (old in names(REFERENCE_FORMALS[[block]])) {
      now <- current_name(block, old)
      if (old %in% DROPPED_ARGS[[block]]) {
        expect_true(is.na(now), info = old)
        next
      }
      expect_false(is.null(now), info = paste(old, "is neither in the spec",
                                              "nor in LEGACY_NAMES"))
      expect_true(now %in% names(spec$args), info = paste(old, "->", now))
    }
  })

  test_that(paste("the", block, "spec adds no argument the reference lacks"), {
    spec <- block_spec(block)
    known <- vapply(names(REFERENCE_FORMALS[[block]]), function(old) {
      current_name(block, old) %||% NA_character_
    }, "")
    expect_setequal(names(spec$args),
                    c(stats::na.omit(known), ADDED_ARGS[[block]]))
  })

  test_that(paste("the", block, "spec keeps the reference defaults"), {
    spec <- block_spec(block)
    for (old in names(REFERENCE_FORMALS[[block]])) {
      now <- current_name(block, old)
      if (is.null(now) || is.na(now) || now %in% CHANGED_DEFAULTS[[block]]) {
        next
      }
      expect_equal(
        spec_default_for_test(spec$args[[now]]),
        reference_default(block, old, REFERENCE_FORMALS[[block]][[old]]),
        info = paste(old, "->", now)
      )
    }
  })
}

test_that("the documented default changes are the ones in the spec", {
  spec <- viz_block_spec("table")
  expect_identical(spec$args$color$default, "auto")
  expect_null(spec$args$sort_dir$default)
})

# Without viz_check() (not written yet) this checks the parts of a default
# the spec itself states: enum values, bounds, NULL, logicals.
check_default <- function(arg, name) {
  d <- arg$default
  if (inherits(d, "viz_opt") || isTRUE(arg$required)) {
    return(invisible())
  }
  t <- arg$type$name
  if (is.null(d)) {
    expect_false(isFALSE(arg$null), info = paste(name, "defaults to NULL"))
    return(invisible())
  }
  if (t %in% c("enum", "enums")) {
    expect_true(all(d %in% arg$values), info = name)
  }
  if (t == "bool") {
    expect_true(is.logical(d) && length(d) == 1L, info = name)
  }
  if (t %in% c("number", "integer")) {
    expect_true(is.numeric(d) && length(d) == 1L, info = name)
    b <- arg$bounds
    if (!is.null(b)) {
      expect_true(d <= b$max && (if (b$min_open) d > b$min else d >= b$min),
                  info = name)
    }
  }
  if (t == "column" && is.character(d)) {
    expect_true(all(d %in% arg$also), info = name)
  }
  if (t == "format") {
    expect_true(d %in% VIZ_FORMATS || d %in% 0:6, info = name)
  }
  if (t == "map") {
    expect_true(is.list(d), info = name)
  }
  invisible()
}

test_that("every default fits its own spec entry", {
  for (spec in all_specs()) {
    for (nm in names(spec$args)) {
      check_default(spec$args[[nm]], paste0(spec$block, "$", nm))
    }
  }
})

test_that("gear option labels match the values they name", {
  for (spec in all_specs()) {
    for (nm in names(spec$args)) {
      a <- spec$args[[nm]]
      labels <- a$gear$labels
      if (is.null(labels)) next
      n <- if (a$type$name == "bool") 2L else length(a$values)
      expect_length(labels, n)
    }
  }
})

test_that("the block specs and the nested specs pass viz_spec_check()", {
  for (block in names(VIZ_BLOCK_SPECS)) {
    expect_s3_class(viz_spec_check(viz_block_spec(block)), "viz_spec")
  }
  for (spec in c(list(table_shading_spec()), unname(table_summary_specs()))) {
    expect_s3_class(viz_spec_check(spec), "viz_spec")
  }
})

test_that("LEGACY_NAMES points at names the specs have", {
  targets <- list(
    chart = names(viz_block_spec("chart")$args),
    table = names(viz_block_spec("table")$args),
    summary = unique(c("type", unlist(lapply(table_summary_specs(),
                                             function(s) names(s$args))))),
    shading = names(table_shading_spec()$args)
  )
  # The heatmap restores as a table: its names land on the table or on its
  # tile summary.
  targets$heatmap <- c(targets$table, targets$summary)
  expect_setequal(unique(LEGACY_NAMES$block), names(targets))
  kept <- LEGACY_NAMES[!is.na(LEGACY_NAMES$new), ]
  for (i in seq_len(nrow(kept))) {
    expect_true(kept$new[[i]] %in% targets[[kept$block[[i]]]],
                info = paste(kept$block[[i]], kept$old[[i]], "->",
                             kept$new[[i]]))
  }
  expect_false(anyDuplicated(paste(LEGACY_NAMES$block, LEGACY_NAMES$old,
                                   LEGACY_NAMES$new)) > 0L)
})

test_that("no spec entry keeps an old name", {
  renamed <- function(block) {
    rows <- LEGACY_NAMES[LEGACY_NAMES$block == block, ]
    rows$old[is.na(rows$new) | rows$old != rows$new]
  }
  expect_length(intersect(renamed("chart"),
                          names(viz_block_spec("chart")$args)), 0L)
  # `color`, `summaries` and `sort_by` map values only; `group` and `value`
  # are old table names that are not table arguments.
  expect_length(intersect(renamed("table"),
                          names(viz_block_spec("table")$args)), 0L)
})

test_that("viz_check() accepts every block default", {
  withr::local_options(blockr.viz.facet_cols = NULL,
                       blockr.viz.default_caption = NULL)
  for (block in c("chart", "table")) {
    spec <- viz_block_spec(block)
    expect_named(viz_check(spec, list()), names(spec$args))
  }
})

test_that("viz_check() refuses a British spelling with a hint", {
  expect_error(viz_check(viz_block_spec("chart"), list(colour = "x")),
               "Did you mean `color`")
})
