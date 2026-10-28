# chart_normalize() and table_normalize() against the reference's recorded
# behaviour (fixtures/chart-record, fixtures/table-golden). For the
# constructor and for every `set` step, normalize(previous state + change)
# must equal the recorded state. Every other recorded state (gear, click,
# data steps) must already be normalized: normalize() leaves it as it is.
# States are read through legacy_restore(), as a saved board is.

# Recorded states that are not the reference's rules, by scenario and step.
NORM_SKIP <- list(
  # The reference kept a column the data lacks when it was set to NULL (a
  # guard against gear echoes). The rewrite's gear sends only changed keys,
  # so a NULL clears it.
  heal_missing_column = 2L
)

# The selection's shape is the kit's (viz_read_selection()); the rules only
# keep or drop it.
norm_comparable <- function(state) {
  state$selection <- !is.null(state$selection)
  state
}

# The table's sort_dir = NULL reads as the reference's explicit default.
norm_sort_resolved <- function(state) {
  if (is.null(state$sort_dir)) {
    state$sort_dir <- if (identical(state$sort_by, "value")) "desc" else "asc"
  }
  state
}

norm_apply_set <- function(spec, prev, args) {
  out <- prev
  for (nm in names(args)) {
    ok <- tryCatch(list(viz_check_value(spec$args[[nm]], args[[nm]], nm)),
                   error = function(e) NULL)
    if (!is.null(ok)) out[nm] <- ok
  }
  out
}

# A call's arguments as the constructor sees them: old names translated,
# and an option default read for an argument the call does not give.
norm_ctor_input <- function(block, args, version) {
  spec <- viz_block_spec(block)
  st <- suppressWarnings(legacy_restore(block, args, version))
  for (nm in names(spec$args)) {
    if (inherits(spec$args[[nm]]$default, "viz_opt") && !nm %in% names(args)) {
      st[nm] <- list(spec_default(spec$args[[nm]]))
    }
  }
  st
}

test_that("chart_normalize() reproduces the recorded chart states", {
  record_register_tbl()
  withr::local_options(blockr.viz.facet_cols = NULL,
                       blockr.viz.default_caption = NULL)
  spec <- viz_block_spec("chart")
  restore <- function(s) suppressWarnings(legacy_restore("chart", s, "0.3.0"))
  n <- 0L
  for (nm in names(record_scenarios())) {
    f <- test_path("fixtures", "chart-record", paste0(nm, ".rds"))
    if (!file.exists(f)) next
    r <- readRDS(f)
    if (!all(c("initial", "steps") %in% names(r))) next
    sc <- record_scenarios()[[nm]]
    got <- chart_normalize(norm_ctor_input("chart", sc$args %||% list(),
                                           "0.3.0"))
    expect_identical(norm_comparable(got), norm_comparable(restore(r$initial)),
                     label = paste(nm, "constructor"))
    prev <- restore(r$steps[[1L]]$state)
    for (i in seq_along(sc$steps)) {
      st <- sc$steps[[i]]
      want <- restore(r$steps[[i + 1L]]$state)
      if (!i %in% NORM_SKIP[[nm]]) {
        input <- if (identical(st$kind, "set")) {
          norm_apply_set(spec, prev, st$args)
        } else {
          want
        }
        expect_identical(norm_comparable(chart_normalize(input, prev)),
                         norm_comparable(want),
                         label = paste(nm, "step", i, st$kind))
        n <- n + 1L
      }
      prev <- want
    }
  }
  expect_gt(n, 80L)
})

test_that("table_normalize() reproduces the recorded table states", {
  vocab <- list(args = TRUE, summary_table = TRUE)
  n <- 0L
  for (nm in names(table_golden_scenarios())) {
    f <- test_path("fixtures", "table-golden", paste0(nm, ".rds"))
    if (!file.exists(f)) next
    r <- readRDS(f)
    if (!all(c("initial", "steps") %in% names(r))) next
    r <- table_golden_translate(r, vocab)
    sc <- table_golden_scenarios()[[nm]]
    withr::local_options(c(list(blockr.viz.default_caption = NULL),
                           sc$options %||% list()))
    restore_old <- function(s) {
      suppressWarnings(legacy_restore("table", s, "0.3.0"))
    }
    restore_new <- function(s) {
      suppressWarnings(legacy_restore("table", s, VIZ_REWRITE_VERSION))
    }
    args <- sc$args %||% list()
    args$.fn <- NULL
    got <- table_normalize(norm_ctor_input("table", args, "0.3.0"))
    want <- restore_new(r$initial)
    expect_identical(norm_sort_resolved(norm_comparable(got)),
                     norm_sort_resolved(norm_comparable(want)),
                     label = paste(nm, "constructor"))
    prev <- want
    for (i in seq_along(r$steps)) {
      want <- restore_new(r$steps[[i]]$state)
      got <- table_normalize(want, prev)
      expect_identical(norm_sort_resolved(norm_comparable(got)),
                       norm_sort_resolved(norm_comparable(want)),
                       label = paste(nm, "step", i))
      prev <- want
      n <- n + 1L
    }
  }
  expect_gt(n, 20L)
})

test_that("an oriented chart keeps orientation only against its type's way", {
  base <- list(type = "bar", category = "cat", value = "num", func = "mean")
  expect_null(chart_normalize(c(base, orientation = "horizontal"))$orientation)
  expect_identical(chart_normalize(c(base, orientation = "vertical"))$orientation,
                   "vertical")
  box <- list(type = "boxplot", category = "cat", value = "num")
  expect_null(chart_normalize(c(box, orientation = "vertical"))$orientation)
  expect_identical(chart_normalize(c(box, orientation = "horizontal"))$orientation,
                   "horizontal")
})

test_that("a type switch keeps the category and the value", {
  prev <- list(type = "bar", category = "cat", value = "num", func = "sum",
               orientation = "vertical")
  got <- chart_normalize(utils::modifyList(prev, list(type = "waterfall")),
                         prev)
  expect_identical(c(got$category, got$value), c("cat", "num"))
  expect_null(got$orientation)
  # A scatter's x and y sit beside them, unused until the type comes back.
  got <- chart_normalize(utils::modifyList(prev, list(type = "scatter",
                                                      x = "a", y = "b")),
                         prev)
  expect_identical(c(got$category, got$value, got$x, got$y),
                   c("cat", "num", "a", "b"))
})

test_that("a histogram has no y, orientation or sort", {
  got <- chart_normalize(list(type = "histogram", x = "num", y = "cat",
                              orientation = "vertical", sort_by = "value",
                              sort_dir = "desc"))
  expect_null(got$y)
  expect_null(got$orientation)
  expect_null(got$sort_by)
  expect_null(got$sort_dir)
})

test_that("interval ends come in pairs", {
  got <- chart_normalize(list(type = "line", ymin = c("a", "b"), ymax = "c"))
  expect_identical(got$ymin, "a")
  expect_identical(got$ymax, "c")
})

test_that("the selection goes with a mapping change or a drill target", {
  sel <- list(type = "categorical", filters = list(cat = "a"))
  prev <- list(type = "bar", category = "cat", drill = "cat", selection = sel)
  expect_identical(chart_normalize(prev, prev)$selection, sel)
  expect_null(chart_normalize(utils::modifyList(prev, list(category = "grp")),
                              prev)$selection)
  expect_null(chart_normalize(utils::modifyList(prev, list(value = "num")),
                              prev)$selection)
  expect_null(chart_normalize(c(prev, drill_target = "f1"), prev)$selection)
  tprev <- list(layout = "groups", by = "grp", drill = "auto",
                summaries = list(table_seed_summary()), selection = sel)
  expect_null(table_normalize(utils::modifyList(tprev, list(by = "cat")),
                              tprev)$selection)
})

test_that("a groups table always has a column", {
  got <- table_normalize(list(layout = "groups", summaries = list()))
  expect_identical(got$summaries, list(table_seed_summary()))
  expect_length(table_normalize(list(layout = "records",
                                     summaries = list()))$summaries, 0L)
})

test_that("a variables table drops summaries on a by column", {
  got <- table_normalize(list(
    layout = "variables", by = "ARM",
    summaries = list(list(type = "dist", col = "AGE"),
                     list(type = "simple", func = "count", color = "ARM"),
                     list(type = "field", col = "ARM"))
  ))
  expect_identical(got$summaries, list(list(type = "dist", col = "AGE")))
})

test_that("data order sorts ascending", {
  prev <- list(layout = "groups", sort_by = "value", sort_dir = "desc")
  got <- table_normalize(utils::modifyList(prev, list(sort_by = "data")), prev)
  expect_null(got$sort_dir)
})

test_that("free text settings are trimmed", {
  got <- chart_normalize(list(type = "bar", unit = " kg ", drill_table = " t "))
  expect_identical(c(got$unit, got$drill_table), c("kg", "t"))
})
