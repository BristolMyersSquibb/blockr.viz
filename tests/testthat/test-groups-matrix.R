# The one-pass build of a faceted summary (R/groups-matrix.R).

matrix_fixture <- function() {
  data.frame(
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B"),
    SUBJ = c("s1", "s1", "s1", "s2", "s2", "s3", "s3", "s4"),
    TERM = c("RASH", "RASH", "COUGH", "RASH", "NAUSEA", "COUGH", "COUGH",
             "RASH"),
    GRADE = factor(c("MILD", "SEVERE", "MILD", "MODERATE", "MILD", "MILD",
                     "MODERATE", NA),
                   levels = c("MILD", "MODERATE", "SEVERE")),
    stringsAsFactors = FALSE
  )
}

matrix_state <- function() {
  list(
    layout = "groups", look = "compact", by = c("ARM", "SUBJ"),
    summaries = list(list(type = "simple", func = "count", show = "tile",
                          name = "Events", facet = "TERM", fill = "GRADE"))
  )
}

test_that("a matrix with empty cells, a group row and a worst level", {
  st <- table_normalize(viz_check(viz_block_spec("table"), matrix_state()))
  m <- table_model_groups(st, matrix_fixture())
  expect_null(m$prep$err)
  v <- table_groups_to_model(m)

  expect_identical(v$n, 6L)
  expect_identical(as.character(v$cols[[1L]]$disp),
                   c("A", "s1", "s2", "B", "s3", "s4"))
  expect_identical(vapply(v$cols[-1L], function(x) x$name, ""),
                   c("RASH", "COUGH", "NAUSEA"))
  expect_identical(v$rows$indent, I(c(0L, 1L, 1L, 0L, 1L, 1L)))
  expect_identical(v$rows$strong, I(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)))

  # The group rows are headers: no text, no paint. An empty cell is empty.
  ramp <- toupper(groups_tile_ramp(3L))
  cell <- function(j) {
    x <- v$cols[[j + 1L]]
    list(disp = as.character(x$disp), key = as.numeric(x$key),
         bg = toupper(as.character(x$bg)))
  }
  expect_identical(cell(1L), list(
    disp = c(NA, "2", "1", NA, NA, "1"),
    key = c(NA, 2, 1, NA, NA, 1),
    # s1: MILD and SEVERE, the worst wins. s4: no grade, the first fill.
    bg = c(NA, ramp[[3L]], ramp[[2L]], NA, NA, ramp[[1L]])
  ))
  expect_identical(cell(2L), list(
    disp = c(NA, "1", NA, NA, "2", NA),
    key = c(NA, 1, NA, NA, 2, NA),
    bg = c(NA, ramp[[1L]], NA, NA, ramp[[2L]], NA)
  ))
  expect_identical(cell(3L), list(
    disp = c(NA, NA, "1", NA, NA, NA),
    key = c(NA, NA, 1, NA, NA, NA),
    bg = c(NA, NA, ramp[[1L]], NA, NA, NA)
  ))
  # The text colour travels once per fill.
  expect_null(v$cols[[2L]]$fg)
  expect_setequal(toupper(names(v$cols[[2L]]$ink)), ramp)
  expect_identical(v$cols[[2L]]$mark$hover$den, 4L)
})

# The same summaries built copy by copy.
per_copy <- function(code) {
  local_mocked_bindings(groups_matrix_ok = function(s, build) FALSE)
  code
}

matrix_cases <- function() {
  s <- function(...) {
    list(list(type = "simple", name = "V", facet = "TERM", ...))
  }
  d <- matrix_fixture()
  d$N <- c(1.5, NA, 3, 4, 5, NA, 7, 8)
  d$TERM_F <- factor(d$TERM, levels = c("NAUSEA", "RASH", "COUGH", "NONE"))
  blank <- d
  blank$SUBJ[blank$SUBJ == "s2"] <- ""
  two <- list(
    list(type = "simple", name = "Events", func = "count", show = "bar",
         facet = "ARM"),
    list(type = "simple", name = "Subjects", func = "count_distinct",
         col = "SUBJ", show = "dot", facet = "TERM")
  )
  list(
    tile = list(d, c("ARM", "SUBJ"), s(func = "count", show = "tile",
                                       fill = "GRADE")),
    tile_value = list(d, "SUBJ", s(func = "count", show = "tile")),
    tile_blank = list(blank, "SUBJ", s(func = "count", show = "tile",
                                       fill = "GRADE")),
    bar = list(d, "ARM", s(func = "count", show = "bar")),
    distinct = list(d, "ARM", s(func = "count_distinct", col = "SUBJ",
                                show = "number")),
    total = list(d, "ARM", s(func = "count", show = "bar"),
                 pct_of = "total"),
    subject = list(d, c("ARM", "SUBJ"), s(func = "count", show = "bar"),
                   subject = "SUBJ"),
    mean = list(d, c("ARM", "SUBJ"), s(func = "mean", col = "N",
                                       show = "bar")),
    sum = list(d, "ARM", s(func = "sum", col = "N", show = "number")),
    first = list(d, "SUBJ", s(func = "identity", col = "N", show = "number")),
    no_by = list(d, NULL, s(func = "count", show = "bar")),
    no_by_total = list(d, NULL, s(func = "count", show = "bar"),
                       pct_of = "total"),
    distinct_total = list(d, "ARM", s(func = "count_distinct", col = "SUBJ",
                                      show = "bar"), pct_of = "total"),
    factor = list(d, "ARM", list(list(type = "simple", name = "V",
                                      func = "count", show = "tile",
                                      facet = "TERM_F", fill = "GRADE"))),
    two = list(d, "SUBJ", two),
    summary_rows = list(d, c("ARM", "SUBJ"), s(func = "count", show = "bar"),
                        group_rows = "summary")
  )
}

test_that("one pass builds what the per-copy build does", {
  for (nm in names(matrix_cases())) {
    cs <- matrix_cases()[[nm]]
    args <- c(list(cs[[1L]], by = cs[[2L]], summaries = cs[[3L]]), cs[-(1:3)])
    fast <- do.call(groups_prepare, args)
    slow <- per_copy(do.call(groups_prepare, args))
    expect_null(fast$err, info = nm)
    expect_identical(fast, slow, info = nm)
    expect_identical(groups_cells(fast), groups_cells(slow), info = nm)
  }
})

test_that("a facet column with missing values is built per copy", {
  d <- matrix_fixture()
  d$TERM[[2L]] <- NA
  s <- list(type = "simple", func = "count", show = "tile", facet = "TERM",
            .facet = "TERM", .flevels = c("COUGH", "NAUSEA", "RASH"))
  build <- list(data = d)
  expect_false(groups_matrix_ok(s, build))
  build$data <- matrix_fixture()
  expect_true(groups_matrix_ok(s, build))
  expect_false(groups_matrix_ok(c(s, list(.color = "ARM")), build))
})

test_that("tile columns painted together equal tile columns painted alone", {
  cases <- matrix_cases()[c("tile", "tile_value", "tile_blank", "factor")]
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    prep <- groups_prepare(cs[[1L]], by = cs[[2L]], summaries = cs[[3L]])
    ctx <- list(rows = prep$rows, n = nrow(prep$rows), prep = prep,
                mx = prep$bar_max, mn = prep$bar_min %||% 0)
    together <- cells_tiles(ctx, prep$plan)
    expect_false(any(vapply(together, is.null, logical(1L))), info = nm)
    alone <- lapply(prep$plan, function(p) cells_tile(ctx, p))
    expect_identical(together, alone, info = nm)
  }
})
