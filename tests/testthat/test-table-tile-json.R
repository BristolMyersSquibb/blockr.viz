# A matrix's tile columns as JSON written by R/table-tile-json.R: the text
# has to be what Shiny writes for the same model.

tj_model <- function(d, by = c("ARM", "SUBJ"), ..., value_labels = TRUE,
                     drill = "auto") {
  st <- list(
    layout = "groups", look = "compact", by = by, drill = drill,
    value_labels = value_labels,
    summaries = list(list(type = "simple", func = "count", show = "tile",
                          name = "Events", facet = "TERM", ...))
  )
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  table_groups_to_model(table_model_groups(st, d))
}

tj_data <- function(n = 60L, terms = 12L) {
  set.seed(4)
  data.frame(
    ARM = sample(c("A", "B"), n, TRUE),
    SUBJ = sprintf("s%02d", sample(20L, n, TRUE)),
    TERM = sprintf("T%02d", sample(terms, n, TRUE)),
    GRADE = factor(sample(c("MILD", "MODERATE", "SEVERE", NA), n, TRUE),
                   levels = c("MILD", "MODERATE", "SEVERE")),
    stringsAsFactors = FALSE
  )
}

expect_same_json <- function(model, info = NULL) {
  expect_identical(as.character(shiny_json(table_tile_json(model))),
                   as.character(shiny_json(model)), info = info)
}

test_that("tile columns are written as Shiny writes them", {
  d <- tj_data()
  models <- list(
    worst = tj_model(d, fill = "GRADE"),
    value = tj_model(d),
    flat = tj_model(d, by = "SUBJ", fill = "GRADE"),
    no_labels = tj_model(d, fill = "GRADE", value_labels = FALSE),
    no_drill = tj_model(d, fill = "GRADE", drill = NULL),
    # Dense columns travel whole, sparse ones by their filled rows.
    dense = tj_model(tj_data(400L, 3L), by = "SUBJ", fill = "GRADE"),
    one_row = tj_model(d[1L, ], fill = "GRADE")
  )
  for (nm in names(models)) {
    m <- models[[nm]]
    out <- table_tile_json(m)
    expect_s3_class(out$cols[[2L]], "json")
    expect_type(out$cols[[1L]], "list")
    expect_same_json(m, info = nm)
  }
  expect_s3_class(table_tile_json(models$worst)$rows$keys, "json")
})

test_that("names and levels are escaped as jsonlite escapes them", {
  d <- tj_data()
  odd <- c("a \"quoted\", term", "back\\slash", "</script>", "tab\there",
           "line\nbreak", "café   中", "{\"x\":[1]}", "'")
  d$TERM <- odd[match(d$TERM, unique(d$TERM)) %% length(odd) + 1L]
  d$SUBJ <- paste0(d$SUBJ, "\",\"")
  names(d)[names(d) == "TERM"] <- "A \"term\""
  st <- list(
    layout = "groups", look = "compact", by = c("ARM", "SUBJ"),
    drill = "auto",
    summaries = list(list(type = "simple", func = "count", show = "tile",
                          name = "Ev\"ents", facet = "A \"term\"",
                          fill = "GRADE"))
  )
  st <- table_normalize(viz_check(viz_block_spec("table"), st))
  m <- table_groups_to_model(table_model_groups(st, d))
  expect_s3_class(table_tile_json(m)$cols[[2L]], "json")
  expect_same_json(m)
})

test_that("tokens are jsonlite's own, element by element", {
  x <- c("a\",\"b", "", NA, "\\", "é")
  expect_identical(tile_json_tokens(x),
                   c("\"a\\\",\\\"b\"", "\"\"", "null", "\"\\\\\"",
                     "\"é\""))
  expect_identical(tile_json_tokens(c(1, 2.5, NA, 1 / 3, 1e6, 123456.7891)),
                   c("1", "2.5", "null", "0.3333333333333333", "1000000",
                     "123456.7891"))
  expect_identical(tile_json_tokens(c(0L, NA, 12L)), c("0", "null", "12"))
  expect_identical(tile_json_tokens(character()), character())
})

test_that("a column of another shape is left to Shiny", {
  m <- tj_model(tj_data(), fill = "GRADE")
  keep <- function(change) {
    x <- m
    x$cols[[3L]] <- change(x$cols[[3L]])
    out <- table_tile_json(x)
    expect_type(out$cols[[3L]], "list")
    expect_s3_class(out$cols[[2L]], "json")
    expect_same_json(x)
  }
  keep(function(cc) c(cc, list(axis = list(at = I(c(0, 50))))))
  keep(function(cc) {
    cc$disp <- unclass(cc$disp)
    cc
  })
  keep(function(cc) {
    cc$mark$hover$words <- list(center = "Mean")
    cc
  })
  keep(function(cc) {
    names(cc$ink)[[1L]] <- ""
    cc
  })
  keep(function(cc) {
    cc$name <- NA_character_
    cc
  })
})

test_that("a table without tiles is not touched", {
  st <- table_normalize(viz_check(viz_block_spec("table"), list(
    layout = "groups", by = "ARM",
    summaries = list(list(type = "simple", func = "count", show = "bar",
                          name = "N"))
  )))
  m <- table_groups_to_model(table_model_groups(st, tj_data()))
  expect_identical(table_tile_json(m), m)
  expect_identical(table_tile_json(list()), list())
})

test_that("header rows are written as Shiny writes them", {
  m <- tj_model(tj_data(), fill = "GRADE")
  expect_s3_class(table_tile_json(m)$head[[1L]], "json")
  # Facet levels over two summaries: the spanners carry a claim and stay
  # lists, the cells around them are written.
  st <- table_normalize(viz_check(viz_block_spec("table"), list(
    layout = "groups", look = "compact", by = "SUBJ", drill = "auto",
    facet_layout = "by_level",
    summaries = list(
      list(type = "simple", func = "count", show = "tile", name = "Events",
           facet = "ARM"),
      list(type = "simple", func = "count_distinct", col = "TERM",
           show = "tile", name = "Terms", facet = "ARM")
    )
  )))
  m <- table_groups_to_model(table_model_groups(st, tj_data()))
  expect_length(m$head, 2L)
  out <- table_tile_json(m)
  expect_type(out$head[[1L]], "list")
  expect_s3_class(out$head[[1L]][[1L]], "json")
  expect_s3_class(out$head[[2L]], "json")
  expect_same_json(m)
})
