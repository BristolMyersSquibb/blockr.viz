# The tile summary and the compact look: the table block's heatmap
# (dev/table-block-rewrite.md, phase 4).

tile_fixture <- function() {
  data.frame(
    ARM = c("A", "A", "A", "A", "B", "B"),
    SUBJ = c("s1", "s1", "s2", "s2", "s3", "s3"),
    TERM = c("RASH", "RASH", "COUGH", "RASH", "RASH", "NAUSEA"),
    GRADE = factor(c("MILD", "SEVERE", "MODERATE", "MILD", "MILD", "MILD"),
                   levels = c("MILD", "MODERATE", "SEVERE")),
    stringsAsFactors = FALSE
  )
}

tile_summary <- function(...) {
  list(c(list(type = "simple", func = "count", show = "tile",
              facet = "TERM"), list(...)))
}

test_that("tile columns order by count and carry the worst level", {
  p <- groups_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary(fill = "GRADE"))
  expect_null(p$err)
  labels <- vapply(p$plan, function(x) x$label, "")
  expect_identical(labels[[1L]], "RASH")  # four rows, the commonest
  rash <- p$plan[[1L]]
  expect_identical(rash$kind, "tile")
  s1 <- p$rows[p$rows$.label == "s1", ]
  expect_identical(s1[[rash$key]], 2)
  expect_identical(s1[[rash$wkey]], 3L)  # SEVERE
})

test_that("without a paint column the tile is painted by its value", {
  p <- groups_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary())
  m <- groups_cells(p)
  rash <- m$cols[[1L]]
  expect_false(all(is.na(rash$bg)))
  expect_true(is.null(p$plan[[1L]]$wkey))
})

test_that("a tile facet takes many levels, other marks keep the cap", {
  d <- data.frame(g = "a", t = sprintf("T%02d", 1:40))
  ok <- groups_prepare_summaries(d, by = "g", summaries = list(list(
    type = "simple", func = "count", show = "tile", facet = "t"
  )))
  expect_null(ok$err)
  expect_length(ok$plan, 40L)
  bad <- groups_prepare_summaries(d, by = "g", summaries = list(list(
    type = "simple", func = "count", show = "bar", facet = "t"
  )))
  expect_match(bad$err, "at most 15", fixed = TRUE)
})

test_that("a tile matrix sorts its rows by their total", {
  # Every subject has two events; one more makes s3 the burden.
  d <- rbind(tile_fixture(), data.frame(ARM = "B", SUBJ = "s3",
                                        TERM = "COUGH", GRADE = "MILD"))
  p <- groups_prepare_summaries(d, by = "SUBJ", summaries = tile_summary(),
                              sort_by = "value", sort_dir = "desc")
  expect_identical(p$rows$.label[[1L]], "s3")
})

test_that("the export carries the tile paint", {
  p <- groups_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary(fill = "GRADE"))
  df <- summarize_export_df(p)
  expect_true(all(c("RASH", ".bg:RASH", ".fg:RASH") %in% names(df)))
})

test_that("the block's look is one setting for every layout", {
  b <- new_table_block(layout = "groups", by = "SUBJ", look = "compact",
                       summaries = tile_summary(fill = "GRADE"))
  set <- function(values) {
    viz_read_set(viz_block_spec("table"), list(rev = 0L, values = values))
  }
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expr <- session$makeScope("expr")
      expect_identical(session$returned$state$look(), "compact")
      expr$setInputs(table_set = set(list(look = "table")))
      expect_identical(session$returned$state$look(), "table")
      # The records layout reads the same setting.
      expr$setInputs(table_set = set(list(look = "compact", layout = "records")))
      expect_identical(session$returned$state$layout(), "records")
      expect_identical(session$returned$state$look(), "compact")
    },
    args = list(x = b, data = list(data = shiny::reactive(tile_fixture())))
  )
})

test_that("group rows of a tile matrix are headers without tiles", {
  p <- groups_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary(fill = "GRADE"))
  m <- groups_cells(p)
  grp <- p$rows$.level == 0L
  expect_true(any(grp))
  for (cc in m$cols) expect_true(all(is.na(cc$bg[grp])))
})

test_that("table_nest() puts a tidy frame under its groups", {
  d <- data.frame(species = c("b", "a", "b"), island = c("x", "y", "z"),
                  n = 1:3)
  out <- table_nest(d, by = "species", rowname = "island")
  expect_identical(out$.group1_level, c("b", "b", "a"))
  expect_identical(out$.label, c("x", "z", "y"))
  expect_identical(names(out), c(".group1", ".group1_level", ".label", "n"))
  expect_true(table_is_structured(out))
  expect_identical(table_nest(d, by = character()), d)
})

test_that("the records layout nests by `by` and outputs the input rows", {
  d <- data.frame(species = c("a", "a", "b"), island = c("x", "y", "x"),
                  n = 1:3)
  b <- new_table_block(by = "species", rowname = "island")
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(d)), {
    # The nesting is display; the code passes the rows on.
    ex <- session$returned$expr()
    expect_no_match(paste(deparse(ex), collapse = " "), "table_nest",
                    fixed = TRUE)
    expect_identical(eval(ex, list(data = d)), d)
  })
  b <- new_table_block(by = "species", rowname = "island",
                       selection = list(group_cols = ".group1_level",
                                        group_vals = "a"))
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(d)), {
    ex <- session$returned$expr()
    expect_identical(eval(ex, list(data = d)), d[d$species == "a", ])
  })
})

test_that("a compact matrix's stub header is the row column's label", {
  d <- tile_fixture()
  attr(d$ARM, "label") <- "Treatment arm"
  attr(d$SUBJ, "label") <- "Subject"
  state <- list(layout = "groups", by = c("ARM", "SUBJ"), look = "compact",
                summaries = tile_summary(fill = "GRADE"))
  stub <- function(st) {
    table_groups_to_model(table_model_groups(st, d))$head[[1L]][[1L]]
  }
  h <- stub(state)
  expect_identical(h$text, "Subject")
  expect_identical(h$tip, "SUBJ")
  expect_null(h$sub)
  # The table look keeps both columns' names over their labels.
  h <- stub(utils::modifyList(state, list(look = "table")))
  expect_identical(h$text, "ARM / SUBJ")
  expect_identical(h$sub, "Treatment arm / Subject")
})
