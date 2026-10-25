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
  p <- lane_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
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

test_that("an empty count is an empty cell, a painted one shows its value", {
  p <- lane_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary(fill = "GRADE"))
  m <- summarize_cells(p)
  html <- summarize_cells_html(m)
  expect_match(html, "blockr-summarize-tile-col", fixed = TRUE)
  expect_match(html, "class=\"blockr-summarize-tile\" style=\"background:",
               fixed = TRUE)
  cough <- m$cols[[which(vapply(p$plan, function(x) x$label, "") ==
                           "COUGH")]]
  s1 <- which(p$rows$.label == "s1")
  expect_true(is.na(cough$bg[[s1]]))
  expect_identical(cough$disp[[s1]], "")
})

test_that("without a paint column the tile is painted by its value", {
  p <- lane_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary())
  m <- summarize_cells(p)
  rash <- m$cols[[1L]]
  expect_false(all(is.na(rash$bg)))
  expect_true(is.null(p$plan[[1L]]$wkey))
})

test_that("a tile facet takes many levels, other marks keep the cap", {
  d <- data.frame(g = "a", t = sprintf("T%02d", 1:40))
  ok <- lane_prepare_summaries(d, by = "g", summaries = list(list(
    type = "simple", func = "count", show = "tile", facet = "t"
  )))
  expect_null(ok$err)
  expect_length(ok$plan, 40L)
  bad <- lane_prepare_summaries(d, by = "g", summaries = list(list(
    type = "simple", func = "count", show = "bar", facet = "t"
  )))
  expect_match(bad$err, "at most 15", fixed = TRUE)
})

test_that("a tile matrix sorts its rows by their total", {
  # Every subject has two events; one more makes s3 the burden.
  d <- rbind(tile_fixture(), data.frame(ARM = "B", SUBJ = "s3",
                                        TERM = "COUGH", GRADE = "MILD"))
  p <- lane_prepare_summaries(d, by = "SUBJ", summaries = tile_summary(),
                              sort_by = "value", sort_dir = "desc")
  expect_identical(p$rows$.label[[1L]], "s3")
})

test_that("compact marks the table and the export carries the paint", {
  p <- lane_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary(fill = "GRADE"))
  expect_match(summarize_table_attrs(p, list(compact = TRUE)),
               "data-summarize-compact=\"1\"", fixed = TRUE)
  expect_false(grepl("data-summarize-compact",
                     summarize_table_attrs(p, list(compact = FALSE))))
  df <- summarize_export_df(p)
  expect_true(all(c("RASH", ".bg:RASH", ".fg:RASH") %in% names(df)))
})

test_that("the block's look is shared by both engines", {
  b <- new_table_block(layout = "groups", by = "SUBJ", look = "compact",
                       summaries = tile_summary(fill = "GRADE"))
  shiny::testServer(b$expr_server,
                    args = list(data = shiny::reactive(tile_fixture())), {
    expect_identical(session$returned$state$look(), "compact")
    session$setInputs(`groups-summarize_table_block_action` = list(
      action = "config", param = "look", value = "table"
    ))
    expect_identical(session$returned$state$look(), "table")
    # The records engine's rotated titles are the compact look.
    session$setInputs(`records-drilldown_table_block_action` = list(
      action = "config", param = "rotate_titles", value = "on"
    ))
    expect_identical(session$returned$state$look(), "compact")
  })
})

test_that("group rows of a tile matrix are headers without tiles", {
  p <- lane_prepare_summaries(tile_fixture(), by = c("ARM", "SUBJ"),
                              summaries = tile_summary(fill = "GRADE"))
  m <- summarize_cells(p)
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
  expect_true(dt_is_structured(out))
  expect_identical(table_nest(d, by = character()), d)
})

test_that("the records layout nests by `by`", {
  d <- data.frame(species = c("a", "a", "b"), island = c("x", "y", "x"),
                  n = 1:3)
  b <- new_table_block(by = "species", rowname = "island")
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(d)), {
    ex <- paste(deparse(session$returned$expr()), collapse = " ")
    expect_match(ex, "blockr.viz::table_nest", fixed = TRUE)
  })
})
