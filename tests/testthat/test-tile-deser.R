# The tile block is gone (a tile is a table block in the cards look). A board
# saved with tiles restores them as table blocks: fixtures/tile-board-main.json
# was saved by blockr.viz 0.2.223 (main before the removal) with four tiles.

tile_board_blocks <- function() {
  ser <- blockr.core:::read_json(test_path("fixtures", "tile-board-main.json"))
  ser$payload$blocks$payload
}

# The method directly: a blockr.core without BristolMyersSquibb/blockr.core#379
# refuses a block whose class changed on restore (the whole-board test below
# covers the dispatch where it can).
restore_tile <- function(id) {
  blockr_deser.tile_block(structure(list(), class = "tile_block"),
                          tile_board_blocks()[[id]])
}

payload_of <- function(b) blockr.core::blockr_ser(b)$payload

test_that("a grouped tile with summaries restores as grouped cards", {
  expect_no_warning(b <- restore_tile("by_region"))
  expect_s3_class(b, "table_block")
  p <- payload_of(b)
  expect_identical(p$layout, "groups")
  expect_identical(p$look, "cards")
  expect_identical(p$by, "region")
  expect_identical(vapply(p$summaries, `[[`, "", "func"), c("sum", "count"))
  expect_identical(p$summaries[[1L]]$col, "revenue")
  # The drill and the saved click carry over.
  expect_identical(p$drill, "region")
  expect_identical(names(p$selection$filters), "region")
  expect_identical(unlist(p$selection$filters$region), "North")
  expect_identical(p$block_name, "Revenue by region")

  d <- data.frame(region = c("North", "North", "South"),
                  revenue = c(1, 2, 3))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(session$returned$state$look(), "cards")
      expect_identical(names(session$returned$state$selection()$filters),
                       "region")
    },
    args = list(x = b, data = list(data = shiny::reactive(d)))
  )
})

test_that("a tile with summaries and no group restores as one row of cards", {
  expect_no_warning(b <- restore_tile("totals"))
  p <- payload_of(b)
  expect_identical(p$layout, "groups")
  expect_identical(p$look, "cards")
  expect_length(p$by, 0L)
  expect_identical(p$summaries[[1L]]$func, "mean")
})

test_that("a long tile frame restores as a plain records table, with a warning", {
  expect_warning(b <- restore_tile("kpis"), "long tile frame")
  p <- payload_of(b)
  expect_identical(p$layout, "records")
  expect_identical(p$look, "table")
  expect_identical(p$rowname, "metric")
  expect_identical(p$columns, c("value", "target"))
  expect_identical(p$block_name, "KPIs")
})

test_that("a wide tile restores as records cards; its unit is dropped", {
  expect_warning(b <- restore_tile("wide"), "unit")
  p <- payload_of(b)
  expect_identical(p$layout, "records")
  expect_identical(p$look, "cards")
  expect_identical(p$columns, c("revenue", "orders"))
})

test_that("the whole saved board restores", {
  ser <- blockr.core:::read_json(test_path("fixtures", "tile-board-main.json"))
  board <- tryCatch(suppressWarnings(blockr.core::blockr_deser(ser)),
                    block_deser_class_error = function(e) NULL)
  skip_if(is.null(board), paste(
    "this blockr.core refuses a class change on restore",
    "(BristolMyersSquibb/blockr.core#379)"
  ))
  cls <- vapply(blockr.core::board_blocks(board), function(b) class(b)[1L],
                character(1L))
  expect_true(all(cls == "table_block"))
  expect_length(cls, 4L)
})
