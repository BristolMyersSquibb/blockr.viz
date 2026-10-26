# Every drill path of the table block on one dock board, each source with a
# listing downstream, for test-table-drill.R. One view per source.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set.
# With BLOCKR_DRILL_RESTORE set to a saved board (JSON), serves that board
# instead: the save/restore check.
#
# Test mode exports every block's result (as blockr.core's plain board does)
# and the board as the Save button would write it (`saved`).

src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)
library(blockr.core)
library(blockr.dock)

options(blockr.dock_is_locked = FALSE)

source("data.R", local = TRUE)

registerS3method(
  "blockr_test_exports", "drill_test_board",
  function(x, rv, ...) {
    rvb <- rv[[1L]]
    shiny::exportTestValues(
      result = {
        blocks <- rvb$blocks
        ids <- intersect(board_block_ids(rvb$board), names(blocks))
        lapply(blocks[ids], function(b) {
          tryCatch(b$server$result(), error = function(e) NULL)
        })
      },
      saved = {
        brd <- rvb$board
        class(brd) <- setdiff(class(brd), "drill_test_board")
        blockr.core:::write_json(
          serialize_board(brd, rvb$blocks, rvb$board_id)
        )
      }
    )
  },
  envir = asNamespace("blockr.core")
)

listing <- function(name) {
  new_table_block(rowname = "USUBJID", block_name = name)
}

# One view per drill source, source left, listing right.
pairs <- lapply(
  c(rec = "rec", nest = "nest", rcards = "rcards", grp = "grp",
    grpm = "grpm", ae = "ae", col = "col", heat = "heat", cards = "cards",
    t1 = "t1"),
  function(id) c(id, paste0(id, "_out"))
)

build_board <- function() {
  new_dock_board(
    blocks = c(
      adsl = new_static_block(drill_adsl, block_name = "ADSL"),
      adae = new_static_block(drill_adae, block_name = "ADAE"),
      arms = new_static_block(drill_arm_frame, block_name = "Arms"),

      # records: a click on a row filters on its SEX
      rec = new_table_block(rowname = "USUBJID", drill = "SEX",
                            block_name = "Records"),
      rec_out = listing("Records out"),
      # records under ARM headers
      nest = new_table_block(by = "ARM", rowname = "USUBJID", drill = "SEX",
                             block_name = "Nested"),
      nest_out = listing("Nested out"),
      # records as cards, from a precomputed frame
      rcards = new_table_block(rowname = "Arm", look = "cards", drill = "Arm",
                               block_name = "Records cards"),
      rcards_out = new_table_block(rowname = "Arm",
                                   block_name = "Records cards out"),

      # groups: one level
      grp = new_table_block(
        layout = "groups", by = "ARM", drill = "ARM",
        summaries = list(list(type = "simple", func = "count", show = "bar",
                              name = "Subjects")),
        block_name = "Groups"
      ),
      grp_out = listing("Groups out"),
      # groups on a column with missing values: "(Missing)" row
      grpm = new_table_block(
        layout = "groups", by = "SEX", drill = "auto",
        summaries = list(list(type = "simple", func = "count", show = "bar",
                              name = "Subjects")),
        block_name = "Groups missing"
      ),
      grpm_out = listing("Groups missing out"),
      # groups: two levels, facet columns
      ae = new_table_block(
        layout = "groups", by = c("AESOC", "AEDECOD"), id = "USUBJID",
        drill = "auto",
        summaries = list(list(type = "simple", func = "count_distinct",
                              col = "USUBJID", show = "bar", facet = "ARM",
                              name = "Subjects")),
        block_name = "AE summary"
      ),
      ae_out = listing("AE out"),
      # groups: colour split
      col = new_table_block(
        layout = "groups", by = "ARM", drill = "auto",
        summaries = list(list(type = "simple", func = "count", show = "bar",
                              color = "SEX", name = "Sex")),
        block_name = "Colour split"
      ),
      col_out = listing("Colour out"),
      # groups: tile heatmap
      heat = new_table_block(
        layout = "groups", by = c("ARM", "USUBJID"), drill = "auto",
        summaries = list(list(type = "simple", func = "count", show = "tile",
                              facet = "AEDECOD", fill = "AESEV",
                              name = "AEs")),
        look = "compact", block_name = "Heatmap"
      ),
      heat_out = listing("Heatmap out"),
      # groups as cards
      cards = new_table_block(
        layout = "groups", by = "ARM", look = "cards", drill = "ARM",
        summaries = list(
          list(type = "simple", func = "count_distinct", col = "USUBJID",
               show = "bar", name = "Subjects"),
          list(type = "dist", col = "AGE", show = "box", name = "Age")
        ),
        block_name = "Cards"
      ),
      cards_out = listing("Cards out"),

      # variables (Table 1), resolved to the source rows
      t1 = new_table_block(
        layout = "variables", by = "ARM", id = "USUBJID", drill = "source",
        summaries = list(
          list(type = "dist", col = "AGE", show = "box"),
          list(type = "simple", func = "count", show = "bar", color = "SEX",
               name = "SEX")
        ),
        block_name = "Table 1"
      ),
      t1_out = listing("Table 1 out"),

      # drill_target: the click is sent to a drill filter block over the
      # control channel, not latched. The filter is in no view.
      dfilt = blockr.dm::new_drill_filter_block(block_name = "Drill filter"),
      ctrl_out = listing("Drill filter out"),
      tgrp = new_table_block(
        layout = "groups", by = "ARM", drill = "ARM", drill_target = "dfilt",
        summaries = list(list(type = "simple", func = "count", show = "bar",
                              name = "Subjects")),
        block_name = "Groups to filter"
      ),
      trec = new_table_block(rowname = "USUBJID", drill = "SEX",
                             drill_target = "dfilt",
                             block_name = "Records to filter")
    ),
    links = links(
      from = c("adsl", "rec", "adsl", "nest", "arms", "rcards",
               "adsl", "grp", "adsl", "grpm", "adae", "ae", "adsl", "col",
               "adae", "heat", "adsl", "cards", "adsl", "t1",
               "adsl", "dfilt", "adsl", "adsl"),
      to = c("rec", "rec_out", "nest", "nest_out", "rcards", "rcards_out",
             "grp", "grp_out", "grpm", "grpm_out", "ae", "ae_out", "col",
             "col_out", "heat", "heat_out", "cards", "cards_out", "t1",
             "t1_out", "dfilt", "ctrl_out", "tgrp", "trec")
    ),
    views = c(
      lapply(pairs, function(p) dock_view(p, name = p[[1L]])),
      list(ctrl = dock_view(c("tgrp", "trec", "ctrl_out"), name = "ctrl"))
    ),
    grids = c(
      lapply(pairs, function(p) dock_grid(blk(p[[1L]]), blk(p[[2L]]))),
      list(ctrl = dock_grid(blk("tgrp"), blk("trec"), blk("ctrl_out")))
    ),
    extensions = list(ctrl_bridge = new_ctrl_bridge_extension()),
    active = "rec",
    options = dock_board_options()
  )
}

restore <- Sys.getenv("BLOCKR_DRILL_RESTORE")
board <- if (nzchar(restore)) {
  blockr_deser(blockr.core:::read_json(restore))
} else {
  build_board()
}
class(board) <- unique(c("drill_test_board", class(board)))

serve(board)
