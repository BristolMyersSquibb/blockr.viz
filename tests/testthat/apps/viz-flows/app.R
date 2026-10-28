# The flows a reader of a clinical board goes through, on one dock board, for
# test-e2e-flows.R: a chart coloured by treatment arm whose click drills to a
# drill filter block, and charts to work the gear, the download menu and the
# title word menu on.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set.
# With BLOCKR_FLOWS_RESTORE set to a saved board (JSON), serves that board
# instead. Test mode exports every block's result and the board as the Save
# button would write it (`saved`).

src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)
library(blockr.core)
library(blockr.dock)

options(blockr.dock_is_locked = FALSE)

registerS3method(
  "blockr_test_exports", "flows_test_board",
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
        class(brd) <- setdiff(class(brd), "flows_test_board")
        blockr.core:::write_json(
          serialize_board(brd, rvb$blocks, rvb$board_id)
        )
      }
    )
  },
  envir = asNamespace("blockr.core")
)

# The arm names of the CDISC pilot study, plus Screen Failure, which the
# scale map below does not pin.
flows_arms <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose",
                "Screen Failure")

flows_adsl <- local({
  n <- 32
  i <- seq_len(n)
  data.frame(
    USUBJID = sprintf("S-%03d", i),
    ARM = flows_arms[(i - 1L) %% 4L + 1L],
    SEX = ifelse(i %% 2 == 0, "F", "M"),
    AGE = 50 + (i * 7) %% 30,
    WEIGHT = 55 + (i * 11) %% 40,
    stringsAsFactors = FALSE
  )
})

flows_arm_colours <- c(
  "Placebo" = "#999999",
  "Xanomeline Low Dose" = "#56B4E9",
  "Xanomeline High Dose" = "#0072B2"
)

build_board <- function() {
  new_dock_board(
    blocks = c(
      adsl = new_static_block(flows_adsl, block_name = "ADSL"),
      # One point per subject, coloured by arm; a click sends the subject to
      # the drill filter.
      arms = new_chart_block(
        "scatter", x = "AGE", y = "WEIGHT", color = "ARM",
        drill = "USUBJID", drill_target = "dfilt",
        title = "Weight against age", block_name = "Arms"
      ),
      # A second chart coloured by arm, for the unpinned level's colour.
      arms_bar = new_chart_block(
        "bar", category = "SEX", value = "AGE", func = "count", color = "ARM",
        title = "Subjects by sex", block_name = "Arms bar"
      ),
      dfilt = blockr.dm::new_drill_filter_block(block_name = "Drill filter"),
      ctrl_out = new_table_block(rowname = "USUBJID",
                                 block_name = "Drill filter out"),
      # The download menu.
      dl = new_chart_block(
        "bar", category = "ARM", value = "AGE", func = "mean",
        title = "Mean age by arm", block_name = "Downloads"
      ),
      # The gear.
      gear = new_chart_block(
        "bar", category = "ARM", value = "AGE", func = "mean",
        title = "Gear chart", block_name = "Gear"
      ),
      # The title word menu.
      words = new_chart_block(
        "bar", category = "ARM", value = "AGE", func = "mean",
        subtitle = "{label(@value)} by {label(@category)}",
        block_name = "Words"
      )
    ),
    links = links(
      from = c("adsl", "adsl", "dfilt", "adsl", "adsl", "adsl", "adsl"),
      to = c("arms", "dfilt", "ctrl_out", "dl", "gear", "words", "arms_bar")
    ),
    views = list(
      drill = dock_view(c("arms", "arms_bar", "ctrl_out"), name = "drill"),
      tools = dock_view(c("dl", "gear", "words"), name = "tools")
    ),
    grids = list(
      drill = dock_grid(blk("arms"), blk("arms_bar"), blk("ctrl_out")),
      tools = dock_grid(blk("dl"), blk("gear"), blk("words"))
    ),
    extensions = list(ctrl_bridge = new_ctrl_bridge_extension()),
    active = "drill",
    # The arms pinned, as a study's scale map does. A level not pinned takes
    # a colour by a hash of its name, the same in every chart.
    options = c(
      dock_board_options(),
      new_board_options(
        blockr.theme::new_scale_map_option(
          blockr.theme::new_scale_map(blockr.theme::scale_binding(
            "ARM",
            color = flows_arm_colours
          ))
        )
      )
    )
  )
}

restore <- Sys.getenv("BLOCKR_FLOWS_RESTORE")
board <- if (nzchar(restore)) {
  blockr_deser(blockr.core:::read_json(restore))
} else {
  build_board()
}
class(board) <- unique(c("flows_test_board", class(board)))

serve(board)
