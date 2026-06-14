# Scale map across renderers — chart + table + tile, one color language.
#
# The board scale map is a board-level *semantic* mapping: "F is always this
# teal, M always this orange", true wherever the SEX variable appears. Today
# only the chart honors it (via the `color`/`group` role). This board is the
# scenario — and the acceptance harness — for extending that to the table and
# the tile, SUBTLY (a swatch / accent, never a full cell tint), so the three
# views read as one.
#
# All three color by the SAME variable (SEX) and must resolve to the SAME hex:
#   - chart : SEX on the `color` role  -> series colors            (works today)
#   - tile  : grouped `by = "SEX"`     -> swatch on the group label (PENDING)
#   - table : SEX is the row-stub      -> swatch on the stub levels (PENDING)
#
# KEY constraint surfaced by this demo: the table already owns a color channel.
# `new_table_block(cell_color = ...)` is a NUMERIC value->background scale
# (sequential / diverging heatmap) on the BODY cells. The scale-map swatch is a
# CATEGORICAL channel and must stay off that axis — it rides the row-stub levels
# (or, in a wide table, the level-headers), never the numeric body. Two color
# languages, different parts of the table, no collision. This board runs both at
# once on purpose so the separation stays honest.
#
# What you should see once the feature lands: glance at the chart legend
# (F = teal), then the tile group headers and the table's SEX rows — the same
# teal dot ties all three together, while the numeric cells keep their heatmap.
# The map OWNS the hex; each block only chooses whether/where to surface it.
# Drop the scale_map option (or uninstall blockr.theme) and every view falls
# back to standard rendering.
#
# Run from workspace root:
#   Rscript blockr.bi/dev/scale-map-table-tile-demo.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)
options(blockr.dock_is_locked = FALSE)
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.theme")
pkgload::load_all("blockr.bi")

# Fixed colors for SEX (deliberately non-standard so it's obvious the table
# and tile take their color from the MAP, not from any built-in default).
# TRT01A is pinned too: even though it is bound and present, the table colors
# along the SINGLE chosen dimension (SEX, the row-stub) — never two colors per
# row.
study_scale_map <- new_scale_map(
  scale_binding("SEX", color = c(F = "#0EA5E9", M = "#E69F00")),
  scale_binding("TRT01A", color = c(
    "Placebo"              = "#6D8196",
    "Xanomeline Low Dose"  = "#8B5CF6",
    "Xanomeline High Dose" = "#8b0000"
  )),
  palette = c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9")
)

board <- new_dock_board(
  blocks = c(
    data = new_dm_example_block(dataset = "pharmaverseadam",
      block_name = "ADaM data"),
    adsl = new_dm_pull_block(table = "adsl", block_name = "Pull adsl"),

    # CHART — colors by SEX on the `color` role. Honors the map TODAY.
    sex_by_arm = new_chart_block(
      chart_type = "bar", group = "TRT01A", color = "SEX",
      metric = ".count", agg_fn = "count",
      block_name = "Patients by arm x sex (chart — color role)"),

    # Aggregate to a small structured frame: one row per SEX, numeric columns.
    # Feeds BOTH the tile and the table so each gets a categorical SEX axis to
    # color and numeric values to show.
    by_sex = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "n",        func = "n",    col = "AGE"),
          list(type = "simple", name = "mean_age", func = "mean", col = "AGE")
        ),
        by = list("SEX")
      ),
      block_name = "Summaries by sex"),

    # TILE — grouped by SEX over the aggregated frame: one labeled sub-grid per
    # SEX level (F / M), each showing n and mean age. The sub-grid label IS the
    # SEX level, so the swatch hangs off the existing group header. PENDING: a
    # teal/orange dot on that label matching the chart.
    sex_tiles = new_tile_block(
      value = c("n", "mean_age"), by = "SEX", measure = "", layout = "cards",
      format = "number", block_name = "By sex (tile — group swatch)"),

    # TABLE — rowname = SEX, so the stub levels F/M are the categorical axis the
    # scale-map swatch would ride. cell_color heatmaps the NUMERIC body so both
    # channels show at once. PENDING: a gear "Color by" select (None + bound
    # categorical columns), defaulting to SEX (the single bound stub here).
    # Proposed arg sits alongside the existing cell_color:
    #   new_table_block(rowname = "SEX", color_by = "SEX", cell_color = ...)
    sex_table = new_table_block(
      rowname = "SEX",
      values  = c("n", "mean_age"),
      cell_color = drilldown_table_color(type = "sequential"),
      # color_by = "SEX",            # <- PENDING categorical scale-map swatch
      block_name = "Summary table (numeric heatmap + SEX swatch)")
  ),
  links = links(
    from = c("data", "adsl", "adsl", "by_sex", "by_sex"),
    to   = c("adsl", "sex_by_arm", "by_sex", "sex_tiles", "sex_table")
  ),
  layouts = list(
    Demo = dock_layout(c("sex_by_arm", "sex_tiles", "sex_table"))
  ),
  options = c(
    dock_board_options(),
    new_board_options(new_scale_map_option(study_scale_map))
  ),
  active = "Demo"
)

serve(board)
