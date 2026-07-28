# COLOR concept verification: `color` (identity tint) + `shadings` (cell
# value-encoding rules) on the table, `color` card tint on the tile.
# Package roots resolve from THIS script's location, so the same command runs
# in the container (/workspace) and on a host clone. Port: command-line arg,
# then BLOCKR_PORT, then the container's forwarded 3838.
.self <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.ws <- normalizePath(if (is.na(.self)) "." else
  file.path(dirname(sub("^--file=", "", .self)), "..", ".."))
.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "3838")[1])

options(shiny.port = .port,
        shiny.host = "0.0.0.0")
pkgload::load_all(file.path(.ws, "blockr.core"))
pkgload::load_all(file.path(.ws, "blockr.ui"))
pkgload::load_all(file.path(.ws, "blockr.dplyr"))
pkgload::load_all(file.path(.ws, "blockr.dock"))
pkgload::load_all(file.path(.ws, "blockr.dag"))
pkgload::load_all(file.path(.ws, "_scratch/worktrees/blockr.viz-color"))

adsl <- safetyData::adam_adsl
pkgload::load_all(file.path(.ws, "blockr.theme"))

study_scale_map <- new_scale_map(
  scale_binding("SEX", color = c(F = "#0EA5E9", M = "#E69F00")),
  scale_binding("ARM", color = c(
    "Placebo" = "#999999",
    "Xanomeline Low Dose" = "#56B4E9",
    "Xanomeline High Dose" = "#0072B2"
  )),
  palette = c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9")
)

sales <- data.frame(
  region = rep(c("EMEA", "APAC", "AMER"), each = 2),
  metric = rep(c("Revenue", "Orders"), 3),
  value  = c(1200, 840, 950, 620, 1600, 990)
)

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),
    kpis = new_static_block(sales, block_name = "Sales long frame"),

    # 1. Table: Color by SEX (row tint) + two shading rules (override:
    #    bar on AGE, diverging on all remaining numerics).
    tbl = new_table_block(
      rowname = "USUBJID", value = c("SEX", "ARM", "AGE", "BMIBL", "HEIGHTBL"),
      color = "SEX",
      shadings = list(
        list(mode = "bar", cols = list("AGE")),
        list(mode = "diverging", cols = list())
      ),
      block_name = "Color by SEX + shadings (bar AGE, diverging rest)"),

    # 2. Table: legacy args still work (cell_color spec + row_color).
    legacy = new_table_block(
      rowname = "USUBJID", value = c("SEX", "ARM", "AGE"),
      cell_color = drilldown_table_color("sequential", columns = "AGE"),
      row_color = "ARM",
      block_name = "LEGACY args (cell_color + row_color)"),

    # 2b. AGGREGATED table: group=ARM + mean summaries, Color by ARM +
    #     diverging shading -- COLOR must apply to the aggregated frame too.
    agg_tbl = new_table_block(
      group = "ARM",
      summaries = list(list(func = "mean", cols = list())),
      color = "ARM",
      shadings = list(list(mode = "diverging", cols = list())),
      block_name = "AGGREGATED: Color by ARM + diverging shading"),

    # 3. Tile: grouped by region, Color by region (card accents via map
    #    palette fallback... ARM bound; region -> board palette).
    tile_g = new_tile_block(
      value = "value", name = "metric", group = "region",
      color = "region",
      block_name = "Tile: Color by group (region)"),

    # 4. Tile: ungrouped KPI list, Color by the Name column.
    tile_n = new_tile_block(
      value = "value", name = "metric",
      color = "metric", layout = "table",
      block_name = "Tile: Color by Name (metric)")
  ),
  links = links(from = c("data", "data", "data", "kpis", "kpis"),
                to = c("tbl", "legacy", "agg_tbl", "tile_g", "tile_n")),
  layouts = list(
    tables = dock_layout("tbl", "legacy", "agg_tbl", name = "1. Tables"),
    tiles  = dock_layout("tile_g", "tile_n", name = "2. Tiles")
  ),
  options = new_board_options(new_scale_map_option(study_scale_map)),
  active = "tables"
)
serve(board)
