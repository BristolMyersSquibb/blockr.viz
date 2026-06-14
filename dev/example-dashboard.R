# blockr.viz — everything at once.
#
# A union of the three per-renderer tours (example-chart / -table / -tile):
# every chart type, every table mode, every tile style. Each view holds about
# three panels, and each panel is a TAB STRIP — click through the variants in
# place instead of squinting at a dense grid. Switch views with the tabs
# top-right. No narrative — just the capabilities.
#
# Views (panel = tab strip)
#   Charts        [bar|pie|treemap] [scatter+lm|line|boxplot] [radar|facet|waterfall]
#   Tables        [flat|Table 1] [crosstab heatmap|correlation] [gt publication]
#   Tiles         [delta|fill|pill] [matrix] [compact|percent|unit]
#   Interactions  drill: chart->table and tile->table (shown side by side)
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.viz/dev/example-dashboard.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)
options(blockr.dock_is_locked = FALSE)
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl   # subjects (ARM, AGE, SEX, RACE, BMIBL, ...)
adae <- safetyData::adam_adae   # adverse events (TRTA, AEBODSYS, AEDECOD, ...)

# Small frames the renderers need: a bridge for the waterfall, the Orange growth
# series for individual lines, and a correlation matrix for the diverging table.
bridge <- data.frame(
  step  = c("Opening", "New", "Upsell", "Churn", "Contraction"),
  value = c(1200, 480, 260, -210, -90)
)
orange <- datasets::Orange
num_vars <- c("AGE", "BMIBL", "WEIGHTBL", "HEIGHTBL")
cmat <- round(stats::cor(adsl[num_vars], use = "pairwise.complete.obs"), 2)
cor_df <- data.frame(Variable = rownames(cmat), cmat,
                     check.names = FALSE, row.names = NULL)
d <- tile_demo_data()           # $scorecard, $regions

board <- new_dock_board(
  blocks = c(
    # --- data ---------------------------------------------------------------
    adsl_d   = new_static_block(adsl, block_name = "ADSL (subjects)"),
    adae_d   = new_static_block(adae, block_name = "ADAE (adverse events)"),
    bridge_d = new_static_block(bridge, block_name = "Revenue bridge"),
    orange_d = new_static_block(orange, block_name = "Orange trees"),
    cor_d    = new_static_block(cor_df, block_name = "Correlation matrix"),
    sc       = new_static_block(d$scorecard, block_name = "Scorecard"),
    rg       = new_static_block(d$regions, block_name = "Regions"),

    # --- CHARTS (every type) -----------------------------------------------
    c_bar     = new_chart_block(chart_type = "bar", group = "ARM", color = "SEX",
                                metric = ".count", agg_fn = "count",
                                block_name = "Bar (counts by arm)"),
    c_scatter = new_chart_block(chart_type = "scatter", x = "AGE", y = "WEIGHTBL",
                                color = "SEX", smoother = "lm",
                                block_name = "Scatter + lm trend"),
    c_line    = new_chart_block(chart_type = "line", x = "age",
                                y = "circumference", series = "Tree",
                                block_name = "Line (per series)"),
    c_pie     = new_chart_block(chart_type = "pie", group = "ARM",
                                metric = ".count", agg_fn = "count",
                                block_name = "Pie (arm share)"),
    c_treemap = new_chart_block(chart_type = "treemap", group = "RACE",
                                metric = ".count", agg_fn = "count",
                                block_name = "Treemap (race share)"),
    c_radar   = new_chart_block(chart_type = "radar", group = "RACE",
                                color = "ARM", metric = "AGE", agg_fn = "mean",
                                block_name = "Radar (mean age)"),
    c_box     = new_chart_block(chart_type = "boxplot", group = "ARM",
                                metric = "AGE", agg_fn = "mean",
                                block_name = "Boxplot (age by arm)"),
    c_facet   = new_chart_block(chart_type = "bar", group = "AGEGR1",
                                facet = "SEX", metric = ".count",
                                agg_fn = "count",
                                block_name = "Facetted bar (by sex)"),
    c_wf      = new_chart_block(chart_type = "waterfall", group = "step",
                                metric = "value", agg_fn = "sum",
                                block_name = "Waterfall (bridge)"),

    # --- TABLES (every mode) -----------------------------------------------
    t_flat    = new_table_block(rowname = "USUBJID",
                                values = c("SEX", "ARM", "AGE", "BMIBL"),
                                block_name = "Flat listing"),
    t_summ    = new_summary_table_block(
                  state = list(vars = list("AGE", "SEX", "RACE"),
                               by = list("ARM"), add_overall = TRUE),
                  block_name = "summary_table (Table 1)"),
    t_summtbl = new_table_block(block_name = "Structured Table 1"),
    xt_summ   = new_summarize_block(
                  state = list(
                    summaries = list(
                      list(type = "simple", name = "n", func = "n", col = "AGE")),
                    by = list("AGEGR1", "ARM")),
                  block_name = "Count by age-group x arm"),
    xt_wide   = new_pivot_wider_block(
                  state = list(id_cols = list("AGEGR1"),
                               names_from = list("ARM"),
                               values_from = list("n")),
                  block_name = "Pivot to crosstab"),
    t_xt      = new_table_block(rowname = "AGEGR1",
                                cell_color = drilldown_table_color(type = "sequential"),
                                block_name = "Crosstab heatmap"),
    t_cor     = new_table_block(rowname = "Variable",
                                cell_color = drilldown_table_color(type = "diverging",
                                                                   domain = c(-1, 1)),
                                block_name = "Correlation (diverging)"),
    t_gt      = new_gt_table_block(title = "Table 1. Demographics",
                                   subtitle = "Safety population",
                                   block_name = "Publication (gt)"),

    # --- TILES (every style) -----------------------------------------------
    ti_delta   = new_tile_block(value = "value", measure = "metric",
                                secondary = "delta", style = "delta",
                                good_when = "up", format = "compact",
                                block_name = "Delta cards"),
    ti_fill    = new_tile_block(value = "value", measure = "metric",
                                secondary = "progress", style = "fill",
                                format = "compact", block_name = "Progress fill"),
    ti_pill    = new_tile_block(value = "value", measure = "metric",
                                secondary = "status", style = "pill",
                                format = "compact", block_name = "Status pills"),
    ti_matrix  = new_tile_block(value = c("revenue", "conversion", "orders"),
                                by = "region", layout = "table",
                                block_name = "Matrix (measures x region)"),
    ti_compact = new_tile_block(value = "value", measure = "metric",
                                format = "compact", block_name = "Compact format"),
    ti_percent = new_tile_block(value = "progress", measure = "metric",
                                format = "percent", block_name = "Percent format"),
    ti_unit    = new_tile_block(value = "orders", by = "region",
                                format = "number", unit = "orders",
                                block_name = "Number + unit"),

    # --- INTERACTIONS (drill) ----------------------------------------------
    ix_chart   = new_chart_block(chart_type = "bar", group = "AEBODSYS",
                                 color = "TRTA", metric = ".count",
                                 agg_fn = "count", drill = "AEBODSYS",
                                 sort_by = "value", sort_dir = "desc",
                                 block_name = "AEs by SOC (click to filter)"),
    ix_tbl     = new_table_block(rowname = "USUBJID",
                                 values = c("AEDECOD", "AEBODSYS", "TRTA"),
                                 block_name = "Adverse events (drilled)"),
    ix_tile    = new_tile_block(value = "orders", by = "region", unit = "orders",
                                drill = TRUE, block_name = "Orders by region (drill)"),
    ix_tiletbl = new_table_block(block_name = "Region (drilled)")
  ),
  links = links(
    from = c(
      "adsl_d", "adsl_d", "orange_d", "adsl_d", "adsl_d", "adsl_d", "adsl_d",
      "adsl_d", "bridge_d",
      "adsl_d", "adsl_d", "t_summ", "t_summ", "adsl_d", "xt_summ", "xt_wide",
      "cor_d",
      "sc", "sc", "sc", "rg", "sc", "sc", "rg",
      "adae_d", "ix_chart", "rg", "ix_tile"
    ),
    to = c(
      "c_bar", "c_scatter", "c_line", "c_pie", "c_treemap", "c_radar", "c_box",
      "c_facet", "c_wf",
      "t_flat", "t_summ", "t_summtbl", "t_gt", "xt_summ", "xt_wide", "t_xt",
      "t_cor",
      "ti_delta", "ti_fill", "ti_pill", "ti_matrix", "ti_compact", "ti_percent",
      "ti_unit",
      "ix_chart", "ix_tbl", "ix_tile", "ix_tiletbl"
    )
  ),
  # ~3 panels per view; each panel is a TABBED leaf (panels()) so a user clicks
  # through the variants in place rather than squinting at a dense grid.
  layouts = list(
    charts = dock_layout(
      panels("c_bar", "c_pie", "c_treemap"),
      panels("c_scatter", "c_line", "c_box"),
      panels("c_radar", "c_facet", "c_wf"),
      orientation = "horizontal", name = "Charts"),
    tables = dock_layout(
      panels("t_flat", "t_summtbl"),
      panels("t_xt", "t_cor"),
      panels("t_gt"),
      orientation = "vertical", name = "Tables (+ gt)"),
    tiles = dock_layout(
      panels("ti_delta", "ti_fill", "ti_pill"),
      panels("ti_matrix"),
      panels("ti_compact", "ti_percent", "ti_unit"),
      orientation = "horizontal", name = "Tiles"),
    # Drill needs the chart and its table visible together, so keep these as
    # side-by-side pairs rather than tabs.
    interactions = dock_layout(
      group("ix_chart", "ix_tbl"),
      group("ix_tile", "ix_tiletbl"),
      orientation = "vertical", name = "Interactions (drill)")
  ),
  options = dock_board_options(),
  active = "charts",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
