# Verify observation-count labels (count_on / count_col).
#   Rscript blockr.viz/dev/verify-count-labels.R
options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(shiny.port = 3838L, shiny.host = "0.0.0.0")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

# Pre-summarised frame for the identity ("None (as is)") case: one row per arm
# with a precomputed N column. There are no subject rows to distinct-count, so
# the label must show the N column AS-IS.
arm_summ <- as.data.frame(table(ARM = adsl$ARM), stringsAsFactors = FALSE)
names(arm_summ)[2] <- "N"

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),
    summ = new_static_block(arm_summ, block_name = "Arm summary (precomputed N)"),
    # Identity bar: bars are the N column as-is; the count label must show N
    # (the column value), NOT a distinct/row count.
    ident_bar = new_chart_block(
      chart_type = "bar", group = "ARM", value = "N", func = "identity",
      count_on = "axis", count_col = "N",
      block_name = "Arm N (identity) — column-value labels"),
    # Axis counts: distinct subjects per arm on the category axis ("Placebo (86)").
    axis_bar = new_chart_block(
      chart_type = "bar", group = "ARM", value = ".count", func = "count",
      count_on = "axis", count_col = "USUBJID",
      block_name = "Subjects per arm — axis counts"),
    # Facet counts: distinct subjects per sex on the facet strips ("F (143)").
    facet_bar = new_chart_block(
      chart_type = "bar", group = "AGEGR1", facet = "SEX",
      value = ".count", func = "count",
      count_on = "facet", count_col = "USUBJID",
      block_name = "Age groups by sex — facet counts"),
    # Both surfaces at once, faceted + grouped.
    both_bar = new_chart_block(
      chart_type = "bar", group = "ARM", facet = "SEX",
      value = ".count", func = "count",
      count_on = "both", count_col = "USUBJID",
      block_name = "Arm x sex — axis + facet counts")
  ),
  links = links(
    from = c("data", "data", "data", "summ"),
    to   = c("axis_bar", "facet_bar", "both_bar", "ident_bar")
  ),
  grids = list(
    Axis     = dock_grid("axis_bar"),
    Facet    = dock_grid("facet_bar"),
    Both     = dock_grid("both_bar"),
    Identity = dock_grid("ident_bar"),
    Data     = dock_grid("dag_extension")
  ),
  active = "Identity",
  extensions = list(dag_extension = blockr.dag::new_dag_extension())
)

serve(board)
