# Verification board for the rank block's chart-parity refinements:
#   1. Gear mapping labels match the chart block (Group / Color / Facet,
#      rank-only extra: Nest under).
#   2. func = "identity" ("None (as is)"): one pre-computed value per patient,
#      ranked as-is, stratified with the chart's vocabulary (color, facet).
#   3. The bar cell carries its own value label -- no separate Value column.
#   4. color + facet COMPOSE (split bars inside each facet column); a plain
#      facet is colour-neutral (no per-level hues, no legend).
#   5. `fields` (identity only): extra row columns beside the bar -- the
#      chart's tooltip fields, as real columns.
#   6. Compare: zero-centred difference vs a comparator arm, delta in-bar.
#
# Run from the workspace root:
#   BLOCKR_PORT=4747 R -q -f blockr.viz/dev/verify-rank-identity.R
options(shiny.port = as.integer(Sys.getenv("BLOCKR_PORT", "3838")),
        shiny.host = "0.0.0.0")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.viz")

adsl <- safetyData::adam_adsl
adsl <- adsl[!is.na(adsl$BMIBL), ]
# Keep the board readable: 40 subjects is enough to see ranking + strata.
set.seed(1)
adsl <- adsl[sample(nrow(adsl), 40L), ]
adsl$TRT01A <- factor(adsl$TRT01A,
                      levels = c("Placebo", "Xanomeline Low Dose",
                                 "Xanomeline High Dose"))
attr(adsl$BMIBL, "label") <- "Baseline BMI (kg/m^2)"
attr(adsl$AGE, "label") <- "Age (years)"
attr(adsl, "label") <- "Subject-level analysis (ADSL)"

adae <- safetyData::adam_adae
adae$TRTA <- factor(adae$TRTA,
                    levels = c("Placebo", "Xanomeline Low Dose",
                               "Xanomeline High Dose"))
adae$AESEV <- factor(adae$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
attr(adae, "label") <- "Adverse events (ADAE)"

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),
    ae = new_static_block(adae, block_name = "ADaM ADAE"),

    # One value per patient, as-is, with the chart's tooltip fields as
    # real columns beside the bar.
    ident = new_rank_block(
      group = "USUBJID", func = "identity", value = "BMIBL",
      fields = c("TRT01A", "SEX", "AGE"),
      title = "Baseline BMI per subject",
      block_name = "Identity + fields"),

    # Same, split by color -- the chart's color stratification.
    ident_color = new_rank_block(
      group = "USUBJID", func = "identity", value = "BMIBL", color = "SEX",
      title = "Baseline BMI per subject, by sex",
      block_name = "Identity + color"),

    # The chart block's identity bar on the same data, for parity diffing.
    chart_ref = new_chart_block(
      chart_type = "bar", group = "USUBJID", func = "identity",
      value = "BMIBL", color = "SEX", orientation = "horizontal",
      title = "Chart reference (identity)",
      block_name = "Chart (reference)"),

    # Facet WITHOUT color: colour-neutral columns, one shared scale.
    ident_facet = new_rank_block(
      group = "USUBJID", func = "identity", value = "BMIBL", facet = "TRT01A",
      title = "Baseline BMI per subject, by arm",
      block_name = "Identity + facet"),

    # Facet AND color composing: split bars inside each arm column.
    facet_color = new_rank_block(
      group = "AEDECOD", facet = "TRTA", color = "AESEV",
      func = "count_distinct", id_var = "USUBJID",
      title = "Adverse events by arm and severity",
      subtitle = "One column per arm; segments = maximum severity",
      block_name = "Facet + color"),

    # Untouched block, for the gear's default state.
    fresh = new_rank_block(block_name = "Fresh rank")
  ),
  links = links(
    from = c("data", "data", "data", "data", "ae", "data"),
    to   = c("ident", "ident_color", "chart_ref", "ident_facet",
             "facet_color", "fresh")
  ),
  options = dock_board_options(),
  views = list(
    Identity = dock_view(c("ident", "ident_color", "chart_ref")),
    Facet = dock_view(c("ident_facet", "facet_color")),
    Fresh = dock_view("fresh")
  ),
  grids = list(
    Identity = dock_grid("ident", "ident_color", "chart_ref"),
    Facet = dock_grid("ident_facet", "facet_color"),
    Fresh = dock_grid("fresh")
  ),
  active = "Identity"
)

cat("\nServing rank-identity verification on http://127.0.0.1:",
    getOption("shiny.port"), "/\n\n", sep = "")
serve(board)
