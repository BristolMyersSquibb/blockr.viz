# Table capabilities tour — one renderer, every trick.
#
# `new_table_block()` is the universal interactive table: it renders a plain
# rectangular frame, a structured "Table 1", a pivoted crosstab, a numeric
# heatmap and a correlation matrix — same block, different input. This board
# is the guided tour. Each dock VIEW (the layout switcher, top-right) isolates
# one capability so you can see them one at a time; the left "Workflow" canvas
# shows how each is wired.
#
# Views
#   1. Simple        flat frame -> table (sticky header, sort, search)
#   2. Structured    summary_table() "Table 1" -> table (sections, indents,
#                    per-arm spanners, overall column)
#   3. Crosstab      summarize() + pivot_wider() -> table, with a SEQUENTIAL
#                    cell-colour heatmap over the counts (pivoting + colouring)
#   4. Correlation   a correlation matrix -> table with a DIVERGING red-white-
#                    blue scale anchored at 0 (domain -1..1)
#   5. Colour by SEX a board scale map (F = teal, M = orange) — the categorical
#                    colour language. The chart honours it today; the table
#                    shows the same SEX split (the stub swatch is the pending
#                    extension, see scale-map-table-tile-demo.R).
#   6. Data bars     in-cell bars proportional to the value (per-column,
#                    absolute magnitude) — the sortable/searchable alternative
#                    to an ECharts bar chart. Scoped to one column via `columns`.
#
# The two colour channels are deliberately different things: `cell_color` is a
# NUMERIC value->background scale (or data bar) on the body, while the scale map
# is a CATEGORICAL hue tied to a variable's levels. They never collide.
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.viz/dev/example-table.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)
options(blockr.dock_is_locked = FALSE)
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.theme")
pkgload::load_all("blockr.viz")

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

# A correlation matrix is data prep, not a block (there is no correlate block
# yet — see table-and-chart-architecture.md). Compute it up front so the table
# can show off its diverging scale on a matrix it understands natively.
num_vars <- c("AGE", "BMIBL", "WEIGHTBL", "HEIGHTBL")
cmat <- round(stats::cor(adsl[num_vars], use = "pairwise.complete.obs"), 2)
cor_df <- data.frame(Variable = rownames(cmat), cmat,
                     check.names = FALSE, row.names = NULL)

# Adverse-event counts per subject — the "data bar" use case. A sortable,
# searchable table with an in-cell bar beats an ECharts bar chart here. Top 15
# subjects by total AEs; `Types` (distinct AE terms, a different scale) rides
# along to show the column scope + per-column normalisation.
adae <- safetyData::adam_adae
ae_df <- adae |>
  dplyr::group_by(USUBJID) |>
  dplyr::summarise(AEs = dplyr::n(),
                   Types = dplyr::n_distinct(AEDECOD),
                   .groups = "drop") |>
  dplyr::slice_max(AEs, n = 15, with_ties = FALSE) |>
  dplyr::rename(Subject = USUBJID) |>
  as.data.frame()

# Categorical colour language: F is always teal, M always orange — wherever SEX
# appears. The chart's `color` role honours it today.
study_scale_map <- new_scale_map(
  scale_binding("SEX", color = c(F = "#0EA5E9", M = "#E69F00")),
  palette = c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9")
)

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl, block_name = "ADaM ADSL"),

    # 1. SIMPLE — a plain frame: one row per subject, mixed text + numeric
    #    columns. No structure, no colour; just the interactive chrome.
    flat = new_table_block(
      rowname = "USUBJID",
      values  = c("SEX", "ARM", "AGE", "BMIBL", "WEIGHTBL"),
      block_name = "Subjects (simple flat table)"),

    # 2. STRUCTURED — summary_table() emits the tidy ".fmt" Table-1 form; the
    #    table block detects it and renders sections + indents + per-arm
    #    spanners + an overall column.
    summ = new_summary_table_block(
      vars = list("AGE", "SEX", "RACE"),
      by = list("ARM"),
      add_overall = TRUE,
      block_name = "Demographics by arm (summary_table)"),
    summ_tbl = new_table_block(block_name = "Table 1 (structured rendering)"),

    # 3. CROSSTAB + HEATMAP — count subjects per age-group x arm, pivot to a
    #    wide grid, then heatmap the counts with a sequential scale. This single
    #    view shows BOTH pivoting and numeric cell colouring.
    xt_summ = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "n", func = "n", col = "AGE")
        ),
        by = list("AGEGR1", "ARM")
      ),
      block_name = "Count by age-group x arm"),
    xt_wide = new_pivot_wider_block(
      state = list(
        id_cols = list("AGEGR1"),
        names_from = list("ARM"),
        values_from = list("n")
      ),
      block_name = "Pivot to crosstab"),
    xt_tbl = new_table_block(
      rowname = "AGEGR1",
      cell_color = drilldown_table_color(type = "sequential"),
      block_name = "Crosstab + heatmap (pivot + cell colour)"),

    # 4. CORRELATION — the matrix renders as a table with a diverging scale
    #    pinned to [-1, 1] so 0 is white, +1 deep blue, -1 deep red.
    cor_data = new_static_block(cor_df, block_name = "Correlation matrix"),
    cor_tbl = new_table_block(
      rowname = "Variable",
      cell_color = drilldown_table_color(type = "diverging", domain = c(-1, 1)),
      block_name = "Correlation (diverging colour)"),

    # 5. COLOUR BY SEX — the categorical scale map. The chart colours its bars
    #    by SEX straight from the map (F teal / M orange). The table is a SIMPLE
    #    table with SEX as the row stub (not a structured Table-1): the scale-map
    #    swatch / row colour can only ride the stub levels of a flat table, so
    #    this is the shape that lets F/M pick up the same teal/orange.
    sex_chart = new_chart_block(
      chart_type = "bar", group = "ARM", color = "SEX",
      metric = ".count", agg_fn = "count",
      block_name = "Patients by arm x sex (chart — colour role)"),
    by_sex = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "n",        func = "n",    col = "AGE"),
          list(type = "simple", name = "mean_age", func = "mean", col = "AGE")
        ),
        by = list("SEX")
      ),
      block_name = "Summaries by sex"),
    sex_tbl = new_table_block(
      rowname = "SEX", values = c("n", "mean_age"),
      block_name = "By sex (simple table — SEX row stub)"),

    # 6. DATA BARS — in-cell bars proportional to the value, per-column
    #    normalised on absolute magnitude. Scoped to AEs via `columns`, so the
    #    bar reads as "most adverse events"; `Types` stays a plain column
    #    (open the gear -> Coloring = bar, Columns to add it). Leaving `columns`
    #    empty would bar every numeric column.
    ae_data = new_static_block(ae_df, block_name = "AE counts per subject"),
    ae_tbl = new_table_block(
      rowname = "Subject",
      cell_color = drilldown_table_color(type = "bar", columns = "AEs"),
      block_name = "Patients with most adverse events (data bars)")
  ),
  links = links(
    from = c("data", "data", "summ", "data", "xt_summ", "xt_wide",
             "cor_data", "data", "data", "by_sex", "ae_data"),
    to   = c("flat", "summ", "summ_tbl", "xt_summ", "xt_wide", "xt_tbl",
             "cor_tbl", "sex_chart", "by_sex", "sex_tbl", "ae_tbl")
  ),
  layouts = list(
    simple      = dock_layout("flat", name = "1. Simple"),
    structured  = dock_layout("summ_tbl", name = "2. Structured"),
    crosstab    = dock_layout("xt_tbl", name = "3. Crosstab + heatmap"),
    correlation = dock_layout("cor_tbl", name = "4. Correlation"),
    colour_sex  = dock_layout("sex_chart", "sex_tbl", name = "5. Colour by SEX"),
    data_bars   = dock_layout("ae_tbl", name = "6. Data bars")
  ),
  options = c(
    dock_board_options(),
    new_board_options(new_scale_map_option(study_scale_map))
  ),
  active = "simple",
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
