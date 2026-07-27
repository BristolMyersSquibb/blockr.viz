# Verify the summarize-table mode: one lane chart block carrying a
# heterogeneous column list (bar + box + text dist + field + swimlane +
# sparkline) over one grouping, plus a faceted variant with a pooled
# Overall column.
#
#   Rscript blockr.viz/dev/verify-summarize-table.R [port]   (default 3838)
#
# Mixed view
#   mixed — AE terms: Subjects bar, Duration box, mean CI text, Arms field,
#           episode swimlane, duration trajectory. Check: each column
#           scales to its OWN domain (the bar and the box do not share);
#           sort by any header; the gear shows the Columns editor (typed
#           rows, expand to edit, add/remove/reorder, presets) instead of
#           the mark picker; Grouping (by) and Facet sit below Columns.
# Facet view
#   facet — Subjects bar per arm (copies adjacent, one shared scale),
#           pooled Overall column and the Arms field rendered ONCE.
# Visits view
#   visits — vital signs by AVISIT: the row order comes from the DATA, not
#           the alphabet. sort_by = "AVISITN" (a raw numeric column, ordered
#           by the group minimum) is the reliable one; sort_by = "data"
#           (factor levels, else first appearance) puts End of Treatment
#           mid-table here, because one subject discontinued early. Both
#           live in the gear's Sort select. The two distribution columns
#           also carry a COLOUR dimension (SEX): one glyph per level in the
#           same cell, one shared scale per column, legend above the table.
# Nested view
#   nested — subjects collapsed under SEX (by = c(SEX, USUBJID)): parents
#           are expandable rows aggregated in their own pass (a parent's
#           swimlane pools every episode of that sex), chevron expands the
#           subjects beneath.
#
# load_all EVERYTHING before touching any blockr namespace.
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(
  shiny.port = {
    args <- commandArgs(trailingOnly = TRUE)
    p <- if (length(args)) args[[1]] else Sys.getenv("BLOCKR_PORT", "3838")
    as.integer(p)
  },
  shiny.host = "0.0.0.0"
)

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", getOption("shiny.port")))

adae <- as.data.frame(pharmaverseadam::adae)
adae <- adae[!is.na(adae$ASTDY) & !is.na(adae$AENDY), ]
adae$DUR <- adae$AENDY - adae$ASTDY + 1
counts <- sort(table(adae$AEDECOD), decreasing = TRUE)
ae <- adae[adae$AEDECOD %in% names(counts)[1:15],
           c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV", "AESER", "TRT01A",
             "SEX", "ASTDY", "AENDY", "DUR")]

vs <- as.data.frame(pharmaverseadam::advs)
vs <- vs[!is.na(vs$AVISIT) &
           vs$PARAM %in% c("Diastolic Blood Pressure (mmHg)",
                           "Pulse Rate (beats/min)"),
         c("USUBJID", "AVISIT", "AVISITN", "PARAM", "PARAMCD", "AVAL", "CHG",
           "TRT01A", "SEX", "ADY")]
dbp <- vs[vs$PARAMCD == "DIABP", ]

mixed_summaries <- list(
  list(type = "simple", name = "Subjects", func = "count_distinct",
       col = "USUBJID", show = "bar"),
  list(type = "dist", name = "Duration", col = "DUR",
       stat = "median_q1_q3", whiskers = "tukey", show = "box"),
  list(type = "dist", name = "Mean (95% CI)", col = "DUR",
       stat = "mean_ci95", show = "text"),
  list(type = "field", name = "Arms", col = "TRT01A"),
  list(type = "spans", name = "Episodes", x = "ASTDY", xend = "AENDY",
       color = "AESEV", label = "AEDECOD", fields = "AESER",
       size = "lg"),
  list(type = "series", name = "Trajectory", x = "ASTDY", col = "DUR",
       ref = "mean_sd")
)

facet_summaries <- list(
  list(type = "simple", name = "Subjects", func = "count_distinct",
       col = "USUBJID", show = "bar"),
  list(type = "dist", name = "Overall duration", col = "DUR",
       stat = "mean_ci95", show = "text", scope = "pooled"),
  list(type = "field", name = "Arms", col = "TRT01A")
)

serve(
  new_dock_board(
    blocks = c(
      ae_data = new_static_block(ae, block_name = "AE rows"),
      mixed = new_lane_chart_block(
        by = "AEDECOD", summaries = mixed_summaries, drill = "AEDECOD",
        title = "AE terms: the full column mix",
        block_name = "Summarize table"
      ),
      facet = new_lane_chart_block(
        by = "AEDECOD", summaries = facet_summaries, facet = "TRT01A",
        drill = "AEDECOD",
        title = "Faceted by arm, with a pooled Overall column",
        block_name = "Faceted + pooled"
      ),
      vs_data = new_static_block(dbp, block_name = "Vital signs"),
      visits = new_summarize_table_block(
        by = "AVISIT",
        summaries = list(
          list(type = "simple", name = "Subjects", func = "count_distinct",
               col = "USUBJID", show = "bar"),
          list(type = "dist", name = "DBP (mmHg)", col = "AVAL",
               stat = "median_q1_q3", whiskers = "tukey", show = "box",
               color = "SEX"),
          list(type = "dist", name = "Change from baseline", col = "CHG",
               stat = "mean_ci95", show = "pointrange", color = "SEX"),
          list(type = "field", name = "Arms", col = "TRT01A")
        ),
        sort_by = "AVISITN", sort_dir = "asc", drill = "AVISIT",
        title = "Diastolic BP by visit, in visit order",
        block_name = "Visits"
      ),
      nested = new_summarize_table_block(
        by = c("SEX", "USUBJID"),
        summaries = list(
          list(type = "simple", name = "Events", func = "count",
               show = "number"),
          list(type = "dist", name = "Duration", col = "DUR", show = "box"),
          list(type = "spans", name = "Episodes", x = "ASTDY",
               xend = "AENDY", color = "AESEV", label = "AEDECOD",
               size = "lg")
        ),
        drill = "USUBJID", sort_dir = "asc", sort_by = "label",
        title = "Subjects collapsed under sex",
        block_name = "Nested"
      )
    ),
    links = list(
      list(from = "ae_data", to = "mixed", input = "data"),
      list(from = "ae_data", to = "facet", input = "data"),
      list(from = "ae_data", to = "nested", input = "data"),
      list(from = "vs_data", to = "visits", input = "data")
    ),
    extensions = new_dag_extension(),
    grids = list(
      Mixed = dock_grid("mixed"),
      Facet = dock_grid("facet"),
      Visits = dock_grid("visits"),
      Nested = dock_grid("nested")
    ),
    active = Sys.getenv("BLOCKR_VIEW", "Mixed")
  )
)
