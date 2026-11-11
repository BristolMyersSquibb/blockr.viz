# Heading rows in each layout, for test-table-heading-levels.R: a nested
# groups table (organ class over term, long class names) with its summaries
# on the outer rows and the same with the default name headings, the
# nesting in the compact look (arm over subject, tiles per term), a finished table
# with empty heading rows (gtsummary style), and a groups table of 600 rows
# for the row window.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set,
# and blockr.ui from BLOCKR_UI_SRC.

ui_src <- Sys.getenv("BLOCKR_UI_SRC")
if (nzchar(ui_src)) pkgload::load_all(ui_src, quiet = TRUE)
src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)
library(blockr.core)
library(blockr.dock)

set.seed(1)
socs <- c("General disorders and administration site conditions",
          "Gastrointestinal disorders",
          "Respiratory, thoracic and mediastinal disorders")
terms <- list(c("Fatigue", "Pyrexia"), c("Diarrhoea", "Nausea"),
              c("Cough", "Dyspnoea"))
arms <- c("Drug A 240 mg + Drug B 1 mg/kg", "Placebo")
i <- sample(seq_along(socs), 200, TRUE)
ae <- data.frame(
  USUBJID = sprintf("S%03d", sample(1:12, 200, TRUE)),
  AEBODSYS = socs[i],
  AEDECOD = vapply(i, function(k) sample(terms[[k]], 1), ""),
  AESEV = factor(sample(c("MILD", "MODERATE", "SEVERE"), 200, TRUE),
                 c("MILD", "MODERATE", "SEVERE")),
  stringsAsFactors = FALSE
)
ae$ARM <- arms[(as.integer(substr(ae$USUBJID, 2, 4)) %% 2) + 1]

finished <- data.frame(
  .label = c("AGE (years)", "Mean (SD)", "Min, Max",
             "Body mass index at screening (kg/m2)", "Mean (SD)",
             "SEX", "F", "M"),
  .indent = c(0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L),
  Active = c(NA, "61.4 (9.8)", "34, 82", NA, "27.1 (4.6)", NA, "58 (49%)",
             "60 (51%)"),
  Placebo = c(NA, "60.9 (10.3)", "29, 84", NA, "26.8 (4.9)", NA, "61 (51%)",
              "59 (49%)"),
  stringsAsFactors = FALSE
)

long <- data.frame(
  SOC = sprintf("Organ class %02d", rep(1:40, each = 15L * 4L)),
  PT = sprintf("Term %02d.%02d", rep(1:40, each = 15L * 4L),
               rep(rep(1:15, each = 4L), 40L)),
  stringsAsFactors = FALSE
)

count <- list(list(type = "simple", func = "count", show = "bar"))
serve(new_dock_board(
  blocks = c(
    ae = new_static_block(ae, block_name = "AEs"),
    fin_data = new_static_block(finished, block_name = "Finished"),
    long_data = new_static_block(long, block_name = "Long"),
    grp = new_table_block(layout = "groups", by = c("AEBODSYS", "AEDECOD"),
                          summaries = count, drill = "auto",
                          group_rows = "summary", block_name = "Groups"),
    grh = new_table_block(layout = "groups", by = c("AEBODSYS", "AEDECOD"),
                          summaries = count, block_name = "Group headings"),
    cmp = new_table_block(
      layout = "groups", look = "compact", by = c("ARM", "USUBJID"),
      summaries = list(list(type = "simple", func = "count", show = "tile",
                            name = "Events", facet = "AEDECOD",
                            fill = "AESEV")),
      color = NULL, block_name = "Compact"
    ),
    fin = new_table_block(block_name = "Finished table"),
    lng = new_table_block(layout = "groups", by = c("SOC", "PT"),
                          summaries = count, block_name = "Long groups")
  ),
  links = links(from = c("ae", "ae", "ae", "fin_data", "long_data"),
                to = c("grp", "grh", "cmp", "fin", "lng")),
  views = list(
    grp = dock_view("grp", name = "grp"),
    grh = dock_view("grh", name = "grh"),
    cmp = dock_view("cmp", name = "cmp"),
    fin = dock_view("fin", name = "fin"),
    lng = dock_view("lng", name = "lng")
  ),
  active = "grp"
))
