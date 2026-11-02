# Two groups tables over medications whose CMDECOD carries a variable label
# and has missing values, for test-table-groups-stub-label.R: one by CMDECOD,
# one by CMCLAS and CMDECOD. Both show missing groups as "(Missing)".
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
n <- 60
cm <- data.frame(
  USUBJID = sprintf("S%02d", sample(1:20, n, TRUE)),
  CMCLAS = sample(c("ANALGESICS", "ANTIBIOTICS"), n, TRUE),
  CMDECOD = sample(c("PARACETAMOL", "IBUPROFEN", "AMOXICILLIN"), n, TRUE),
  stringsAsFactors = FALSE
)
cm$CMDECOD[c(3, 9)] <- NA
attr(cm$CMDECOD, "label") <- "Standardized Medication Name"
attr(cm$CMCLAS, "label") <- "Medication Class"

s <- list(list(type = "simple", name = "Patients", func = "count_distinct",
               col = "USUBJID", show = "bar"))
serve(new_dock_board(
  blocks = c(
    cm = new_static_block(cm, block_name = "CM"),
    flat = new_table_block(layout = "groups", by = "CMDECOD", summaries = s,
                           block_name = "Flat"),
    nested = new_table_block(layout = "groups", by = c("CMCLAS", "CMDECOD"),
                             summaries = s, block_name = "Nested")
  ),
  links = links(from = c("cm", "cm"), to = c("flat", "nested")),
  views = list(meds = dock_view(c("flat", "nested"), name = "Medications")),
  active = "meds"
))
