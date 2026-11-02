# A nested groups table (system organ class over preferred term, one count)
# in a dock card, for test-table-groups-chevron.R. Long organ class names, as
# in an AE summary.
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
socs <- c("Gastrointestinal disorders",
          "General disorders and administration site conditions",
          "Blood and lymphatic system disorders",
          "Skin and subcutaneous tissue disorders")
terms <- list(c("Diarrhoea", "Nausea"), c("Fatigue", "Pyrexia"),
              c("Anaemia", "Neutropenia"), c("Rash", "Pruritus"))
i <- sample(seq_along(socs), 200, TRUE)
ae <- data.frame(
  USUBJID = sprintf("S%03d", sample(1:60, 200, TRUE)),
  AEBODSYS = socs[i],
  AEDECOD = vapply(i, function(k) sample(terms[[k]], 1), ""),
  stringsAsFactors = FALSE
)

serve(new_dock_board(
  blocks = c(
    ae = new_static_block(ae, block_name = "AEs"),
    soc = new_table_block(
      layout = "groups", by = c("AEBODSYS", "AEDECOD"),
      summaries = list(list(type = "simple", func = "count", show = "bar")),
      block_name = "AE summary"
    )
  ),
  links = links(from = "ae", to = "soc"),
  views = list(soc = dock_view("soc", name = "AE summary")),
  active = "soc"
))
