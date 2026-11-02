# A nested groups table with a patient count (an N line under its name) and
# a dumbbell column faceted by arm (arm name, then "Onset to end · N = ..",
# then the axis), for test-table-groups-header-align.R.
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
socs <- c("Gastrointestinal disorders", "General disorders")
terms <- list(c("Diarrhoea", "Nausea"), c("Fatigue", "Pyrexia"))
n <- 120
i <- sample(seq_along(socs), n, TRUE)
subj <- sample(1:40, n, TRUE)
onset <- sample(1:250, n, TRUE)
ae <- data.frame(
  USUBJID = sprintf("S%03d", subj),
  Group = ifelse(subj <= 20, "Placebo", "Drug"),
  AEBODSYS = socs[i],
  AEDECOD = vapply(i, function(k) sample(terms[[k]], 1), ""),
  AETOXGR = sample(c("3", "2", "1"), n, TRUE),
  Onset = as.numeric(onset),
  End = as.numeric(onset + sample(1:150, n, TRUE)),
  stringsAsFactors = FALSE
)

serve(new_dock_board(
  blocks = c(
    ae = new_static_block(ae, block_name = "AEs"),
    tb = new_table_block(
      layout = "groups", by = c("AEBODSYS", "AEDECOD"),
      summaries = list(
        list(type = "simple", name = "Patients", func = "count_distinct",
             col = "USUBJID", show = "number"),
        list(type = "pair", name = "Onset to end", from = "Onset",
             from_func = "mean", to = "End", to_func = "mean",
             color = "AETOXGR", facet = "Group")
      ),
      block_name = "AE onset"
    )
  ),
  links = links(from = "ae", to = "tb"),
  views = list(tb = dock_view("tb", name = "AE onset")),
  active = "tb"
))
