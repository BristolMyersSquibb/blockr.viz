# Every chart type in a panel grid, as one blockr.page document: rows by
# sex, columns by treatment. A check, not a demo.
#
#   Rscript dev/page-facet-grid-types.R [port]
#
# Run from the blockr.viz checkout; see dev/page-facet-grid.R for the setup.
lib <- path.expand("~/.cache/blockr-prodlike-lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "4013")[1])

page_dir <- Filter(dir.exists, c("../blockr.page", "../../blockr.page"))[1]
library(blockr.core)
pkgload::load_all(page_dir, quiet = TRUE)
pkgload::load_all(".", quiet = TRUE)
options(blockr.tabular_display = blockr.ui::html_table_display)

lb <- pharmaverseadam::adlb
lb <- lb[lb$PARAM == "Alanine Aminotransferase (U/L)" &
           grepl("^(Baseline|Week )", lb$AVISIT) & nzchar(lb$TRTA),
         c("USUBJID", "TRTA", "SEX", "AVISIT", "AVISITN", "ADY", "AVAL",
           "CHG")]
lb <- as.data.frame(lb[!is.na(lb$AVAL), ])
ae <- as.data.frame(pharmaverseadam::adae)
ae <- ae[nzchar(ae$ACTARM) & !is.na(ae$ASTDY),
         c("USUBJID", "ACTARM", "SEX", "AESEV", "AEBODSYS", "ASTDY", "AENDY")]
ae <- ae[ae$AEBODSYS %in% names(sort(-table(ae$AEBODSYS)))[1:6], ]

g <- function(type, ..., src = "lab") {
  cols <- if (src == "lab") "TRTA" else "ACTARM"
  new_chart_block(type = type, ..., facet = cols, facet_row = "SEX",
                  facet_scales = "free_y", title = type, block_name = type,
                  visible = "inputs")
}
charts <- list(
  bar = g("bar", category = "AEBODSYS", func = "count", src = "ae"),
  histogram = g("histogram", x = "AVAL"),
  waterfall = g("waterfall", category = "AVISIT", value = "CHG", func = "mean",
                sort_by = "AVISITN"),
  pie = g("pie", category = "AESEV", func = "count", src = "ae"),
  treemap = g("treemap", category = "AEBODSYS", func = "count", src = "ae"),
  boxplot = g("boxplot", category = "AVISIT", value = "AVAL",
              sort_by = "AVISITN"),
  pointrange = g("pointrange", category = "AVISIT", value = "AVAL",
                 sort_by = "AVISITN"),
  scatter = g("scatter", x = "ADY", y = "AVAL"),
  line = g("line", x = "AVISITN", y = "AVAL", series = "USUBJID",
           line_width = 0.5),
  area = g("area", x = "AVISITN", y = "AVAL", func = "mean"),
  band = g("band", x = "AVISITN", y = "AVAL"),
  gantt = g("gantt", x = "ASTDY", xend = "AENDY", y = "AEBODSYS", src = "ae")
)
src_of <- c(bar = "ae", pie = "ae", treemap = "ae", gantt = "ae")
from <- ifelse(names(charts) %in% names(src_of), "ae", "lab")

board <- new_page_board(
  blocks = c(list(lab = new_static_block(lb, block_name = "ADLB, ALT"),
                  ae = new_static_block(ae, block_name = "ADAE")), charts),
  links = links(from = from, to = names(charts)),
  items = c(
    list(list(text = "# Every chart type in a grid"),
         list(text = "Rows by sex, columns by treatment, free y per row."),
         list(block = "lab", output = FALSE), list(block = "ae", output = FALSE)),
    lapply(names(charts), function(id) list(block = id))
  )
)

message("blockr.page facet grid types on http://127.0.0.1:", port)
shiny::runApp(serve(board, id = "page"), port = port, host = "127.0.0.1",
              launch.browser = FALSE)
