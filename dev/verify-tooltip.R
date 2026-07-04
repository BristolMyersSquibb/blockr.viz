# Verification harness for the richer tooltips:
#  - gantt: full role list (term / subject / from->to+duration / severity)
#           + extra tooltip fields (AESER, AEREL) via the "+" tt_fields role
#  - pie / bar: aggregation label + n
# Serve on 3838.
options(shiny.port = 3838L, shiny.host = "0.0.0.0")
options(blockr.html_table_preview = TRUE)

pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.viz", quiet = TRUE)

# --- Synthetic AE data (ms date columns so the gantt uses a time axis) ------
day <- function(d) as.numeric(as.POSIXct(d, tz = "UTC")) * 1000
ae <- data.frame(
  USUBJID = c("CA-0001", "CA-0001", "CA-0001",
              "CA-0002", "CA-0002",
              "CA-0003", "CA-0003", "CA-0003"),
  AETERM  = c("Stomatitis", "Nausea", "Cyst",
              "Rash maculo-papular", "Fatigue",
              "Anaemia", "Weight decreased", "Diarrhoea"),
  AESTDT  = day(c("2021-03-01", "2021-05-10", "2021-06-15",
                  "2021-02-20", "2021-04-01",
                  "2021-01-15", "2021-03-20", "2021-05-05")),
  AENDT   = day(c("2021-04-15", "2021-05-20", "2021-06-18",
                  "2021-04-10", "2021-04-05",
                  "2021-03-01", "2021-03-25", "2021-06-30")),
  AETOXGR = c(3, 1, 2, 2, 1, 3, 1, 2),
  AESER   = c("Y", "N", "N", "N", "N", "Y", "N", "N"),
  AEREL   = c("RELATED", "NOT RELATED", "RELATED", "NOT RELATED",
              "NOT RELATED", "RELATED", "POSSIBLE", "RELATED"),
  stringsAsFactors = FALSE
)
attr(ae$AESTDT, "label") <- "Onset date"
attr(ae$AENDT, "label")  <- "Resolution date"
attr(ae$AETOXGR, "label") <- "Severity grade"
attr(ae$USUBJID, "label") <- "Subject"

board <- new_board(
  blocks = c(
    data = new_static_block(ae, block_name = "AE"),
    gantt = new_chart_block(
      chart_type = "gantt",
      x = "AESTDT", xend = "AENDT", y = "USUBJID",
      label = "AETERM", color = "AETOXGR",
      tt_fields = c("AESER", "AEREL"),
      block_name = "AE timeline"),
    pie = new_chart_block(
      chart_type = "pie", group = "AETOXGR",
      metric = ".count", agg_fn = "count",
      block_name = "By severity (pie)"),
    bar = new_chart_block(
      chart_type = "bar", group = "USUBJID",
      metric = "AETOXGR", agg_fn = "mean",
      orientation = "vertical",
      block_name = "Mean severity (bar)")
  ),
  links = links(
    from = c("data", "data", "data"),
    to   = c("gantt", "pie", "bar")
  )
)

serve(board)
