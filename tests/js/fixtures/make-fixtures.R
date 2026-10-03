# Writes chart-data.json, the fixed data set the chart snapshot tests draw.
#
# Run from the package root after changing the data or the R payload
# builders, then re-run `UPDATE_SNAPSHOTS=1 npm test` and review the diff:
#
#   Rscript tests/js/fixtures/make-fixtures.R
#
# The rows are arithmetic, not random, so the file is the same on every R.
# The payload goes through the same builders and the same JSON writer Shiny
# uses for a custom message, so the browser side sees what it sees live.

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

subj <- sprintf("S%02d", 1:12)
arms <- c("Placebo", "Low Dose", "High Dose")
visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
visitn <- c(0, 2, 4, 8, 12)
terms <- c("Headache", "Nausea", "Dizziness", "Fatigue",
           "Upper respiratory tract infection", "Rash")
sev <- c("MILD", "MODERATE", "SEVERE")

rows <- expand.grid(v = seq_along(visits), s = seq_along(subj))
i <- seq_len(nrow(rows))
s <- rows$s
v <- rows$v

d <- data.frame(
  USUBJID = subj[s],
  ARM = arms[(s - 1) %% 3 + 1],
  SEX = factor(ifelse(s %% 2 == 0, "F", "M"), levels = c("M", "F", "U")),
  AVISIT = visits[v],
  AVISITN = visitn[v],
  ADY = visitn[v] * 7 + (s %% 4),
  AVAL = round(20 + (s * 7) %% 13 + visitn[v] * 0.75 + (i %% 5) * 0.37, 2),
  AETERM = terms[(i * 5) %% length(terms) + 1],
  AESEV = sev[(i %% 3) + 1],
  ASTDY = (i * 3) %% 40 + 1,
  AENDY = (i * 3) %% 40 + 1 + (i %% 9),
  ANRHI = 30 + (s %% 3),
  stringsAsFactors = FALSE
)
# Change from the subject's first visit: negative and positive steps for the
# waterfall.
d$CHG <- round(d$AVAL - ave(d$AVAL, d$USUBJID, FUN = function(x) x[1]) -
  (s %% 3), 2)
d$LO <- d$AVAL - 1.5
d$HI <- d$AVAL + 1.5

# Missing values where they change a render path: a group key, a colour key,
# a series key, a facet key, a value, an interval end.
d$AETERM[c(4, 19, 33)] <- NA
d$AESEV[c(7, 40)] <- NA
d$ARM[s == 12] <- NA
d$USUBJID[c(58, 59)] <- NA
d$AVAL[c(5, 22, 47)] <- NA
d$CHG[c(5, 22, 47)] <- NA
d$LO[c(5, 22, 47)] <- NA
d$HI[c(5, 22, 47)] <- NA
d$AENDY[c(3, 10, 26)] <- NA

attr(d$AVAL, "label") <- "Analysis Value"
attr(d$ADY, "label") <- "Analysis Relative Day"
attr(d$AETERM, "label") <- "Reported Term"
attr(d$ARM, "label") <- "Planned Arm"

band <- function(...) {
  compute_band_series(d, "ADY", "AVAL", ..., window = "fixed",
                      window_size = 15, min_n = 2, id_col = "USUBJID",
                      n_grid = 12L)
}
smooth <- function(...) {
  compute_smoother_series(d, "lm", "ADY", "AVAL", ...)
}

fixture <- list(
  columns = dd_col_meta(d),
  data = chart_data_json(d),
  # The same rows, every column dictionary-encoded (the >= 500-row path).
  data_dict = chart_data_json(d, min_rows = 1L, max_ratio = 1),
  band_series = list(
    plain = band(NULL, NULL),
    color = band("ARM", NULL),
    facet = band(NULL, NULL, facet_by = "SEX")
  ),
  band_refs = list(hi = band_reference(d, "ANRHI")),
  smoother_series = list(
    plain = smooth(NULL, NULL),
    color = smooth("ARM", NULL),
    facet = smooth(NULL, NULL, facet_by = "SEX")
  )
)

json <- shiny:::toJSON(fixture)
writeLines(jsonlite::prettify(json, indent = 1),
           file.path("tests", "js", "fixtures", "chart-data.json"))
