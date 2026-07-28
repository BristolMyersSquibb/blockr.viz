# =============================================================================
# Wire-payload profiler: summarize table vs table block vs chart block
# =============================================================================
#
# Answers "what actually crosses the websocket, and is it data or markup?".
# Shiny's websocket is NEVER compressed, so RAW bytes are what the browser
# waits on -- gzip sizes are reported only to show how much of the payload is
# repetition.
#
#   Rscript blockr.viz/dev/profile-payload-bytes.R
#   REPS=20 Rscript blockr.viz/dev/profile-payload-bytes.R

root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
suppressMessages({
  pkgload::load_all(file.path(root, "blockr.core"), quiet = TRUE)
  pkgload::load_all(file.path(root, "blockr.viz"), quiet = TRUE)
})

REPS <- as.integer(Sys.getenv("REPS", "10"))

bench <- function(f, reps = REPS) {
  f()
  t <- numeric(reps)
  for (i in seq_len(reps)) {
    a <- Sys.time(); f(); t[i] <- as.numeric(Sys.time() - a, "secs") * 1000
  }
  stats::median(t)
}

nbytes <- function(s) sum(nchar(s, type = "bytes"))
gzbytes <- function(s) length(memCompress(charToRaw(paste(s, collapse = "")),
                                          "gzip"))
kb <- function(b) sprintf("%.1f KB", b / 1024)

# ---- data: an AE-like long frame --------------------------------------------
mk_ae <- function(n_subj, n_terms) {
  set.seed(42)
  terms <- sprintf("PREFERRED TERM %03d", seq_len(n_terms))
  n <- n_subj * 6L
  data.frame(
    USUBJID = sprintf("SUBJ-%05d", sample(n_subj, n, TRUE)),
    AEDECOD = sample(terms, n, TRUE),
    AEBODSYS = sample(sprintf("SOC %02d", 1:12), n, TRUE),
    TRT = sample(c("Placebo", "Low dose", "High dose"), n, TRUE),
    AESEV = sample(c("MILD", "MODERATE", "SEVERE"), n, TRUE),
    AVAL = round(rnorm(n, 50, 15), 2),
    CHG = round(rnorm(n, 0, 8), 2),
    stringsAsFactors = FALSE
  )
}

# ---- the three blocks' payload builders -------------------------------------
# summarize table: aggregates in R, ships the aggregated cell model.
rank_json <- function(data, ...) {
  rank_payload_json(rank_build_payload(data, chrome = list(), ...))
}
# and its historical server-rendered HTML, same config, for the ratio.
rank_html <- function(data, ...) {
  as.character(htmltools::renderTags(rank_table(data, ...))$html)
}
# table block: ships a cell model of whatever frame it is handed.
table_json <- function(data) dt_payload_json(dt_build_payload(data))
# chart block: ships the RAW rows of the columns the mapping needs.
chart_json <- function(data, cols) {
  as.character(jsonlite::toJSON(data[, cols, drop = FALSE],
                                dataframe = "columns", digits = NA))
}

CFG_BAR <- list(
  group = "AEDECOD", func = "count_distinct", value = "USUBJID",
  id_var = "USUBJID", facet = "TRT", sort_by = "value", sort_dir = "desc"
)
CFG_SUM <- list(
  by = "AEDECOD",
  summaries = list(
    list(type = "simple", func = "count_distinct", col = "USUBJID",
         show = "bar", name = "Subjects"),
    list(type = "dist", col = "AVAL", show = "box", name = "Severity"),
    list(type = "dist", col = "CHG", style = "dot", inner = "mean_sd",
         outer = "none", name = "Change")
  ),
  facet = "TRT"
)

report <- function(label, n_subj, n_terms) {
  d <- mk_ae(n_subj, n_terms)
  cat(sprintf("\n== %s : %s input rows, %s terms x 3 arms\n",
              label, format(nrow(d), big.mark = ","), n_terms))
  cat(sprintf("%-34s %10s %10s %10s %9s\n",
              "payload", "raw", "gzip", "bytes/row", "build ms"))
  line <- function(nm, s, rows, ms) {
    b <- nbytes(s)
    cat(sprintf("%-34s %10s %10s %10.1f %9.1f\n", nm, kb(b), kb(gzbytes(s)),
                b / rows, ms))
  }

  jb <- do.call(rank_json, c(list(d), CFG_BAR))
  hb <- do.call(rank_html, c(list(d), CFG_BAR))
  js <- do.call(rank_json, c(list(d), CFG_SUM))
  hs <- do.call(rank_html, c(list(d), CFG_SUM))

  line("summarize (bar+facet) JSON", jb, n_terms,
       bench(function() do.call(rank_json, c(list(d), CFG_BAR))))
  line("summarize (bar+facet) HTML", hb, n_terms,
       bench(function() do.call(rank_html, c(list(d), CFG_BAR))))
  line("summarize (3 cols+dist) JSON", js, n_terms,
       bench(function() do.call(rank_json, c(list(d), CFG_SUM))))
  line("summarize (3 cols+dist) HTML", hs, n_terms,
       bench(function() do.call(rank_html, c(list(d), CFG_SUM))))

  # Same rows, table block: hand it the aggregated frame the summarize
  # table produces, so both ship the same number of table rows.
  agg <- as.data.frame(do.call(rbind, lapply(split(d, d$AEDECOD), function(g) {
    data.frame(AEDECOD = g$AEDECOD[1],
               Placebo = sum(g$TRT == "Placebo"),
               Low = sum(g$TRT == "Low dose"),
               High = sum(g$TRT == "High dose"),
               AVAL = round(mean(g$AVAL), 2))
  })))
  line("table block, SAME agg rows", table_json(agg), nrow(agg),
       bench(function() table_json(agg)))
  # And the chart block on the same input: raw rows, no aggregation.
  cc <- c("AEDECOD", "TRT", "USUBJID")
  line("chart block, RAW input rows", chart_json(d, cc), nrow(d),
       bench(function() chart_json(d, cc)))
  line("table block, RAW input rows", table_json(d), nrow(d),
       bench(function() table_json(d)))
  invisible(NULL)
}

cat("=============================================================\n")
cat(" wire payload per render (Shiny ws is uncompressed)\n")
cat("=============================================================\n")
report("small  (a real AE table)", 300L, 120L)
report("prod   (a big AE table)", 800L, 400L)
report("stress", 2000L, 790L)

# ---- where do the bytes go, inside the summarize payload? -------------------
cat("\n== field-level breakdown, prod summarize (3 cols + dist)\n")
d <- mk_ae(800L, 400L)
p <- do.call(rank_build_payload, c(list(d, chrome = list()), CFG_SUM))
tot <- nbytes(rank_payload_json(p))
sz <- function(x) nbytes(as.character(jsonlite::toJSON(x, auto_unbox = TRUE,
                                                       na = "null")))
parts <- c(
  head_markup = sz(p$head),
  label = sz(p$label),
  chrome = sz(p$chrome),
  cols_total = sz(p$cols)
)
for (i in seq_along(p$cols)) {
  cn <- p$cols[[i]]
  for (f in names(cn)) {
    key <- paste0("  col", i, ".", cn$kind, ".", f)
    parts[key] <- sz(cn[[f]])
  }
}
for (nm in names(parts)) {
  cat(sprintf("%-40s %10s  %5.1f%%\n", nm, kb(parts[[nm]]),
              100 * parts[[nm]] / tot))
}
cat(sprintf("%-40s %10s\n", "TOTAL", kb(tot)))
