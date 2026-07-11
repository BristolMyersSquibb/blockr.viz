# =============================================================================
# R-side block build-cost profiler
# =============================================================================
#
# Answers: how much time does each block spend on the R side, and where?
# Two costs are separated:
#   1. CONSTRUCTION  -- new_*_block(): builds the block object (no data).
#   2. RENDER        -- the HTML/expr build the block runs every time its data
#                       changes. This is the cost the user waits on when a view
#                       switch wakes a dormant block and the pipeline re-runs.
#
# It does NOT measure module wiring (block_server) or Shiny round trips -- those
# are the E2E harness (profile-blocks-e2e.R). This isolates pure R work so we
# can see loops / hot spots via Rprof.
#
# Run from the workspace root:
#   Rscript blockr.viz/dev/profile-blocks-r.R
# Optional: N reps (median reported), and a size multiplier for the data.
#   REPS=50 SIZE=large Rscript blockr.viz/dev/profile-blocks-r.R

root <- if (file.exists("blockr.viz/DESCRIPTION")) "." else ".."
suppressMessages({
  for (p in c("blockr.core", "blockr.viz", "blockr.dplyr", "blockr.ggplot")) {
    pkgload::load_all(file.path(root, p), quiet = TRUE)
  }
})

REPS <- as.integer(Sys.getenv("REPS", "30"))
SIZE <- Sys.getenv("SIZE", "prod")   # "prod" (realistic) or "large" (stress)

# ---- timing helper: median of REPS, in milliseconds --------------------------
bench <- function(expr, reps = REPS) {
  expr <- substitute(expr)
  env  <- parent.frame()
  # one warm-up (JIT / first-touch), then timed reps
  eval(expr, env)
  t <- numeric(reps)
  for (i in seq_len(reps)) {
    a <- Sys.time()
    eval(expr, env)
    t[i] <- as.numeric(Sys.time() - a, units = "secs") * 1000
  }
  stats::median(t)
}

fmt <- function(ms) sprintf("%8.2f ms", ms)

# ---- representative data -----------------------------------------------------
# adsl-like subject-level frame (one row per subject). prod: ~300 subjects
# (a real study arm set); large: 20k rows to expose per-row cost.
n_subj <- if (SIZE == "large") 20000L else 300L
set.seed(1)
adsl <- data.frame(
  USUBJID = sprintf("SUBJ-%05d", seq_len(n_subj)),
  TRT     = sample(c("Placebo", "Low", "High"), n_subj, TRUE),
  SEX     = sample(c("M", "F"), n_subj, TRUE),
  RACE    = sample(c("WHITE", "BLACK", "ASIAN", "OTHER"), n_subj, TRUE),
  AGE     = round(rnorm(n_subj, 55, 12)),
  BMI     = round(rnorm(n_subj, 27, 4), 1),
  WEIGHT  = round(rnorm(n_subj, 78, 14), 1),
  stringsAsFactors = FALSE
)

# The STRUCTURED (Table-1) frame the table block actually renders in prod:
# summary_table output (tens of rows, a handful of arm columns).
summ_df <- summary_table(
  adsl, vars = c("AGE", "BMI", "WEIGHT"), by = "TRT",
  stats = "mean_sd", add_overall = TRUE
)

cat(sprintf("\nSIZE=%s  adsl=%d x %d   summary=%d x %d   REPS=%d\n\n",
            SIZE, nrow(adsl), ncol(adsl), nrow(summ_df), ncol(summ_df), REPS))

results <- list()
add <- function(pkg, block, stage, ms, note = "") {
  results[[length(results) + 1]] <<- data.frame(
    pkg = pkg, block = block, stage = stage,
    ms = round(ms, 2), note = note, stringsAsFactors = FALSE)
}

# =============================================================================
# blockr.viz
# =============================================================================

# ---- TABLE (the block under scrutiny) ----------------------------------------
# Construction
add("blockr.viz", "table", "construct", bench(new_table_block()))

# Render, STRUCTURED path (what prod hits): chrome + body, as the server does.
add("blockr.viz", "table", "render:structured:chrome",
    bench(dt_chrome(elem_id = "x", structured = TRUE, max_height = "600px",
                    inner = "body")))
add("blockr.viz", "table", "render:structured:is_structured",
    bench(dt_is_structured(summ_df)))
add("blockr.viz", "table", "render:structured:body",
    bench(dt_table_tag(summ_df, drill = NULL)),
    note = sprintf("%dx%d structured", nrow(summ_df), ncol(summ_df)))

# Render, FLAT path (a plain data table off the raw frame).
add("blockr.viz", "table", "render:flat:body",
    bench(dt_table_tag(adsl)),
    note = sprintf("%dx%d flat", nrow(adsl), ncol(adsl)))
add("blockr.viz", "table", "render:flat:gear_cols_json",
    bench(dt_gear_cols_json(adsl)))

# ---- TILE --------------------------------------------------------------------
add("blockr.viz", "tile", "construct", bench(new_tile_block()))
add("blockr.viz", "tile", "render",
    bench(tile_html(adsl[1, , drop = FALSE], value = c("AGE", "BMI"))))

# ---- SUMMARY_TABLE (upstream of the structured table) ------------------------
add("blockr.viz", "summary_table", "construct", bench(new_summary_table_block()))
add("blockr.viz", "summary_table", "compute",
    bench(summary_table(adsl, vars = c("AGE", "BMI", "WEIGHT"), by = "TRT",
                        stats = "mean_sd", add_overall = TRUE)),
    note = "the summarise, not a render")

# ---- CHART (R cost = data prep only; marks drawn in JS) -----------------------
add("blockr.viz", "chart", "construct", bench(new_chart_block()))

# =============================================================================
# blockr.dplyr  (JS-driven blocks; R cost = the column-summary they ship)
# =============================================================================
add("blockr.dplyr", "filter", "construct", bench(new_filter_block()))
add("blockr.dplyr", "select", "construct", bench(new_select_block()))
add("blockr.dplyr", "mutate", "construct", bench(new_mutate_block()))
if (exists("build_column_summary")) {
  add("blockr.dplyr", "filter", "columns_meta",
      bench(build_column_summary(adsl)),
      note = "shipped to the JS picker")
}

# =============================================================================
# blockr.ggplot  (the interesting one -- ggplot build can be costly)
# =============================================================================
add("blockr.ggplot", "ggplot", "construct",
    bench(new_ggplot_block(type = "point", x = "AGE", y = "BMI")))
# The actual plot build (ggplot_build + render to the device) is where the cost
# is. Build the grob for a representative point layer.
gg <- ggplot2::ggplot(adsl, ggplot2::aes(AGE, BMI)) + ggplot2::geom_point()
add("blockr.ggplot", "ggplot", "ggplot_build",
    bench(ggplot2::ggplot_build(gg)),
    note = sprintf("%d points", nrow(adsl)))

# =============================================================================
# report
# =============================================================================
df <- do.call(rbind, results)
df <- df[order(df$pkg, df$block, -df$ms), ]
cat(sprintf("%-13s %-15s %-32s %10s  %s\n",
            "pkg", "block", "stage", "median", "note"))
cat(strrep("-", 100), "\n")
for (i in seq_len(nrow(df))) {
  r <- df[i, ]
  cat(sprintf("%-13s %-15s %-32s %s  %s\n",
              r$pkg, r$block, r$stage, fmt(r$ms), r$note))
}

# ---- Rprof deep-dive on the two heaviest render paths ------------------------
cat("\n\n=== Rprof: table structured-body render (", nrow(summ_df), "rows) ===\n")
prof <- tempfile()
Rprof(prof, interval = 0.002, line.profiling = FALSE)
for (i in 1:400) dt_table_tag(summ_df, drill = NULL)
Rprof(NULL)
print(head(summaryRprof(prof)$by.total, 15))

cat("\n\n=== Rprof: table FLAT-body render (", nrow(adsl), "rows) ===\n")
prof2 <- tempfile()
Rprof(prof2, interval = 0.002, line.profiling = FALSE)
for (i in 1:200) dt_table_tag(adsl)
Rprof(NULL)
print(head(summaryRprof(prof2)$by.total, 15))
