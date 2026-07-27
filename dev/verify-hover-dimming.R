# Verify where the hover veil / blur applies after the dimming rework.
#
#   Rscript blockr.viz/dev/verify-hover-dimming.R [port]   (default 3838)
#
# Scatter tab
#   cloud   — dense ALT scatter. Hover any dot: the REST OF THE CLOUD MUST NOT
#             FADE. Native `emphasis.focus: 'self'` blurred every other point in
#             the coordinate system, so the cloud strobed as the cursor moved.
#             The dot is still identified by the tooltip and the pointer cursor.
#   forest  — scatter with lo/hi whiskers (a coefficient plot). Same rule.
#
# Line tab
#   crowd   — 10 subjects. Hover a trajectory: the veil DOES rise (this is the
#             case the scrim was built for) and the hovered line gets its thick
#             gradient + dots.
#   single  — one mean line with a 95% CI. Hover it: the line still promotes,
#             but NO veil — nothing to dim, and the veil used to grey the CI
#             whiskers (a z:1 custom series under the z:8 scrim) as well.
#   two_arm — one mean line + CI per treatment arm (3 in ADaM). Veil back on;
#             the threshold is >1 line.
#
# load_all EVERYTHING before touching any blockr namespace: an option set via
# `blockr.ui::` above these lines would resolve against the INSTALLED package,
# which may be older than source.
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

adlb <- as.data.frame(pharmaverseadam::adlb)
alt <- adlb[adlb$PARAMCD == "ALT" & !is.na(adlb$ADY) & !is.na(adlb$AVAL), ]
alt <- alt[, c("USUBJID", "ADY", "AVAL", "TRT01A")]

# Dense cloud: every ALT observation, ~thousands of dots.
cloud <- alt

# Ten trajectories — the crowd the scrim exists for.
subj <- unique(alt$USUBJID)
crowd <- alt[alt$USUBJID %in% subj[1:10], ]

# Mean +/- 95% CI by study week. `by` NULL = pooled (one line), or a treatment
# column (one line per arm).
summarise_ci <- function(d, by = NULL) {
  d$WEEK <- floor(d$ADY / 7)
  keys <- if (is.null(by)) list(d$WEEK) else list(d$WEEK, d[[by]])
  parts <- split(d, keys, drop = TRUE)
  out <- do.call(rbind, lapply(parts, function(p) {
    m <- mean(p$AVAL)
    se <- stats::sd(p$AVAL) / sqrt(nrow(p))
    res <- data.frame(
      WEEK = p$WEEK[1], MEAN = m,
      LO = m - 1.96 * se, HI = m + 1.96 * se, N = nrow(p)
    )
    if (!is.null(by)) res[[by]] <- p[[by]][1]
    res
  }))
  # Drop thin cells: a 1-observation week has no usable interval.
  out <- out[out$N >= 5, ]
  out[order(out$WEEK), ]
}

single <- summarise_ci(alt)
two_arm <- summarise_ci(alt, by = "TRT01A")

# Coefficient / forest style: a scatter carrying lo/hi whiskers.
forest <- single

serve(
  new_dock_board(
    blocks = c(
      cloud_data = new_static_block(cloud, block_name = "ALT, all observations"),
      crowd_data = new_static_block(crowd, block_name = "ALT, 10 subjects"),
      single_data = new_static_block(single, block_name = "Mean ALT + 95% CI"),
      two_data = new_static_block(two_arm, block_name = "Mean ALT + 95% CI by arm"),
      cloud = new_chart_block(
        chart_type = "scatter", x = "ADY", y = "AVAL", color = "TRT01A",
        block_name = "Dense cloud (no blur on hover)"
      ),
      forest = new_chart_block(
        chart_type = "scatter", x = "WEEK", y = "MEAN", lo = "LO", hi = "HI",
        block_name = "Scatter + CI whiskers (no blur)"
      ),
      crowd = new_chart_block(
        chart_type = "line", x = "ADY", y = "AVAL", series = "USUBJID",
        color = "TRT01A", block_name = "10 trajectories (veil KEPT)"
      ),
      single = new_chart_block(
        chart_type = "line", x = "WEEK", y = "MEAN", lo = "LO", hi = "HI",
        block_name = "One line + CI (veil DROPPED)"
      ),
      two_arm = new_chart_block(
        chart_type = "line", x = "WEEK", y = "MEAN", series = "TRT01A",
        lo = "LO", hi = "HI", block_name = "Three arms + CI (veil KEPT)"
      )
    ),
    links = list(
      list(from = "cloud_data", to = "cloud", input = "data"),
      list(from = "single_data", to = "forest", input = "data"),
      list(from = "crowd_data", to = "crowd", input = "data"),
      list(from = "single_data", to = "single", input = "data"),
      list(from = "two_data", to = "two_arm", input = "data")
    ),
    extensions = new_dag_extension(),
    grids = list(
      Scatter = dock_grid("cloud", "forest"),
      # crowd / single / two_arm together: the veil's whole threshold (>1 line)
      # is visible in one view, no tab switching to compare.
      Line = dock_grid("crowd", "single", "two_arm")
    ),
    # BLOCKR_VIEW picks the view the board opens on, so an automated probe can
    # land straight on the line charts instead of driving the view dropdown.
    active = Sys.getenv("BLOCKR_VIEW", "Scatter")
  )
)
