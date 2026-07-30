# Static side of the static-chart parity harness: renders every state in
# dev/parity/states.R through static_chart() to static-<id>.png, sized to
# the canvas capture from sizes.csv when the drive script has run (falls
# back to 1500x800 px at 150 dpi).
#
#   Rscript dev/parity/static.R [outdir]

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else "/workspace/_scratch/static-chart-parity"

dir.create(out, recursive = TRUE, showWarnings = FALSE)

viz <- local({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  d <- if (length(f)) normalizePath(f[[1L]]) else "dev/parity/static.R"
  dirname(dirname(dirname(d)))
})
pkgload::load_all(viz, helpers = FALSE, attach_testthat = FALSE,
                  export_all = FALSE)

source(file.path(viz, "dev", "parity", "states.R"))

ds <- parity_datasets()

sizes <- local({
  f <- file.path(out, "sizes.csv")
  if (file.exists(f)) utils::read.csv(f) else NULL
})

# The canvas draws at CSS pixels (96/in); render at the same density so
# text and marks occupy the same relative space.
dpi <- 96

for (id in names(parity_states)) {
  s <- parity_states[[id]]
  px <- if (!is.null(sizes) && id %in% sizes$id) {
    sizes[sizes$id == id, c("width", "height")]
  } else {
    data.frame(width = 1500, height = 800)
  }
  # px caps (barMaxWidth, boxWidth) resolve against the render device.
  options(blockr.viz.gg_device_width = px$width / dpi)
  p <- do.call(static_chart, c(list(ds[[s$data]]), s$args))
  if (!inherits(p, "ggplot")) {
    message("fallback (no ggplot) for: ", id)
    next
  }
  f <- file.path(out, paste0("static-", id, ".png"))
  ggplot2::ggsave(
    f, p,
    width = px$width / dpi, height = px$height / dpi, dpi = dpi,
    bg = "white"
  )
  message("rendered ", f, " (", px$width, "x", px$height, ")")
}

message("static done")
