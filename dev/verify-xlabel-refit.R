# Verify that widening a chart UNDOES the x-axis label thinning / rotation it
# needed while it was narrow (and that narrowing puts it back).
#
#   Rscript blockr.viz/dev/verify-xlabel-refit.R   (BLOCKR_PORT, else 3838)
#
# Both blocks put ~30 long category labels on x, so at a narrow width the axis
# has to rotate to vertical and (line only) skip labels to keep them from
# colliding. Resize the browser window and the labels have to re-fit:
#   line  — decimates: skipped labels come BACK as the panel widens.
#   bar   — never decimates, but must un-rotate once the labels fit flat.
options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(
  shiny.port = as.integer(
    Sys.getenv("BLOCKR_PORT", as.character(httpuv::randomPort()))
  ),
  shiny.host = "0.0.0.0"
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.viz")

# Ordered visit labels, long enough that they cannot sit flat side by side.
visits <- sprintf("Cycle %02d Day 1 Assessment", 1:30)
subjects <- sprintf("SUBJ-%03d", 1:6)
set.seed(42)
dat <- data.frame(
  visit = factor(rep(visits, times = length(subjects)), levels = visits),
  subject = rep(subjects, each = length(visits)),
  value = round(50 + cumsum(stats::rnorm(length(visits) * length(subjects), 0, 3)), 1)
)

# Short labels over few categories: rotated in a narrow panel, FLAT once wide.
# That flip changes the gutter, so it also exercises the canvas-height path.
weeks <- sprintf("Week %d", seq(2, 24, by = 2))
short <- data.frame(
  visit = factor(rep(weeks, times = 3), levels = weeks),
  subject = rep(sprintf("SUBJ-%03d", 1:3), each = length(weeks)),
  value = round(50 + cumsum(stats::rnorm(length(weeks) * 3, 0, 3)), 1)
)

cat(sprintf(
  "\n  http://127.0.0.1:%d/\n\n", getOption("shiny.port")
))

serve(
  new_dock_board(
    blocks = c(
      visit_data = new_static_block(dat, block_name = "Visit data"),
      line = new_chart_block(
        chart_type = "line", x = "visit", y = "value", series = "subject",
        block_name = "Line (rotates AND thins when narrow)"),
      bars = new_chart_block(
        chart_type = "bar", group = "visit", value = "value", func = "mean",
        orientation = "vertical",
        block_name = "Bar (rotates when narrow, never thins)"),
      short_data = new_static_block(short, block_name = "Short-label data"),
      short_line = new_chart_block(
        chart_type = "line", x = "visit", y = "value", series = "subject",
        block_name = "Line, short labels (un-rotates when widened)")
    ),
    links = list(
      list(from = "visit_data", to = "line", input = "data"),
      list(from = "visit_data", to = "bars", input = "data"),
      list(from = "short_data", to = "short_line", input = "data")
    ),
    extensions = new_dag_extension(),
    grids = list(Charts = dock_grid("line", "bars", "short_line"))
  )
)
