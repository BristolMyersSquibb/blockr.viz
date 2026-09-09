# Chart block `script`: a prepare step whose declarations become controls in
# the strip above the chart. Three charts, so it can be compared side by side.
#
#   Rscript dev/prepare-script-demo.R
pkgload::load_all("/workspace/blockr.viz", quiet = TRUE)

library(blockr.core)

# 1. No script. The other nineteen in twenty: no strip at all, and the gear
#    carries one extra header line.
plain <- new_chart_block(
  chart_type = "bar", group = "Species", value = ".count", func = "count",
  subtitle = "counts by {@group}[, coloured by {@color}]", title = "No script"
)

# 2. A script with three kinds of knob: a number, a multi-select fed from the
#    data, and a flag. All three land in the strip.
knobs <- new_chart_block(
  chart_type = "bar", group = "Species", value = "Sepal.Length",
  func = "mean", title = "Script knobs",
  subtitle = "mean {label(@value)} by {@group}[, coloured by {@color}]",
  script = paste(
    "min_len <- 5           #| number(min = 4, max = 8, step = 0.1)",
    "species <- factor(c(\"setosa\", \"versicolor\"), levels = unique(data$Species))",
    "wide_only <- FALSE",
    "",
    ".d <- dplyr::filter(data, Species %in% species, Sepal.Length >= min_len)",
    "if (wide_only) .d <- dplyr::filter(.d, Sepal.Width > 3)",
    ".d",
    sep = "\n"
  )
)

# 3. A script that computes a column the upstream data does not have, which is
#    the case a mapping cannot reach at all.
derived <- new_chart_block(
  chart_type = "scatter", x = "Sepal.Length", y = "ratio",
  title = "Derived column",
  script = paste(
    "round_to <- 2  #| number(min = 0, max = 4)",
    "dplyr::mutate(data, ratio = round(Sepal.Length / Sepal.Width, round_to))",
    sep = "\n"
  )
)

board <- new_board(
  blocks = c(
    data = new_dataset_block("iris", package = "datasets"),
    plain = plain, knobs = knobs, derived = derived
  ),
  links = c(
    new_link("data", "plain", "data"),
    new_link("data", "knobs", "data"),
    new_link("data", "derived", "data")
  )
)

port <- blockr_port()
cat("\n  http://127.0.0.1:", port, "\n\n", sep = "")
app <- serve(board)
shiny::runApp(app, port = port, host = "0.0.0.0")
