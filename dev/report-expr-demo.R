# What a chart block's report code looks like under the two styles.
#
# Style "code" (the feat/report-expr default): report_call() compiles the
# block state to a plain dplyr + ggplot2 pipeline -- the document reproduces
# the chart with no blockr dependency. Style "static": the original
# blockr.viz::static_chart(<var>, <state...>) call.
#
# Run: Rscript dev/report-expr-demo.R

pkgload::load_all(".", quiet = TRUE)

d <- transform(datasets::iris, Grp = rep(c("A", "B"), 75))

show <- function(label, ...) {
  cat("\n== ", label, " ", strrep("=", max(1L, 60L - nchar(label))), "\n\n",
      sep = "")
  cat(chart_code(chart_expr("chart1", ..., data = d)), "\n")
}

show("stacked bar", "bar", group = "Species", color = "Grp")
show("grouped vertical bar, mean", "bar", group = "Species", color = "Grp",
     value = "Sepal.Width", func = "mean", bar_mode = "grouped",
     orientation = "vertical")
show("percent bar + facet + counts", "bar", group = "Species", color = "Grp",
     bar_mode = "percent", facet = "Grp",
     count_on = "axis", title = "Split across {n} rows")
show("boxplot", "boxplot", group = "Species", value = "Sepal.Width",
     color = "Grp", box_points = "all")
show("scatter", "scatter", x = "Sepal.Length", y = "Sepal.Width",
     color = "Species", smoother = "lm", identity_line = TRUE)
show("line", "line", x = "Petal.Length", y = "Petal.Width",
     series = "Species", color = "Species")

# The same state through a real block: report_call() is what blockr.outline
# consumes. Default style compiles to code (self-qualified); the static
# style is one option away.
cat("\n== report_call(), default (code) ", strrep("=", 27), "\n\n", sep = "")
b <- new_chart_block(chart_type = "bar", group = "Species", color = "Grp")
cat(chart_code(report_call(b, "chart1")), "\n")

cat("\n== report_call(), static ", strrep("=", 35), "\n\n", sep = "")
options(blockr.viz.report_style = "static")
cat(deparse(report_call(b, "chart1")), sep = "\n")
options(blockr.viz.report_style = NULL)
