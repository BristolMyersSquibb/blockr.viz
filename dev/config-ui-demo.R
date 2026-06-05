# Dev harness for the drilldown config-UI overhaul (block-config-ui spec).
# From /workspace:  Rscript blockr.bi/dev/config-ui-demo.R
# then open http://127.0.0.1:3838/
pkgload::load_all("/workspace/blockr.core", quiet = TRUE)
pkgload::load_all("/workspace/blockr.bi", quiet = TRUE)

df <- mtcars
df$cyl  <- factor(df$cyl)
df$gear <- factor(df$gear)
df$am   <- factor(df$am, labels = c("auto", "manual"))
attr(df$mpg, "label") <- "Miles per gallon"
attr(df$hp,  "label") <- "Horsepower"
attr(df$wt,  "label") <- "Weight (1000 lbs)"

blk <- new_drilldown_chart_block(
  chart_type = "bar",
  group      = "cyl",
  metric     = ".count",
  agg_fn     = "count",
  drill      = "cyl"
)

shiny::runApp(
  serve(blk, data = list(data = df)),
  port = 3838, host = "0.0.0.0", launch.browser = FALSE
)
