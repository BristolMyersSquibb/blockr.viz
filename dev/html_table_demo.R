# HTML Table Demo
#
# Exercises summary_table_block → html_table_block in a real blockr
# run_app() context, alongside gt_table_block for visual comparison.
#
# Layout:
#   [Data] --> [Summary Table] --> [HTML Table]
#                              \-> [gt Table]

library(blockr)
library(blockr.dag)
library(blockr.io)
library(blockr.bi)

app <- run_app(
  blocks = c(
    data = new_read_block(
      path = system.file("extdata", "bi_demo_data.csv", package = "blockr.bi")
    ),

    summary = new_summary_table_block(
      state = list(
        vars     = c("Revenue", "Profit", "Transactions"),
        sections = character(),
        by       = "Category",
        stats    = "expanded",
        add_overall = TRUE,
        overall_label = "Total"
      )
    ),

    html = new_html_table_block(
      title = "Summary by Category (HTML)",
      default_expanded = TRUE
    ),

    gt = new_gt_table_block(
      title = "Summary by Category (gt)"
    )
  ),
  links = c(
    new_link("data", "summary", "data"),
    new_link("summary", "html", "data"),
    new_link("summary", "gt", "data")
  ),
  extensions = list(new_dag_extension())
)

shiny::runApp(app)
