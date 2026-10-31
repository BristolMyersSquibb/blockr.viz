# Two tables in a card narrower than their columns, for
# test-table-fixed-layout.R: a composer-shaped annotated frame (indented,
# bold parameter rows in the row-label column) and a plain data frame (the
# records layout). Both carry R's column widths.
#
# Loads blockr.viz from BLOCKR_VIZ_SRC (the source tree under test) when set.

src <- Sys.getenv("BLOCKR_VIZ_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else library(blockr.viz)

params <- c("Absolute Neutrophil Count (10^9/L)",
            "Alanine Aminotransferase (U/L)")
visits <- c("Baseline", "Week 4", "Week 8")
stats <- c("N", "Mean", "SD", "Min", "Median", "Max")

label <- c("All", unlist(lapply(params, function(p) c(p, visits))))
indent <- c(0L, rep(c(1L, 2L, 2L, 2L), length(params)))
num <- ifelse(indent == 2L, "12.34", "")
structured <- data.frame(.label = label, .indent = indent,
                         .strong = indent < 2L, stringsAsFactors = FALSE)
for (s in stats) structured[[paste0("Value||", s)]] <- num

flat <- data.frame(Parameter = params, stringsAsFactors = FALSE)
for (s in stats) flat[[s]] <- c(12.34, 56.78)

card <- function(x, id) {
  htmltools::div(style = "width: 480px; margin: 16px; border: 1px solid #ddd;",
                 interactive_table(x, drill = FALSE, elem_id = id))
}

shiny::shinyApp(
  shiny::fluidPage(card(structured, "structured"), card(flat, "flat")),
  function(input, output, session) {}
)
