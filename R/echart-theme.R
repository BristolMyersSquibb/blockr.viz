# The board's ECharts theme: the option, and the reactive the chart reads.
# The JS registers the "blockr" theme itself
# (inst/js/drilldown-theme-register.js).

# The board's theme option as a reactive theme name ("default", "blockr", ...).
board_echart_theme <- function() {
  shiny::reactive({
    sess <- blockr.core::get_session()
    blockr.core::get_board_option_or_null("echart_theme", sess) %||% "default"
  })
}

#' ECharts theme board option for drill-down charts
#'
#' Board option that adds an "ECharts Theme" selector to the board sidebar.
#' It matches `blockr.echarts::new_echart_theme_option()`.
#'
#' @param value Default theme name.
#' @param category Settings sidebar category.
#' @param ... Forwarded to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#' @examples
#' new_drilldown_theme_option()
#' @export
new_drilldown_theme_option <- function(value = "default",
                                       category = "Chart options", ...) {
  new_input_option(
    "echart_theme", value,
    input = function(inputId) {
      shiny::selectInput(
        inputId, "ECharts Theme",
        choices = c(
          "Default" = "default", "Blockr" = "blockr", "Dark" = "dark",
          "Vintage" = "vintage", "Westeros" = "westeros", "Essos" = "essos",
          "Wonderland" = "wonderland", "Walden" = "walden",
          "Chalk" = "chalk", "Infographic" = "infographic",
          "Macarons" = "macarons", "Roma" = "roma", "Shine" = "shine",
          "Purple Passion" = "purple-passion"
        ),
        selected = value
      )
    },
    update = function(session, id, val) {
      shiny::updateSelectInput(session, id, selected = val)
    },
    category = category, ...
  )
}
