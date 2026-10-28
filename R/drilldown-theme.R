# Vendored from blockr.echarts/R/echart-theme.R so the chart block does not
# depend on blockr.echarts. With both loaded the theme name is the same and
# registration is idempotent.

.drilldown_theme_env <- new.env(parent = emptyenv())

#' Register the drill-down echarts theme
#'
#' Registers the colorblind-friendly "blockr" echarts theme. Called from
#' `.onLoad()`.
#'
#' @return NULL, invisibly.
#' @keywords internal
echart_theme_blockr_viz <- function() {
  if (isTRUE(.drilldown_theme_env$registered)) {
    return(invisible(NULL))
  }
  .drilldown_theme_env$registered <- TRUE

  echarts4r::e_theme_register(
    name = "blockr",
    theme = '{
      "color": ["#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00"],
      "backgroundColor": "#ffffff",
      "textStyle": {"color": "#333333", "fontFamily": "Open Sans"}
    }'
  )
  invisible(NULL)
}

#' Reactive for the board-level echarts theme
#'
#' Returns a reactive that tracks the board's `echart_theme` option.
#'
#' @param session Ignored. The board options come from
#'   `blockr.core::get_session()`.
#' @return A reactive resolving to a theme name (e.g. `"default"`, `"blockr"`).
#' @keywords internal
setup_drilldown_theme_sync <- function(session = NULL) {
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
  blockr.core::new_board_option(
    id = "echart_theme",
    default = value,
    ui = function(id) {
      shiny::selectInput(
        shiny::NS(id, "echart_theme"),
        "ECharts Theme",
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
    server = function(..., session) {
      shiny::observeEvent(
        blockr.core::get_board_option_or_null("echart_theme", session),
        shiny::updateSelectInput(
          session, "echart_theme",
          selected = blockr.core::get_board_option_value(
            "echart_theme", session
          )
        )
      )
    },
    category = category,
    ...
  )
}
