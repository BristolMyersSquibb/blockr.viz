# Vendored from blockr.echarts/R/echart-theme.R. Kept local so the drill-down
# block doesn't depend on blockr.echarts. If blockr.echarts and blockr.bi are
# both loaded, the theme name is identical and registration is idempotent.

.drilldown_theme_env <- new.env(parent = emptyenv())
.drilldown_theme_env$registered <- FALSE

#' Register the drill-down echarts theme
#'
#' Registers the colorblind-friendly "blockr" echarts theme. Called from
#' `.onLoad()` so any drill-down chart that renders after package load can
#' reference it by name.
#'
#' @return NULL, invisibly.
#' @keywords internal
echart_theme_blockr_bi <- function() {
  if (.drilldown_theme_env$registered) {
    return(invisible(NULL))
  }
  .drilldown_theme_env$registered <- TRUE

  echarts4r::e_theme_register(
    name = "blockr",
    theme = '{
      "color": ["#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#CC79A7"],
      "backgroundColor": "#ffffff",
      "textStyle": {"color": "#333333", "fontFamily": "Open Sans"}
    }'
  )
  invisible(NULL)
}

#' Reactive for the board-level echarts theme
#'
#' Returns a reactive that tracks the board's `echart_theme` option. Used by
#' the drill-down block to re-render charts when the theme changes.
#'
#' @param session Shiny session; ignored — the helper uses
#'   `blockr.core::get_session()` to reach the session that carries the
#'   board options.
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
#' Matches `blockr.echarts::new_echart_theme_option()` so boards can mix the
#' two mechanisms without collision.
#'
#' @param value Default theme name.
#' @param category Settings sidebar category.
#' @param ... Forwarded to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
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
          "Default" = "default",
          "Blockr" = "blockr",
          "Dark" = "dark",
          "Vintage" = "vintage",
          "Westeros" = "westeros",
          "Essos" = "essos",
          "Wonderland" = "wonderland",
          "Walden" = "walden",
          "Chalk" = "chalk",
          "Infographic" = "infographic",
          "Macarons" = "macarons",
          "Roma" = "roma",
          "Shine" = "shine",
          "Purple Passion" = "purple-passion"
        ),
        selected = value
      )
    },
    server = function(..., session) {
      shiny::observeEvent(
        blockr.core::get_board_option_or_null("echart_theme", session),
        {
          theme_val <- blockr.core::get_board_option_value(
            "echart_theme", session
          )
          shiny::updateSelectInput(
            session, "echart_theme", selected = theme_val
          )
        }
      )
    },
    category = category,
    ...
  )
}
