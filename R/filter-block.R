#' Filter block
#'
#' Minimal value filter for data frames. The columns to filter on are chosen
#' behind the gear icon (top-right of the block). Each column can be toggled
#' between single-select (always constrains — auto-picks first value) and
#' multi-select (empty selection passes through).
#'
#' Gear/popover UX, select widget, and click-through pill styling are reused
#' from `blockr.dplyr` to match the look-and-feel of the crossfilter and dplyr
#' transform blocks.
#'
#' @param state List with `columns` (character vector of active columns),
#'   `modes` (named list: column -> "single" | "multi"), and `values` (named
#'   list: column -> character vector of selected values).
#' @param ... Additional arguments forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.bi)
#'   serve(
#'     new_bi_filter_block(
#'       state = list(
#'         columns = "Species",
#'         modes = list(Species = "single"),
#'         values = list(Species = "setosa")
#'       )
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @importFrom blockr.dplyr blockr_core_js_dep blockr_blocks_css_dep blockr_select_dep
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_bi_filter_block <- function(
  state = list(
    columns = character(),
    modes = list(),
    values = list()
  ),
  ...
) {
  blockr.core::new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- shiny::reactiveVal(state)

        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # Send column metadata + per-column unique values on data change.
        shiny::observeEvent(data(), {
          df <- data()
          if (!is.data.frame(df) || ncol(df) == 0) return()
          session$sendCustomMessage(
            "bi-filter-columns",
            list(
              id = ns("filter_input"),
              columns = build_column_meta(df),
              values = build_value_options(df)
            )
          )
          # Re-apply single-select rule against fresh data.
          s <- enforce_single_rule(r_state(), df)
          if (!identical(s, r_state())) {
            self_write$active <- FALSE
            r_state(s)
          }
        })

        # JS -> R: user changed state.
        shiny::observeEvent(input$filter_input, {
          self_write$active <- TRUE
          s <- enforce_single_rule(input$filter_input, shiny::isolate(data()))
          r_state(s)
        })

        # R -> JS: external control or server-side rewrite.
        shiny::observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "bi-filter-update",
              list(id = ns("filter_input"), state = normalize_state_for_json(r_state()))
            )
          }
        })

        list(
          expr = shiny::reactive({
            s <- r_state()
            make_filter_block_expr(
              s$columns %||% character(),
              s$modes   %||% list(),
              s$values  %||% list(),
              shiny::isolate(data())
            )
          }),
          state = list(state = r_state)
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    function(id) {
      shiny::tagList(
        blockr.dplyr::blockr_core_js_dep(),
        blockr.dplyr::blockr_blocks_css_dep(),
        blockr.dplyr::blockr_select_dep(),
        bi_filter_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(
            id = shiny::NS(id, "filter_input"),
            class = "bi-filter-container"
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    class = "bi_filter_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' Build `{value, label}` column metadata list for the JS side.
#' @noRd
build_column_meta <- function(df) {
  lapply(names(df), function(cn) {
    lbl <- attr(df[[cn]], "label", exact = TRUE)
    list(
      value = cn,
      label = if (is.null(lbl)) "" else as.character(lbl)[1L]
    )
  })
}

#' Build per-column value options for the JS side.
#' If the column has haven-style `labels` attribute (named vector), the options
#' are `{value, label}` pairs. Otherwise, plain stringified unique values.
#' @noRd
build_value_options <- function(df) {
  out <- lapply(names(df), function(cn) {
    col <- df[[cn]]
    labs <- attr(col, "labels", exact = TRUE)
    uv <- unique(col)
    uv <- uv[!is.na(uv)]
    if (length(uv) == 0) return(list())
    # Stable ordering: factor levels preserve order; otherwise sort.
    if (is.factor(col)) {
      uv <- uv[order(match(as.character(uv), levels(col)))]
    } else if (is.numeric(uv) || is.logical(uv)) {
      uv <- sort(uv)
    } else {
      uv <- sort(as.character(uv))
    }
    if (!is.null(labs) && is.vector(labs) && !is.null(names(labs))) {
      lab_names <- names(labs)
      lapply(uv, function(v) {
        idx <- match(v, labs)
        list(
          value = as.character(v),
          label = if (is.na(idx)) "" else as.character(lab_names[idx])
        )
      })
    } else {
      as.list(as.character(uv))
    }
  })
  names(out) <- names(df)
  out
}

#' Enforce "single-select always has a value": any active single-mode column
#' without a selected value gets the first unique value.
#' @noRd
enforce_single_rule <- function(s, df) {
  if (is.null(s)) {
    return(list(columns = character(), modes = list(), values = list()))
  }
  cols <- s$columns %||% character()
  modes <- s$modes %||% list()
  values <- s$values %||% list()
  if (!is.data.frame(df) || length(cols) == 0) {
    return(list(columns = cols, modes = modes, values = values))
  }
  for (col in cols) {
    mode <- modes[[col]] %||% "single"
    if (identical(mode, "single")) {
      cur <- values[[col]]
      if (is.null(cur) || length(cur) == 0) {
        if (col %in% names(df)) {
          uv <- unique(df[[col]])
          uv <- uv[!is.na(uv)]
          if (length(uv) > 0) {
            if (is.factor(uv)) {
              uv <- uv[order(match(as.character(uv), levels(df[[col]])))]
            } else if (is.numeric(uv) || is.logical(uv)) {
              uv <- sort(uv)
            } else {
              uv <- sort(as.character(uv))
            }
            values[[col]] <- as.character(uv[[1]])
          }
        }
      }
    }
  }
  list(columns = cols, modes = modes, values = values)
}

#' Build the `dplyr::filter(...)` expression for the block.
#'
#' Multi-select with no selections is skipped (pass-through). If nothing
#' constrains the data, returns `dplyr::filter(data, TRUE)` to match
#' `blockr.dplyr`'s filter block empty semantics.
#' @noRd
make_filter_block_expr <- function(columns, modes, values, df) {
  if (length(columns) == 0) {
    return(bquote(dplyr::filter(data, TRUE)))
  }
  exprs <- list()
  for (col in columns) {
    v <- values[[col]]
    if (is.null(v) || length(v) == 0) next
    v <- as.character(v)
    # If the column is numeric/logical in the source df, coerce for a
    # type-matched membership test. R's %in% already coerces, but making it
    # explicit avoids surprise on grouped or stringsAsFactors inputs.
    casted <- v
    if (is.data.frame(df) && col %in% names(df)) {
      src <- df[[col]]
      if (is.numeric(src)) {
        num <- suppressWarnings(as.numeric(v))
        if (!any(is.na(num))) casted <- num
      } else if (is.logical(src)) {
        bool <- as.logical(v)
        if (!any(is.na(bool))) casted <- bool
      } else if (is.integer(src)) {
        intv <- suppressWarnings(as.integer(v))
        if (!any(is.na(intv))) casted <- intv
      }
    }
    sym <- as.name(col)
    exprs[[length(exprs) + 1L]] <- bquote(.(sym) %in% .(casted))
  }
  if (length(exprs) == 0) {
    return(bquote(dplyr::filter(data, TRUE)))
  }
  combined <- exprs[[1L]]
  if (length(exprs) > 1L) {
    for (i in seq.int(2L, length(exprs))) {
      combined <- bquote(.(combined) & .(exprs[[i]]))
    }
  }
  as.call(list(quote(dplyr::filter), quote(data), combined))
}

#' Convert state lists to a shape that survives `toJSON(auto_unbox = TRUE)`.
#' Length-1 vectors would otherwise serialize as JSON scalars and trip up the
#' JS side which expects arrays for `columns` and per-column `values`.
#' @noRd
normalize_state_for_json <- function(s) {
  if (is.null(s)) s <- list()
  s$columns <- as.list(s$columns %||% character())
  s$values  <- lapply(s$values %||% list(), as.list)
  s$modes   <- s$modes %||% list()
  s
}

#' HTML dependency for filter block JS + CSS
#' @noRd
bi_filter_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-bi-filter-js",
      version = utils::packageVersion("blockr.bi"),
      src = system.file("js", package = "blockr.bi"),
      script = "filter-block.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-bi-filter-css",
      version = utils::packageVersion("blockr.bi"),
      src = system.file("css", package = "blockr.bi"),
      stylesheet = "filter-block.css"
    )
  )
}
