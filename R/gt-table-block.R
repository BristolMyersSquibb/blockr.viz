#' gt Table Renderer for the Table-Blocks Quartet
#'
#' Renders the output of [summary_table()] (or any block following the
#' "wide tibble with dotted section columns" convention) as a styled gt
#' table. Pivoted display grids are produced upstream by composing
#' `summarize` with `tidyr::pivot_wider`, not by a dedicated block.
#' Also supports the legacy
#' long-format input from `blockr.sandbox`'s `tidy_summary_block` and
#' `occurrence_summary_block` via a separate code path.
#'
#' Column headers are driven by `attr(col, "label")` — a standard R
#' convention. `gt::gt()` reads these labels natively. If a column
#' has no label attribute, the column name is used. Upstream blocks
#' (including [summary_table()]) can set labels on the columns they
#' produce, and the renderer picks them up automatically. Users who
#' want custom labels can use a `mutate_block` upstream to overwrite
#' `attr(col, "label")`.
#'
#' @param data A data.frame. Either:
#'   - **New wide format**: plain tibble with dotted columns
#'     `.section_1, ..., .section_k` (optional), `.strong` (optional),
#'     `.label` (optional), and data cells (pipe-delimited if nested).
#'   - **Legacy long format**: has `label`, `depth`, `col_var` columns
#'     plus stat columns (`n`/`N`/`pct` or `mean`/`sd` or `value`).
#' @param title Optional table title.
#' @param subtitle Optional subtitle shown under the title.
#' @param full_width Logical. If `TRUE` (default) the table stretches
#'   to 100% width. Set `FALSE` for auto-sized.
#' @param borders Logical. If `TRUE` (default) a 2px solid border
#'   brackets the table top and bottom, and the title area gets a
#'   2px bottom border.
#' @param na_rep Character. Text to display for missing (`NA`) cells.
#'   Default `"\u2014"` (em dash), the clinical-table convention. Use
#'   `""` for blank, `"NA"` for gt's built-in default, or any other
#'   string.
#' @return A `gt_tbl` object.
#'
#' @examples
#' tbl <- summary_table(iris, vars = "Sepal.Length", by = "Species")
#' gt_table(tbl, title = "Sepal length by species")
#' @export
gt_table <- function(data, title = NULL, subtitle = NULL,
                     full_width = TRUE, borders = TRUE,
                     na_rep = "\u2014") {
  if (is.null(title) || !nzchar(title)) {
    title <- attr(data, "label")
  }

  # Tidy `.fmt` form (numbers + per-row template + `.group`) → wide
  # display grid (format-then-spread). No-op on already-wide input.
  data <- fmt_to_wide(data)

  # Dual-path detection: if the input has the legacy long-format
  # columns, route to the legacy renderer. Otherwise treat it as the
  # new wide-tibble format.
  if (all(c("label", "depth") %in% names(data)) &&
      any(c("col_var", "n", "value") %in% names(data))) {
    return(gt_table_legacy(data, title = title, na_rep = na_rep))
  }

  gt_table_wide(
    data,
    title      = title,
    subtitle   = subtitle,
    full_width = full_width,
    borders    = borders,
    na_rep     = na_rep
  )
}

# ---------------------------------------------------------------------------
# New wide-tibble renderer
# ---------------------------------------------------------------------------

#' @noRd
gt_table_wide <- function(data, title = NULL, subtitle = NULL,
                          full_width = TRUE, borders = TRUE,
                          na_rep = "\u2014") {
  # Section columns are any dotted `.section_N` columns.
  section_cols <- grep("^\\.section_\\d+$", names(data), value = TRUE)
  stub_col <- if (".label" %in% names(data)) ".label" else NULL

  # Row-level styling columns (hidden from display, drive tab_style).
  # See blockr.design/open/table-blocks/3-design.md amendment.
  styling_cols <- intersect(c(".indent", ".strong", ".emph"), names(data))

  # Data columns = everything that isn't section, stub, or styling
  data_cols <- setdiff(names(data), c(section_cols, stub_col, styling_cols))

  # gt::gt() with groupname_col handles section rendering natively.
  # Column labels flow from `attr(col, "label")` — gt reads them
  # automatically. The shaper (summary_table) is responsible for
  # setting them; the renderer stays dumb.
  tbl <- if (length(section_cols) > 0L || !is.null(stub_col)) {
    args <- list(data)
    if (length(section_cols) > 0L) args$groupname_col <- section_cols
    if (!is.null(stub_col)) args$rowname_col <- stub_col
    do.call(gt::gt, args)
  } else {
    gt::gt(data)
  }

  # Hide the row-styling columns from display — they drive tab_style
  # below but are not rendered as their own columns.
  if (length(styling_cols) > 0L) {
    tbl <- gt::cols_hide(tbl, columns = dplyr::all_of(styling_cols))
  }

  tbl <- gt::sub_missing(tbl, missing_text = na_rep %||% "")

  # Pipe-delimited column spanners (for length-2 `by`)
  if (any(grepl("||", names(data), fixed = TRUE))) {
    tbl <- gt::tab_spanner_delim(tbl, delim = "||")
  }

  # Column labels: render newlines as <br> for clean two-line headers
  # (e.g. "Placebo\nN = 121" → "Placebo<br>N = 121"). No bold/styling.
  col_labels <- list()
  for (cn in data_cols) {
    lbl <- attr(data[[cn]], "label")
    if (!is.null(lbl) && is.character(lbl) && nzchar(lbl)) {
      if (grepl("\n", lbl, fixed = TRUE)) {
        col_labels[[cn]] <- gt::html(gsub("\n", "<br>", lbl))
      } else {
        col_labels[[cn]] <- lbl
      }
    }
  }
  if (length(col_labels) > 0L) {
    tbl <- gt::cols_label(tbl, .list = col_labels)
  }

  # Title + subtitle
  if (!is.null(title) && nzchar(title)) {
    if (!is.null(subtitle) && nzchar(subtitle)) {
      tbl <- gt::tab_header(tbl, title = title, subtitle = subtitle)
    } else {
      tbl <- gt::tab_header(tbl, title = title)
    }
  }

  # Row-level styling driven by hidden `.indent` / `.bold` / `.italic`
  # columns. `INDENT_PX` is hardcoded to 16 for now — no UI control,
  # no block parameter. See spec amendment in 3-design.md.
  INDENT_PX <- 16L
  if (!is.null(stub_col) && ".indent" %in% styling_cols) {
    lvls <- suppressWarnings(as.integer(data[[".indent"]]))
    for (lvl in sort(unique(lvls[!is.na(lvls) & lvls > 0L]))) {
      rows <- which(!is.na(lvls) & lvls == lvl)
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(indent = gt::px(lvl * INDENT_PX)),
        locations = gt::cells_stub(rows = rows)
      )
    }
  }
  if (!is.null(stub_col) && ".strong" %in% styling_cols) {
    flag <- as.logical(data[[".strong"]])
    bold_rows <- which(!is.na(flag) & flag)
    if (length(bold_rows)) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_stub(rows = bold_rows)
      )
    }
  }
  if (!is.null(stub_col) && ".emph" %in% styling_cols) {
    flag <- as.logical(data[[".emph"]])
    italic_rows <- which(!is.na(flag) & flag)
    if (length(italic_rows)) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_stub(rows = italic_rows)
      )
    }
  }

  # Table options
  tab_opts <- list(
    tbl,
    table.font.size           = gt::px(13),
    column_labels.font.weight = "bold",
    row_group.font.weight     = "bold"
  )
  if (isTRUE(full_width)) {
    tab_opts$table.width <- gt::pct(100)
  }
  if (isTRUE(borders)) {
    tab_opts$table.border.top.style      <- "solid"
    tab_opts$table.border.top.width      <- gt::px(2)
    tab_opts$table.border.bottom.style   <- "solid"
    tab_opts$table.border.bottom.width   <- gt::px(2)
    tab_opts$heading.border.bottom.style <- "solid"
    tab_opts$heading.border.bottom.width <- gt::px(2)
  }
  tbl <- do.call(gt::tab_options, tab_opts)

  tbl
}

# ---------------------------------------------------------------------------
# Legacy long-format renderer
# (preserved from blockr.sandbox::gt_clinical_table for back-compat
# with tidy_summary_block / occurrence_summary_block output during
# the deprecation window)
# ---------------------------------------------------------------------------

#' @noRd
prepare_table_wide <- function(data) {
  has_nesting <- "col_var_1" %in% names(data)

  if (all(c("n", "N", "pct") %in% names(data))) {
    data$value <- sprintf("%d (%0.1f%%)", data$n, data$pct)
    denom_map <- unique(data[, c("col_var", "N"), drop = FALSE])
  } else if (all(c("mean", "sd") %in% names(data))) {
    data$value <- sprintf("%.1f (%.1f)", data$mean, data$sd)
    denom_df <- unique(data[data$depth == 0L, c("col_var", "n"), drop = FALSE])
    denom_map <- denom_df
    names(denom_map)[names(denom_map) == "n"] <- "N"
  } else if ("value" %in% names(data)) {
    d0 <- data[data$depth == 0L, ]
    if (nrow(d0) > 0 && all(grepl("^\\d+$", trimws(d0$value)))) {
      denom_map <- data.frame(
        col_var = d0$col_var,
        N = as.integer(trimws(d0$value)),
        stringsAsFactors = FALSE
      )
    } else {
      denom_map <- data.frame(
        col_var = unique(data$col_var),
        N = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  } else {
    stop("Data must contain stat columns (n/N/pct or mean/sd) or a pre-formatted 'value' column")
  }

  n_cols <- length(unique(data$col_var))
  data$.row_id <- rep(seq_len(ceiling(nrow(data) / n_cols)), each = n_cols)[seq_len(nrow(data))]

  if (has_nesting) {
    data$col_key <- paste0(data$col_var, " || ", data$col_var_1)
    wide <- data |>
      dplyr::select(".row_id", "label", "depth", "col_key", "value") |>
      tidyr::pivot_wider(names_from = "col_key", values_from = "value", values_fill = list(value = "0")) |>
      dplyr::select(-".row_id")
  } else {
    wide <- data |>
      dplyr::select(".row_id", "label", "depth", "col_var", "value") |>
      tidyr::pivot_wider(names_from = "col_var", values_from = "value", values_fill = list(value = "0")) |>
      dplyr::select(-".row_id")
  }

  col_names <- setdiff(names(wide), c("label", "depth"))

  list(
    wide = wide,
    denom_map = denom_map,
    col_names = col_names,
    has_nesting = has_nesting,
    data = data
  )
}

#' @noRd
gt_table_legacy <- function(data, title = NULL, na_rep = "\u2014") {
  prep <- prepare_table_wide(data)
  wide <- prep$wide

  max_depth <- max(wide$depth)
  tbl <- gt::gt(wide |> dplyr::select(-"depth"))
  tbl <- gt::sub_missing(tbl, missing_text = na_rep %||% "")

  if (!is.null(title)) {
    tbl <- gt::tab_header(tbl, title = title)
  }

  if (prep$has_nesting) {
    col_keys <- prep$col_names
    for (key in col_keys) {
      parts <- strsplit(key, " \\|\\| ", fixed = FALSE)[[1]]
      cv <- parts[1]
      cv1 <- parts[2]
      n_val <- prep$denom_map$N[prep$denom_map$col_var == cv][1]
      new_label <- sprintf("%s (N=%d)", cv1, n_val)
      col_labels <- stats::setNames(list(new_label), key)
      tbl <- gt::cols_label(tbl, .list = col_labels)
    }
    for (cv in unique(prep$data$col_var)) {
      matching_cols <- grep(paste0("^", gsub("([.|()\\^{}+$*?])", "\\\\\\1", cv), " \\|\\| "),
                            col_keys, value = TRUE)
      if (length(matching_cols) > 0) {
        tbl <- gt::tab_spanner(tbl, label = cv, columns = dplyr::all_of(matching_cols))
      }
    }
  } else {
    col_labels <- stats::setNames(
      lapply(prep$col_names, function(cn) {
        n_val <- prep$denom_map$N[prep$denom_map$col_var == cn][1]
        if (is.na(n_val)) cn else sprintf("%s (N=%d)", cn, n_val)
      }),
      prep$col_names
    )
    tbl <- gt::cols_label(tbl, .list = col_labels)
  }

  tbl <- gt::cols_label(tbl, label = "")

  bold_rows <- which(wide$depth < max_depth)
  if (length(bold_rows) > 0) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = bold_rows)
    )
  }

  indent_rows <- which(wide$depth == max_depth)
  if (length(indent_rows) > 0) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(indent = gt::px(20)),
      locations = gt::cells_body(columns = "label", rows = indent_rows)
    )
  }

  d0_rows <- which(wide$depth == 0L)
  if (length(d0_rows) > 0) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_borders(sides = "bottom", weight = gt::px(1)),
      locations = gt::cells_body(rows = d0_rows)
    )
  }

  tbl <- gt::tab_options(
    tbl,
    table.font.size = gt::px(13),
    column_labels.font.weight = "bold"
  )

  tbl
}

# ---------------------------------------------------------------------------
# Block: gt Table
# ---------------------------------------------------------------------------

# Scoped CSS for the gt block's gear settings form. Same config-form treatment
# as the design system (stacked full-width fields, a 12px/600 label, a 36px
# control on a soft grey fill with an 8px radius and a blue focus ring) so the
# gear matches the renderer / the other blocks' popovers instead of reading as
# default Bootstrap. Tokens map to blockr CSS vars with design-hex fallbacks.
#' @noRd
gt_gear_css <- function() {
  paste(
    ".gt-gear{display:flex;flex-direction:column;gap:11px;padding-top:2px;}",
    ".gt-gear .shiny-input-container,.gt-gear .form-group{width:100%!important;margin:0!important;}",
    ".gt-gear .control-label{display:block;font-size:12px;font-weight:600;",
    "color:var(--blockr-color-text-secondary,#5b6573);margin-bottom:5px;}",
    ".gt-gear .form-control{height:36px;padding:0 11px;font-size:13px;",
    "color:var(--blockr-color-text-primary,#111827);",
    "background:var(--blockr-color-bg-subtle,#f6f8fa);",
    "border:1px solid var(--blockr-color-border,#e8ebef);",
    "border-radius:8px;box-shadow:none;width:100%;",
    "transition:border-color .15s ease,background .15s ease,box-shadow .15s ease;}",
    ".gt-gear .form-control::placeholder{color:var(--blockr-color-text-muted,#9aa3b0);}",
    ".gt-gear .form-control:focus{background:#fff;",
    "border-color:var(--blockr-color-primary,#2563eb);",
    "box-shadow:0 0 0 3px rgba(37,99,235,.12);outline:none;}",
    ".gt-gear__toggles{display:flex;gap:22px;align-items:center;margin-top:2px;}",
    ".gt-gear__toggles .checkbox{margin:0;}",
    ".gt-gear__toggles .checkbox label{font-size:13px;font-weight:500;",
    "color:var(--blockr-color-text-primary,#111827);",
    "display:inline-flex;align-items:center;gap:7px;}",
    ".gt-gear__toggles .checkbox input[type=checkbox]{margin:0;}",
    collapse = ""
  )
}

#' gt Table Block
#'
#' Blockr transform block wrapping [gt_table()]. Accepts the output of
#' [summary_table()] (or any block following the wide-dotted-column
#' convention) and renders it as a styled gt table.
#'
#' Column headers come from `attr(col, "label")` on the data columns,
#' which gt reads natively. Upstream blocks set these labels (for
#' instance, [summary_table()] writes the per-column subject count
#' as the label). The renderer does not compute column labels itself.
#'
#' @param title Optional table title.
#' @param subtitle Optional subtitle shown under the title.
#' @param full_width Logical. 100% width table. Default `TRUE`.
#' @param borders Logical. 2px top/bottom/heading borders. Default `TRUE`.
#' @param na_rep Character. Text shown for missing cells. Default
#'   `"\u2014"` (em dash).
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block of class `gt_table_block`.
#' @examplesIf interactive()
#' new_gt_table_block()
#' @export
new_gt_table_block <- function(title = "",
                               subtitle = "",
                               full_width = TRUE,
                               borders = TRUE,
                               na_rep = "\u2014",
                               ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        r_title      <- shiny::reactiveVal(title)
        r_subtitle   <- shiny::reactiveVal(subtitle)
        r_full_width <- shiny::reactiveVal(isTRUE(full_width))
        r_borders    <- shiny::reactiveVal(isTRUE(borders))
        r_na_rep     <- shiny::reactiveVal(na_rep %||% "")

        shiny::observeEvent(input$title,      r_title(input$title))
        shiny::observeEvent(input$subtitle,   r_subtitle(input$subtitle))
        shiny::observeEvent(input$full_width, r_full_width(isTRUE(input$full_width)))
        shiny::observeEvent(input$borders,    r_borders(isTRUE(input$borders)))
        shiny::observeEvent(input$na_rep,     r_na_rep(input$na_rep), ignoreNULL = FALSE)

        list(
          expr = shiny::reactive({
            ttl <- r_title()
            sub <- r_subtitle()
            nar <- r_na_rep()
            bquote(
              blockr.viz::gt_table(
                data,
                title      = .(ttl),
                subtitle   = .(sub),
                full_width = .(r_full_width()),
                borders    = .(r_borders()),
                na_rep     = .(nar)
              )
            )
          }),
          state = list(
            title      = r_title,
            subtitle   = r_subtitle,
            full_width = r_full_width,
            borders    = r_borders,
            na_rep     = r_na_rep
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::tags$style(htmltools::HTML(gt_gear_css())),
        shiny::div(
          class = "block-container gt-gear",
          shiny::div(
            class = "gt-gear__field",
            shiny::textInput(ns("title"), "Title", value = title, width = "100%")
          ),
          shiny::div(
            class = "gt-gear__field",
            shiny::textInput(ns("subtitle"), "Subtitle", value = subtitle, width = "100%")
          ),
          shiny::div(
            class = "gt-gear__field",
            shiny::textInput(ns("na_rep"), "NA display",
                             value = na_rep, width = "100%",
                             placeholder = "\u2014")
          ),
          shiny::div(
            class = "gt-gear__toggles",
            shiny::checkboxInput(ns("full_width"), "Full width",
                                 value = isTRUE(full_width)),
            shiny::checkboxInput(ns("borders"), "Borders",
                                 value = isTRUE(borders))
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame")

      # Accept either:
      # 1) New wide format: plain tibble with dotted columns
      is_wide <- !any(c("label", "depth", "col_var") %in% names(data)) &&
        (".label" %in% names(data) ||
         any(grepl("^\\.(section_\\d+|var|indent|bold|italic)$", names(data))))
      if (is_wide) return(invisible(NULL))

      # 2) Legacy long format
      required <- c("label", "depth")
      missing <- setdiff(required, names(data))
      if (length(missing) > 0) {
        stop("Missing required columns: ", paste(missing, collapse = ", "))
      }
      has_occurrence <- all(c("n", "N", "pct") %in% names(data))
      has_continuous <- all(c("mean", "sd") %in% names(data))
      has_value <- "value" %in% names(data)
      if (!has_occurrence && !has_continuous && !has_value) {
        stop("Data must contain stat columns (n/N/pct or mean/sd) or a 'value' column")
      }
    },
    class = "gt_table_block",
    external_ctrl = TRUE,
    allow_empty_state = c("title", "subtitle", "na_rep"),
    ...
  )
}

#' @importFrom blockr.core block_ui
#' @method block_ui gt_table_block
#' @export
block_ui.gt_table_block <- function(id, x, ...) {
  shiny::tagList(
    gt::gt_output(shiny::NS(id, "result"))
  )
}

#' @importFrom blockr.core block_output
#' @method block_output gt_table_block
#' @export
block_output.gt_table_block <- function(x, result, session) {
  gt::render_gt(result)
}
