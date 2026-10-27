# The groups layout's view: the cell model (R/table-model-cells.R) as the
# table the browser draws, a TableModel (inst/js/table/table.d.ts, plus the
# `mark` and `fold` fields of tests/js/fixtures/table/groups-marks.d.ts.txt).
# The reference path writes a <thead> as HTML and ships the cells to
# summarize-table.js; this one sends the numbers and the browser lays out.
#
#   R/table-groups-view.R   the model: columns, header, rows
#   R/table-groups-marks.R  the `mark` of each column kind
#   R/table-groups-text.R   display text, axis domain and ticks, plain text
#
# What stays out of the model: the selection (`on`, `on_cell` and `pick` of
# the cell model; the browser lights rows from `config.selection`), the cards
# look (R/table-cards.R), the column widths (the cell model has none; marks
# size from `dw` in ch), and the chrome: summarize_legend_spec() is the
# legend and summarize_foot_spec() the footer line, both for `derived`.

#' The groups layout as the browser draws it
#'
#' Nesting: a two-level `by` is `rows$indent` (0 for a parent row, 1 for a
#' child) with `rows$strong` on the parent rows. Parent rows carry numbers of
#' their own, so they are rows, not `sections`. The Grid folds every row that
#' heads an indented run; `collapsed` starts the table folded.
#'
#' Claims: with the drill on, `rows$keys[[i]]` is the row's path as
#' `{column: [value]}` (a parent row its outer column, a child both). A facet
#' column's `keys` is its level, which a cell click adds to the row's claim;
#' the header cells over such a column carry the same claim.
#'
#' @param m table_model_groups()'s result, `list(prep, cells)`.
#' @param axis Whether the marks carry `ticks` (the column axis switch).
#' @return A TableModel list: `n`, `stub`, `cols`, `head`, `rows`,
#'   `collapsed` and `fold`. `NULL` when `m$cells` is `NULL` (the preparation
#'   failed), where the block says why instead of drawing.
#' @noRd
table_groups_view <- function(m, axis = TRUE) {
  cells <- m$cells
  if (is.null(cells)) {
    return(NULL)
  }
  prep <- m$prep
  pick <- isTRUE(cells$pick)
  ctx <- table_groups_ctx(prep)
  stub <- list(name = summarize_label_header(prep), disp = cells$label,
               num = FALSE)
  cols <- lapply(seq_along(cells$cols), function(i) {
    table_groups_col(cells$cols[[i]], prep$plan[[i]], prep, ctx, pick, axis)
  })
  nested <- isTRUE(cells$nested)
  out <- list(
    n = cells$n, stub = 0L, cols = c(list(stub), cols),
    head = table_groups_head(m, pick),
    rows = table_groups_rows(cells, pick),
    collapsed = if (nested && !isTRUE(cells$expanded)) TRUE,
    fold = cells$fold
  )
  table_view_arrays(table_groups_compact(out))
}

# The context summarize_cells() hands its column builders, with the value
# labels on: the labels of a column that draws none are rebuilt from it, for
# `disp` (table_groups_disp()).
table_groups_ctx <- function(prep) {
  list(rows = prep$rows, n = nrow(prep$rows), prep = prep, mx = prep$bar_max,
       mn = prep$bar_min %||% 0, vals_on = TRUE)
}

# One body column. A plain number is `num` (right-aligned); every other kind
# is left-aligned and sorts on `key`, the cell model's sort value. `keys` is
# the facet level a click on a cell adds to the row's claim.
table_groups_col <- function(c, p, prep, ctx, pick, axis) {
  is_num <- identical(c$kind, "num") && !isTRUE(c$text)
  fkey <- if (pick) c$fkey
  table_groups_compact(list(
    name = as.character(p$label %||% "")[[1L]],
    num = is_num,
    disp = table_groups_disp(c, p, ctx),
    key = if (!isTRUE(c$text)) c$v,
    keys = if (!is.null(fkey)) {
      table_structured_claim(fkey$column, list(fkey$value))
    },
    mark = table_groups_mark(c, p, prep, pick, axis)
  ))
}

# Header rows. One row, the stub's cell then one per column; the by_level
# facet layout adds a spanning row on top: the stub and the leading columns
# reach down both rows, and each facet level spans its summaries.
table_groups_head <- function(m, pick) {
  prep <- m$prep
  cells <- m$cells
  leaf <- function(i) {
    p <- prep$plan[[i]]
    fkey <- if (pick) cells$cols[[i]]$fkey
    table_groups_compact(list(
      text = as.character(p$label %||% "")[[1L]],
      sub = summarize_chr1(p$sub_label), col = i,
      keys = if (!is.null(fkey)) {
        table_structured_claim(fkey$column, list(fkey$value))
      }
    ))
  }
  stub <- table_groups_compact(list(
    text = summarize_label_header(prep), sub = prep$group_label, col = 0L,
    stub = TRUE
  ))
  fs <- prep$facet_spans
  if (is.null(fs)) {
    return(list(c(list(stub), lapply(seq_along(prep$plan), leaf))))
  }
  lead <- seq_len(fs$lead)
  tall <- function(x) c(x, list(rows = 2L))
  row1 <- c(
    list(tall(stub)), lapply(lead, function(i) tall(leaf(i))),
    table_groups_spanners(prep, fs, pick)
  )
  list(row1, lapply(setdiff(seq_along(prep$plan), lead), leaf))
}

# The facet levels over their columns: the level, its N, and the claim a
# click on it makes.
table_groups_spanners <- function(prep, fs, pick) {
  first <- fs$lead + 1L + c(0L, cumsum(vapply(fs$groups, `[[`, 1L, "n")))
  lapply(seq_along(fs$groups), function(j) {
    g <- fs$groups[[j]]
    fcol <- prep$plan[[first[[j]]]]$fcol
    table_groups_compact(list(
      text = as.character(g$label), sub = if (!is.null(g$pop)) {
        paste0("N = ", g$pop)
      },
      span = g$n,
      keys = if (pick && length(fcol)) {
        table_structured_claim(fcol, list(as.character(g$label)))
      }
    ))
  })
}

# Per-row extras. A nested table (two-level `by`) indents its child rows and
# marks its parent rows strong; with the drill on every row claims its path.
table_groups_rows <- function(cells, pick) {
  nested <- isTRUE(cells$nested)
  parent <- !is.na(cells$parent_row) & cells$parent_row
  out <- table_groups_compact(list(
    indent = if (nested) as.integer(cells$level),
    strong = if (nested) parent,
    keys = if (pick) table_groups_row_keys(cells, parent)
  ))
  if (length(out)) out
}

# The claim of each row: `path` is the columns outer to inner. A parent row
# claims the outer column's key, a child row the outer key and its own label
# (`parent_key` holds the parent's key on both). Without a nested path every
# row claims its label under the one column.
table_groups_row_keys <- function(cells, parent) {
  path <- cells$path
  lapply(seq_along(cells$label), function(i) {
    if (length(path) < 2L) {
      return(table_structured_claim(path[[1L]], list(cells$label[[i]])))
    }
    if (parent[[i]]) {
      return(table_structured_claim(path[[1L]], list(cells$parent_key[[i]])))
    }
    table_structured_claim(path, list(cells$parent_key[[i]], cells$label[[i]]))
  })
}

# A list without its NULL entries.
table_groups_compact <- function(x) {
  x[!vapply(x, is.null, logical(1L))]
}

# A per-row vector as a JSON array: Shiny writes with `auto_unbox`, which
# would turn a one-row column into a bare value.
table_groups_arr <- function(x) {
  I(unname(x))
}
