#' The groups layout as the browser draws it (a TableModel).
#' @return A TableModel list, or `NULL` when `m$cells` is `NULL` (the
#'   preparation failed).
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
    note = cells$fold
  )
  table_view_arrays(table_groups_compact(out))
}

# The context summarize_cells() hands its column builders, with value labels
# on, so a column that draws none can have its labels rebuilt for `disp`.
table_groups_ctx <- function(prep) {
  list(rows = prep$rows, n = nrow(prep$rows), prep = prep, mx = prep$bar_max,
       mn = prep$bar_min %||% 0, vals_on = TRUE)
}

# One body column. A plain number is `num` (right-aligned); other kinds sort on
# `key`. `keys` is what a click adds to the row's claim; `pct` the "(43%)" tail.
table_groups_col <- function(c, p, prep, ctx, pick, axis) {
  pct <- if (!is.null(c$pct)) table_groups_text(c$pct)
  table_groups_compact(list(
    name = as.character(p$label %||% "")[[1L]],
    num = identical(c$kind, "num") && !isTRUE(c$text),
    disp = table_groups_disp(c, p, ctx),
    key = c$v,
    pct = if (!is.null(pct)) summarize_arr(pct),
    bg = if (identical(c$kind, "tile")) c$bg,
    fg = if (identical(c$kind, "tile")) c$fg,
    keys = if (pick) table_groups_col_keys(c),
    axis = if (axis) table_groups_axis(p, prep),
    mark = table_groups_mark(c)
  ))
}

# The claim of a click on a cell: the facet level, and for a colour split the
# column its segments name.
table_groups_col_keys <- function(c) {
  cols <- c(c$fkey$column, c$scol)
  if (!length(cols)) {
    return(NULL)
  }
  vals <- c(if (!is.null(c$fkey)) list(c$fkey$value),
            if (!is.null(c$scol)) list(c$names))
  table_structured_claim(cols, vals)
}

# Header rows: the stub's cell then one per column. The by_level facet layout
# adds a spanning row on top; the stub and leading columns span both rows.
table_groups_head <- function(m, pick) {
  prep <- m$prep
  leaf <- function(i) {
    p <- prep$plan[[i]]
    table_groups_compact(list(
      text = as.character(p$label %||% "")[[1L]],
      sub = chr_state(p$sub_label), col = i
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

# The facet levels over their columns: level, its N, and the claim a click makes.
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

# Per-row extras: a nested table indents child rows and marks parents strong;
# with the drill on every row claims its path.
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
# row claims its label under the one column. A row without a label does not
# click.
table_groups_row_keys <- function(cells, parent) {
  path <- cells$path
  lapply(seq_along(cells$label), function(i) {
    if (is.na(cells$label[[i]])) {
      return(NULL)
    }
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
