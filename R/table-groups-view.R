#' The groups layout as the browser draws it (a TableModel).
#' @return A TableModel list, or `NULL` when `m$cells` is `NULL` (the
#'   preparation failed).
#' @noRd
table_groups_to_model <- function(m, axis = TRUE) {
  cells <- m$cells
  if (is.null(cells)) {
    return(NULL)
  }
  prep <- m$prep
  pick <- isTRUE(cells$pick)
  stub <- list(name = groups_label_header(prep), disp = cells$label,
               num = FALSE)
  cols <- lapply(seq_along(cells$cols), function(i) {
    text <- cells$text[[i]] %||% rep(NA_character_, cells$n)
    table_groups_col(cells$cols[[i]], text, prep$plan[[i]], prep, pick, axis)
  })
  nested <- isTRUE(cells$nested)
  out <- list(
    n = cells$n, stub = 0L, cols = c(list(stub), cols),
    head = table_groups_head(m, pick),
    rows = table_groups_rows(cells, pick),
    collapsed = if (nested && !isTRUE(cells$expanded)) TRUE,
    note = cells$fold
  )
  table_model_arrays(table_groups_compact(out))
}

# One body column. A plain number is `num` (right-aligned); other kinds sort on
# `key`. `disp` is the column's text (cells_text()), `keys` what a click adds
# to the row's claim, `pct` the "(43%)" tail.
table_groups_col <- function(c, text, p, prep, pick, axis) {
  pct <- if (!is.null(c$pct)) table_groups_text(c$pct)
  table_groups_compact(list(
    name = as.character(p$label %||% "")[[1L]],
    num = identical(c$kind, "num") && !isTRUE(c$text),
    disp = table_groups_text(text),
    key = c$v,
    pct = if (!is.null(pct)) groups_arr(pct),
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
  stub <- table_groups_stub_head(prep, isTRUE(m$cells$compact))
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

# The stub's header: the `by` names over their labels. A compact matrix shows
# the row column's label alone, its name as the tooltip: the outer groups are
# its section rows, and the long label line would set the column's width.
table_groups_stub_head <- function(prep, compact = FALSE) {
  if (compact && !isTRUE(prep$all) && !is.null(prep$group)) {
    label <- prep$row_label %||% prep$group
    return(table_groups_compact(list(
      text = label, col = 0L, stub = TRUE,
      tip = if (!identical(label, prep$group)) prep$group
    )))
  }
  table_groups_compact(list(
    text = groups_label_header(prep), sub = prep$group_label, col = 0L,
    stub = TRUE
  ))
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
