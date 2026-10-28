# Column metadata for the browser: what the gear's mapping pickers read to
# build their options, and what the renderers read for category order.
# No nrow gate: a 0-row frame still has columns, and the pickers must stay
# usable while an upstream filter has emptied the data.
dd_col_meta <- function(d) {
  stopifnot(is.data.frame(d))

  kinds <- column_kinds(d)

  lapply(names(d), function(col) {
    vals <- d[[col]]
    lbl <- attr(vals, "label")

    res <- list(
      name = col,
      type = if (is.numeric(vals)) "numeric" else "categorical",
      n_unique = length(unique(vals))
    )

    if (!is.null(lbl) && nzchar(lbl)) res$label <- lbl

    # A date or date-time column draws on a time axis. Its `type` stays
    # "categorical" (the values travel as ISO strings).
    if (inherits(vals, "Date")) {
      res$temporal <- "date"
    } else if (inherits(vals, "POSIXt")) {
      res$temporal <- "datetime"
    }

    # What the column is FOR (mark_column_kinds()). An unmarked column in a
    # marked frame has no `kind` field; the client tells the two apart by it.
    if (col %in% names(kinds)) res$kind <- unname(kinds[[col]])

    # Factor level order travels to JS as the category and legend order.
    if (is.factor(vals)) res$levels <- as.list(levels(vals))

    res
  })
}
