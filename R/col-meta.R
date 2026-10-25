# Column metadata for the browser: what every mapping picker in the gear (and
# on a block's exposed band) reads to build its options, and what the
# renderers read for category order.
#
# Pulled out of the chart block's server so it can be tested directly -- the
# reactive it used to live in is unreachable from testServer.
#
# No nrow gate: a 0-row frame still HAS columns (names/types/labels/levels),
# and the pickers must stay usable while an upstream filter has emptied the
# data.
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

    # A date or date-time column: a line or a scatter draws it on a time
    # axis. Its `type` stays "categorical" (the values travel as ISO strings),
    # so the gear's pickers offer it where they did.
    if (inherits(vals, "Date")) {
      res$temporal <- "date"
    } else if (inherits(vals, "POSIXt")) {
      res$temporal <- "datetime"
    }

    # What the column is FOR (mark_column_kinds()). Roles that declare which
    # kinds they accept narrow their picker to matching columns -- but only
    # once SOMETHING in the frame is marked, so an unmarked board still offers
    # every column to every role. An unmarked column in a marked frame carries
    # no `kind` field at all, which is how the client tells the two apart.
    if (col %in% names(kinds)) res$kind <- unname(kinds[[col]])

    # Factor level order travels to JS as the category/legend order (the
    # data-level "order lives in factors" contract).
    if (is.factor(vals)) res$levels <- as.list(levels(vals))

    res
  })
}
