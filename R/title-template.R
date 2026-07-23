# Dynamic block titles ------------------------------------------------------
#
# The chart (and table) blocks take free-text `title` / `subtitle` / `caption`
# state. The text may reference the CURRENT data through a small, closed set
# of `{...}` tokens, resolved server-side on every render -- so a title stays
# correct as upstream blocks (value filter, picker) reshape the data:
#
#   {col}             the distinct values of column `col`, collapsed with
#                     ", " (factor columns in level order, others in order of
#                     appearance). An upstream value filter on ARM makes
#                     `{ARM}` read "Placebo" -- no channel needed, the
#                     selection is already visible in the data.
#   {label(col)}      the column's variable label attribute (falls back to
#                     the column name). The picker block stamps the picked
#                     measure's label onto its `into` column, so
#                     `{label(value)}` follows the pick.
#   {n}               the number of rows.
#   {n_distinct(col)} the number of distinct values of `col`.
#
# Tokens are data lookups, never code: nothing is evaluated (the same
# decision as dropping glue from the ggplot exprs). A token naming a column
# the data does not carry resolves to "" instead of erroring -- the
# tt_fields policy: display text must not take the block down when an
# upstream edit drops a column.

# How many distinct values a `{col}` token spells out before eliding with
# ", ...". Guards against `{USUBJID}` turning the title into a subject list.
TITLE_MAX_VALUES <- 8L

resolve_title_template <- function(template, data) {
  template <- as.character(template)[[1L]]
  if (!nzchar(template)) return(template)
  if (!is.data.frame(data)) return(template)

  starts <- gregexpr("\\{[^{}]+\\}", template)[[1L]]
  if (identical(starts[[1L]], -1L)) return(template)

  lens <- attr(starts, "match.length")
  out <- template
  # Replace right-to-left so earlier match positions stay valid.
  for (i in rev(seq_along(starts))) {
    tok <- substr(out, starts[[i]] + 1L, starts[[i]] + lens[[i]] - 2L)
    val <- resolve_title_token(trimws(tok), data)
    out <- paste0(
      substr(out, 1L, starts[[i]] - 1L),
      val,
      substr(out, starts[[i]] + lens[[i]], nchar(out))
    )
  }
  out
}

resolve_title_token <- function(token, data) {
  if (identical(token, "n")) return(format(nrow(data), big.mark = ""))

  fn_arg <- function(fn) {
    m <- regmatches(
      token,
      regexec(paste0("^", fn, "\\(\\s*([^()]+?)\\s*\\)$"), token)
    )[[1L]]
    if (length(m) == 2L) m[[2L]] else NULL
  }

  col <- fn_arg("label")
  if (!is.null(col)) {
    if (!col %in% names(data)) return("")
    lbl <- attr(data[[col]], "label", exact = TRUE)
    if (is.character(lbl) && length(lbl) && nzchar(lbl[[1L]])) {
      return(lbl[[1L]])
    }
    return(col)
  }

  col <- fn_arg("n_distinct")
  if (!is.null(col)) {
    if (!col %in% names(data)) return("")
    v <- data[[col]]
    return(format(length(unique(v[!is.na(v)])), big.mark = ""))
  }

  # Bare column token: the distinct values, collapsed.
  if (!token %in% names(data)) return("")
  v <- data[[token]]
  if (is.factor(v)) {
    vals <- levels(v)[levels(v) %in% unique(as.character(v[!is.na(v)]))]
  } else {
    vals <- unique(as.character(v[!is.na(v)]))
  }
  if (!length(vals)) return("")
  if (length(vals) > TITLE_MAX_VALUES) {
    vals <- c(vals[seq_len(TITLE_MAX_VALUES)], "…")
  }
  paste(vals, collapse = ", ")
}

# The three-tier title contract shared by the viz blocks (the table block's
# `color` split, applied to text): NULL = auto (use `auto`, normally the
# data frame's label attribute -- the gt block's existing fallback), "" =
# explicitly none (a label-carrying input whose title the user turned off),
# anything else = a template resolved against the data. Returns NULL when
# there is nothing to show, so JS falsy checks hide the band.
resolve_block_title <- function(x, data, auto = NULL) {
  if (is.null(x)) {
    # The auto tier is a label written upstream, not user text: shown
    # verbatim, never treated as a template.
    auto <- if (is.character(auto) && length(auto)) auto[[1L]] else NULL
    if (!is.null(auto) && nzchar(auto)) return(auto) else return(NULL)
  }
  x <- as.character(x)[[1L]]
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  resolve_title_template(x, data)
}

# A free-text title slot: NULL = auto, "" = explicitly none, else a template.
# chr_state() cannot be used here -- it drops "", which is a real value for
# these slots (the same reason the table block's color heals via null_state).
title_state <- function(x) {
  x <- null_state(x)
  if (is.null(x)) return(NULL)
  as.character(x)[[1L]]
}

# The data frame label attribute used for the auto title tier. Read from the
# block's RAW input: as_plain_df() subsets columns, and base subsetting drops
# data-frame-level attributes, so the label must be captured before coercion.
# Non-data-frame inputs (composer tables) are coerced through the annotated-df
# generic, which is where their label lives.
input_data_label <- function(d) {
  if (!is.data.frame(d)) {
    d <- tryCatch(as_annotated_df(d), error = function(e) NULL)
  }
  lbl <- attr(d, "label", exact = TRUE)
  if (is.character(lbl) && length(lbl) && nzchar(lbl[[1L]])) lbl[[1L]] else NULL
}
