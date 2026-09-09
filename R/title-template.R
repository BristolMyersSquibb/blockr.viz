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
#   {filters}         the filter trail: what the filters upstream of this
#                     block actually applied, in the order they applied it
#                     ("SEX = F; TRTEMFL"). Written onto the data by the
#                     filter blocks themselves -- see
#                     `blockr.dm::filter_trail()` -- so it reports the filters
#                     on THIS block's path and no others. A block fed from a
#                     branch the global filter does not reach says so by
#                     leaving it out.
#
#                     Resolves to "" when nothing is filtered, and a caption
#                     that resolves to "" hides its band. So `{filters}` on
#                     its own disappears on an unfiltered board, whereas
#                     "Filtered: {filters}" leaves the label behind.
#
# Tokens are data lookups, never code: nothing is evaluated (the same
# decision as dropping glue from the ggplot exprs). A token naming a column
# the data does not carry resolves to "" instead of erroring -- the
# tt_fields policy: display text must not take the block down when an
# upstream edit drops a column.

# How many distinct values a `{col}` token spells out before eliding with
# ", ...". Guards against `{USUBJID}` turning the title into a subject list.
TITLE_MAX_VALUES <- 8L

# Two more forms, for the sentence a block writes about itself (see
# blockr.docs design-system/pinned-controls.md):
#
#   {@arg}            the current value of the block argument `arg`, printed
#                     literally. `{@color}` reads "TRT" -- NOT the values in
#                     column TRT, which is what the bare `{TRT}` token does.
#                     A word printed from an `@` token is a SLOT: on the face
#                     it is clickable and opens that argument's own control.
#   {label(@arg)}     the variable label of the column the argument names,
#                     falling back to the column name. Still a slot for `arg`:
#                     the reading is derived, the editor is unchanged.
#   {n_distinct(@arg)} how many distinct values that column has. NOT a slot --
#                     a count is not something a reader can set.
#
#   [ ... ]           a segment that leaves with its token. Any token inside
#                     resolving to "" drops the whole segment, so
#                     "[, faceted by {@facet}]" says nothing at all when facet
#                     is unset, rather than "faceted by (none)". The engine
#                     already had half of this rule: a caption resolving to ""
#                     hides its band.
#
# One walk serves both renderers. `title_template_parts()` returns the pieces
# and `resolve_title_template()` pastes them, so an export and the block can
# never disagree about what the sentence says.

# Split a template into segments. Each is `list(text=, optional=)`; an
# unmatched "[" is literal text, because a template is display copy and must
# not error.
split_template_segments <- function(template) {
  out <- list()
  buf <- ""
  i <- 1L
  n <- nchar(template)
  while (i <= n) {
    ch <- substr(template, i, i)
    if (identical(ch, "[")) {
      close <- regexpr("]", substr(template, i, n), fixed = TRUE)
      if (close > 0L) {
        if (nzchar(buf)) out <- c(out, list(list(text = buf, optional = FALSE)))
        buf <- ""
        out <- c(out, list(list(
          text = substr(template, i + 1L, i + close - 2L),
          optional = TRUE
        )))
        i <- i + close
        next
      }
    }
    buf <- paste0(buf, ch)
    i <- i + 1L
  }
  if (nzchar(buf)) out <- c(out, list(list(text = buf, optional = FALSE)))
  out
}

# The pieces of one segment: plain text, and one piece per {token}. `arg` is
# set on the pieces a reader can click; `empty` marks a token that resolved to
# nothing, which is what makes an optional segment disappear.
resolve_template_chunk <- function(text, data, args) {
  starts <- gregexpr("\\{[^{}]+\\}", text)[[1L]]
  if (identical(starts[[1L]], -1L)) {
    return(list(list(text = text, arg = NULL, empty = FALSE)))
  }
  lens <- attr(starts, "match.length")
  out <- list()
  pos <- 1L
  for (i in seq_along(starts)) {
    if (starts[[i]] > pos) {
      out <- c(out, list(list(
        text = substr(text, pos, starts[[i]] - 1L), arg = NULL, empty = FALSE
      )))
    }
    tok <- trimws(substr(text, starts[[i]] + 1L, starts[[i]] + lens[[i]] - 2L))
    res <- resolve_title_token(tok, data, args)
    out <- c(out, list(list(
      text = res$text, arg = res$arg, by = res$by, empty = !nzchar(res$text)
    )))
    pos <- starts[[i]] + lens[[i]]
  }
  if (pos <= nchar(text)) {
    out <- c(out, list(list(
      text = substr(text, pos, nchar(text)), arg = NULL, empty = FALSE
    )))
  }
  out
}

title_template_parts <- function(template, data, args = list()) {
  template <- as.character(template)[[1L]]
  if (!length(template) || is.na(template) || !nzchar(template)) return(list())
  out <- list()
  for (seg in split_template_segments(template)) {
    parts <- resolve_template_chunk(seg$text, data, args)
    if (isTRUE(seg$optional) &&
        any(vapply(parts, function(p) isTRUE(p$empty), logical(1L)))) {
      next
    }
    out <- c(out, parts)
  }
  # Adjacent plain pieces merge, so the face renders one text node per run
  # instead of one per template fragment.
  merged <- list()
  for (p in out) {
    last <- if (length(merged)) merged[[length(merged)]] else NULL
    if (!is.null(last) && is.null(last$arg) && is.null(p$arg)) {
      merged[[length(merged)]]$text <- paste0(last$text, p$text)
    } else {
      merged <- c(merged, list(p))
    }
  }
  merged
}

resolve_title_template <- function(template, data, args = list()) {
  template <- as.character(template)[[1L]]
  if (!nzchar(template)) return(template)
  # Nothing to resolve, and no data needed to say so.
  if (!grepl("[{[]", template)) return(template)
  # Without a frame the data tokens resolve to "" rather than erroring, which
  # is the tt_fields policy: display text must not take the block down.
  if (!is.data.frame(data)) data <- data.frame()
  parts <- title_template_parts(template, data, args)
  paste0(vapply(parts, function(p) p$text, character(1L)), collapse = "")
}

# Returns `list(text=, arg=)`: the resolved string, and the argument whose
# control the word opens when it is one.
resolve_title_token <- function(token, data, args = list()) {
  plain <- function(x) list(text = x, arg = NULL)

  # `@arg` forms first: they read block state, not the data, and a bare
  # `{@color}` must print the argument's VALUE rather than the values in the
  # column it names.
  m <- regmatches(token, regexec("^@\\s*([^()@]+?)\\s*$", token))[[1L]]
  if (length(m) == 2L) {
    return(list(text = arg_token_value(m[[2L]], args), arg = m[[2L]],
                by = "name"))
  }
  m <- regmatches(
    token, regexec("^([A-Za-z_.][A-Za-z0-9_.]*)\\(\\s*@\\s*([^()@]+?)\\s*\\)$", token)
  )[[1L]]
  if (length(m) == 3L) {
    col <- arg_token_value(m[[3L]], args)
    if (!nzchar(col)) return(plain(""))
    inner <- resolve_title_token(paste0(m[[2L]], "(", col, ")"), data, args)
    # A count is not a value a reader can set, so it is text, not a slot.
    if (identical(m[[2L]], "n_distinct")) return(plain(inner$text))
    # `by` travels so the menu can lead with the same half of the option the
    # sentence printed: click "Actual Treatment" and a list whose rows read
    # "TRTA / Actual Treatment" bolds a string that is nowhere on screen.
    return(list(text = inner$text, arg = m[[3L]],
                by = if (identical(m[[2L]], "label")) "label" else "name"))
  }

  if (identical(token, "n")) return(plain(format(nrow(data), big.mark = "")))

  if (identical(token, "filters")) {
    # Read straight off the frame rather than threaded in: every caller hands
    # the resolver the block's own input, and the one place that subsets
    # columns first (the chart block's `plain_data()`) carries the attribute
    # across deliberately. Names are the producing block ids, used for
    # deduplication upstream; a reader wants the clauses.
    trail <- attr(data, "blockr_filters", exact = TRUE)
    trail <- trail[!is.na(trail) & nzchar(trail)]
    if (!length(trail)) return(plain(""))
    return(plain(paste(unname(trail), collapse = "; ")))
  }

  fn_arg <- function(fn) {
    m <- regmatches(
      token,
      regexec(paste0("^", fn, "\\(\\s*([^()]+?)\\s*\\)$"), token)
    )[[1L]]
    if (length(m) == 2L) m[[2L]] else NULL
  }

  col <- fn_arg("label")
  if (!is.null(col)) {
    if (!col %in% names(data)) return(plain(""))
    lbl <- attr(data[[col]], "label", exact = TRUE)
    if (is.character(lbl) && length(lbl) && nzchar(lbl[[1L]])) {
      return(plain(lbl[[1L]]))
    }
    return(plain(col))
  }

  col <- fn_arg("n_distinct")
  if (!is.null(col)) {
    if (!col %in% names(data)) return(plain(""))
    v <- data[[col]]
    return(plain(format(length(unique(v[!is.na(v)])), big.mark = "")))
  }

  # Bare column token: the distinct values, collapsed.
  if (!token %in% names(data)) return(plain(""))
  v <- data[[token]]
  if (is.factor(v)) {
    vals <- levels(v)[levels(v) %in% unique(as.character(v[!is.na(v)]))]
  } else {
    vals <- unique(as.character(v[!is.na(v)]))
  }
  if (!length(vals)) return(plain(""))
  if (length(vals) > TITLE_MAX_VALUES) {
    # Horizontal ellipsis, \u-escaped rather than literal: R sources must
    # stay ASCII (R CMD check "non-ASCII characters" warning).
    vals <- c(vals[seq_len(TITLE_MAX_VALUES)], "\u2026")
  }
  plain(paste(vals, collapse = ", "))
}

# The printed value of a block argument. Everything the roles use for "unset"
# collapses to "", which is what makes an optional segment disappear: NULL,
# "", NA, and the literal "(none)" sentinel the config engine writes.
arg_token_value <- function(name, args) {
  v <- args[[name]]
  if (is.null(v) || !length(v)) return("")
  v <- as.character(v)[[1L]]
  if (is.na(v) || identical(v, "(none)")) return("")
  v
}

# The three-tier title contract shared by the viz blocks (the table block's
# `color` split, applied to text): NULL = auto (use `auto`, normally the
# data frame's label attribute -- the gt block's existing fallback), "" =
# explicitly none (a label-carrying input whose title the user turned off),
# anything else = a template resolved against the data. Returns NULL when
# there is nothing to show, so JS falsy checks hide the band.
resolve_block_title <- function(x, data, auto = NULL, args = list()) {
  if (is.null(x)) {
    # The auto tier is a label written upstream, not user text: shown
    # verbatim, never treated as a template.
    auto <- if (is.character(auto) && length(auto)) auto[[1L]] else NULL
    if (!is.null(auto) && nzchar(auto)) return(auto) else return(NULL)
  }
  x <- as.character(x)[[1L]]
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  resolve_title_template(x, data, args)
}

# The same three tiers, as pieces. The face renders these; every other
# renderer takes the string from `resolve_block_title()`. NULL when there is
# nothing to show, and a single plain piece for the auto tier, which is a
# label written upstream rather than a template.
block_title_parts <- function(x, data, auto = NULL, args = list()) {
  if (is.null(x)) {
    auto <- if (is.character(auto) && length(auto)) auto[[1L]] else NULL
    if (is.null(auto) || !nzchar(auto)) return(NULL)
    return(list(list(text = auto)))
  }
  x <- as.character(x)[[1L]]
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  parts <- title_template_parts(x, data, args)
  if (!length(parts)) return(NULL)
  # `empty` is a parser flag; the client only needs the word and its argument.
  lapply(parts, function(p) {
    if (is.null(p$arg)) {
      list(text = p$text)
    } else {
      list(text = p$text, arg = p$arg, by = p$by %||% "name")
    }
  })
}

# A free-text title slot: NULL = auto, "" = explicitly none, else a template.
# chr_state() cannot be used here -- it drops "", which is a real value for
# these slots (the same reason the table block's color heals via null_state).
title_state <- function(x) {
  x <- null_state(x)
  if (is.null(x)) return(NULL)
  as.character(x)[[1L]]
}

# The table-level display attributes used for the auto tiers: `label` (the
# title -- the established display-name attribute), `subtitle` and `caption`.
# Producers stamp them on the annotated df (e.g. the composer methods recover
# them from the gt heading / source notes). Read from the block's RAW input:
# as_plain_df() subsets columns, and base subsetting drops data-frame-level
# attributes, so they must be captured before that coercion. Non-data-frame
# inputs (composer tables) are coerced once through the annotated-df generic,
# which is where their attributes live.
input_display_attrs <- function(d) {
  if (!is.data.frame(d)) {
    d <- tryCatch(as_annotated_df(d), error = function(e) NULL)
  }
  one <- function(which) {
    v <- attr(d, which, exact = TRUE)
    if (is.character(v) && length(v) && nzchar(v[[1L]])) v[[1L]] else NULL
  }
  list(
    label    = one("label"),
    subtitle = one("subtitle"),
    caption  = one("caption")
  )
}
