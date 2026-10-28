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
#   {highlight}       the values a chart highlights ("Switzerland and
#                     Germany"), "" when it highlights nothing. Read off the
#                     frame's `blockr_highlight` attribute, which the chart
#                     block sets; elsewhere it is a column name like any other.
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

title_column_label <- function(data, col) {
  if (!is.data.frame(data) || !col %in% names(data)) {
    return("")
  }

  direct <- attr(data[[col]], "label", exact = TRUE)
  source <- attr(data[[col]], "blockr_source", exact = TRUE)

  source_label <- NULL
  if (is.character(source) && length(source) == 1L &&
      !is.na(source) && nzchar(source)) {
    if (source %in% names(data)) {
      source_label <- attr(data[[source]], "label", exact = TRUE)
    }
    if (!(is.character(source_label) && length(source_label) == 1L &&
          !is.na(source_label) && nzchar(source_label))) {
      source_label <- source
    }
  }

  if (is.character(direct) && length(direct) == 1L &&
      !is.na(direct) && nzchar(direct) && !identical(direct, col)) {
    return(direct)
  }
  if (!is.null(source_label)) {
    return(source_label)
  }
  if (is.character(direct) && length(direct) == 1L &&
      !is.na(direct) && nzchar(direct)) {
    return(direct)
  }
  col
}

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
#   {@arg in a, b}    a slot whose menu offers only `a` and `b` (plus the
#                     current pick). Works inside `label()` too:
#                     `{label(@value in AVAL, CHG, PCHG)}`. Optional; without
#                     a list the menu offers what the gear does.
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

# `\n` written in a template is a line break. Every surface a template is
# typed on is one line -- the gear's text field, a composer script's `#|`
# declaration -- so the two characters are the only way to say "break here",
# and they are the same two an R string takes for it in a block ctor. The
# bands render them (`white-space: pre-line`), which is why the rule is the
# same for the title, the subtitle and the caption.
tt_line_breaks <- function(template) {
  if (is.na(template)) return(template)
  gsub("\\n", "\n", template, fixed = TRUE)
}

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
  template <- tt_line_breaks(as.character(template)[[1L]])
  if (!length(template) || is.na(template) || !nzchar(template)) return(list())
  out <- list()
  # Arguments whose clause dropped. They are the ones a reader cannot reach:
  # the word that would open them left with the segment, so the face offers
  # them at the end of the sentence instead. Without this, promoting `facet`
  # on a chart that has no facet yet promotes nothing.
  offers <- character()
  for (seg in split_template_segments(template)) {
    parts <- resolve_template_chunk(seg$text, data, args)
    if (isTRUE(seg$optional) &&
        any(vapply(parts, function(p) isTRUE(p$empty), logical(1L)))) {
      dropped <- vapply(
        parts,
        function(p) if (is.null(p$arg)) NA_character_ else p$arg,
        character(1L)
      )
      offers <- c(offers, dropped[!is.na(dropped)])
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
  # An argument named twice, once in a clause that stayed, is not on offer.
  kept <- vapply(
    merged, function(p) if (is.null(p$arg)) NA_character_ else p$arg,
    character(1L)
  )
  offers <- setdiff(unique(offers), kept[!is.na(kept)])
  attr(merged, "offers") <- offers
  merged
}

resolve_title_template <- function(template, data, args = list()) {
  template <- tt_line_breaks(as.character(template)[[1L]])
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
    nm <- slot_arg_name(m[[2L]])
    return(list(text = arg_token_value(nm, args), arg = nm, by = "name"))
  }
  m <- regmatches(
    token, regexec("^([A-Za-z_.][A-Za-z0-9_.]*)\\(\\s*@\\s*([^()@]+?)\\s*\\)$", token)
  )[[1L]]
  if (length(m) == 3L) {
    m[[3L]] <- slot_arg_name(m[[3L]])
    col <- arg_token_value(m[[3L]], args)
    if (!nzchar(col)) {
      # Empty, but still this argument's slot. Dropping the `arg` here would
      # lose the offer as well as the word: a clause written
      # `[, split by {label(@facet)}]` would vanish with nothing left to
      # switch faceting on with. A count is the exception, as below.
      if (identical(m[[2L]], "n_distinct")) return(plain(""))
      return(list(text = "", arg = m[[3L]],
                  by = if (identical(m[[2L]], "label")) "label" else "name"))
    }
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

  hl <- attr(data, "blockr_highlight", exact = TRUE)
  if (identical(token, "highlight") && !is.null(hl)) return(plain(hl))

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
    return(plain(title_column_label(data, col)))
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

# A slot may name its choices: `{@func in mean, median}`,
# `{label(@value in AVAL, CHG, PCHG)}`. The list only narrows what the word's
# menu offers; everything else about the token reads the bare name.
SLOT_CHOICES_RE <- "^([^[:space:]]+)[[:space:]]+in[[:space:]]+(.*)$"

slot_arg_name <- function(x) {
  x <- trimws(x)
  m <- regmatches(x, regexec(SLOT_CHOICES_RE, x))[[1L]]
  if (length(m) == 3L) m[[2L]] else x
}

# Every `{... @arg in a, b ...}` list in the given templates, as a named list
# of character vectors keyed by argument. Read off the text alone, so the
# chart can ship the listed columns before any data is resolved. An argument
# listed twice gets the union, in the order written.
template_slot_choices <- function(templates) {
  out <- list()
  for (tpl in templates) {
    if (!is.character(tpl) || !length(tpl) || is.na(tpl[[1L]])) next
    toks <- regmatches(tpl[[1L]], gregexpr("\\{[^{}]+\\}", tpl[[1L]]))[[1L]]
    for (tok in toks) {
      m <- regmatches(
        tok, regexec("@[[:space:]]*([^()@{}]+)", tok)
      )[[1L]]
      if (length(m) != 2L) next
      inner <- trimws(m[[2L]])
      mm <- regmatches(inner, regexec(SLOT_CHOICES_RE, inner))[[1L]]
      if (length(mm) != 3L) next
      ch <- trimws(strsplit(mm[[3L]], ",", fixed = TRUE)[[1L]])
      ch <- ch[nzchar(ch)]
      if (!length(ch)) next
      out[[mm[[2L]]]] <- unique(c(out[[mm[[2L]]]], ch))
    }
  }
  out
}

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
  offers <- attr(parts, "offers", exact = TRUE)
  if (!length(parts)) {
    # Every clause dropped: there is no sentence, and the offers are all
    # there is to show.
    if (!length(offers)) return(NULL)
    out <- list()
    attr(out, "offers") <- offers
    return(out)
  }
  # `empty` is a parser flag; the client only needs the word and its argument.
  out <- lapply(parts, function(p) {
    if (is.null(p$arg)) {
      list(text = p$text)
    } else {
      list(text = p$text, arg = p$arg, by = p$by %||% "name")
    }
  })
  attr(out, "offers") <- offers
  out
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
