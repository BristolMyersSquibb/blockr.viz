# Dynamic block titles ------------------------------------------------------
#
# The chart and table blocks take free-text `title` / `subtitle` / `caption`
# state. The text may reference the current data through a closed set of
# `{...}` tokens, resolved on every render, so a title follows upstream blocks
# (value filter, picker) as they reshape the data:
#
#   {col}             the distinct values of column `col`, collapsed with
#                     ", " (factor columns in level order, others in order of
#                     appearance). After an upstream value filter on ARM,
#                     `{ARM}` reads "Placebo".
#   {label(col)}      the column's variable label attribute (falls back to
#                     the column name).
#   {n}               the number of rows.
#   {n_distinct(col)} the number of distinct values of `col`.
#   {highlight}       the values a chart highlights ("Switzerland and
#                     Germany"), "" when it highlights nothing. Read off the
#                     frame's `blockr_highlight` attribute, which the chart
#                     block sets; elsewhere it is a column name like any other.
#   {filters}         the filter trail: what the filters upstream of this
#                     block applied, in order ("SEX = F; TRTEMFL"). The filter
#                     blocks write it onto the data (see
#                     `blockr.dm::filter_trail()`). It is "" when nothing is
#                     filtered, and a caption that resolves to "" hides its
#                     band.
#
# Tokens are data lookups and nothing is evaluated. A token naming a column
# the data does not carry resolves to "" instead of erroring, so an upstream
# edit that drops a column does not take the block down.

title_column_label <- function(data, col) {
  if (!is.data.frame(data) || !col %in% names(data)) {
    return("")
  }

  has_text <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }

  direct <- attr(data[[col]], "label", exact = TRUE)
  source <- attr(data[[col]], "blockr_source", exact = TRUE)

  source_label <- NULL
  if (has_text(source)) {
    source_label <- if (source %in% names(data)) {
      attr(data[[source]], "label", exact = TRUE)
    }
    if (!has_text(source_label)) {
      source_label <- source
    }
  }

  if (has_text(direct) && !identical(direct, col)) {
    return(direct)
  }
  if (!is.null(source_label)) {
    return(source_label)
  }
  if (has_text(direct)) {
    return(direct)
  }
  col
}

# How many distinct values a `{col}` token spells out before eliding with
# "...". Keeps `{USUBJID}` from turning the title into a subject list.
TITLE_MAX_VALUES <- 8L

# Block state forms, for the sentence a block writes about itself:
#
#   {@arg}            the current value of the block argument `arg`, printed
#                     literally. `{@color}` reads "TRT", not the values in
#                     column TRT. On the face the word is a slot: clicking it
#                     opens that argument's control.
#   {label(@arg)}     the variable label of the column the argument names,
#                     falling back to the column name. Also a slot for `arg`.
#   {n_distinct(@arg)} how many distinct values that column has. Not a slot.
#   {@arg in a, b}    a slot whose menu offers only `a` and `b` (plus the
#                     current pick). Works inside `label()` too:
#                     `{label(@value in AVAL, CHG, PCHG)}`.
#
#   [ ... ]           a segment that leaves with its token. If any token inside
#                     resolves to "", the whole segment drops, so
#                     "[, faceted by {@facet}]" says nothing when facet is
#                     unset.
#
# `title_template_parts()` returns the pieces and `resolve_title_template()`
# pastes them, so an export and the block face say the same sentence.

# `\n` written in a template is a line break. Templates are typed on one-line
# surfaces (the gear's text field, a composer `#|` declaration), so the two
# characters are the way to say "break here". The bands render them with
# `white-space: pre-line`.
tt_line_breaks <- function(template) {
  if (is.na(template)) return(template)
  gsub("\\n", "\n", template, fixed = TRUE)
}

# Split a template into segments, each `list(text=, optional=)`. An unmatched
# "[" is literal text.
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
# nothing, which drops an optional segment.
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
  # Arguments whose clause dropped. The word that would open them left with
  # the segment, so the face offers them at the end of the sentence.
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
  # Adjacent plain pieces merge into one text node.
  merged <- list()
  for (p in out) {
    last <- if (length(merged)) merged[[length(merged)]] else NULL
    if (!is.null(last) && is.null(last$arg) && is.null(p$arg)) {
      merged[[length(merged)]]$text <- paste0(last$text, p$text)
    } else {
      merged <- c(merged, list(p))
    }
  }
  # An argument also named in a clause that stayed is not on offer.
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
  if (!grepl("[{[]", template)) return(template)
  # Without a frame the data tokens resolve to "".
  if (!is.data.frame(data)) data <- data.frame()
  parts <- title_template_parts(template, data, args)
  paste0(vapply(parts, function(p) p$text, character(1L)), collapse = "")
}

# Returns `list(text=, arg=)`: the resolved string, and the argument whose
# control the word opens when it is one. `by` ("name" or "label") says which
# half of the option the sentence printed, so the menu can lead with the same.
resolve_title_token <- function(token, data, args = list()) {
  plain <- function(x) list(text = x, arg = NULL)

  # `@arg` forms read block state, not the data, so a bare `{@color}` prints
  # the argument's value rather than the values in the column it names.
  m <- regmatches(token, regexec("^@\\s*([^()@]+?)\\s*$", token))[[1L]]
  if (length(m) == 2L) {
    nm <- slot_arg_name(m[[2L]])
    return(list(text = arg_token_value(nm, args), arg = nm, by = "name"))
  }
  m <- regmatches(
    token,
    regexec("^([A-Za-z_.][A-Za-z0-9_.]*)\\(\\s*@\\s*([^()@]+?)\\s*\\)$", token)
  )[[1L]]
  if (length(m) == 3L) {
    fn <- m[[2L]]
    nm <- slot_arg_name(m[[3L]])
    col <- arg_token_value(nm, args)
    text <- if (nzchar(col)) {
      resolve_title_token(paste0(fn, "(", col, ")"), data, args)$text
    } else {
      ""
    }
    # A count is text, not a value a reader can set. An empty slot keeps its
    # `arg`, so a dropped clause can still offer the argument.
    if (identical(fn, "n_distinct")) return(plain(text))
    return(list(text = text, arg = nm,
                by = if (identical(fn, "label")) "label" else "name"))
  }

  if (identical(token, "n")) return(plain(format(nrow(data), big.mark = "")))

  hl <- attr(data, "blockr_highlight", exact = TRUE)
  if (identical(token, "highlight") && !is.null(hl)) return(plain(hl))

  if (identical(token, "filters")) {
    # Names are the producing block ids; a reader wants the clauses.
    trail <- attr(data, "blockr_filters", exact = TRUE)
    trail <- trail[!is.na(trail) & nzchar(trail)]
    if (!length(trail)) return(plain(""))
    return(plain(paste(unname(trail), collapse = "; ")))
  }

  m <- regmatches(
    token, regexec("^(label|n_distinct)\\(\\s*([^()]+?)\\s*\\)$", token)
  )[[1L]]
  if (length(m) == 3L) {
    if (!m[[3L]] %in% names(data)) return(plain(""))
    if (identical(m[[2L]], "label")) {
      return(plain(title_column_label(data, m[[3L]])))
    }
    v <- data[[m[[3L]]]]
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
    # \u-escaped: R sources must stay ASCII.
    vals <- c(vals[seq_len(TITLE_MAX_VALUES)], "\u2026")
  }
  plain(paste(vals, collapse = ", "))
}

# A slot may name its choices: `{@func in mean, median}`,
# `{label(@value in AVAL, CHG, PCHG)}`. The list only narrows the word's menu.
SLOT_CHOICES_RE <- "^([^[:space:]]+)[[:space:]]+in[[:space:]]+(.*)$"

slot_arg_name <- function(x) {
  x <- trimws(x)
  m <- regmatches(x, regexec(SLOT_CHOICES_RE, x))[[1L]]
  if (length(m) == 3L) m[[2L]] else x
}

# Every `{... @arg in a, b ...}` list in the given templates, as a named list
# of character vectors keyed by argument, read off the text alone. An argument
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

# The auto tier of a title: a label written upstream, shown verbatim and never
# treated as a template. NULL when empty.
auto_title <- function(auto) {
  auto <- if (is.character(auto) && length(auto)) auto[[1L]]
  if (!is.null(auto) && nzchar(auto)) auto
}

# The three-tier title contract shared by the viz blocks: NULL = auto (use
# `auto`, normally the data frame's label attribute), "" = explicitly none,
# anything else = a template resolved against the data. Returns NULL when there
# is nothing to show, so JS falsy checks hide the band.
resolve_block_title <- function(x, data, auto = NULL, args = list()) {
  if (is.null(x)) {
    return(auto_title(auto))
  }
  x <- as.character(x)[[1L]]
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  resolve_title_template(x, data, args)
}

# The same three tiers, as pieces for the face; every other renderer takes the
# string from `resolve_block_title()`. NULL when there is nothing to show, and
# one plain piece for the auto tier.
block_title_parts <- function(x, data, auto = NULL, args = list()) {
  if (is.null(x)) {
    auto <- auto_title(auto)
    return(if (!is.null(auto)) list(list(text = auto)))
  }
  x <- as.character(x)[[1L]]
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  parts <- title_template_parts(x, data, args)
  offers <- attr(parts, "offers", exact = TRUE)
  if (!length(parts)) {
    # Every clause dropped: only the offers are left to show.
    if (!length(offers)) return(NULL)
    out <- list()
    attr(out, "offers") <- offers
    return(out)
  }
  # `empty` is a parser flag; the client needs the word and its argument.
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

# The table-level attributes behind the auto tiers: `label` (the title),
# `subtitle` and `caption`. Producers stamp them on the annotated df. Read
# them from the block's raw input, before as_plain_df(), whose column subsetting
# drops data-frame attributes. Non-data-frame inputs are coerced through
# as_annotated_df().
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
