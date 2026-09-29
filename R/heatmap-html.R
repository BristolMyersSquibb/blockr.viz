# Matrix heatmap renderer: long event rows in, a row x column matrix out,
# with TWO channels per cell -- the DISPLAYED number is the event count, the
# PAINT is the worst level of `color` (severity).
#
# Layout (design system, _scratch/heatmap-ds/): the header row carries the
# title, the sentence and the tools; then the legend band, one continuous
# matrix (sticky rotated header), each group opening with a section-title
# row, the caption and the status line. Empty cells are truly empty. The
# exhibit speaks in column labels; the status line keeps names.

#' Resolve the color column's ordered levels: a factor keeps its own order
#' (vocabularies live in the data), numeric grades order numerically, and a
#' bare character column falls back to sorted unique values.
#' @noRd
hmb_levels <- function(x) {
  if (is.factor(x)) return(levels(x))
  if (is.numeric(x)) {
    v <- sort(unique(x[is.finite(x)]))
    return(as.character(v))
  }
  sort(unique(as.character(x[!is.na(x) & nzchar(as.character(x))])))
}

#' Aggregate the long rows to the matrix model.
#'
#' @return `list(err = <message>)` when not renderable, else
#'   `list(rows, group_of, groups, terms, n_terms_total, count, worst,
#'   levels)` -- `count` / `worst` are rows x terms matrices (NA = no
#'   event), `worst` in level indices, `groups` a list of
#'   `list(label, n)` in row order.
#' @noRd
heatmap_prep <- function(data, row, col, color = NULL, group = NULL) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(list(err = "No data", err_kind = "empty"))
  }
  if (is.null(row) || !length(row) || !nzchar(row[1L]) ||
        is.null(col) || !length(col) || !nzchar(col[1L])) {
    return(list(err = "Pick a row and a column in the settings.",
                err_kind = "empty"))
  }
  row <- row[1L]
  col <- col[1L]
  if (!row %in% names(data)) {
    return(list(err = paste0("Column '", row, "' is not in the data."),
                err_kind = "danger"))
  }
  if (!col %in% names(data)) {
    return(list(err = paste0("Column '", col, "' is not in the data."),
                err_kind = "danger"))
  }
  color <- if (!is.null(color) && length(color) && nzchar(color[1L]) &&
                 color[1L] %in% names(data)) color[1L]
  group <- if (!is.null(group) && length(group) && nzchar(group[1L]) &&
                 group[1L] %in% names(data)) group[1L]

  # Grouped by a column carrying a group definition: one copy of each
  # group's rows (expand_groups()), so a subject in two groups gets a row in
  # each group's section. The row key is then (group, id), since the same id
  # is two rows; `rows` still reports the id.
  split_rows <- FALSE
  if (!is.null(group) && group_expand_needed(data[[group]])) {
    data <- expand_groups(data, group)
    split_rows <- isTRUE(group_def_origin(data[[group]])$overlap)
  }

  rid <- as.character(data[[row]])
  trm <- as.character(data[[col]])
  keep0 <- !is.na(rid) & nzchar(rid) & !is.na(trm) & nzchar(trm)
  if (!any(keep0)) return(list(err = "No data", err_kind = "empty"))
  rkey <- if (split_rows) {
    paste(as.character(data[[group]]), rid, sep = "\037")
  } else {
    rid
  }
  d <- data.frame(rid = rkey[keep0], trm = trm[keep0],
                  stringsAsFactors = FALSE)

  lv <- if (!is.null(color)) hmb_levels(data[[color]])
  if (!is.null(color)) {
    cv <- data[[color]][keep0]
    d$lvl <- if (is.numeric(cv)) {
      match(as.character(cv), lv)
    } else {
      match(as.character(cv), lv)
    }
  }
  gv <- if (!is.null(group)) data[[group]][keep0]

  # Every term, most events first; ties by first appearance. A cap is the
  # prepare script's business, not the engine's.
  tc <- sort(table(d$trm), decreasing = TRUE)
  n_terms_total <- length(tc)
  terms <- names(tc)

  # Per cell (row x term): event count + worst level index.
  key <- paste(d$rid, d$trm, sep = "\r")
  cnt <- tapply(rep(1L, nrow(d)), key, sum)
  wst <- if (!is.null(color)) {
    tapply(d$lvl, key, function(g) {
      if (all(is.na(g))) NA_integer_ else max(g, na.rm = TRUE)
    })
  }

  # Row order: group first (factor order wins over alphabet, so dose groups
  # keep their order), then total burden descending, then the id.
  burden <- tapply(rep(1L, nrow(d)), d$rid, sum)
  ids <- names(burden)
  gf <- if (!is.null(gv)) {
    g1 <- gv[match(ids, d$rid)]
    if (is.factor(gv)) factor(as.character(g1), levels(gv)) else
      factor(as.character(g1))
  }
  ord <- if (is.null(gf)) {
    order(-burden, ids)
  } else {
    order(as.integer(gf), -burden, ids)
  }
  ids <- ids[ord]

  count <- matrix(NA_integer_, length(ids), length(terms),
                  dimnames = list(ids, terms))
  worst <- matrix(NA_integer_, length(ids), length(terms),
                  dimnames = list(ids, terms))
  kr <- sub("\r.*$", "", names(cnt))
  kt <- sub("^.*\r", "", names(cnt))
  count[cbind(kr, kt)] <- as.integer(cnt)
  if (!is.null(wst)) worst[cbind(kr, kt)] <- as.integer(wst)

  group_of <- if (!is.null(gf)) as.character(gf)[ord]
  groups <- if (!is.null(group_of)) {
    r <- rle(ifelse(is.na(group_of), "(Missing)", group_of))
    Map(function(l, n) list(label = l, n = n), r$values, r$lengths)
  }
  list(
    rows = if (split_rows) sub("^.*\037", "", ids) else ids,
    group_of = group_of, groups = groups,
    terms = terms, n_terms_total = n_terms_total,
    count = count, worst = worst,
    levels = lv, row_col = row, col_col = col,
    color_col = color, group_col = group,
    row_label = hmb_label(data, row),
    col_label = hmb_label(data, col),
    color_label = if (!is.null(color)) hmb_label(data, color),
    group_label = if (!is.null(group)) hmb_label(data, group)
  )
}

#' A column's label, or its name when it has none (design system, "Column
#' names and their labels": an exhibit speaks in labels).
#' @noRd
hmb_label <- function(data, col) {
  lab <- attr(data[[col]], "label", exact = TRUE)
  if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) {
    lab
  } else {
    col
  }
}

#' The heatmap's matrix as a data frame
#'
#' The block's result: one row per matrix row, in the order the heatmap draws
#' them, the group column when there is one, then one column per matrix column
#' holding the event count, most frequent first. The same aggregation as the
#' block's cells (see [new_heatmap_block()]), so the result and the picture
#' agree.
#'
#' @param data Long event rows, one per event.
#' @param row,col,color,group As in [new_heatmap_block()]. `color` does not
#'   change the counts; it is accepted so the call mirrors the block's.
#' @return A data frame. Cells with no event hold 0.
#' @examples
#' d <- data.frame(USUBJID = c("s1", "s1", "s2"),
#'                 AEDECOD = c("RASH", "RASH", "NAUSEA"))
#' heatmap_matrix(d, row = "USUBJID", col = "AEDECOD")
#' @export
heatmap_matrix <- function(data, row, col, color = NULL, group = NULL) {
  p <- heatmap_prep(data, row, col, color, group)
  if (!is.null(p$err)) {
    stop(p$err, call. = FALSE)
  }
  out <- hmb_matrix_frame(p, empty = 0L)
  for (nm in intersect(c(p$row_col, p$group_col), names(data))) {
    lab <- attr(data[[nm]], "label", exact = TRUE)
    if (!is.null(lab)) attr(out[[nm]], "label") <- lab
  }
  out
}

#' The prepared matrix as a frame; `empty` fills the cells with no event.
#' @noRd
hmb_matrix_frame <- function(p, empty = 0L) {
  out <- stats::setNames(
    data.frame(p$rows, stringsAsFactors = FALSE, check.names = FALSE),
    p$row_col
  )
  if (!is.null(p$group_of)) out[[p$group_col]] <- p$group_of
  for (tm in p$terms) {
    v <- p$count[, tm]
    v[is.na(v)] <- empty
    out[[tm]] <- unname(v)
  }
  out
}

#' The cell paint, as one vectorized `function(v) list(bg =, fg =)`.
#'
#' TWO sources, and the board wins. When the board's scale map binds the
#' colour column (option "scale_map" -- the same seam the chart and the
#' summarize table resolve through, provenance-aware so a picker copy
#' keeps the source column's binding) and it covers every level present,
#' the cell takes the DECLARED colour per level: a study that pins CTCAE
#' grade 5 to red gets red, not whatever the theme ramp lands on, and the
#' colours stay put as filters change the levels in view. Only without a
#' binding does the block fall back to its sequential ramp, which is the
#' right default for an unlabelled ordinal scale but is theme-derived and
#' says nothing about the vocabulary.
#'
#' `v` is a level INDEX when the matrix is levelled, an event count
#' otherwise. Falls back per call, never per cell.
#' @noRd
hmb_paint <- function(prep, data = NULL, scale_map = NULL) {
  lv <- prep$levels
  if (!is.null(lv) && length(lv) && !is.null(scale_map) &&
        !is.null(prep$color_col) && has_blockr_theme()) {
    column <- if (is.data.frame(data) && prep$color_col %in% names(data)) {
      data[[prep$color_col]]
    } else {
      lv
    }
    res <- tryCatch(dd_resolve_scales(scale_map, prep$color_col, column),
                    error = function(e) NULL)
    pal <- res$color
    # blockr.theme completes the palette itself: levels the board declared
    # keep their colour, the rest get stable palette entries -- so a bound
    # column never mixes declared colours with RAMP positions (which would
    # move as filters change the levels in view). The check is therefore
    # "bound at all", and the same one rank_level_colors uses.
    if (!is.null(pal) && all(lv %in% names(pal))) {
      hex <- unname(pal[lv])
      if (!anyNA(hex)) {
        fg <- dt_fg_for_hex(hex)
        return(function(v) {
          i <- pmax(pmin(as.integer(v), length(hex)), 1L)
          list(bg = hex[i], fg = fg[i])
        })
      }
    }
  }
  dom <- if (!is.null(lv)) {
    c(1L, max(2L, length(lv)))
  } else {
    r <- range(prep$count, na.rm = TRUE)
    if (!all(is.finite(r))) r <- c(0, 1)
    if (r[1L] == r[2L]) r[2L] <- r[1L] + 1L
    r
  }
  dt_color_fun("sequential", dom, NULL)
}

#' The legend band (design system, "Charts: legend band"): the label, then
#' 25 x 14 swatches. Levelled paint decodes the levels; count paint shows the
#' ramp ends. A second group says what the number in a cell is, and goes with
#' the numbers.
#' @noRd
hmb_legend <- function(prep, fun) {
  items <- if (!is.null(prep$levels)) {
    k <- length(prep$levels)
    cols <- fun(seq_len(k))
    lapply(seq_len(k), function(i) {
      htmltools::tags$span(
        class = "hmb-li",
        htmltools::tags$i(style = paste0("background:", cols$bg[i])),
        prep$levels[i]
      )
    })
  } else {
    rng <- range(prep$count, na.rm = TRUE)
    cols <- fun(rng)
    list(
      htmltools::tags$span(
        class = "hmb-li",
        htmltools::tags$i(style = paste0("background:", cols$bg[1])), rng[1]
      ),
      htmltools::tags$span(
        class = "hmb-li",
        htmltools::tags$i(style = paste0("background:", cols$bg[2])), rng[2]
      )
    )
  }
  htmltools::tags$div(
    class = "hmb-legend",
    htmltools::tags$span(
      class = "hmb-lg",
      htmltools::tags$span(class = "hmb-lt",
                           if (!is.null(prep$color_col)) {
                             paste("Worst", prep$color_label)
                           } else {
                             "Events"
                           }),
      items
    ),
    if (!is.null(prep$color_col)) {
      htmltools::tags$span(
        class = "hmb-lg hmb-lg-num",
        htmltools::tags$span(class = "hmb-lt", "Numbers: events")
      )
    }
  )
}

#' The column list the gear's pickers read (name + coarse type), stamped on
#' the root as `data-hmb-cols`. Data-dependent, so it travels with the body
#' payload rather than with the chrome.
#' @noRd
hmb_cols_json <- function(data) {
  if (!is.data.frame(data)) return("[]")
  as.character(jsonlite::toJSON(dd_col_meta(data), auto_unbox = TRUE))
}

#' The gear's and the header's current state, stamped as `data-hmb-config`.
#' Config only: the chrome can build it at mount time, before any data has
#' arrived. `titles$resolved` and the script half fill in once it has.
#' @noRd
hmb_cfg_json <- function(row = NULL, col = NULL, color = NULL, group = NULL,
                         cell_numbers = TRUE, download = FALSE, target = "",
                         titles = list(), script = list()) {
  res <- titles$resolved
  parts <- res$parts
  offers <- function(p) as.list(attr(p, "offers", exact = TRUE))
  cfg <- list(
    row = row %||% "", col = col %||% "", color = color %||% "",
    group = group %||% "",
    cell_numbers = if (isTRUE(cell_numbers)) "on" else "off",
    download = if (isTRUE(download)) "on" else "off",
    # The board's drill filter, resolved (ctrl_auto_target()); "" = none,
    # and the rows do not take clicks.
    ctrl_target = target %||% "",
    # The three-tier text: NULL (auto) travels as JSON null.
    title = titles$title, subtitle = titles$subtitle,
    caption = titles$caption,
    title_resolved = res$title %||% "",
    subtitle_resolved = res$subtitle %||% "",
    caption_resolved = res$caption %||% "",
    title_parts = parts$title, subtitle_parts = parts$subtitle,
    caption_parts = parts$caption,
    title_offers = offers(parts$title),
    subtitle_offers = offers(parts$subtitle),
    caption_offers = offers(parts$caption),
    title_arg_values = res$arg_values %||% structure(list(), names = character()),
    script = script$text %||% "",
    script_inputs = script$inputs %||% list(),
    script_error = script$error
  )
  cfg <- c(cfg, script$cfg %||% list())
  as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null"))
}

# ---------------------------------------------------------------------------
# Cell model + its two consumers (blockr.viz's table block draws the same
# shape, see R/table-push.R). The matrix body used to exist only as pasted
# HTML. It is now a MODEL -- row ids, term names, group runs, and the filled
# cells as index / count / palette slot -- with two renderers over it:
#   - hmb_assemble_rows() pastes the historical HTML (heatmap_html(), the
#     standalone render, the test surface), while
#   - the same model travels as JSON to heatmap-block.js, which assembles
#     the rows client-side.
# The two outputs must not drift; test-heatmap-block.R pins the R one and
# the payload it is built from.
#
# Why it is worth a model at all: the matrix is SPARSE. A 194 x 25 AE
# heatmap is 4850 cells of which ~460 carry an event, so the HTML is mostly
# 4386 copies of the empty cell -- 157 KB, against 7 KB for the model.
# ---------------------------------------------------------------------------

#' The cell model.
#'
#' Cells are sparse: `idx` are 0-based COLUMN-MAJOR positions into the
#' `n` x `k` matrix (so `r = idx %% n`, `c = idx %/% n`, the layout R's own
#' matrix already has), `cnt` the event counts shown, and `pal` a 1-based
#' slot in the `bg` / `fg` palettes.
#'
#' The palette is keyed by DISTINCT paint value, not per cell: level indices
#' when the colour column is levelled, the counts themselves when it is not.
#' That is what lets one mechanism carry both paint modes without shipping a
#' colour ramp to the client -- `fun` has already been evaluated here.
#' @noRd
hmb_cell_model <- function(prep, fun) {
  n <- length(prep$rows)
  k <- length(prep$terms)
  cnt <- prep$count
  src <- if (!is.null(prep$levels)) prep$worst else prep$count
  filled <- !is.na(cnt)

  pal <- integer()
  bg <- character()
  fg <- character()
  if (any(filled)) {
    pv <- src[filled]
    # A filled cell whose SOURCE is missing (an event with no recorded
    # grade) paints at the floor -- level index 1, or the low end of the
    # count ramp -- rather than dropping out of the matrix. `fun` reads
    # level indices when levelled and counts otherwise, and both floors
    # sit at the bottom of their own scale.
    pv[is.na(pv)] <- if (!is.null(prep$levels)) 1L else min(cnt, na.rm = TRUE)
    pv <- as.numeric(pv)
    keys <- sort(unique(pv))
    cols <- fun(keys)
    bg <- as.character(cols$bg)
    fg <- as.character(cols$fg)
    pal <- match(pv, keys)
  }

  groups <- if (!is.null(prep$groups)) {
    lapply(prep$groups, function(g) list(label = g$label, n = g$n))
  }

  # The worst level per filled cell, for the data tooltip: 0 = none recorded.
  lvl <- if (!is.null(prep$levels) && any(filled)) {
    w <- prep$worst[filled]
    w[is.na(w)] <- 0L
    as.integer(w)
  } else {
    integer()
  }

  list(
    n = n, k = k,
    rows = as.character(prep$rows),
    terms = as.character(prep$terms),
    row_col = prep$row_col,
    groups = groups,
    idx = as.integer(which(filled) - 1L),
    cnt = as.integer(cnt[filled]),
    pal = as.integer(pal),
    bg = bg, fg = fg,
    lvl = lvl,
    levels = as.character(prep$levels %||% character()),
    row_label = prep$row_label %||% prep$row_col,
    col_label = prep$col_label %||% prep$col_col,
    color_label = prep$color_label %||% "",
    group_label = prep$group_label %||% ""
  )
}

#' The model as the payload nests it.
#'
#' Every vector is `I()`-wrapped: under `auto_unbox` a one-row matrix, a
#' single term or a one-colour palette would otherwise ship as a scalar and
#' the client's indexing would come apart (the auto_unbox trap).
#' @noRd
hmb_model_payload <- function(m) {
  if (is.null(m)) return(NULL)
  list(
    n = m$n, k = m$k, rowCol = m$row_col %||% "",
    rows = I(m$rows), terms = I(m$terms),
    # unname()d: `prep$groups` is a NAMED list, and a named list serializes
    # as a JSON object, not an array -- the client would read no groups at
    # all and drop the group rows silently.
    groups = unname(m$groups) %||% list(),
    idx = I(m$idx), cnt = I(m$cnt), pal = I(m$pal),
    bg = I(m$bg), fg = I(m$fg),
    lvl = I(m$lvl %||% integer()), levels = I(m$levels %||% character()),
    rowLabel = m$row_label %||% "", colLabel = m$col_label %||% "",
    colorLabel = m$color_label %||% "", groupLabel = m$group_label %||% ""
  )
}

#' Paste the model into the `<tr>` markup.
#'
#' Column-vectorized string assembly, not per-cell tag objects: those
#' dominate render time (dt_flat_assemble_tag's argument). Text and
#' attribute content use htmltools' own escaper, so `&` `<` `>` are escaped
#' and quotes are not -- heatmap-block.js's assembler applies the same rule.
#' A group opens with a section-title row spanning the matrix.
#' @noRd
hmb_assemble_rows <- function(m) {
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  n <- m$n
  k <- m$k
  cells <- matrix('<td class="hmb-c"></td>', n, k)
  if (length(m$idx)) {
    cells[m$idx + 1L] <- paste0(
      '<td class="hmb-c" style="background:', m$bg[m$pal],
      ";color:", m$fg[m$pal], '"><span>', m$cnt, "</span></td>"
    )
  }
  stub <- paste0('<td class="hmb-stub">', esc(m$rows), "</td>")
  body_rows <- paste0('<tr class="hmb-r" data-hmb-i="', seq_len(n) - 1L,
                      '" data-hmb-id="', esc(m$rows), '">', stub,
                      apply(cells, 1L, paste0, collapse = ""), "</tr>")

  if (length(m$groups)) {
    gn <- vapply(m$groups, `[[`, 0L, "n")
    starts <- utils::head(cumsum(c(1L, gn)), -1L)
    heads <- vapply(m$groups, function(g) {
      paste0('<tr class="hmb-grp"><td colspan="', k + 1L,
             '"><span class="hmb-gt">', esc(g$label),
             '</span><span class="hmb-gn">', g$n, "</span></td></tr>")
    }, "")
    body_rows[starts] <- paste0(heads, body_rows[starts])
  }
  paste0(body_rows, collapse = "")
}

#' Build the data-dependent half: legend, matrix and the status count.
#'
#' Returns character HTML rather than tags, because this is what ships over
#' the custom-message channel to the client (the table block's `kind:
#' "html"` payload, dev/table-data-push-design.md).
#'
#' @return `list(err=)` when not renderable, else `list(legend, head, model,
#'   table, count, row_col, cols)`.
#' @noRd
hmb_body <- function(data, row = NULL, col = NULL, color = NULL,
                     group = NULL, scale_map = NULL) {
  prep <- heatmap_prep(data, row, col, color, group)
  if (!is.null(prep$err)) {
    return(list(err = prep$err, err_kind = prep$err_kind %||% "empty",
                row_col = row %||% "", cols = hmb_cols_json(data)))
  }

  # One vectorized colour fun over level indices (or counts when
  # unlevelled): the board's declared level colours when the scale map
  # binds the column, else the sequential ramp. See hmb_paint().
  fun <- hmb_paint(prep, data, scale_map)

  # ---- thead ----------------------------------------------------------
  # The stub header is the row column's label (an exhibit speaks in labels);
  # the name is its tooltip. A term cut off by the 150px header gets its
  # full text as the tooltip, set by the client once it can measure.
  ths <- c(
    list(htmltools::tags$th(
      class = "hmb-stubh",
      `data-blockr-tooltip` = if (!identical(prep$row_label, prep$row_col)) {
        prep$row_col
      },
      prep$row_label
    )),
    lapply(prep$terms, function(tm) {
      htmltools::tags$th(class = "hmb-rot", htmltools::tags$span(tm))
    })
  )
  thead <- htmltools::tags$thead(htmltools::tags$tr(ths))

  model <- hmb_cell_model(prep, fun)
  tbody <- htmltools::tags$tbody(htmltools::HTML(hmb_assemble_rows(model)))

  list(
    legend = as.character(hmb_legend(prep, fun)),
    # The <table> shell and its rotated header stay R markup; the body is
    # the cell model, assembled by whichever consumer asked.
    head = as.character(
      htmltools::tags$table(class = "hmb-table", thead,
                            htmltools::tags$tbody())
    ),
    model = model,
    table = as.character(
      htmltools::tags$table(class = "hmb-table", thead, tbody)
    ),
    # The status line stays on the board, so it speaks in names.
    count = sprintf("%d %s \u00d7 %d %s", model$n, prep$row_col, model$k,
                    prep$col_col),
    row_col = prep$row_col,
    cols = hmb_cols_json(data)
  )
}

#' The search icon for the header's search tool (16px grid, stroke).
#' @noRd
hmb_search_icon <- function() {
  htmltools::HTML(paste0(
    '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true">',
    '<g fill="none" stroke="currentColor" stroke-width="1.4" ',
    'stroke-linecap="round"><circle cx="7" cy="7" r="4.5"/>',
    '<path d="M10.5 10.5L14 14"/></g></svg>'
  ))
}

#' The persistent shell: header row (title, sentence, tools; the JS adds the
#' gear and its tray), search row, legend slot, scroll, caption, status line.
#'
#' Depends on CONFIG only, never on the data -- that is the whole point.
#' The dock publishes a transient `on_screen=[]` while it arranges, which
#' closes core's data gate for a tick; a chrome that read the data would
#' render empty on that tick and Shiny would wipe the panel. The chart,
#' table, rank and summarize blocks all avoid it the same way: a shell that
#' renders once and a body pushed over a custom message.
#'
#' `body` is the standalone escape hatch -- pass a [hmb_body()] result and
#' the slots come back filled, which is what [heatmap_html()] and the tests
#' use. In the block, the slots ship empty and heatmap-block.js fills them.
#' @noRd
hmb_chrome <- function(elem_id = NULL, cell_numbers = TRUE,
                       cfg_json = "{}", cols_json = "[]", row_col = "",
                       download_slot = NULL, status = NULL, body = NULL) {
  head <- htmltools::tags$div(
    class = "hmb-head",
    htmltools::tags$div(
      class = "hmb-titles",
      htmltools::tags$div(class = "hmb-title"),
      htmltools::tags$div(class = "hmb-subtitle")
    ),
    htmltools::tags$div(
      class = "hmb-tools",
      blockr.ui::tool_button(hmb_search_icon(), "Search",
                             class = "hmb-search-btn",
                             `aria-pressed` = "false"),
      download_slot
    )
  )

  scroll_inner <- if (!is.null(body$err)) {
    if (identical(body$err_kind, "danger")) {
      htmltools::tags$div(class = "hmb-msg hmb-msg--danger", body$err)
    } else {
      htmltools::tags$p(class = "blockr-empty blockr-empty--block", body$err)
    }
  } else if (!is.null(body)) {
    htmltools::HTML(body$table)
  }

  htmltools::tags$div(
    class = paste0("hmb-block",
                   if (!isTRUE(cell_numbers)) " hmb-nonum"),
    `data-hmb-elem-id` = elem_id,
    `data-hmb-cols` = cols_json,
    `data-hmb-config` = cfg_json,
    `data-hmb-drill` = "0",
    `data-hmb-row-col` = row_col,
    heatmap_block_dep(),
    head,
    htmltools::tags$div(
      class = "hmb-search-row",
      htmltools::tags$input(
        type = "search", class = "blockr-text-input hmb-search",
        placeholder = "Search rows", `aria-label` = "Search rows"
      )
    ),
    htmltools::tags$div(
      class = "hmb-legend-slot",
      if (is.null(body$err) && !is.null(body)) htmltools::HTML(body$legend)
    ),
    htmltools::tags$div(class = "hmb-scroll", scroll_inner),
    htmltools::tags$div(class = "hmb-caption"),
    htmltools::tags$div(
      class = "hmb-footer",
      htmltools::tags$span(class = "hmb-count", body$count %||% ""),
      # The drill receipt is its own tiny output (the `status` slot) so a row
      # click never re-renders the matrix.
      status
    )
  )
}

#' Assemble the whole block HTML, chrome and body in one tag tree.
#'
#' The standalone render: tests, and any caller that wants the matrix as a
#' self-contained fragment. The block itself does NOT use this -- it mounts
#' [hmb_chrome()] once and pushes [hmb_body()] over the data channel.
#' @noRd
heatmap_html <- function(data, row = NULL, col = NULL, color = NULL,
                         group = NULL, cell_numbers = TRUE,
                         download = FALSE, elem_id = NULL, status = NULL,
                         download_slot = NULL, scale_map = NULL) {
  body <- hmb_body(data, row, col, color, group, scale_map)
  hmb_chrome(
    elem_id = elem_id,
    cell_numbers = cell_numbers,
    cfg_json = hmb_cfg_json(row, col, color, group, cell_numbers, download),
    cols_json = body$cols,
    row_col = body$row_col %||% (row %||% ""),
    download_slot = download_slot,
    status = status,
    body = body
  )
}

#' The status line's drill half: the receipt of the last send, which fades
#' (the table's and the composer's words). A click only ever sends, so there
#' is no selection to report and no Reset.
#' @noRd
hmb_status_tag <- function(receipt) {
  htmltools::tags$span(
    class = "hmb-status hmb-status-receipt",
    htmltools::tags$span(class = "hmb-status-text", receipt)
  )
}
