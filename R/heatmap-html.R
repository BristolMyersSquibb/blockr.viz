# Matrix heatmap renderer: long event rows in, a row x column matrix out,
# with TWO channels per cell -- the DISPLAYED number is the event count, the
# PAINT is the worst level of `color` (severity). Server-rendered whole (the
# matrix is small -- hundreds of rows -- so a full re-render per state change
# is cheap and makes restore free, the tile block's argument).
#
# Layout (the "variant B" pick, _scratch/ae-heatmap-design/): one continuous
# matrix -- one sticky rotated header, one scroll -- with a rotated grey rail
# tile spanning each group's rows on the RIGHT, the table equivalent of the
# chart block's facet strip. Legend row in the summarize-table vocabulary.
# Empty cells are truly empty: a faint neutral tile, no dash.

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
heatmap_prep <- function(data, row, col, color = NULL, group = NULL,
                         top_n = 25L) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(list(err = "No data"))
  }
  if (is.null(row) || !length(row) || !nzchar(row[1L]) ||
        is.null(col) || !length(col) || !nzchar(col[1L])) {
    return(list(err = "Pick the row and column identities"))
  }
  row <- row[1L]
  col <- col[1L]
  if (!row %in% names(data)) {
    return(list(err = paste0("Column '", row, "' not in the data")))
  }
  if (!col %in% names(data)) {
    return(list(err = paste0("Column '", col, "' not in the data")))
  }
  color <- if (!is.null(color) && length(color) && nzchar(color[1L]) &&
                 color[1L] %in% names(data)) color[1L]
  group <- if (!is.null(group) && length(group) && nzchar(group[1L]) &&
                 group[1L] %in% names(data)) group[1L]

  rid <- as.character(data[[row]])
  trm <- as.character(data[[col]])
  keep0 <- !is.na(rid) & nzchar(rid) & !is.na(trm) & nzchar(trm)
  if (!any(keep0)) return(list(err = "No data"))
  d <- data.frame(rid = rid[keep0], trm = trm[keep0],
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

  # Top-N terms by total event count; ties by first appearance.
  tc <- sort(table(d$trm), decreasing = TRUE)
  n_terms_total <- length(tc)
  top_n <- max(1L, as.integer(top_n %||% 25L))
  terms <- utils::head(names(tc), top_n)
  sel <- d$trm %in% terms
  if (!any(sel)) return(list(err = "No data"))
  d <- d[sel, , drop = FALSE]
  if (!is.null(gv)) gv <- gv[sel]

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
    rows = ids, group_of = group_of, groups = groups,
    terms = terms, n_terms_total = n_terms_total,
    count = count, worst = worst,
    levels = lv, row_col = row, col_col = col,
    color_col = color, group_col = group
  )
}

#' A design-system checkbox (`.blockr-checkbox`): 16px box, primary fill
#' when checked, native input underneath. The boolean control for a DATA
#' option -- a self-labelling pill would not say whether its text is the
#' current state or the action (blockr.docs ux-principles, Boolean
#' controls).
#' @noRd
hmb_checkbox <- function(cls, label, checked) {
  htmltools::tags$label(
    class = paste("blockr-checkbox", cls),
    htmltools::tags$input(
      type = "checkbox", checked = if (isTRUE(checked)) NA
    ),
    htmltools::tags$span(
      class = "blockr-checkbox__box",
      htmltools::HTML(paste0(
        '<svg width="10" height="10" viewBox="0 0 16 16" ',
        'fill="currentColor"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 ',
        '7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646',
        '-6.647a.5.5 0 0 1 .708 0"/></svg>'
      ))
    ),
    htmltools::tags$span(label)
  )
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

#' The legend row (summarize-table vocabulary: uppercase group title +
#' swatches). Leveled paint decodes the levels; count paint shows the ramp
#' ends. A second text-only group says what the cell number is.
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
                             paste("Worst", prep$color_col)
                           } else {
                             "Events"
                           }),
      items
    ),
    htmltools::tags$span(
      class = "hmb-lg",
      htmltools::tags$span(class = "hmb-lt", "Cell number"),
      htmltools::tags$span(class = "hmb-li hmb-muted", "event count")
    )
  )
}

#' The column list the gear's pickers read (name + coarse type), stamped on
#' the root as `data-hmb-cols`. Data-dependent, so it travels with the body
#' payload rather than with the chrome.
#' @noRd
hmb_cols_json <- function(data) {
  if (!is.data.frame(data)) return("[]")
  as.character(jsonlite::toJSON(
    lapply(names(data), function(nm) {
      list(name = nm,
           type = if (is.numeric(data[[nm]])) "numeric" else "categorical")
    }),
    auto_unbox = TRUE
  ))
}

#' The gear's current state, stamped as `data-hmb-config`. Config only: the
#' chrome can build it at mount time, before any data has arrived.
#' @noRd
hmb_cfg_json <- function(row = NULL, col = NULL, color = NULL, group = NULL,
                         top_n = 25L, cell_numbers = TRUE, drill = FALSE,
                         download = FALSE, ctrl = list()) {
  as.character(jsonlite::toJSON(
    list(
      row = row %||% "", col = col %||% "", color = color %||% "",
      group = group %||% "", top_n = as.integer(top_n %||% 25L),
      cell_numbers = isTRUE(cell_numbers), drill = isTRUE(drill),
      download = if (isTRUE(download)) "on" else "off",
      ctrl_target = ctrl$target %||% "", ctrl_table = ctrl$table %||% "",
      ctrl_choices = ctrl$choices %||% list()
    ),
    auto_unbox = TRUE
  ))
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

  list(
    n = n, k = k,
    rows = as.character(prep$rows),
    terms = as.character(prep$terms),
    row_col = prep$row_col,
    groups = groups,
    idx = as.integer(which(filled) - 1L),
    cnt = as.integer(cnt[filled]),
    pal = as.integer(pal),
    bg = bg, fg = fg
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
    # all and drop the rail silently.
    groups = unname(m$groups) %||% list(),
    idx = I(m$idx), cnt = I(m$cnt), pal = I(m$pal),
    bg = I(m$bg), fg = I(m$fg)
  )
}

#' Paste the model into the historical `<tr>` markup.
#'
#' Column-vectorized string assembly, not per-cell tag objects: those
#' dominate render time (dt_flat_assemble_tag's argument). Text and
#' attribute content use htmltools' own escaper, so `&` `<` `>` are escaped
#' and quotes are not -- heatmap-block.js's assembler applies the same rule.
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
  stub <- paste0('<td class="hmb-stub" data-raw="', esc(m$rows), '">',
                 esc(m$rows), "</td>")
  body_rows <- paste0(stub, apply(cells, 1L, paste0, collapse = ""))

  if (!is.null(m$groups)) {
    gn <- vapply(m$groups, `[[`, 0L, "n")
    rail_at <- utils::head(cumsum(c(1L, gn)), -1L)
    rails <- rep("", n)
    rails[rail_at] <- vapply(m$groups, function(g) {
      paste0('<td class="hmb-rail" rowspan="', g$n, '" title="',
             esc(g$label), " \u00b7 ", g$n, ' rows"><span>',
             esc(g$label), "</span></td>")
    }, "")
    body_rows <- paste0(body_rows, rails)
    # Slim separator between groups (after each group's last row); sits
    # OUTSIDE the rowspans, so the rail math stays per group.
    seps <- rep("", n)
    ncols <- k + 2L
    seps[utils::head(cumsum(gn), -1L)] <-
      paste0('</tr><tr class="hmb-gsep"><td colspan="', ncols, '"></td>')
    body_rows <- paste0(body_rows, seps)
  }
  paste0('<tr class="hmb-r" data-hmb-id="', esc(m$rows), '">',
         body_rows, "</tr>", collapse = "")
}

#' Build the data-dependent half: legend, matrix, footer count, and the
#' bounds the Top-n field takes from the frame.
#'
#' Returns character HTML rather than tags, because this is what ships over
#' the custom-message channel to the client (the table block's `kind:
#' "html"` payload, dev/table-data-push-design.md): the EXISTING builders
#' render it, so there is one markup source and the client wires the same
#' DOM it always has.
#'
#' @return `list(err=)` when not renderable, else `list(legend, table,
#'   count, top_max, top_val, row_col, cols)`.
#' @noRd
hmb_body <- function(data, row = NULL, col = NULL, color = NULL,
                     group = NULL, top_n = 25L, scale_map = NULL) {
  prep <- heatmap_prep(data, row, col, color, group, top_n)
  if (!is.null(prep$err)) {
    return(list(err = prep$err, row_col = row %||% "",
                cols = hmb_cols_json(data)))
  }

  # One vectorized colour fun over level indices (or counts when
  # unlevelled): the board's declared level colours when the scale map
  # binds the column, else the sequential ramp. See hmb_paint().
  fun <- hmb_paint(prep, data, scale_map)

  # ---- thead ----------------------------------------------------------
  ths <- c(
    list(htmltools::tags$th(class = "hmb-stubh", prep$row_col)),
    lapply(prep$terms, function(tm) {
      htmltools::tags$th(class = "hmb-rot", title = tm,
                         htmltools::tags$span(tm))
    }),
    if (!is.null(prep$groups)) list(htmltools::tags$th(class = "hmb-railh"))
  )
  thead <- htmltools::tags$thead(htmltools::tags$tr(ths))

  model <- hmb_cell_model(prep, fun)
  tbody <- htmltools::tags$tbody(htmltools::HTML(hmb_assemble_rows(model)))

  n <- model$n
  k <- model$k
  n_terms <- prep$n_terms_total
  top_max <- max(n_terms, 1L)
  list(
    legend = as.character(hmb_legend(prep, fun)),
    # The <table> shell and its (small, fiddly) rotated header stay R
    # markup; the body is the cell model, assembled by whichever consumer
    # asked. See hmb_cell_model().
    head = as.character(
      htmltools::tags$table(class = "hmb-table", thead,
                            htmltools::tags$tbody())
    ),
    model = model,
    table = as.character(
      htmltools::tags$table(class = "hmb-table", thead, tbody)
    ),
    count = sprintf("%d × %d of %d %s", n, k, n_terms, prep$col_col),
    top_max = top_max,
    top_val = min(max(as.integer(top_n), 1L), top_max),
    row_col = prep$row_col,
    cols = hmb_cols_json(data)
  )
}

#' The persistent shell: toolbar (Top-N + cell numbers, left of the gear the
#' JS prepends), legend slot, scroll, footer.
#'
#' Depends on CONFIG only, never on the data -- that is the whole point.
#' The dock publishes a transient `on_screen=[]` while it arranges, which
#' closes core's data gate for a tick; a chrome that read the data would
#' render empty on that tick and Shiny would wipe the panel, so the reader
#' sees the matrix, then white, then the matrix again. The chart, table,
#' rank and summarize blocks all avoid it the same way: a shell that
#' renders once and a body pushed over a custom message.
#'
#' `body` is the standalone escape hatch -- pass a [hmb_body()] result and
#' the slots come back filled, which is what [heatmap_html()] and the tests
#' use. In the block, the slots ship empty and heatmap-block.js fills them.
#' @noRd
hmb_chrome <- function(elem_id = NULL, cell_numbers = TRUE, drill = FALSE,
                       download = FALSE, top_n = 25L, max_height = "600px",
                       cfg_json = "{}", cols_json = "[]", row_col = "",
                       active_values = NULL, download_slot = NULL,
                       status = NULL, body = NULL) {
  # ---- toolbar --------------------------------------------------------
  # Design-system primitives only (blockr.docs/design-system): a number
  # field is `.blockr-num-input` inside a bordered wrap, committing on
  # Enter / blur via Blockr.textCommit -- the slice block's "n rows"
  # control, and the reason a slider is wrong here: "top 25" is a value
  # you type, not one you drag to. Booleans are `.blockr-checkbox` (a data
  # option, not a value pill). No bespoke badge: the field shows the value.
  #
  # Before the first payload the bounds are the config's own top_n: the
  # field is honest about what was asked for, and the body's arrival
  # narrows `max` to the terms that actually exist.
  top_max <- body$top_max %||% max(as.integer(top_n %||% 25L), 1L)
  top_val <- body$top_val %||% min(max(as.integer(top_n %||% 25L), 1L),
                                   top_max)
  toolbar <- htmltools::tags$div(
    class = "hmb-toolbar",
    htmltools::tags$span(class = "hmb-tb-label", "Top n"),
    htmltools::tags$div(
      # The wrap carries the border and hosts the Enter chip textCommit
      # inserts next to the input (slice block's .slb-n-wrap).
      class = "hmb-topn-wrap",
      htmltools::tags$input(
        type = "number", class = "blockr-num-input hmb-topn",
        min = 1L, max = top_max, step = 1L, value = top_val,
        title = paste0("Columns shown, most frequent first (1-", top_max,
                       "). Enter to apply.")
      )
    ),
    hmb_checkbox("hmb-nums", "Cell numbers", cell_numbers),
    htmltools::tags$span(class = "hmb-tb-spacer"),
    htmltools::tags$input(
      type = "search", class = "hmb-search", placeholder = "Search…"
    ),
    download_slot
  )

  # An error state keeps the toolbar: the way out of "Pick the row and
  # column identities" is the gear, which lives in that toolbar.
  scroll_inner <- if (!is.null(body$err)) {
    htmltools::tags$div(class = "hmb-empty", body$err)
  } else if (!is.null(body)) {
    htmltools::HTML(body$table)
  }

  htmltools::tags$div(
    class = paste0("hmb-block",
                   if (!isTRUE(cell_numbers)) " hmb-nonum"),
    `data-hmb-elem-id` = elem_id,
    `data-hmb-cols` = cols_json,
    `data-hmb-config` = cfg_json,
    `data-hmb-drill` = if (isTRUE(drill)) "1" else "0",
    `data-hmb-row-col` = row_col,
    `data-hmb-active` = if (length(active_values)) {
      as.character(jsonlite::toJSON(as.character(active_values)))
    },
    heatmap_block_dep(),
    if (!is.null(download_slot)) {
      htmltools::tags$style(htmltools::HTML(dl_chrome_css()))
    },
    toolbar,
    htmltools::tags$div(
      class = "hmb-legend-slot",
      if (is.null(body$err) && !is.null(body)) htmltools::HTML(body$legend)
    ),
    htmltools::tags$div(
      class = "hmb-scroll",
      style = paste0("max-height:", max_height, ";"),
      scroll_inner
    ),
    htmltools::tags$div(
      class = "hmb-footer",
      htmltools::tags$span(class = "hmb-count", body$count %||% ""),
      # The drill status is its own tiny output (the `status` slot) so a row
      # click never re-renders the matrix -- the split the table block and the
      # composer preview both draw. Standalone use (tests) renders it inline.
      status %||% hmb_status_tag(row_col, active_values)
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
                         group = NULL, top_n = 25L, cell_numbers = TRUE,
                         drill = FALSE, download = FALSE, elem_id = NULL,
                         active_values = NULL, status = NULL,
                         download_slot = NULL, scale_map = NULL,
                         ctrl = list(), max_height = "600px") {
  body <- hmb_body(data, row, col, color, group, top_n, scale_map)
  hmb_chrome(
    elem_id = elem_id,
    cell_numbers = cell_numbers,
    drill = drill,
    download = download,
    top_n = top_n,
    max_height = max_height,
    cfg_json = hmb_cfg_json(row, col, color, group, top_n, cell_numbers,
                            drill, download, ctrl),
    cols_json = body$cols,
    row_col = body$row_col %||% (row %||% ""),
    active_values = active_values,
    download_slot = download_slot,
    status = status,
    body = body
  )
}

#' The drill-status line (dot + text + Reset), shared by the server's small
#' status output and the standalone inline render.
#' @noRd
hmb_status_tag <- function(row_col, active_values) {
  htmltools::tags$span(
    class = "hmb-status",
    style = if (!length(active_values)) "display:none",
    htmltools::tags$span(class = "hmb-dot"),
    htmltools::tags$span(
      class = "hmb-status-text",
      if (length(active_values)) {
        paste0("Filtering downstream: ", row_col, " = ",
               paste(active_values, collapse = ", "))
      }
    ),
    htmltools::tags$button(type = "button", class = "hmb-reset", "Reset")
  )
}
