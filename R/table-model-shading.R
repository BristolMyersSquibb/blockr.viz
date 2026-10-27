# Shading in the table model: the `shadings` rules of the records layout
# resolved to per-column cell visuals (data bars, heatmap ramps, colour
# sources), and the paint both the screen and the downloads read.

#' Normalize the `shadings` list (cell value-encoding rules) from a restored
#' block state (an R list) or from the gear (a JSON string) to
#' `list(list(mode, cols), ...)`. Mirrors dd_parse_summaries -- the two lists
#' share their shape family (`{func, cols}` / `{mode, cols}`).
#' @noRd
dd_parse_shadings <- function(v) {
  if (is.null(v)) return(list())
  ss <- if (is.character(v) && length(v) == 1L) {
    tryCatch(jsonlite::fromJSON(v, simplifyVector = FALSE),
             error = function(e) list())
  } else if (is.list(v)) {
    # A single drilldown_table_color()-style spec (has $type) reads as one rule.
    if (!is.null(v$type)) list(v) else v
  } else {
    list()
  }
  lapply(ss, function(s) {
    out <- list(
      mode = as.character(s$mode %||% s$type %||% "diverging")[1L],
      cols = as.character(unlist(s$cols %||% s$columns %||% character()))
    )
    # Optional power knobs (per-rule palette / fixed domain / color source)
    # ride along when present -- the gear never writes them, ctor / legacy
    # specs may. `source` decouples the painted value from the displayed one
    # (see dd_shading_visuals).
    if (!is.null(s$palette)) out$palette <- s$palette
    if (!is.null(s$domain)) {
      # A JSON restore delivers c(lo, hi) as a two-element list; the visuals
      # resolver does arithmetic on it, so normalize to numeric here.
      out$domain <- suppressWarnings(as.numeric(unlist(s$domain)))
    }
    if (!is.null(s$source) && nzchar(as.character(s$source)[1L])) {
      out$source <- as.character(s$source)[1L]
    }
    out
  })
}

#' Resolve the `shadings` rules to per-column cell visuals.
#'
#' Override semantics, mirroring dd_metric_plan: a rule with EMPTY `cols`
#' covers ALL numeric columns except those explicitly claimed by other rules
#' (so `bar on DOSE` + `diverging on []` bars DOSE and shades everything
#' else); explicit-but-dropped columns stay a visible no-op; among competing
#' rules the FIRST wins a column. diverging/sequential pool ONE domain across
#' the rule's resolved columns (a correlation matrix reads on one scale);
#' `bar` normalizes per column on absolute magnitude.
#'
#' **Color source** (diverging/sequential only, ctor-level): a rule may name
#' a `source` -- the column whose values drive the paint while the cell keeps
#' displaying its own. Two forms: a plain column name paints ALL of the
#' rule's columns by that one column's row values (the BI "shade A by B"),
#' and a `{col}` template (e.g. `"{col} (sev)"`) pairs each shaded column
#' with its own companion, re-resolved per render so it follows a dynamic
#' matrix (the AE heatmap: counts displayed, worst grade painted). Columns
#' referenced as a source are paint metadata, not data for the eye: they are
#' excluded from shading candidates here and reported in the `"hidden"`
#' attribute so the caller drops them from the display. The domain pools
#' over the SOURCE values (that is what is being encoded); sources must be
#' numeric.
#'
#' @return named list: column -> `list(kind = "bar", fill, max)` or
#'   `list(kind = "bg", fun)` (a dt_color_fun closure, plus `src` when a
#'   source drives the paint). Unlisted columns render plain. Attribute
#'   `"hidden"`: source columns the display must drop.
#' @noRd
dd_shading_visuals <- function(shadings, data, value_cols) {
  out <- list()
  if (!length(shadings)) return(out)
  num_cols <- value_cols[vapply(data[value_cols], is.numeric, logical(1L))]
  # Pass 1 -- expand every rule's `source` against the columns it could
  # cover, so sources are known BEFORE candidates resolve (a companion
  # column must never itself end up shaded, or displayed).
  expand_src <- function(src, cols) {
    if (grepl("{col}", src, fixed = TRUE)) {
      vapply(cols, function(cn) gsub("{col}", cn, src, fixed = TRUE),
             character(1L))
    } else {
      rep(src, length(cols))
    }
  }
  hidden <- unique(unlist(lapply(shadings, function(s) {
    src <- as.character(s$source %||% "")[1L]
    if (!nzchar(src)) return(character())
    intersect(unique(expand_src(src, num_cols)), names(data))
  })))
  num_cols <- setdiff(num_cols, hidden)
  claimed <- unique(unlist(lapply(
    shadings, function(s) as.character(s$cols %||% character())
  )))
  taken <- character()
  for (s in shadings) {
    mode <- as.character(s$mode %||% "diverging")[1L]
    raw  <- as.character(s$cols %||% character())
    cols <- if (length(raw)) intersect(raw, num_cols)
            else setdiff(num_cols, claimed)
    cols <- setdiff(cols, taken)
    if (!length(cols)) next
    taken <- c(taken, cols)
    if (identical(mode, "bar")) {
      fill <- (s$palette %||% "rgba(37, 99, 235, 0.22)")[1L]
      for (cn in cols) {
        v <- data[[cn]][is.finite(data[[cn]])]
        out[[cn]] <- list(kind = "bar", fill = fill,
                          max = if (length(v)) max(abs(v)) else 0)
      }
    } else {
      # Paired source per column (NA = none resolvable -> that column's
      # paint reads its own values, so a missing companion degrades to the
      # sourceless behaviour instead of rendering plain).
      src <- as.character(s$source %||% "")[1L]
      srcs <- if (nzchar(src)) {
        sc <- expand_src(src, cols)
        ifelse(sc %in% names(data), sc, NA_character_)
      } else {
        rep(NA_character_, length(cols))
      }
      pool_cols <- ifelse(is.na(srcs), cols, srcs)
      vals <- suppressWarnings(as.numeric(unlist(
        data[unique(pool_cols)], use.names = FALSE
      )))
      vals <- vals[is.finite(vals)]
      dom <- s$domain
      if (is.null(dom) && length(vals)) {
        dom <- if (identical(mode, "diverging")) {
          m <- max(abs(vals))   # symmetric around 0 (white at 0)
          c(-m, m)
        } else {
          range(vals)
        }
      }
      if (!is.null(dom) && length(vals) && diff(range(dom)) > 0) {
        fun <- dt_color_fun(mode, dom, s$palette)
        for (k in seq_along(cols)) {
          out[[cols[k]]] <- c(list(kind = "bg", fun = fun),
                              if (!is.na(srcs[k])) list(src = srcs[k]))
        }
      }
    }
  }
  attr(out, "hidden") <- hidden
  out
}

#' Per-cell inline style for a data bar: a left-anchored CSS gradient whose
#' width is `|v| / mx * 100%` (absolute-magnitude normalisation, no center
#' baseline -- matches the crossfilter block). Vectorised over the column so the
#' table stays on the single-`HTML()` fast path. Returns `""` for a degenerate
#' column (all-zero / non-finite max), i.e. no bar.
#' @noRd
dt_bar_style <- function(v, mx, fill) {
  if (!is.finite(mx) || mx <= 0) return(rep("", length(v)))
  # as.character on the rounded value formats each width independently (no
  # vectorized decimal-alignment, so 100 stays "100", not "100.0").
  pct <- as.character(round(pmax(0, pmin(100, abs(v) / mx * 100)), 2L))
  paste0(" style=\"background:linear-gradient(90deg,", fill, " 0 ", pct,
         "%,transparent ", pct, "%);\"")
}

#' Readable text colour over a background: white on dark, near-black on
#' light, by perceived luminance. ONE rule for every painted cell, so a
#' ramp cell and a scale-map cell (the heatmap block's two paint sources)
#' cannot disagree about when text flips to white. Vectorized.
#' @noRd
dt_fg_for_rgb <- function(r, g, b) {
  lum <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
  ifelse(lum < 0.55, "#ffffff", "#111827")
}

#' @noRd
dt_fg_for_hex <- function(hex) {
  m <- grDevices::col2rgb(hex)
  dt_fg_for_rgb(m[1L, ], m[2L, ], m[3L, ])
}

#' Build the heatmap ramp for a shading rule. The returned function is
#' VECTORIZED over `v` end-to-end (clamp, interpolation t, per-channel lerp,
#' `rgb()`, and the luminance contrast test all take vectors) and returns
#' `list(bg =, fg =)` of hex vectors -- the caller styles a whole column in
#' one call, keeping the shaded table on the same vectorized fast path as
#' dt_bar_style(). A 10k x 10 shaded table used to burn 100k scalar closure
#' calls per render here.
#' @noRd
dt_color_fun <- function(type, domain, palette) {
  lo <- domain[1L]
  hi <- domain[2L]
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1L]
  # Per-channel r/g/b vectors -> bg hex + contrast-tested fg hex.
  bg_fg <- function(r, g, b) {
    list(bg = grDevices::rgb(r, g, b, maxColorValue = 255),
         fg = dt_fg_for_rgb(r, g, b))
  }

  if (identical(type, "diverging")) {
    pal <- palette %||% viz_palette("diverging", 3L, DT_DIVERGING_FALLBACK)
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    c3 <- hex2rgb(pal[3L])
    # Diverging is centred on 0 (white-at-zero); the domain is symmetric around
    # 0 (set at inference). A meaningful zero is what "diverging" means -- for a
    # correlation matrix this puts white at r = 0 and gives +/- equal saturation.
    mid <- 0
    function(v) {
      v <- pmax(pmin(v, hi), lo)
      low <- v <= mid
      t_lo <- if (mid == lo) rep(0, length(v)) else (v - lo) / (mid - lo)
      t_hi <- if (hi == mid) rep(0, length(v)) else (v - mid) / (hi - mid)
      ch <- function(i) {
        round(ifelse(low, c1[i] + (c2[i] - c1[i]) * t_lo,
                          c2[i] + (c3[i] - c2[i]) * t_hi))
      }
      bg_fg(ch(1L), ch(2L), ch(3L))
    }
  } else {
    pal <- palette %||% viz_palette("sequential", 2L, DT_SEQUENTIAL_FALLBACK)
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    function(v) {
      v <- pmax(pmin(v, hi), lo)
      t <- if (hi == lo) rep(0, length(v)) else (v - lo) / (hi - lo)
      ch <- function(i) round(c1[i] + (c2[i] - c1[i]) * t)
      bg_fg(ch(1L), ch(2L), ch(3L))
    }
  }
}

#' The paint of one shaded column, as the screen draws it.
#'
#' `sv` is a `kind = "bg"` entry of dd_shading_visuals(). Returns
#' `list(bg =, fg =)`, one hex per row of `data`, NA where the cell is
#' unpainted: an NA cell, or a row whose paint value is missing. With a colour
#' SOURCE (`sv$src`) the paint reads the companion column at the same rows
#' while the cell keeps displaying its own value. dt_flat_build() turns this
#' into the cell styles, the downloads into paint columns, so the two cannot
#' disagree.
#' @noRd
dt_bg_paint <- function(sv, data, col) {
  n <- nrow(data)
  bg <- fg <- rep(NA_character_, n)
  v <- data[[col]]
  pv <- if (is.null(sv$src)) {
    suppressWarnings(as.numeric(v))
  } else {
    suppressWarnings(as.numeric(data[[sv$src]]))
  }
  ok <- !is.na(v) & is.finite(pv)
  if (any(ok)) {
    p <- sv$fun(pv[ok])
    bg[ok] <- p$bg
    fg[ok] <- p$fg
  }
  list(bg = bg, fg = fg)
}

