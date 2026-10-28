# Shading in the table model: `shadings` rules resolved to per-column cell
# visuals (data bars, heatmap ramps, colour sources), and the paint the screen
# and the downloads share.

# Normalizes `shadings` from restored state (a list) or the gear (a JSON
# string) to list(list(mode, cols), ...).
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
    # Optional power knobs the gear never writes. `source` decouples the
    # painted value from the displayed one (dd_shading_visuals()).
    if (!is.null(s$palette)) out$palette <- s$palette
    if (!is.null(s$domain)) {
      # JSON delivers c(lo, hi) as a list; the resolver does arithmetic on it.
      out$domain <- suppressWarnings(as.numeric(unlist(s$domain)))
    }
    if (!is.null(s$source) && nzchar(as.character(s$source)[1L])) {
      out$source <- as.character(s$source)[1L]
    }
    out
  })
}

# Resolves `shadings` to per-column cell visuals: column -> list(kind = "bar",
# fill, max) or list(kind = "bg", fun, src). Unlisted columns render plain.
# A rule with empty `cols` covers the numeric columns no other rule claims;
# explicit columns that drop out stay a visible no-op; the first rule wins a
# column. diverging/sequential pool one domain over the rule's columns; `bar`
# scales per column on absolute magnitude.
# `source` (diverging/sequential): the column whose values drive the paint, as
# a plain name (paints all the rule's columns) or a `{col}` template pairing
# each column with its own companion. Source columns are not shading
# candidates and come back in attribute "hidden" for the caller to drop; the
# domain pools over source values, which must be numeric.
dd_shading_visuals <- function(shadings, data, value_cols) {
  out <- list()
  if (!length(shadings)) return(out)
  num_cols <- value_cols[vapply(data[value_cols], is.numeric, logical(1L))]
  # Expand each rule's `source` first, so sources are known before candidates
  # resolve (a companion must never be shaded or displayed).
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
      # Paired source per column; NA (none resolvable) paints from the
      # column's own values.
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

# Text colour over a background: white on dark, near-black on light, by
# luminance. One rule for every painted cell. Vectorized.
dt_fg_for_rgb <- function(r, g, b) {
  lum <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
  ifelse(lum < 0.55, "#ffffff", "#111827")
}

dt_fg_for_hex <- function(hex) {
  m <- grDevices::col2rgb(hex)
  dt_fg_for_rgb(m[1L, ], m[2L, ], m[3L, ])
}

# The heatmap ramp for a shading rule: a function vectorized over `v` that
# returns list(bg =, fg =) hex vectors.
dt_color_fun <- function(type, domain, palette) {
  lo <- domain[1L]
  hi <- domain[2L]
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1L]
  bg_fg <- function(r, g, b) {
    list(bg = grDevices::rgb(r, g, b, maxColorValue = 255),
         fg = dt_fg_for_rgb(r, g, b))
  }

  if (identical(type, "diverging")) {
    pal <- palette %||% viz_palette("diverging", 3L, DT_DIVERGING_FALLBACK)
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    c3 <- hex2rgb(pal[3L])
    # Centred on 0 (white at zero); the domain is symmetric around 0.
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

# The paint of one shaded column as the screen draws it. `sv` is a "bg" entry
# of dd_shading_visuals(). Returns list(bg =, fg =), a hex per row of `data`,
# NA where unpainted (an NA cell or a missing paint value). With a colour source
# (`sv$src`) the paint reads the companion column. Downloads read it too.
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

#' Cell-colour spec for a table
#'
#' How numeric cells are painted; the data is never changed. `"diverging"`
#' is a scale centred on 0 (a correlation matrix), `"sequential"` a low to
#' high ramp, `"bar"` an in-cell bar scaled per column on absolute values.
#'
#' @param type `"diverging"`, `"sequential"`, or `"bar"`.
#' @param domain `NULL` to infer from the data (symmetric around 0 for
#'   diverging), or `c(min, max)`. Ignored for `"bar"`.
#' @param palette `NULL` for the defaults, or colours: three for diverging
#'   (low, mid, high), two for sequential, one for the bar.
#' @param columns `NULL` for every numeric column, re-resolved on new data,
#'   or the columns to paint. A missing column is skipped.
#' @param source `NULL` to paint by the cell's own value, a column name to
#'   paint every shaded column by that column, or a template such as
#'   `"{col} (sev)"` for one companion per column. Source columns are hidden
#'   and must be numeric. Diverging and sequential only.
#' @return A cell-colour spec list.
#' @examples
#' drilldown_table_color("sequential", domain = c(0, 100))
#' @export
drilldown_table_color <- function(type = c("diverging", "sequential", "bar"),
                                   domain = NULL, palette = NULL,
                                   columns = NULL, source = NULL) {
  type <- match.arg(type)
  list(type = type, domain = domain, palette = palette, columns = columns,
       source = source)
}
