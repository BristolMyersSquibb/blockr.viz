# Heatmap cell coloring: golden equivalence of the vectorized dt_color_fun()
# ramp against the original scalar closure, at both levels -- the ramp
# function itself and the styles it lands in dt_table_tag()'s HTML. The
# rewrite (pmin/pmax clamp, vectorized lerp + rgb() + luminance test) must be
# byte-identical; this reference copy of the old closure is the oracle.

ref_color_fun <- function(type, domain, palette) {
  lo <- domain[1L]
  hi <- domain[2L]
  hex2rgb <- function(h) grDevices::col2rgb(h)[, 1L]
  lerp <- function(a, b, t) round(a + (b - a) * t)
  lum <- function(rgb) {
    (0.299 * rgb[1L] + 0.587 * rgb[2L] + 0.114 * rgb[3L]) / 255
  }
  to_hex <- function(rgb) {
    grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)
  }
  if (identical(type, "diverging")) {
    pal <- palette %||% c("#99000d", "#ffffff", "#08306b")
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    c3 <- hex2rgb(pal[3L])
    mid <- 0
    function(v) {
      v <- max(min(v, hi), lo)
      if (v <= mid) {
        t <- if (mid == lo) 0 else (v - lo) / (mid - lo)
        rgb <- c(lerp(c1[1L], c2[1L], t), lerp(c1[2L], c2[2L], t),
                 lerp(c1[3L], c2[3L], t))
      } else {
        t <- if (hi == mid) 0 else (v - mid) / (hi - mid)
        rgb <- c(lerp(c2[1L], c3[1L], t), lerp(c2[2L], c3[2L], t),
                 lerp(c2[3L], c3[3L], t))
      }
      list(bg = to_hex(rgb), fg = if (lum(rgb) < 0.55) "#ffffff" else "#111827")
    }
  } else {
    pal <- palette %||% c("#eef2ff", "#1d4ed8")
    c1 <- hex2rgb(pal[1L])
    c2 <- hex2rgb(pal[2L])
    function(v) {
      v <- max(min(v, hi), lo)
      t <- if (hi == lo) 0 else (v - lo) / (hi - lo)
      rgb <- c(lerp(c1[1L], c2[1L], t), lerp(c1[2L], c2[2L], t),
               lerp(c1[3L], c2[3L], t))
      list(bg = to_hex(rgb), fg = if (lum(rgb) < 0.55) "#ffffff" else "#111827")
    }
  }
}

expect_ramp_golden <- function(type, domain, palette, vals) {
  ref <- ref_color_fun(type, domain, palette)
  new <- dt_color_fun(type, domain, palette)
  old <- lapply(vals, ref)
  got <- new(vals)
  expect_identical(got$bg, vapply(old, `[[`, character(1L), "bg"))
  expect_identical(got$fg, vapply(old, `[[`, character(1L), "fg"))
}

test_that("dt_color_fun() vectorized ramp matches the scalar closure", {
  # Domain edges, mid crossings, out-of-domain clamps, infinities, tiny +/-.
  vals <- c(-10, -5, -1.2345, -1e-9, 0, 1e-9, 0.5, 2.999, 5, 7, 10, Inf, -Inf)
  expect_ramp_golden("diverging", c(-5, 5), NULL, vals)
  expect_ramp_golden("diverging", c(0, 10), NULL, vals)     # mid == lo branch
  expect_ramp_golden("diverging", c(-3, 3),
                     c("#ff0000", "#eeeeee", "#0000ff"), vals)
  expect_ramp_golden("sequential", c(-5, 5), NULL, vals)
  expect_ramp_golden("sequential", c(0, 100),
                     c("#ffffff", "#000000"), vals)
  set.seed(9)
  expect_ramp_golden("diverging", c(-5, 5), NULL, runif(500, -6, 6))
  expect_ramp_golden("sequential", c(-2, 8), NULL, runif(500, -3, 9))
})

extract_styles <- function(tag) {
  html <- as.character(htmltools::renderTags(tag)$html)
  m <- gregexpr("background:#[0-9A-Fa-f]{6};color:#[0-9A-Fa-f]{6};", html)
  regmatches(html, m)[[1L]]
}

test_that("dt_table_tag() heatmap styles equal the scalar-closure styles", {
  set.seed(7)
  df <- data.frame(
    region = paste0("r", 1:50),
    x = c(NA, rnorm(48, 10, 4), NA),        # NA cells: no style, em-dash cell
    y = c(rnorm(49, -2, 3), NA),            # negative values, diverging scale
    stringsAsFactors = FALSE
  )

  # Diverging over both columns pools ONE symmetric domain (dd_shading_visuals).
  vals <- unlist(df[c("x", "y")], use.names = FALSE)
  m <- max(abs(vals[is.finite(vals)]))
  ref <- ref_color_fun("diverging", c(-m, m), NULL)
  # Styles appear in the HTML in row-major cell order (per row: x, then y),
  # skipping NA cells.
  cell_vals <- as.vector(t(as.matrix(df[c("x", "y")])))
  expected <- vapply(
    cell_vals[!is.na(cell_vals)],
    function(v) {
      s <- ref(v)
      paste0("background:", s$bg, ";color:", s$fg, ";")
    },
    character(1L)
  )
  tag <- dt_table_tag(df, label_col = "region", value_cols = c("x", "y"),
                      shadings = list(list(mode = "diverging",
                                           cols = character())))
  expect_identical(extract_styles(tag), expected)

  # Sequential on one column with a custom palette.
  ref_x <- ref_color_fun("sequential", range(df$x[is.finite(df$x)]),
                         c("#ffffff", "#123456"))
  expected_x <- vapply(df$x[!is.na(df$x)], function(v) {
    s <- ref_x(v)
    paste0("background:", s$bg, ";color:", s$fg, ";")
  }, character(1L))
  tag_x <- dt_table_tag(df, label_col = "region", value_cols = c("x", "y"),
                        shadings = list(list(mode = "sequential", cols = "x",
                                             palette = c("#ffffff", "#123456"))))
  expect_identical(extract_styles(tag_x), expected_x)

  # NA cells carry no background style at all (they render as em-dash cells).
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_identical(
    lengths(regmatches(html, gregexpr("&mdash;", html)))[[1L]],
    sum(is.na(df$x)) + sum(is.na(df$y))
  )
})

test_that("dt_table_tag() zero-range column renders unshaded", {
  # All-equal values -> zero-width domain -> dd_shading_visuals emits no ramp
  # (same as before the vectorization): not a single background style.
  df <- data.frame(region = c("a", "b", "c"), x = c(3, 3, 3),
                   stringsAsFactors = FALSE)
  tag <- dt_table_tag(df, label_col = "region", value_cols = "x",
                      shadings = list(list(mode = "sequential", cols = "x")))
  expect_identical(extract_styles(tag), character(0L))
})

test_that("dt_table_tag() row_color accent bar is untouched by the ramp", {
  # The row_color path (box-shadow accent from precomputed row_hex) does not
  # go through dt_color_fun; a shaded table with row tint keeps both.
  df <- data.frame(region = c("a", "b", "c"), x = c(-1, 0, 2),
                   stringsAsFactors = FALSE)
  tag <- dt_table_tag(df, label_col = "region", value_cols = "x",
                      shadings = list(list(mode = "diverging", cols = "x")),
                      row_hex = c("#ff0000", NA, "#00ff00"), color = "region")
  html <- as.character(htmltools::renderTags(tag)$html)
  expect_identical(
    lengths(regmatches(html, gregexpr("box-shadow:inset 3px 0 0 0 #",
                                      html)))[[1L]],
    2L
  )
  expect_identical(length(extract_styles(tag)), 3L)
})
