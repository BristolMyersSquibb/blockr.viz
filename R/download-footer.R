# Download footer -------------------------------------------------------------
#
# One line a board prints under every picture it lets out: the chart,
# summarize table and heatmap images, and the decks and web pages that carry
# those images. Tables written as tables (xlsx, a table's own html and pptx)
# do not get it.
#
# Set per board as the `download_footer` option (Downloads in the settings
# sidebar), so the builder sets it and a locked board keeps it. The default
# for a new board is `getOption("blockr.viz.download_footer")`, which is how a
# deployment puts its disclaimer on every board. Empty, nothing is printed.
#
# The text may name:
#
#   {user}      who downloaded: `session$user`, empty without a login
#   {time}      when: "2026-10-09 14:32"
#   {filters}   the filter trail, as in a caption
#   {dataset}, {note}, {path}, ...
#               any field of the data's `blockr_provenance` attribute
#               (see `blockr.dm::provenance()`)
#
# A name the data does not have prints nothing.

#' Download footer board option
#'
#' A line of text printed under every downloaded picture: the image download
#' of a chart, summarize table or heatmap, and the PowerPoint and web page
#' downloads that carry such a picture. `{user}`, `{time}`, `{filters}` and
#' the fields of the data's `blockr_provenance` attribute (`{dataset}`,
#' `{note}`, ...) are filled in when the file is downloaded.
#'
#' @param value Default text. Empty prints nothing.
#' @param category Settings sidebar category.
#' @param ... Forwarded to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#' @examplesIf interactive()
#' new_download_footer_option("Exported by {user} on {time}")
#' @export
new_download_footer_option <- function(
    value = getOption("blockr.viz.download_footer", ""),
    category = "Downloads", ...) {

  blockr.core::new_board_option(
    id = "download_footer",
    default = value,
    ui = function(id) {
      shiny::tagList(
        shiny::textAreaInput(
          shiny::NS(id, "download_footer"),
          "Footer on downloaded pictures",
          value = value,
          rows = 2L
        ),
        shiny::helpText(
          paste(
            "Printed under charts and tables downloaded as images or slides.",
            "{user}, {time}, {dataset}, {note} and {filters} are filled in",
            "at download."
          )
        )
      )
    },
    server = function(..., session) {
      shiny::observeEvent(
        blockr.core::get_board_option_or_null("download_footer", session),
        {
          val <- blockr.core::get_board_option_value(
            "download_footer", session
          )
          shiny::updateTextAreaInput(session, "download_footer", value = val)
        }
      )
    },
    category = category,
    ...
  )
}

# The blocks whose downloads carry a picture contribute the option, so a
# board with one of them on it shows the setting without the app naming it
# (as with the exhibit font). Duplicates are dropped by
# `combine_board_options()`.

#' @exportS3Method blockr.core::board_options
board_options.chart_block <- function(x, ...) {
  blockr.core::combine_board_options(new_download_footer_option(),
                                     NextMethod())
}

#' @exportS3Method blockr.core::board_options
board_options.heatmap_block <- function(x, ...) {
  blockr.core::combine_board_options(new_download_footer_option(),
                                     NextMethod())
}

# The summarize table already contributes the exhibit font
# (R/exhibit-font.R); its method adds both.

# The footer text for one download, or NULL when there is none.
#' @noRd
download_footer_text <- function(template, data, user = NULL,
                                 time = Sys.time()) {

  template <- as.character(template %||% "")[1L]
  if (is.na(template) || !nzchar(trimws(template))) {
    return(NULL)
  }

  prov <- attr(data, "blockr_provenance", exact = TRUE)
  prov <- if (is.list(prov)) prov else list()
  trail <- attr(data, "blockr_filters", exact = TRUE)
  trail <- trail[!is.na(trail) & nzchar(trail)]

  fields <- c(
    prov,
    list(
      user = user,
      time = format(time, "%Y-%m-%d %H:%M"),
      filters = paste(unname(trail), collapse = "; ")
    )
  )

  value <- function(name) {
    v <- fields[[name]]
    if (is.null(v) || !length(v) || is.list(v)) return("")
    v <- as.character(v)[[1L]]
    if (is.na(v)) "" else v
  }

  m <- gregexpr("\\{\\s*[A-Za-z_][A-Za-z0-9_.]*\\s*\\}", template)
  tokens <- regmatches(template, m)[[1L]]
  regmatches(template, m) <- list(
    vapply(gsub("[{}[:space:]]", "", tokens), value, character(1L),
           USE.NAMES = FALSE)
  )

  out <- trimws(template)
  if (nzchar(out)) out
}

# The board's footer template, or NULL: no session, no board options, or a
# board without this one. Never an error, as for the exhibit font.
#' @noRd
board_download_footer <- function(session = blockr.core::get_session()) {
  tryCatch(
    blockr.core::get_board_option_or_null("download_footer", session),
    error = function(e) NULL
  )
}

# A picture with the board's footer under it, or the picture as it was when
# there is no footer to print. `data` is the block's input, for the
# provenance and the filter trail.
#' @noRd
download_footer_stamp <- function(x, data, session) {

  if (!inherits(x, "chart_capture")) {
    return(x)
  }

  txt <- download_footer_text(
    board_download_footer(session),
    tryCatch(data, error = function(e) NULL),
    user = session$user
  )
  if (is.null(txt)) {
    return(x)
  }

  stamp <- function(cap) {
    out <- png_add_footer(cap$png, txt, css_width = cap$width * 96)
    cap$png <- out$png
    cap$height <- cap$height + out$band / 96
    cap
  }

  x <- stamp(x)
  if (length(x$pages)) {
    x$pages <- lapply(x$pages, stamp)
  }
  x
}

# PNG bytes with a band of grey text added at the bottom, wrapped to the
# picture's width. Sized in CSS pixels, scaled by the density the picture was
# drawn at, so the text matches the caption the canvas drew. Returns the new
# bytes and the band's height in CSS pixels.
#' @noRd
png_add_footer <- function(png, text, css_width, font_px = 11, pad = 12) {

  if (!requireNamespace("png", quietly = TRUE) ||
        !requireNamespace("ragg", quietly = TRUE)) {
    stop("A download footer needs the 'png' and 'ragg' packages.",
         call. = FALSE)
  }

  img <- png::readPNG(png)
  h <- dim(img)[1L]
  w <- dim(img)[2L]
  ratio <- if (is.finite(css_width) && css_width > 0) w / css_width else 1
  res <- 96 * ratio
  ps <- font_px * 72 / 96
  pad_px <- pad * ratio
  line_px <- font_px * 1.4 * ratio

  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)

  # Measured on the device the text is drawn with, so the wrap is the one
  # the reader sees. A first device to measure, since the band's height
  # depends on how many lines there are.
  ragg::agg_png(file, width = w, height = h, units = "px", res = res)
  graphics::par(mar = c(0, 0, 0, 0), ps = ps)
  graphics::plot.new()
  graphics::plot.window(c(0, w), c(0, h), xaxs = "i", yaxs = "i")
  lines <- footer_wrap(text, w - 2 * pad_px)
  grDevices::dev.off()

  band <- ceiling(length(lines) * line_px + pad_px)

  ragg::agg_png(file, width = w, height = h + band, units = "px", res = res,
                background = "white")
  graphics::par(mar = c(0, 0, 0, 0), ps = ps)
  graphics::plot.new()
  graphics::plot.window(c(0, w), c(0, h + band), xaxs = "i", yaxs = "i")
  graphics::rasterImage(img, 0, band, w, h + band, interpolate = FALSE)
  ys <- band - pad_px / 2 - (seq_along(lines) - 0.5) * line_px
  graphics::text(pad_px, ys, lines, adj = c(0, 0.5), col = "#6b7280")
  grDevices::dev.off()

  list(png = readBin(file, "raw", file.size(file)), band = band / ratio)
}

# Words packed into lines no wider than `width` user units on the open
# device. A word wider than a line gets a line of its own.
#' @noRd
footer_wrap <- function(text, width) {
  out <- character()
  for (para in strsplit(text, "\n", fixed = TRUE)[[1L]]) {
    line <- ""
    for (word in strsplit(trimws(para), "\\s+")[[1L]]) {
      try_line <- if (nzchar(line)) paste(line, word) else word
      if (nzchar(line) && graphics::strwidth(try_line) > width) {
        out <- c(out, line)
        line <- word
      } else {
        line <- try_line
      }
    }
    out <- c(out, line)
  }
  out
}
