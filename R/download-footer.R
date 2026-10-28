# Download footer -------------------------------------------------------------
#
# One line a board prints under everything it lets out: drawn into a picture,
# or the last caption line of a table written as a table (xlsx, html, pptx).
# Set per board as the `download_footer` option; the default for a new board is
# `getOption("blockr.viz.download_footer")`. Empty prints nothing.
#
# The text may name {user} (`session$user`, empty without a login), {time}
# ("2026-10-09 14:32"), {filters} (the filter trail, as in a caption) and any
# field of the data's `blockr_provenance` attribute ({dataset}, {note}, ...).
# A name the data does not have prints nothing.

#' Download footer board option
#'
#' A line of text printed under every download of a chart or table: drawn
#' under the picture for an image (and the PowerPoint and web page that carry
#' it), added as the last caption line for a table written as a table.
#' `{user}`, `{time}`, `{filters}` and the fields of the data's
#' `blockr_provenance` attribute (`{dataset}`, `{note}`, ...) are filled in
#' when the file is downloaded.
#'
#' @param value Default text. Empty prints nothing.
#' @param category Settings sidebar category.
#' @param ... Forwarded to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#' @examples
#' new_download_footer_option("Exported by {user} on {time}")
#' @export
new_download_footer_option <- function(
    value = getOption("blockr.viz.download_footer", ""),
    category = "Downloads", ...) {

  new_input_option(
    "download_footer", value,
    input = function(inputId) {
      shiny::textAreaInput(inputId, "Footer on downloads", value = value,
                           rows = 2L)
    },
    update = function(session, id, val) {
      shiny::updateTextAreaInput(session, id, value = val)
    },
    help = paste("Printed under every downloaded chart and table.",
                 "{user}, {time}, {dataset}, {note} and {filters} are filled",
                 "in at download."),
    category = category, ...
  )
}

# A board option with one input in the settings sidebar. `input(inputId)`
# builds the widget; `update(session, id, value)` pushes a changed value to it.
new_input_option <- function(option_id, default, input, update, help,
                             category, ...) {
  blockr.core::new_board_option(
    id = option_id,
    default = default,
    ui = function(id) {
      shiny::tagList(input(shiny::NS(id, option_id)), shiny::helpText(help))
    },
    server = function(..., session) {
      shiny::observeEvent(
        blockr.core::get_board_option_or_null(option_id, session),
        update(session, option_id,
               blockr.core::get_board_option_value(option_id, session))
      )
    },
    category = category,
    ...
  )
}

# Chart blocks contribute the option. Table blocks add it in R/exhibit-font.R.

#' @exportS3Method blockr.core::board_options
board_options.chart_block <- function(x, ...) {
  blockr.core::combine_board_options(new_download_footer_option(),
                                     NextMethod())
}

# The footer text for one download, or NULL when there is none.
download_footer_text <- function(template, data, user = NULL,
                                 time = Sys.time()) {

  template <- as.character(template %||% "")[1L]
  if (is.na(template) || !nzchar(trimws(template))) return(NULL)

  prov <- attr(data, "blockr_provenance", exact = TRUE)
  prov <- if (is.list(prov)) prov else list()
  trail <- attr(data, "blockr_filters", exact = TRUE)
  trail <- trail[!is.na(trail) & nzchar(trail)]

  fields <- c(prov, list(user = user,
                         time = format(time, "%Y-%m-%d %H:%M"),
                         filters = paste(unname(trail), collapse = "; ")))

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
# board without this one.
board_download_footer <- function(session = blockr.core::get_session()) {
  tryCatch(
    blockr.core::get_board_option_or_null("download_footer", session),
    error = function(e) NULL
  )
}

# The footer text for a download, or NULL. `data` is the block's input, for
# the provenance and the filter trail.
download_footer_for <- function(data, session) {
  download_footer_text(board_download_footer(session),
                       tryCatch(data, error = function(e) NULL),
                       user = session$user)
}

# The caption with the board's footer as its last line.
download_footer_caption <- function(caption, data, session) {
  txt <- download_footer_for(data, session)
  if (is.null(txt)) return(caption)
  cap <- if (is.character(caption) && length(caption)) caption[[1L]]
  if (is.null(cap) || is.na(cap) || !nzchar(cap)) txt else paste(cap, txt, sep = "\n")
}

# A picture with the board's footer under it, or the picture itself when
# there is no footer.
download_footer_stamp <- function(x, data, session) {
  if (!inherits(x, "chart_capture")) return(x)
  txt <- download_footer_for(data, session)
  if (is.null(txt)) return(x)
  out <- png_add_footer(x$png, txt, css_width = x$width * 96)
  x$png <- out$png
  x$height <- x$height + out$band / 96
  x
}

# PNG bytes with a band of grey text added at the bottom, wrapped to the
# picture's width. Sized in CSS pixels, scaled by the density the picture was
# drawn at, so the text matches the caption the canvas drew. Returns the new
# bytes and the band's height in CSS pixels.
png_add_footer <- function(png, text, css_width, font_px = 11, pad = 12) {

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
  open_device <- function(height) {
    ragg::agg_png(file, width = w, height = height, units = "px", res = res,
                  background = "white")
    graphics::par(mar = c(0, 0, 0, 0), ps = ps)
    graphics::plot.new()
    graphics::plot.window(c(0, w), c(0, height), xaxs = "i", yaxs = "i")
  }

  # A first device measures the wrap, since the band's height depends on the
  # number of lines.
  open_device(h)
  lines <- footer_wrap(text, w - 2 * pad_px)
  grDevices::dev.off()

  band <- ceiling(length(lines) * line_px + pad_px)

  open_device(h + band)
  graphics::rasterImage(img, 0, band, w, h + band, interpolate = FALSE)
  ys <- band - pad_px / 2 - (seq_along(lines) - 0.5) * line_px
  graphics::text(pad_px, ys, lines, adj = c(0, 0.5), col = "#6b7280")
  grDevices::dev.off()

  list(png = readBin(file, "raw", file.size(file)), band = band / ratio)
}

# Words packed into lines no wider than `width` user units on the open
# device. A word wider than a line gets a line of its own.
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
