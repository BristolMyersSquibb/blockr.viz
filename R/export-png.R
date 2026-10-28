# The Image download.

# The browser's picture, else the render object's painted image.
export_png <- function(x, file) {
  p <- exhibit_picture(x)
  if (!is.null(p)) {
    return(chart_capture_file(p, file))
  }
  obj <- exhibit_render(x)
  if (is.null(obj)) {
    stop("the picture is not ready yet; open the menu again", call. = FALSE)
  }
  write_exhibit_png(obj, file)
}

# A chart always gets a picture when the menu opens. A table has an image only
# when its render object paints one.
png_accepts <- function(x) {
  !is.null(exhibit_picture(x)) || identical(exhibit_kind(x), "chart") ||
    !is.null(exhibit_render(x))
}
