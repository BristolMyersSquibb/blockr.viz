# The Image download.

# The picture when the browser drew one; else the render object's own
# painted image (the groups table and cards draw themselves in R).
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

# A chart always has a picture to give: when the menu opens the view sends
# one. A table has an image only when its render object paints one.
png_accepts <- function(x) {
  !is.null(exhibit_picture(x)) || identical(exhibit_kind(x), "chart") ||
    !is.null(exhibit_render(x))
}
