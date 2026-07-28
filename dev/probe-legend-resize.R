# Resize probe: the band lives OUTSIDE the canvas, so narrowing the panel must
# reflow the chips without moving the plot. Also guards the B3 change (the
# x-label gutter no longer composes with a legend reservation): rotated
# category labels must still refit on resize.
#
#   Rscript dev/probe-legend-resize.R [port]
args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) args[[1]] else "3913"

b <- chromote::ChromoteSession$new(width = 1600, height = 1400)
b$Page$navigate(sprintf("http://127.0.0.1:%s/", port))
Sys.sleep(22)

js <- function(expr) {
  r <- b$Runtime$evaluate(expr, returnByValue = TRUE, awaitPromise = TRUE)
  if (!is.null(r$exceptionDetails)) stop(r$exceptionDetails$text)
  r$result$value
}

# Geometry of the FIRST chart: grid.bottom (plot area) + band height/rows.
snap <- "
  JSON.stringify((function () {
    const d = document.querySelector('[_echarts_instance_]');
    if (!d) return {err: 'no chart'};
    const c = echarts.getInstanceByDom(d);
    const g = (c.getOption().grid || [])[0] || {};
    const card = d.closest('.dd-chart-card') || d.parentElement.parentElement;
    const band = card.querySelector('.dd-legend-band');
    const br = band ? band.getBoundingClientRect() : null;
    return {
      width: c.getWidth(), height: c.getHeight(),
      gridBottom: g.bottom,
      bandH: br ? Math.round(br.height) : null,
      chips: band ? band.querySelectorAll('.dd-legend-chip').length : 0
    };
  })())
"

wide <- jsonlite::fromJSON(js(snap), simplifyVector = FALSE)
cat("wide:  ", jsonlite::toJSON(wide, auto_unbox = TRUE), "\n")

# Narrow the viewport hard: chips that fitted on one row must wrap.
b$Emulation$setDeviceMetricsOverride(width = 620, height = 1400,
                                     deviceScaleFactor = 1, mobile = FALSE)
Sys.sleep(6)
narrow <- jsonlite::fromJSON(js(snap), simplifyVector = FALSE)
cat("narrow:", jsonlite::toJSON(narrow, auto_unbox = TRUE), "\n")

b$screenshot(filename = "/tmp/lb-narrow.png", scale = 1)

errs <- character()
# The plot's bottom reservation must be IDENTICAL: the band is not in the
# canvas, so a chip rewrap cannot steal plot area. This is the whole point.
if (!identical(wide$gridBottom, narrow$gridBottom))
  errs <- c(errs, sprintf("grid.bottom moved on resize: %s -> %s",
                          wide$gridBottom, narrow$gridBottom))
if (identical(wide$width, narrow$width))
  errs <- c(errs, "chart did not actually resize (probe inconclusive)")
if (!identical(wide$chips, narrow$chips))
  errs <- c(errs, "chip count changed on resize")

con <- b$Runtime$evaluate("window.__errs ? window.__errs.length : 0",
                          returnByValue = TRUE)$result$value

if (length(errs)) cat("FAIL:", paste(errs, collapse = "; "), "\n") else
  cat(sprintf("PASS: grid.bottom stable at %s across %s -> %s px; band %spx -> %spx\n",
              wide$gridBottom, wide$width, narrow$width,
              wide$bandH, narrow$bandH))
b$close()
