# Feasibility probe: can a chart be rendered and captured OFF SCREEN, at a
# size we choose, without anyone opening its panel?
#
#   Rscript blockr.viz/dev/capture-offscreen-probe.R <port> [outdir]
#
# That is the question slide generation asks. A dormant dock panel has no
# canvas, so fronting every chart tab in turn is one answer (visible, slow,
# and it captures the panel's box rather than the slide's). The other is to
# mount a chart in a hidden host sized to the SLIDE and capture that: no
# flicker, no fronting, and the canvas makes its layout decisions against the
# box the picture will actually live in.
#
# This drives the second by hand through an already-open board: copy a live
# block's columns / data / config into a fresh instance in an offscreen div
# at slide width, then compose it.

args <- commandArgs(trailingOnly = TRUE)
port <- args[[1L]]
out <- if (length(args) > 1L) args[[2L]] else "/workspace/_scratch/capture-proto"

dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(chromote)

b <- ChromoteSession$new()
b$Emulation$setDeviceMetricsOverride(
  width = 1900L, height = 1000L, deviceScaleFactor = 1, mobile = FALSE
)
b$Page$navigate(sprintf("http://127.0.0.1:%s/", port))
Sys.sleep(25)

eval_js <- function(js) {
  b$Runtime$evaluate(js, returnByValue = TRUE)$result$value
}

front <- function(name) {
  eval_js(sprintf("(function(name){
    const t = [...document.querySelectorAll('.dv-default-tab')]
      .find(el => el.textContent.trim() === name);
    if (!t) return 'missing';
    for (const type of ['pointerdown','mousedown','pointerup','mouseup','click'])
      t.dispatchEvent(new MouseEvent(type, {bubbles: true}));
    return 'ok';
  })(%s)", jsonlite::toJSON(name, auto_unbox = TRUE)))
}

message("fronting the source chart: ",
        front(if (length(args) > 2L) args[[3L]] else "Oneway Anova Analysis"))
Sys.sleep(8)

# The offscreen host: laid out (a display:none element measures 0 and the
# composer bails), just parked outside the viewport. 11.9in of slide at 96dpi.
# The box the picture will occupy on the slide: the panel's own aspect ratio
# fitted into the slide's content area, so the canvas lays out for the shape
# the image ends up at rather than for the panel it came from.
ratio <- if (length(args) > 3L) as.numeric(args[[4L]]) else 2

message("mounting offscreen at the slide box: ", eval_js(sprintf("(function(pr){
  const src = [...document.querySelectorAll('.drilldown-chart-container')]
    .map(e => e._block).find(x => x && x.columns && x.columns.length);
  if (!src) return 'no source chart';
  document.getElementById('capture-host-wrap')?.remove();
  // Slide content box at 96dpi: 11.9in wide, 5.5in tall under the title.
  const BOX_W = 1142, BOX_H = 528;
  const r = src.el.getBoundingClientRect();
  const aspect = (r.width && r.height) ? r.width / r.height : BOX_W / BOX_H;
  const fit = Math.min(BOX_W / aspect, BOX_H);
  const W = Math.round(fit * aspect), H = Math.round(fit);
  const wrap = document.createElement('div');
  wrap.id = 'capture-host-wrap';
  wrap.style.cssText =
    'position:fixed;left:-20000px;top:0;width:' + W + 'px;height:' + H + 'px;';
  const host = document.createElement('div');
  host.id = 'capture-host';
  host.className = 'drilldown-chart-container';
  host.style.cssText = 'width:' + W + 'px;height:' + H + 'px;';
  wrap.appendChild(host);
  document.body.appendChild(wrap);
  // Construct the same class the binding constructs. Reached through the
  // live instance's prototype, so the probe needs no global export.
  const Ctor = Object.getPrototypeOf(src).constructor;
  host._block = new Ctor(host);
  if (!host._block) return 'not constructed';
  host._block.setData(src.columns, src.data, src.config, src.argHelp,
                      (src.dataRev || 0) + 1);
  host._block._captureRatio = pr;
  return W + ' x ' + H + ' css px at pixelRatio ' + pr;
})(%s)", ratio)))

Sys.sleep(6)

message("composing: ", eval_js(
  "(function(b){ b._downloadImage(false, {pixelRatio: b._captureRatio});
     return 'ok'; })(document.getElementById('capture-host')._block)"
))

url <- local({
  t0 <- Sys.time()
  repeat {
    v <- eval_js(
      "(document.getElementById('capture-host')._block._lastCapture) || null"
    )
    if (!is.null(v) && nzchar(v)) return(v)
    if (as.numeric(Sys.time() - t0, units = "secs") > 30) return(NULL)
    Sys.sleep(1)
  }
})

if (is.null(url)) {
  message("nothing composed offscreen")
} else {
  raw <- jsonlite::base64_dec(sub("^data:image/png;base64,", "", url))
  f <- file.path(out, sprintf("offscreen-pr%g.png", ratio))
  writeBin(raw, f)
  d <- tryCatch(dim(png::readPNG(f)), error = function(e) NULL)
  message(sprintf("offscreen capture: %.0f KB, %s px -> %s", length(raw) / 1024,
                  if (is.null(d)) "?" else paste(d[[2L]], "x", d[[1L]]), f))
}

b$close()
