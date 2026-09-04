# Drive side of the capture prototype: front a chart tab, open its download
# menu (which is what makes the canvas compose itself and post the bitmap to
# R), and write both the on-screen card and the composed capture to disk so
# the two can be compared.
#
#   Rscript blockr.viz/dev/capture-drive.R <port> [outdir]

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

eval_js <- function(js) {
  b$Runtime$evaluate(js, returnByValue = TRUE)$result$value
}

message("waiting for the board ...")
Sys.sleep(25)

front_tab <- function(name) {
  eval_js(sprintf("(function(name){
    const t = [...document.querySelectorAll('.dv-default-tab')]
      .find(el => el.textContent.trim() === name);
    if (!t) return 'missing';
    for (const type of ['pointerdown','mousedown','pointerup','mouseup','click'])
      t.dispatchEvent(new MouseEvent(type, {bubbles: true}));
    return 'ok';
  })(%s)", jsonlite::toJSON(name, auto_unbox = TRUE)))
}

chart_rect <- function() {
  v <- eval_js("(function(){
    const el = [...document.querySelectorAll('.drilldown-chart-container')]
      .find(e => {
        const r = e.getBoundingClientRect();
        if (r.width < 200 || r.height < 120) return false;
        const c = e.querySelector('canvas');
        return c && c.getBoundingClientRect().height > 100;
      });
    if (!el) return null;
    const r = el.closest('.blockr-card, .dv-view') || el;
    const rr = r.getBoundingClientRect();
    return JSON.stringify({x: rr.x, y: rr.y, width: rr.width, height: rr.height});
  })()")
  if (is.null(v)) NULL else jsonlite::fromJSON(v)
}

await <- function(f, timeout = 40) {
  t0 <- Sys.time()
  repeat {
    v <- f()
    if (!is.null(v) && !identical(v, FALSE)) return(v)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) return(NULL)
    Sys.sleep(1)
  }
}

tab <- if (length(args) > 2L) args[[3L]] else "Oneway Anova Analysis"
message("fronting: ", tab, " -> ", front_tab(tab))

rect <- await(chart_rect)
if (is.null(rect)) stop("the chart never drew")
message(sprintf("panel on screen: %.0f x %.0f css px", rect$width, rect$height))
Sys.sleep(3)

b$screenshot(file.path(out, "screen.png"), selector = ".drilldown-chart-container")

# The download menu is what triggers the capture (see chart.js _hoistDownload).
message("opening the download menu: ", eval_js("(function(){
  const s = document.querySelector('.blockr-dl-menu > summary, .blockr-dl-xlsx');
  if (!s) return 'no menu';
  for (const type of ['pointerdown','mousedown','pointerup','mouseup','click'])
    s.dispatchEvent(new MouseEvent(type, {bubbles: true}));
  return 'clicked';
})()"))

url <- await(function() {
  v <- eval_js("(function(){
    const b = [...document.querySelectorAll('.drilldown-chart-container')]
      .map(e => e._block).find(x => x && x._lastCapture);
    return (b && b._lastCapture) || null;
  })()")
  if (is.null(v) || !nzchar(v)) NULL else v
}, timeout = 30)

if (is.null(url)) {
  message("no capture read back from the page; check the app log for ",
          "'chart capture: W x H px'")
} else {
  raw <- jsonlite::base64_dec(sub("^data:image/png;base64,", "", url))
  writeBin(raw, file.path(out, "capture.png"))
  message(sprintf("capture: %.0f KB -> %s", length(raw) / 1024,
                  file.path(out, "capture.png")))
}

b$close()
