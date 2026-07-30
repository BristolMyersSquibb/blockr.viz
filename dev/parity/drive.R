# Drive side of the static-chart parity harness: fronts each chart tab in
# the dock (dockview tabs ignore .click(); dispatch the full pointer
# sequence) and screenshots the chart card -- title band, canvas, legend
# band -- to canvas-<id>.png. Writes the captured pixel size per state to
# sizes.csv so static.R can render at the same aspect.
#
#   Rscript dev/parity/drive.R <port> <outdir>

args <- commandArgs(trailingOnly = TRUE)
port <- args[[1L]]
out <- if (length(args) > 1L) args[[2L]] else "/workspace/_scratch/static-chart-parity"

dir.create(out, recursive = TRUE, showWarnings = FALSE)

viz <- "/workspace/_worktrees/blockr.viz-static-charts"
source(file.path(viz, "dev", "parity", "states.R"))

library(chromote)

b <- ChromoteSession$new()
b$Emulation$setDeviceMetricsOverride(
  width = 1500L, height = 900L, deviceScaleFactor = 1, mobile = FALSE
)
b$Page$navigate(sprintf("http://127.0.0.1:%s/", port))

message("waiting for the board to build ...")
Sys.sleep(25)

eval_js <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  r$result$value
}

front_tab <- function(name) {
  js <- sprintf("(function(name){
    const tabs = [...document.querySelectorAll('.dv-default-tab')];
    const t = tabs.find(el => el.textContent.trim() === name);
    if (!t) return 'missing';
    for (const type of ['pointerdown','mousedown','pointerup','mouseup','click']) {
      t.dispatchEvent(new MouseEvent(type, {bubbles: true}));
    }
    return 'ok';
  })(%s)", jsonlite::toJSON(name, auto_unbox = TRUE))
  eval_js(js)
}

# The fronted panel's chart card, but only once it has actually DRAWN: a
# lazy panel binds and renders seconds after its tab fronts, so require a
# visible container with a canvas of real size inside it.
visible_chart_rect <- function() {
  js <- "(function(){
    const els = [...document.querySelectorAll('.drilldown-chart-container')];
    const el = els.find(e => {
      const r = e.getBoundingClientRect();
      if (r.width < 200 || r.height < 120) return false;
      const c = e.querySelector('canvas');
      return c && c.getBoundingClientRect().height > 100;
    });
    if (!el) return null;
    const r = el.getBoundingClientRect();
    return JSON.stringify({x: r.x, y: r.y, width: r.width, height: r.height});
  })()"
  v <- eval_js(js)
  if (is.null(v)) NULL else jsonlite::fromJSON(v)
}

await_chart <- function(timeout = 30) {
  t0 <- Sys.time()
  repeat {
    rect <- visible_chart_rect()
    if (!is.null(rect)) {
      return(rect)
    }
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) {
      return(NULL)
    }
    Sys.sleep(1)
  }
}

sizes <- list()

for (id in names(parity_states)) {
  st <- front_tab(id)
  if (!identical(st, "ok")) {
    message("tab not found: ", id, " (", st, ")")
    next
  }
  rect <- await_chart()
  if (is.null(rect)) {
    message("no visible chart for: ", id)
    next
  }
  Sys.sleep(1.5) # let the draw settle (animations, legend band)
  f <- file.path(out, paste0("canvas-", id, ".png"))
  b$screenshot(
    f,
    cliprect = c(rect$x, rect$y, rect$width, rect$height)
  )
  sizes[[id]] <- data.frame(id = id, width = rect$width, height = rect$height)
  message("captured ", f, " (", rect$width, "x", rect$height, ")")
}

if (length(sizes)) {
  utils::write.csv(do.call(rbind, sizes), file.path(out, "sizes.csv"),
                   row.names = FALSE)
}

b$close()
message("drive done")
