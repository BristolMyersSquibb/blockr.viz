# Headless probe for verify-legend-band.R: asserts that NO chart draws a native
# in-canvas ECharts legend and that every colour-mapped chart shows the HTML
# band instead. Uses chromote directly (its own browser profile) because the
# Playwright MCP profile is often held by a concurrent session.
#
#   Rscript dev/probe-legend-band.R [port] [out.png]
args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) args[[1]] else "3910"
out <- if (length(args) >= 2) args[[2]] else "/tmp/legend-band.png"

b <- chromote::ChromoteSession$new(width = 1600, height = 2400)
b$Page$navigate(sprintf("http://127.0.0.1:%s/", port))
Sys.sleep(20)  # board boot + block eval + first chart render

js <- function(expr) {
  r <- b$Runtime$evaluate(expr, returnByValue = TRUE, awaitPromise = TRUE)
  if (!is.null(r$exceptionDetails)) stop(r$exceptionDetails$text)
  r$result$value
}

# Wait for charts to appear.
for (i in 1:30) {
  n <- js("document.querySelectorAll('[_echarts_instance_]').length")
  if (n > 0) break
  Sys.sleep(2)
}
cat("echarts instances:", n, "\n")

probe <- js("
  JSON.stringify((function () {
    const panels = Array.from(document.querySelectorAll('[_echarts_instance_]'));
    const nativeShown = [];
    for (const d of panels) {
      const c = echarts.getInstanceByDom(d);
      if (!c) continue;
      const lg = (c.getOption().legend || [])[0];
      // show:true == a legend is painted INTO the canvas. Must never happen.
      if (lg && lg.show === true) nativeShown.push(lg.data ? lg.data.length : 0);
    }
    const bands = Array.from(document.querySelectorAll('.dd-legend-band'));
    const visible = bands.filter(e => e.offsetParent !== null);
    return {
      panels: panels.length,
      nativeLegendsShown: nativeShown.length,
      bandsInDom: bands.length,
      bandsVisible: visible.length,
      bandChips: visible.map(e => ({
        title: (e.querySelector('.dd-legend-title') || {}).textContent || null,
        chips: Array.from(e.querySelectorAll('.dd-legend-chip')).map(
          c => c.textContent.trim())
      }))
    };
  })())
")

cat(probe, "\n")
p <- jsonlite::fromJSON(probe, simplifyVector = FALSE)

# Chip toggle: click the FIRST chip of the LAST band and assert the level goes
# unselected in EVERY panel of that widget (the fan-out is the whole point of
# the band; with facets it must hit all panels, not just one).
toggle <- js("
  JSON.stringify((function () {
    const band = Array.from(document.querySelectorAll('.dd-legend-band'))
      .filter(e => e.offsetParent !== null).pop();
    if (!band) return {err: 'no band'};
    const chip = band.querySelector('.dd-legend-chip');
    if (!chip) return {err: 'no chip'};
    const name = chip.textContent.trim();
    const card = band.closest('.dd-chart-card') || band.parentElement;
    const panels = Array.from(card.querySelectorAll('[_echarts_instance_]'));
    const sel = () => panels.map(d => {
      const o = echarts.getInstanceByDom(d).getOption();
      const s = (o.legend || [])[0];
      return s && s.selected ? s.selected[name] : null;
    });
    const before = sel();
    chip.click();
    return {name: name, panels: panels.length, before: before,
            after: sel(), dimmed: chip.classList.contains('dd-legend-chip-off')};
  })())
")
cat("toggle:", toggle, "\n")
tg <- jsonlite::fromJSON(toggle, simplifyVector = FALSE)

b$screenshot(filename = out, scale = 1)
cat("screenshot:", out, "\n")

errs <- character()
if (p$nativeLegendsShown > 0)
  errs <- c(errs, sprintf("%d panel(s) still draw a NATIVE legend",
                          p$nativeLegendsShown))
if (p$bandsVisible == 0)
  errs <- c(errs, "no visible .dd-legend-band")
if (!is.null(tg$err)) {
  errs <- c(errs, paste("toggle probe:", tg$err))
} else {
  if (!isTRUE(tg$dimmed))
    errs <- c(errs, "clicked chip did not get .dd-legend-chip-off")
  off <- vapply(tg$after, function(x) identical(x, FALSE), logical(1))
  if (!all(off))
    errs <- c(errs, sprintf("chip '%s' unselected in only %d/%d panels",
                            tg$name, sum(off), length(off)))
}
if (length(errs)) {
  cat("FAIL:", paste(errs, collapse = "; "), "\n")
} else {
  cat(sprintf("PASS: %d panels, 0 native legends, %d visible bands\n",
              p$panels, p$bandsVisible))
}
b$close()
