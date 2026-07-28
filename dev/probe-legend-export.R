# Console + export probe. Two things the deletions could plausibly break:
#  - a dangling reference to a removed helper (would surface as a console error
#    at render time, not at parse time);
#  - _downloadImage's band compositing, which previously only ran for FACETED
#    charts (it keys off legendEl visibility, now true for single panels too).
#
#   Rscript dev/probe-legend-export.R [port]
args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) args[[1]] else "3913"

b <- chromote::ChromoteSession$new(width = 1600, height = 1400)
b$Page$enable()
# Installed BEFORE any page script, so boot-time errors are captured too.
b$Page$addScriptToEvaluateOnNewDocument(source = "
  window.__errs = [];
  addEventListener('error', e => window.__errs.push('onerror: ' + e.message));
  const ce = console.error;
  console.error = function (...a) { window.__errs.push(a.join(' ')); ce.apply(console, a); };
  // Record that the export actually composed a PNG instead of downloading it.
  window.__exported = [];
  const origClick = HTMLAnchorElement.prototype.click;
  HTMLAnchorElement.prototype.click = function () {
    if (this.download) { window.__exported.push({name: this.download, len: (this.href || '').length}); return; }
    return origClick.apply(this, arguments);
  };
")
b$Page$navigate(sprintf("http://127.0.0.1:%s/", port))
Sys.sleep(22)

js <- function(expr) {
  r <- b$Runtime$evaluate(expr, returnByValue = TRUE, awaitPromise = TRUE)
  if (!is.null(r$exceptionDetails)) stop(r$exceptionDetails$text)
  r$result$value
}

boot_errs <- js("JSON.stringify(window.__errs)")
cat("console errors after boot:", boot_errs, "\n")

# Trigger the export on the FIRST chart (single panel -> the newly-exercised path).
js("(function () {
  const btn = document.querySelector('.dd-chart-dl');
  if (!btn) return 'no button';
  btn.click();
  return 'clicked';
})()")
Sys.sleep(6)

exported <- js("JSON.stringify(window.__exported)")
after_errs <- js("JSON.stringify(window.__errs)")
cat("exported:", exported, "\n")
cat("console errors after export:", after_errs, "\n")

e1 <- jsonlite::fromJSON(boot_errs, simplifyVector = FALSE)
e2 <- jsonlite::fromJSON(after_errs, simplifyVector = FALSE)
ex <- jsonlite::fromJSON(exported, simplifyVector = FALSE)

errs <- character()
if (length(e2)) {
  errs <- c(errs, sprintf("%d console error(s)", length(e2)))
}
if (!length(ex)) {
  errs <- c(errs, "export produced no PNG")
} else if (ex[[1]]$len < 5000) {
  errs <- c(errs, "exported data URL suspiciously short")
}

if (length(errs)) {
  cat("FAIL:", paste(errs, collapse = "; "), "\n")
} else {
  cat(sprintf("PASS: clean console, export wrote %s (%d bytes of data URL)\n",
              ex[[1]]$name, ex[[1]]$len))
}
b$close()
