# Probe: how wide does a glyph column end up when the facet has many levels?
# Renders static rank_table()s to /tmp so the widths can be measured in a
# browser without a Shiny app. Three shapes: faceted counts (the squeeze),
# a plain single-bar table (the lane must still take the slack), and a
# faceted box glyph (the other mark family).
pkgload::load_all("/workspace/blockr.viz", quiet = TRUE)

set.seed(1)
n <- 600
d <- data.frame(
  SOC = sample(paste("System organ class", 1:6), n, TRUE),
  SITE = sample(sprintf("Site %02d", 1:10), n, TRUE),
  AGE = round(rnorm(n, 55, 12)),
  SEX = sample(c("F", "M"), n, TRUE)
)

faceted <- rank_table(d, group = "SOC", func = "count", facet = "SITE",
                      search = FALSE)
plain <- rank_table(d, group = "SOC", func = "count", search = FALSE)
boxes <- rank_table(
  d,
  summaries = list(
    list(type = "dist", name = "Age", col = "AGE", style = "box")
  ),
  by = list("SOC"), facet = "SITE", search = FALSE
)

probe <- "
window.addEventListener('load', function () {
  var out = Array.from(document.querySelectorAll('.blockr-rank-container'))
    .map(function (root, i) {
      var w = root.querySelector('.blockr-table-wrapper');
      var cells = Array.from(root.querySelectorAll('td.blockr-rank-bar-col'))
        .slice(0, 3).map(function (c) {
          var lane = c.querySelector('.blockr-rank-track, .blockr-rank-lane');
          return c.getBoundingClientRect().width.toFixed(1) + '/' +
            (lane ? lane.getBoundingClientRect().width.toFixed(1) : 'NA');
        });
      return {table: i, wrapper: w.clientWidth, scroll: w.scrollWidth,
              cells: cells};
    });
  document.getElementById('out').textContent = JSON.stringify(out, null, 1);
});
"

htmltools::save_html(
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(
      "body{margin:0;font-family:system-ui;} #panel{width:900px;border:1px solid #ccc;}"
    )),
    htmltools::tags$div(id = "panel", faceted, plain, boxes),
    htmltools::tags$pre(id = "out"),
    htmltools::tags$script(htmltools::HTML(probe))
  ),
  file = "/tmp/facet-width.html"
)
cat("wrote /tmp/facet-width.html\n")
