# Slide-shaped HTML frames for previewing deck tables in a browser: a real
# flextable drawn at true slide scale, with a badge counting how much of it
# landed on the slide. Shared by study.R and bms-preview.R.

library(htmltools)

SLIDE_W <- 13.333
SLIDE_H <- 7.5

slide <- function(ft, title = "", scale = 0.58, top = 1.1, left = NULL,
                  note = NULL) {

  dim <- tryCatch(flextable::flextable_dim(ft)$widths, error = function(e) NA)
  # Same placement rule as write_exhibit_pptx(): centred, and back on the
  # template's own margin once the table is full width.
  if (is.null(left)) {
    left <- if (is.na(dim)) 0.4 else max(0.25, (SLIDE_W - dim) / 2)
  }

  div(class = "slide-wrap",
      style = sprintf("width:%fin;height:%fin;", SLIDE_W * scale,
                      SLIDE_H * scale),
    div(class = "slide",
        style = sprintf("width:%fin;height:%fin;transform:scale(%f);",
                        SLIDE_W, SLIDE_H, scale),
      if (nzchar(title)) div(class = "slide-title", title),
      div(class = "slide-body",
          style = sprintf("top:%fin;left:%fin;", top, left),
          flextable::htmltools_value(ft))
    ),
    if (!is.null(note)) div(class = "note", note)
  )
}

slide_css <- "
:root { --ink:#1c2530; --mut:#5b6875; --line:#dfe4ea; --accent:#2563EB; }
* { box-sizing: border-box; }
body { margin:0; background:#eef1f5; color:var(--ink);
  font-family: ui-sans-serif, system-ui, 'Segoe UI', Roboto, Helvetica, Arial;
  font-size:16px; line-height:1.55; }
main { max-width: 68rem; margin: 0 auto; padding: 3rem 1.5rem 6rem; }
h1 { font-size: 2rem; margin:0 0 .4rem; letter-spacing:-.02em; }
h2 { font-size: 1.35rem; margin: 0 0 .6rem; letter-spacing:-.01em; }
h3 { font-size: 1rem; margin: 1.6rem 0 .4rem; }
.lede { color: var(--mut); font-size:1.05rem; margin:0 0 2rem; }
section { background:#fff; border:1px solid var(--line); border-radius:14px;
  padding:1.6rem 1.8rem 2rem; margin: 0 0 1.5rem;
  box-shadow: 0 1px 2px rgba(20,30,45,.05); }
.num { display:inline-block; min-width:1.9rem; color:var(--accent);
  font-variant-numeric: tabular-nums; }
p { margin:.6rem 0; }
p.q { color:var(--mut); border-left:3px solid var(--accent);
  padding:.1rem 0 .1rem .8rem; margin:1.1rem 0; }
code { background:#f1f3f7; border-radius:4px; padding:.1em .35em;
  font-size:.87em; }
ul { margin:.5rem 0 .5rem 1.1rem; padding:0; }
li { margin:.25rem 0; }
.slide-wrap { position:relative; overflow:hidden; margin:1.2rem 0; }
.slide { position:absolute; top:0; left:0; transform-origin: top left;
  background:#fff; border:1px solid #c8d0da; box-shadow:0 2px 10px
  rgba(20,30,45,.12); }
.slide-title { position:absolute; top:0.35in; left:0.4in; right:0.4in;
  font: 700 24pt/1.15 Arial, Helvetica, sans-serif; color:#222; }
.slide-body { position:absolute; }
.slide .tabwid { overflow: visible !important; }
.note { position:absolute; left:.5rem; bottom:.5rem; z-index:3;
  background:rgba(28,37,48,.82); color:#fff; font-size:.72rem;
  padding:.15rem .55rem; border-radius:5px; }
.tag { display:inline-block; font-size:.75rem; text-transform:uppercase;
  letter-spacing:.06em; color:var(--mut); background:#f1f3f7;
  border-radius:99px; padding:.15rem .6rem; margin-left:.5rem;
  vertical-align:.15em; }
.tag.win { background:#e7f0ff; color:#1d4ed8; }
.tag.hard { background:#fdf0e6; color:#b45309; }
.fit { position:absolute; right:.5rem; bottom:.5rem; z-index:3;
  background:rgba(28,37,48,.82); color:#fff; font-size:.72rem;
  padding:.15rem .55rem; border-radius:5px; font-variant-numeric:tabular-nums;
  pointer-events:none; }
.fit.bad { background:rgba(180,60,30,.86); }
.wtbl { border-collapse:collapse; font-size:.85rem; margin:1rem 0; }
.wtbl th, .wtbl td { border:1px solid var(--line); padding:.25rem .6rem;
  text-align:left; }
.wtbl caption { caption-side:top; text-align:left; color:var(--mut);
  padding-bottom:.3rem; }
.wtbl td:last-child { text-align:right; font-variant-numeric:tabular-nums; }
.cols { display:grid; grid-template-columns: 1fr 1fr; gap:1.2rem; }
.knob { background:#f7f9fc; border:1px dashed #c3ccd8; border-radius:8px;
  padding:.7rem 1rem; margin-top:1rem; font-size:.92rem; }
.knob b { font-weight:600; }
"

# Each slide labels itself with how much of the table actually landed on it.
# flextable renders into a shadow root, so the walk has to reach through it,
# and it retries until the tables are up.
slide_js <- "
function labelSlides() {
  var slides = document.querySelectorAll('.slide-wrap');
  var ready = 0;
  slides.forEach(function (wrap) {
    var sl = wrap.querySelector('.slide');
    var host = sl.querySelector('.flextable-shadow-host');
    if (!host || !host.shadowRoot) return;
    var root = host.shadowRoot;
    var tbl = root.querySelector('table');
    if (!tbl) return;
    ready++;
    if (wrap.querySelector('.fit')) return;
    var scale = parseFloat(sl.style.transform.match(/scale\\(([\\d.]+)\\)/)[1]);
    var bottom = sl.getBoundingClientRect().top + 7.5 * 96 * scale;
    var right = sl.getBoundingClientRect().left + 13.333 * 96 * scale;
    var rows = root.querySelectorAll('tbody tr');
    var fit = 0;
    rows.forEach(function (r) {
      if (r.getBoundingClientRect().bottom <= bottom + 1) fit++;
    });
    var box = tbl.getBoundingClientRect();
    // A tenth of an inch of slack: HTML adds the collapsed cell borders on
    // top of the column widths, PowerPoint does not.
    var over = box.right > right + 0.1 * 96 * scale;
    var tag = document.createElement('div');
    tag.className = 'fit' + (fit < rows.length || over ? ' bad' : '');
    tag.textContent = fit + ' of ' + rows.length + ' rows on the slide, '
      + (box.width / scale / 96).toFixed(1) + 'in wide, '
      + (box.height / scale / 96).toFixed(1) + 'in tall';
    wrap.appendChild(tag);
  });
  if (ready < slides.length) setTimeout(labelSlides, 150);
}
window.addEventListener('load', function () { setTimeout(labelSlides, 100); });
"
