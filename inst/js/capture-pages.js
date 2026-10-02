// @ts-check
/**
 * capture-pages.js -- cuts one export picture into slide pages.
 *
 * The heatmap and the summarize table export as a picture of the block as
 * the browser drew it (R/chart-capture.R). On a slide that picture is fitted
 * into one box, so a long table came out too small to read. Here the picture
 * is cut between rows into pages that each fit the slide box at a readable
 * size: the first page keeps the title, legend and header, later pages
 * repeat the header (and the group heading a page opens inside), the last
 * page keeps the caption. R puts one page on each slide
 * (pptx_add_exhibit.chart_capture).
 *
 * The rule is the text tables' (write_exhibit_pptx): one slide while its text
 * stays at or above the board's smallest exhibit font, otherwise pages at
 * that size. Only rows are cut; a picture wider than the slide is scaled to
 * its width.
 *
 * Exposes window.BlockrCapturePages(canvas, root, box, sectionSel).
 */
(function () {
  'use strict';

  /**
   * @typedef {{ w: number, h: number, minPt?: number }} PageBox
   *   The slide's picture box in inches and the font floor in points.
   * @typedef {{ png: string, width: number, height: number }} Page
   *   One page: a PNG data URL and its size in CSS px.
   */

  /**
   * Must run while `root` is still laid out exactly as it was drawn into
   * `canvas`. Returns [] when the picture fits one slide as it is.
   * @param {HTMLCanvasElement} canvas the whole picture, drawn from `root`
   * @param {HTMLElement} root the element the picture is of
   * @param {PageBox | null | undefined} box
   * @param {string | null} sectionSel group heading rows, repeated on a page
   *   that opens inside their group
   * @returns {Page[]}
   */
  function capturePages(canvas, root, box, sectionSel) {
    if (!box || !(box.w > 0) || !(box.h > 0)) return [];
    var table = root.querySelector('table');
    var tbody = table && table.tBodies[0];
    var thead = table && table.tHead;
    if (!tbody || !thead) return [];

    var rb = root.getBoundingClientRect();
    var W = rb.width, H = rb.height;
    if (!(W > 0) || !(H > 0)) return [];
    /** @param {Element} el */
    var span = function (el) {
      var r = el.getBoundingClientRect();
      return { top: r.top - rb.top, bottom: r.bottom - rb.top };
    };

    /** @type {{ top: number, bottom: number, sec: boolean }[]} */
    var rows = [];
    Array.prototype.forEach.call(tbody.rows, function (/** @type {HTMLTableRowElement} */ tr) {
      if (!tr.offsetHeight) return;
      var s = span(tr);
      rows.push({ top: s.top, bottom: s.bottom,
                  sec: !!sectionSel && tr.matches(sectionSel) });
    });
    if (rows.length < 2) return [];

    // Scale the slide gives the picture (1 = its size on screen), and the
    // smallest one whose text still reaches the floor. 1pt = 1/72in and the
    // picture is laid out at 96 CSS px per inch, so px text is 0.75*px pt.
    var cell = tbody.querySelector('td');
    var px = (cell && parseFloat(getComputedStyle(cell).fontSize)) || 13;
    var floor = (box.minPt || 11) / (0.75 * px);
    var widthFit = box.w * 96 / W;
    var oneSlide = Math.min(widthFit, box.h * 96 / H, 2);
    if (oneSlide >= Math.min(floor, widthFit) - 1e-6) return [];
    var pageH = box.h * 96 / Math.min(widthFit, floor);

    var pad = parseFloat(getComputedStyle(root).paddingTop) || 0;
    var head = span(thead);
    var bodyTop = rows[0].top;
    var bodyEnd = rows[rows.length - 1].bottom;
    var tail = H - bodyEnd;

    // Each page is a list of strips [from, to] of the picture, in CSS px;
    // [null, h] is h px of white.
    /** @type {[number | null, number][][]} */
    var plan = [];
    var n = rows.length, i = 0, lastSec = -1;
    while (i < n) {
      var first = plan.length === 0;
      /** @type {[number | null, number][]} */
      var strips = first ? [[0, bodyTop]] : [[null, pad], [head.top, head.bottom]];
      var used = first ? bodyTop : pad + head.bottom - head.top;
      if (!first && !rows[i].sec && lastSec >= 0) {
        strips.push([rows[lastSec].top, rows[lastSec].bottom]);
        used += rows[lastSec].bottom - rows[lastSec].top;
      }
      var avail = pageH - used;
      var start = i;
      if (bodyEnd - rows[i].top + tail <= avail || i === n - 1) {
        strips.push([rows[i].top, H]);
        i = n;
      } else {
        var j = i;
        while (j < n && rows[j].bottom - rows[i].top <= avail) j++;
        // Every row fits but the caption does not: carry the last row over,
        // so the caption does not sit on a page of its own.
        if (j === n) j = n - 1;
        // A page never ends on a group heading.
        while (j - 1 > i && rows[j - 1].sec) j--;
        if (j <= i) j = i + 1;
        strips.push([rows[i].top, rows[j - 1].bottom], [null, pad]);
        i = j;
      }
      for (var k = start; k < i; k++) if (rows[k].sec) lastSec = k;
      plan.push(strips);
    }
    if (plan.length < 2) return [];

    var s = canvas.width / W;
    return plan.map(function (strips) {
      var parts = strips.map(function (st) {
        if (st[0] === null) return { sy: -1, sh: Math.round(st[1] * s) };
        var sy = Math.round(st[0] * s);
        return { sy: sy, sh: Math.round(st[1] * s) - sy };
      });
      var total = parts.reduce(function (a, p) { return a + p.sh; }, 0);
      var page = document.createElement('canvas');
      page.width = canvas.width;
      page.height = total;
      var ctx = page.getContext('2d');
      if (!ctx) return { png: '', width: 0, height: 0 };
      ctx.fillStyle = '#ffffff';
      ctx.fillRect(0, 0, page.width, page.height);
      var dy = 0;
      for (var q = 0; q < parts.length; q++) {
        var p = parts[q];
        if (p.sy >= 0 && p.sh > 0) {
          ctx.drawImage(canvas, 0, p.sy, canvas.width, p.sh,
                        0, dy, canvas.width, p.sh);
        }
        dy += p.sh;
      }
      return { png: page.toDataURL('image/png'), width: W, height: total / s };
    }).filter(function (p) { return !!p.png; });
  }

  /** @type {any} */ (window).BlockrCapturePages = capturePages;
})();
