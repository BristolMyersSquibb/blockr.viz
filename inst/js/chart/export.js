// @ts-check
/**
 * Chart block: the chart as one picture. picture() composes the titles,
 * every panel at its place in the grid, the legend band and the caption on
 * one canvas. The view sends it to R when the reader opens the download
 * control; the capture service draws a chart R asks for in a hidden host
 * and answers with it.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The bands' plain text. @param {VizTitles | null | undefined} titles
   * @returns {Record<string, string>} */
  const titleText = (titles) => Object.fromEntries(['title', 'subtitle', 'caption']
    .map((k) => [k, String((titles && /** @type {any} */ (titles)[k] &&
                            /** @type {any} */ (titles)[k].text) || '')]));

  /** A function that breaks a text block into lines at the picture width:
   * newlines first, then word wrap.
   * @param {number} W @param {number} pad @param {(font: string) => (s: string) => number} measureFont */
  const lineWrapper = (W, pad, measureFont) => {
    /** @param {{ text: string, font: string, color: string }} b */
    return (b) => {
      const width = measureFont(b.font);
      const lines = [];
      for (const src of String(b.text).split(/\r?\n/)) {
        let cur = '';
        for (const w of src.split(/\s+/).filter(Boolean)) {
          const cand = cur ? cur + ' ' + w : w;
          if (cur && width(cand) > W - 2 * pad) {
            lines.push(cur);
            cur = w;
          } else {
            cur = cand;
          }
        }
        lines.push(cur);
      }
      const px = parseFloat((b.font.match(/(\d+(?:\.\d+)?)px/) || ['', '13'])[1]);
      return { lines, lineH: Math.round(px * 1.4) };
    };
  };

  /** Everything the picture is laid out from: the wrapped title and caption
   * blocks, the band heights, the legend's place, the canvas size, the background.
   * @param {ChartView} view @param {DOMRect} gridR @param {number} pr */
  const exportContext = (view, gridR, pr) => {
    const frame = view.frame;
    const text = titleText(view.derived.titles);
    const pad = 12;
    // Text styles come from the live bands, so the picture follows the theme.
    /** @param {Element} el @param {string} fb */
    const fontOf = (el, fb) => {
      const cs = getComputedStyle(el);
      if (!cs.fontSize) return fb + ' ' + view._ink.face;
      return (cs.fontStyle === 'italic' ? 'italic ' : '') + cs.fontWeight + ' ' + cs.fontSize +
        ' ' + cs.fontFamily;
    };
    /** @param {Element} el @param {string} fb */
    const colorOf = (el, fb) => getComputedStyle(el).color || fb;
    const W = Math.round(gridR.width);
    const wrapLines = lineWrapper(W, pad, (font) => view._measureFont(font));
    /** @param {string} key @param {Element} el @param {string} font @param {string} color */
    const block = (key, el, font, color) => {
      if (!text[key]) return null;
      const b = { text: text[key], font: fontOf(el, font), color: colorOf(el, color) };
      return { ...b, ...wrapLines(b) };
    };
    const top = /** @type {any[]} */ ([block('title', frame.titleEl, '600 15px', '#1f2937'),
                                       block('subtitle', frame.subEl, '13px', '#6b7280')])
      .filter(Boolean);
    const cap = block('caption', frame.capEl, 'italic 11px', '#6b7280');
    const topH = top.length
      ? pad + top.reduce((a, b) => a + b.lines.length * b.lineH, 0) + 6 : pad / 2;
    const capH = cap ? 6 + cap.lines.length * cap.lineH + pad : pad;
    const legendEl = view.legend.el;
    const legR = legendEl.offsetParent !== null ? legendEl.getBoundingClientRect() : null;
    const legH = legR ? legR.height : 0;
    const legTop = !!legR && legendEl.classList.contains('vz-chart-legend--top');
    const H = Math.round(topH + gridR.height + legH + capH);

    let bg = getComputedStyle(frame.el).backgroundColor;
    if (!bg || bg === 'transparent' || bg === 'rgba(0, 0, 0, 0)') bg = '#ffffff';
    return { pad, W, H, pr, gridR, fontOf, colorOf, top, cap, topH, legendEl, legR, legH, legTop, bg };
  };

  /** Each panel's bitmap, loaded; a panel whose picture fails is null.
   * @param {any[]} slots @param {number} pr @returns {Promise<any[]>} */
  const panelBitmaps = (slots, pr) => Promise.all(slots.map((/** @type {any} */ s) =>
    new Promise((resolve) => {
      const im = new Image();
      im.onload = () => resolve({ im, slot: s });
      im.onerror = () => resolve(null);
      im.src = s.chart.getDataURL({
        pixelRatio: pr, backgroundColor: 'transparent', excludeComponents: ['toolbox']
      });
    })));

  /** A wrapped text block from `y`; returns the y below it.
   * @param {CanvasRenderingContext2D} ctx @param {any} b @param {number} x @param {number} y */
  const paintBlock = (ctx, b, x, y) => {
    ctx.font = b.font;
    ctx.fillStyle = b.color;
    ctx.textBaseline = 'top';
    for (const ln of b.lines) {
      ctx.fillText(ln, x, y);
      y += b.lineH;
    }
    return y;
  };

  /** Every panel at its place in the grid, with its facet label.
   * @param {CanvasRenderingContext2D} ctx @param {any} k @param {any[]} panels */
  const paintPanels = (ctx, k, panels) => {
    const { gridR, fontOf, colorOf, topH, legH, legTop } = k;
    const y = topH + (legTop ? legH : 0);
    for (const p of panels) {
      if (!p) continue;
      const r = p.slot.chartDiv.getBoundingClientRect();
      const x0 = r.left - gridR.left;
      const y0 = y + (r.top - gridR.top);
      if (p.slot.labelEl) {
        const lr = p.slot.labelEl.getBoundingClientRect();
        ctx.font = fontOf(p.slot.labelEl, '600 11px');
        ctx.fillStyle = colorOf(p.slot.labelEl, '#6b7280');
        ctx.textBaseline = 'top';
        ctx.textAlign = 'center';
        ctx.fillText(p.slot.labelEl.textContent || '',
                     lr.left - gridR.left + lr.width / 2, y + (lr.top - gridR.top) + 2);
        ctx.textAlign = 'left';
      }
      ctx.drawImage(p.im, x0, y0, r.width, r.height);
    }
  };

  /** Legend chips where they are on screen; a dimmed chip keeps its opacity.
   * @param {CanvasRenderingContext2D} ctx @param {any} k */
  const paintLegend = (ctx, k) => {
    const { gridR, fontOf, colorOf, topH, legendEl, legR, legTop } = k;
    const bandY = legTop ? topH : topH + gridR.height;
    /** @param {Element} el2 */
    const drawLabel = (el2) => {
      const r2 = el2.getBoundingClientRect();
      ctx.font = fontOf(el2, '11px');
      ctx.fillStyle = colorOf(el2, '#333');
      ctx.textBaseline = 'middle';
      ctx.fillText(el2.textContent || '', r2.left - gridR.left,
                   bandY + (r2.top - legR.top) + r2.height / 2);
    };
    for (const child of Array.from(legendEl.children)) {
      const alpha = parseFloat(getComputedStyle(/** @type {Element} */ (child)).opacity || '1');
      ctx.globalAlpha = Number.isFinite(alpha) ? alpha : 1;
      const sw = /** @type {Element} */ (child).querySelector('.vz-chart-legend-swatch');
      if (sw) {
        const sr = sw.getBoundingClientRect();
        const swCs = getComputedStyle(sw);
        ctx.fillStyle = swCs.backgroundColor || '#999';
        const sx = sr.left - gridR.left, sy = bandY + (sr.top - legR.top);
        // A dot is round, a square and a line have small corners.
        const rad = Math.min(parseFloat(swCs.borderTopLeftRadius) || 0, sr.width / 2, sr.height / 2);
        if (rad > 0 && typeof ctx.roundRect === 'function') {
          ctx.beginPath();
          ctx.roundRect(sx, sy, sr.width, sr.height, rad);
          ctx.fill();
        } else {
          ctx.fillRect(sx, sy, sr.width, sr.height);
        }
        if (sw.nextElementSibling) drawLabel(sw.nextElementSibling);
      } else {
        drawLabel(/** @type {Element} */ (child));
      }
    }
    ctx.globalAlpha = 1;
    ctx.textBaseline = 'top';
  };

  /** The bands and the panels on one canvas, or null without a 2d context.
   * @param {any} k @param {any[]} panels @returns {HTMLCanvasElement | null} */
  const composeCanvas = (k, panels) => {
    const { W, H, pr, bg, legR, cap } = k;
    const canvas = document.createElement('canvas');
    canvas.width = W * pr;
    canvas.height = H * pr;
    const ctx = canvas.getContext('2d');
    if (!ctx) return null;
    ctx.scale(pr, pr);
    ctx.fillStyle = bg;
    ctx.fillRect(0, 0, W, H);
    let y = k.pad;
    for (const b of k.top) y = paintBlock(ctx, b, k.pad, y);
    paintPanels(ctx, k, panels);
    if (legR) paintLegend(ctx, k);
    if (cap) paintBlock(ctx, cap, k.pad, k.topH + k.gridR.height + k.legH + 6);
    return canvas;
  };

  /**
   * The chart as one picture, at `pixelRatio` (else R's capture_ratio).
   * Rejects when nothing is drawn or the grid has no size.
   * @param {ChartView} view @param {{ pixelRatio?: number }} [opts]
   * @returns {Promise<VizPicture>}
   */
  const picture = (view, opts = {}) => {
    const slots = view._slots.filter((s) => s.chart);
    if (!slots.length) return Promise.reject(new Error('the chart has nothing drawn'));
    const gridR = view.chartGrid.getBoundingClientRect();
    if (!gridR.width || !gridR.height) return Promise.reject(new Error('the chart has no size'));
    const pr = opts.pixelRatio || view.derived.capture_ratio || 2;
    const k = exportContext(view, gridR, pr);
    return panelBitmaps(slots, pr).then((panels) => {
      const canvas = composeCanvas(k, panels);
      if (!canvas) throw new Error('the browser cannot draw the picture');
      return { png: canvas.toDataURL('image/png'), width: k.W, height: k.H };
    });
  };

  // Capture service: R asks for a picture of a block at a given box
  // (`blockr-viz:capture`: the payload plus `req`, `width`, `height`); a
  // fresh view draws it in a host core/capture.js parks off screen.

  // The capture view draws still, so a panel is complete when ECharts says
  // it finished rendering. A panel that never says so (a host the browser
  // does not lay out) is taken after this long.
  const FINISH_MS = 3000;
  let captureSeq = 0;

  /** Resolves when every panel has finished rendering; at once for an empty
   * state. @param {ChartView} view @param {VizScope} scope @returns {Promise<void>} */
  const whenDrawn = (view, scope) => {
    const charts = view._slots.filter((s) => s.chart).map((s) => s.chart);
    /** @param {any} chart @returns {Promise<void>} */
    const finished = (chart) => new Promise((resolve) => chart.on('finished', () => resolve()));
    const late = new Promise((resolve) => scope.timeout(() => resolve(undefined), FINISH_MS));
    return Promise.race([Promise.all(charts.map(finished)), late]).then(() => {});
  };

  /** @param {ChartViewClass} View @param {ChartCaptureMessage} msg @returns {Promise<VizPicture>} */
  const capturePicture = (View, msg) => {
    const scope = new V.Scope();
    const p = V.capture({
      width: msg.width, height: msg.height,
      draw: (/** @type {HTMLElement} */ host) => new Promise((resolve, reject) => {
        const el = host.appendChild(document.createElement('div'));
        el.id = 'vz-capture-chart-' + (++captureSeq);
        el.style.cssText = 'width:' + msg.width + 'px;height:' + msg.height + 'px;';
        const view = scope.own(new View(el, scope, { still: true }));
        view.render(msg);
        whenDrawn(view, scope)
          .then(() => picture(view, { pixelRatio: msg.ratio }))
          .then(resolve, reject);
      })
    });
    return p.finally(() => scope.dispose());
  };

  /** Register the capture service's message for the chart's views.
   * @param {ChartViewClass} View */
  const registerCapture = (View) => {
    const shiny = root.Shiny;
    if (!shiny || typeof shiny.addCustomMessageHandler !== 'function') return;
    shiny.addCustomMessageHandler('blockr-viz:capture', (/** @type {ChartCaptureMessage} */ msg) =>
      V.captureReply(msg.req, capturePicture(View, msg)));
  };

  Object.assign(NS, { picture, titleText, registerCapture });
})();
