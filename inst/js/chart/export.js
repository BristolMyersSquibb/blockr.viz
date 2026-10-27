// @ts-check
/**
 * Chart block: the chart as one picture. _downloadImage() composes the
 * titles, every panel at its place in the grid, the legend band and the
 * caption on one canvas. The capture service draws a chart R asks for in a
 * hidden host and answers with that picture.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  /** @type {any} */ let measureCtx = null;

  /** A function that breaks a text block into lines at the picture width:
   * newlines first, then word wrap.
   * @param {number} W @param {number} pad */
  const lineWrapper = (W, pad) => {
    const meas = measureCtx || (measureCtx = document.createElement('canvas').getContext('2d'));
    /** @param {{ text: string, font: string, color: string }} b */
    return (b) => {
      meas.font = b.font;
      const lines = [];
      for (const src of String(b.text).split(/\r?\n/)) {
        let cur = '';
        for (const w of src.split(/\s+/).filter(Boolean)) {
          const cand = cur ? cur + ' ' + w : w;
          if (cur && meas.measureText(cand).width > W - 2 * pad) {
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
   * @param {any} view @param {DOMRect} gridR @param {number} pr */
  const exportContext = (view, gridR, pr) => {
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
    const wrapLines = lineWrapper(W, pad);
    /** @param {string} key @param {Element} el @param {string} font @param {string} color */
    const block = (key, el, font, color) => {
      if (!view._titleText[key]) return null;
      const b = { text: view._titleText[key], font: fontOf(el, font), color: colorOf(el, color) };
      return { ...b, ...wrapLines(b) };
    };
    const top = /** @type {any[]} */ ([block('title', view.titleEl, '600 15px', '#1f2937'),
                                       block('subtitle', view.subtitleEl, '13px', '#6b7280')])
      .filter(Boolean);
    const cap = block('caption', view.captionEl, 'italic 11px', '#6b7280');
    const topH = top.length
      ? pad + top.reduce((a, b) => a + b.lines.length * b.lineH, 0) + 6 : pad / 2;
    const capH = cap ? 6 + cap.lines.length * cap.lineH + pad : pad;
    const legendEl = view.legendEl;
    const legR = legendEl.offsetParent !== null ? legendEl.getBoundingClientRect() : null;
    const legH = legR ? legR.height : 0;
    const legTop = !!legR && legendEl.classList.contains('dd-legend-band--top');
    const H = Math.round(topH + gridR.height + legH + capH);

    let bg = getComputedStyle(view.card).backgroundColor;
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
      const sw = /** @type {Element} */ (child).querySelector('.dd-legend-swatch');
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

  const exporter = {
    // `send` posts the picture to R as <id>_capture; `opts.done` takes it
    // instead; neither saves chart.png in the browser.
    /** @this {any}
     *  @param {boolean} [send]
     *  @param {{pixelRatio?: number,
     *           done?: (url: string, w: number, h: number) => void,
     *           fail?: (e: Error) => void}} [opts] */
    _downloadImage(send, opts) {
      const none = (/** @type {string} */ why) => { if (opts && opts.fail) opts.fail(new Error(why)); };
      const slots = this._slots.filter((/** @type {any} */ s) => s.chart);
      if (!slots.length) { none('the chart has nothing drawn'); return; }
      const gridR = this.chartGrid.getBoundingClientRect();
      if (!gridR.width || !gridR.height) { none('the chart has no size'); return; }
      const pr = (opts && opts.pixelRatio) || this.derived.capture_ratio || 2;
      const k = exportContext(this, gridR, pr);

      panelBitmaps(slots, pr).then((panels) => {
        const canvas = composeCanvas(k, panels);
        if (!canvas) return;
        const url = canvas.toDataURL('image/png');
        if (opts && opts.done) {
          opts.done(url, k.W, k.H);
          return;
        }
        if (send) {
          V.util.send(this.el.id, 'capture', { png: url, width: k.W, height: k.H });
          return;
        }
        const a = document.createElement('a');
        a.href = url;
        a.download = 'chart.png';
        a.click();
      });
    }
  };

  // Capture service: R asks for a picture of a block at a given box
  // (`blockr-viz:capture`: the payload plus `req`, `width`, `height`); a
  // fresh view draws it in a host core/capture.js parks off screen.

  // ECharts animates in for about a second after a draw; an earlier read
  // captures a blank first frame.
  const CAPTURE_SETTLE_MS = 1400;
  let captureSeq = 0;

  /** Wait until a panel has a height, then let the animation settle; an
   * empty state ends the wait at once.
   * @param {any} view @param {VizScope} scope @param {() => void} then @param {number} left */
  const whenDrawn = (view, scope, then, left) => {
    const charted = view._slots.filter((/** @type {any} */ s) => s.chart);
    if (view.drawn && !charted.length) { then(); return; }
    const ready = charted.some((/** @type {any} */ s) => s.chartDiv.clientHeight > 40);
    if (ready || left <= 0) scope.timeout(then, CAPTURE_SETTLE_MS);
    else scope.timeout(() => whenDrawn(view, scope, then, left - 1), 100);
  };

  /** @param {VizViewClass} View @param {any} msg @returns {Promise<VizPicture>} */
  const capturePicture = (View, msg) => {
    const scope = new V.Scope();
    const p = V.capture({
      width: msg.width, height: msg.height,
      draw: (/** @type {HTMLElement} */ host) => new Promise((resolve, reject) => {
        const el = host.appendChild(document.createElement('div'));
        el.id = 'blockr-capture-chart-' + (++captureSeq);
        el.style.cssText = 'width:' + msg.width + 'px;height:' + msg.height + 'px;';
        const view = scope.own(new View(el, scope));
        view.render(msg);
        whenDrawn(view, scope, () => {
          try {
            /** @type {any} */ (view)._downloadImage(false, {
              pixelRatio: msg.ratio, fail: reject,
              done: (/** @type {string} */ png, /** @type {number} */ width,
                     /** @type {number} */ height) => resolve({ png, width, height })
            });
          } catch (e) {
            reject(e);
          }
        }, 50);
      })
    });
    return p.finally(() => scope.dispose());
  };

  /** Register the capture service's message for the chart's views.
   * @param {VizViewClass} View */
  const registerCapture = (View) => {
    const shiny = root.Shiny;
    if (!shiny || typeof shiny.addCustomMessageHandler !== 'function') return;
    shiny.addCustomMessageHandler('blockr-viz:capture', (/** @type {any} */ msg) =>
      V.captureReply(msg.req, capturePicture(View, msg)));
  };

  NS.exporter = exporter;
  NS.registerCapture = registerCapture;
})();
