// @ts-check
/**
 * Chart block: the chart as one picture.
 *
 * _downloadImage() composes the title band, every panel at its place in the
 * grid (facet labels redrawn), the legend band and the caption on one
 * canvas, from each panel's own bitmap. The capture service draws a chart R
 * asks for in a hidden host and answers with that picture.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const B = /** @type {any} */ (root.Blockr = root.Blockr || {});
  const V = /** @type {any} */ (B.viz = B.viz || { spec: {} });
  const NS = /** @type {any} */ (V.chart = V.chart || {});

  /** @type {any} */
  let measureCtx = null;

  const exporter = {
    // `send` posts the picture to R as <id>_capture; `opts.done` takes it
    // instead; neither saves chart.png in the browser. `pixelRatio` is the
    // resolution, and the only thing it changes.
    /** @this {any}
     *  @param {boolean} [send]
     *  @param {{pixelRatio?: number,
     *           done?: (url: string, w: number, h: number) => void}} [opts] */
    _downloadImage(send, opts) {
      const slots = (this._slots || []).filter((/** @type {any} */ s) => s && s.chart);
      if (!slots.length || !this.chartGrid) return;
      const gridR = this.chartGrid.getBoundingClientRect();
      if (!gridR.width || !gridR.height) return;
      const pr = (opts && opts.pixelRatio) || (this.config && this.config.capture_ratio) || 2;
      const pad = 12;
      const face = (this._ink || NS.INK_DEFAULT).face;

      // Text styles from the live bands, so the picture follows the theme.
      /** @param {Element | undefined} el @param {string} fb */
      const fontOf = (el, fb) => {
        const cs = el ? getComputedStyle(el) : null;
        if (!cs || !cs.fontSize) return fb + ' ' + face;
        const style = cs.fontStyle === 'italic' ? 'italic ' : '';
        return style + cs.fontWeight + ' ' + cs.fontSize + ' ' + cs.fontFamily;
      };
      /** @param {Element | undefined} el @param {string} fb */
      const colorOf = (el, fb) => {
        const cs = el ? getComputedStyle(el) : null;
        return (cs && cs.color) || fb;
      };
      const tx = this._titleText || {};
      const blocksTop = [
        { text: tx.title || '', font: fontOf(this.titleEl, '600 15px'),
          color: colorOf(this.titleEl, '#1f2937') },
        { text: tx.subtitle || '', font: fontOf(this.subtitleEl, '13px'),
          color: colorOf(this.subtitleEl, '#6b7280') }
      ].filter((b) => b.text);
      const capBlock = (tx.caption || '') && {
        text: tx.caption, font: fontOf(this.captionEl, 'italic 11px'),
        color: colorOf(this.captionEl, '#6b7280')
      };

      const W = Math.round(gridR.width);
      const meas = measureCtx || (measureCtx = document.createElement('canvas').getContext('2d'));
      // Explicit newlines first, then word wrap to the grid width (the
      // caption is pre-line on screen).
      /** @param {{ text: string, font: string, color: string }} b */
      const wrapLines = (b) => {
        meas.font = b.font;
        const lines = [];
        for (const src of String(b.text).split(/\r?\n/)) {
          const words = src.split(/\s+/).filter(Boolean);
          if (!words.length) { lines.push(''); continue; }
          let cur = '';
          for (const w of words) {
            const cand = cur ? cur + ' ' + w : w;
            if (meas.measureText(cand).width > W - 2 * pad && cur) {
              lines.push(cur); cur = w;
            } else {
              cur = cand;
            }
          }
          if (cur) lines.push(cur);
        }
        const px = parseFloat((b.font.match(/(\d+(?:\.\d+)?)px/) || ['', '13'])[1]);
        return { lines, lineH: Math.round(px * 1.4) };
      };
      const top = blocksTop.map((b) => ({ ...b, ...wrapLines(b) }));
      const cap = capBlock ? { ...capBlock, ...wrapLines(capBlock) } : null;
      const topH = top.length
        ? pad + top.reduce((a, b) => a + b.lines.length * b.lineH, 0) + 6 : pad / 2;
      const capH = cap ? 6 + cap.lines.length * cap.lineH + pad : pad;
      const legR = (this.legendEl && this.legendEl.offsetParent !== null)
        ? this.legendEl.getBoundingClientRect() : null;
      const legH = legR ? legR.height : 0;
      // The band sits above the plot with `legend = "top"`, else below it.
      const legTop = !!legR && this.legendEl.classList.contains('dd-legend-band--top');
      const H = Math.round(topH + gridR.height + legH + capH);

      let bg = getComputedStyle(this.card).backgroundColor;
      if (!bg || bg === 'transparent' || bg === 'rgba(0, 0, 0, 0)') bg = '#ffffff';

      const jobs = slots.map((/** @type {any} */ s) => new Promise((resolve) => {
        const im = new Image();
        im.onload = () => resolve({ im, slot: s });
        im.onerror = () => resolve(null);
        im.src = s.chart.getDataURL({
          pixelRatio: pr, backgroundColor: 'transparent', excludeComponents: ['toolbox']
        });
      }));
      Promise.all(jobs).then((panels) => {
        const canvas = document.createElement('canvas');
        canvas.width = W * pr;
        canvas.height = H * pr;
        const ctx = canvas.getContext('2d');
        if (!ctx) return;
        ctx.scale(pr, pr);
        ctx.fillStyle = bg;
        ctx.fillRect(0, 0, W, H);

        let y = pad;
        for (const b of top) {
          ctx.font = b.font;
          ctx.fillStyle = b.color;
          ctx.textBaseline = 'top';
          for (const ln of b.lines) { ctx.fillText(ln, pad, y); y += b.lineH; }
        }
        y = topH + (legTop ? legH : 0);

        for (const p of /** @type {any[]} */ (panels)) {
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

        // Legend chips where they are on screen, above or below the panels;
        // chips toggled off keep their dimmed opacity.
        if (legR) {
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
          for (const child of Array.from(this.legendEl.children)) {
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
        }

        if (cap) {
          ctx.font = cap.font;
          ctx.fillStyle = cap.color;
          ctx.textBaseline = 'top';
          let cy = topH + gridR.height + legH + 6;
          for (const ln of cap.lines) { ctx.fillText(ln, pad, cy); cy += cap.lineH; }
        }

        const url = canvas.toDataURL('image/png');
        // Kept so a driven browser can read back what was composed.
        this._lastCapture = url;

        if (opts && typeof opts.done === 'function') {
          opts.done(url, W, H);
          return;
        }
        if (send) {
          V.util.send(this.el.id, 'capture', { png: url, width: W, height: H });
          return;
        }
        const a = document.createElement('a');
        a.href = url;
        a.download = 'chart.png';
        a.click();
      });
    }
  };

  // -- Capture service --------------------------------------------------------
  //
  // R asks for a picture of a block at a given box (`blockr-viz:capture`,
  // the payload plus `req`, `width`, `height`); a fresh view draws it in a
  // host core/capture.js parks off screen, and the reply goes out once.

  // ECharts animates in for about a second after a draw; reading the
  // panels the moment they exist captures a blank first frame.
  const CAPTURE_SETTLE_MS = 1400;
  let captureSeq = 0;

  /** Wait until a panel has a height, then let the animation settle.
   * @param {any} view @param {VizScope} scope @param {() => void} then @param {number} left */
  const whenDrawn = (view, scope, then, left) => {
    const ready = (view._slots || []).some((/** @type {any} */ s) =>
      s && s.chart && s.chartDiv && s.chartDiv.clientHeight > 40);
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
              pixelRatio: msg.ratio,
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
