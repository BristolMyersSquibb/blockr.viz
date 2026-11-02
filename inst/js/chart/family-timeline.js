// @ts-check
/**
 * Chart block: the timeline family (gantt) as the view uses it. Its model
 * and option are in model-timeline.js and option-timeline.js. A click on a
 * bar selects its lane, or with a drill column that column's values. The
 * time axis stays in view while a tall chart scrolls (stickyAxis()).
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);
  const NS = V.chart;

  /** The first non-transparent background above `el`, else white.
   * @param {Element | null} el */
  const backdrop = (el) => {
    for (; el; el = el.parentElement) {
      const bg = getComputedStyle(el).backgroundColor;
      if (bg && bg !== 'transparent' && bg !== 'rgba(0, 0, 0, 0)') return bg;
    }
    return '#ffffff';
  };

  /** A strip above the panel, sticky at the top of whatever scrolls, holds a copy of the
   * canvas band the time axis is drawn in (`band` px high). It shows once the axis has
   * scrolled out of view and is copied again after every render while it shows. The axis
   * itself stays on the canvas, so the picture and the downloads have it.
   * @param {ChartSlot} slot @param {number} band */
  const stickyAxis = (slot, band) => {
    const scope = slot.scope;
    if (!scope || !band) return;
    let strip = slot.axisStrip;
    if (!strip || !strip.isConnected) {
      strip = slot.axisStrip = /** @type {HTMLCanvasElement} */ (
        scope.node(document.createElement('canvas')));
      strip.className = 'vz-chart-axis-strip';
      strip.setAttribute('aria-hidden', 'true');
      slot.container.insertBefore(strip, slot.chartDiv);
      const s = strip;
      let stuck = false;
      let queued = false;
      const copy = () => {
        const src = slot.chartDiv.querySelector('canvas');
        const w = src ? src.width : 0;
        const css = slot.chartDiv.clientWidth;
        if (!src || !w || !css) return;
        const h = Math.round(Number(s.dataset.band) * w / css);
        if (s.width !== w) s.width = w;
        if (s.height !== h) s.height = h;
        const ctx = s.getContext('2d');
        if (!ctx) return;
        ctx.clearRect(0, 0, w, h);
        ctx.drawImage(src, 0, 0, w, h, 0, 0, w, h);
      };
      // Stuck: the strip sits lower than the panel's top edge, which has scrolled past.
      const update = () => {
        queued = false;
        const now = s.getBoundingClientRect().top - slot.chartDiv.getBoundingClientRect().top > 1;
        if (now === stuck) return;
        stuck = now;
        if (stuck) copy();
        s.style.visibility = stuck ? 'visible' : 'hidden';
      };
      // Any scroll moves the panel: the capture listener sees every scroller's.
      scope.on(document, 'scroll', () => {
        if (!queued) { queued = true; scope.frame(update); }
      }, { capture: true, passive: true });
      slot.chart.on('finished', () => { if (stuck) copy(); });
    }
    strip.dataset.band = String(band);
    strip.style.height = band + 'px';
    strip.style.marginBottom = -band + 'px';
    strip.style.background = backdrop(slot.container);
  };

  /** @type {ChartFamily} */
  const timeline = {
    name: 'timeline',
    model: NS.model.timeline,
    option: NS.option.timeline,
    legend: NS.option.timelineLegend,
    widths: false,
    alwaysHighlight: false,

    attach(view, slot) {
      slot.chart.on('click', (/** @type {any} */ params) => {
        if (NS.drillState(view.settings) === 'off') return;
        if (params.componentType !== 'series' || !params.value) return;
        const filters = NS.model.timelineClick(view.settings, slot.facetVal, params.value);
        if (filters) view.selection.click(slot, params, filters);
      });
    },

    after(slot, po, view) {
      if (!view.still) stickyAxis(slot, po.axisBand);
    },

    patch: (c) => NS.option.timelinePatch(c.m, c.panel, c.cfg, c.range ? null : c.lit, c.dimmed,
                                          c.ink)
  };

  NS.families = Object.assign(NS.families || {}, { timeline });
})();
