// @ts-check
/**
 * A view's data as a table under it, after Power BI's "Show as a table". A
 * tool in the frame's header, left of the download control, opens it. Its
 * tabs, each with its count: Summary, one row per mark, and Rows, the rows
 * behind the selection ("Rows 40 of 240"). A view without a Summary (a
 * chart that draws rows) shows the rows and their count alone. R builds
 * both tables (R/chart-data-view.R); the table view (table/view.js) draws
 * them.
 *
 * The reader's choice goes to R as `<id>_data` ({on, tab, n}). While it is
 * on, every payload carries `derived.data_view`: the counts and the table
 * view's payload for the tab that shows. The table's rows come once per
 * data revision and are kept here; a payload without rows this view does
 * not hold (a re-mounted block) asks R again with a new request count `n`.
 * Shiny keeps the input, so a re-mounted block opens the table again from
 * its first payload. Nothing is saved with the board.
 *
 * A row click on Summary goes to R as `<id>-data_select` and selects that
 * mark; a selected mark lights its row.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  const ICON = '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true" ' +
    'fill="none" stroke="currentColor" stroke-width="1.5" stroke-linejoin="round">' +
    '<rect x="2" y="2.5" width="12" height="11" rx="1.2"/>' +
    '<path d="M2 6.2 H14 M2 9.8 H14 M6.5 6.2 V13.5"/></svg>';

  /** One request count for the page, so a re-mounted view never repeats
   * the count R last answered. */
  let requests = 0;

  /**
   * @param {string} tag @param {string} cls @param {HTMLElement} [parent]
   */
  function el(tag, cls, parent) {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  }

  /** "40 of 240", or "240" when nothing is filtered.
   * @param {VizDataViewState} dv */
  function rowsCount(dv) {
    if (dv.rows == null) return '';
    return dv.total != null && dv.total !== dv.rows ? dv.rows + ' of ' + dv.total : String(dv.rows);
  }

  class DataView {
    /**
     * @param {VizFrame} frame The view's frame: the tool goes in its header,
     *   the table at the end of its body.
     * @param {VizScope} scope
     * @param {{ onToggle?: () => void }} [opts] `onToggle`: the view's room
     *   changed (the chart resizes).
     */
    constructor(frame, scope, opts = {}) {
      this.frame = frame;
      this.scope = scope;
      this.onToggle = opts.onToggle || (() => {});
      this.on = false;
      /** @type {'summary' | 'rows'} */
      this.tab = 'summary';
      /** What the reader asked R for and R has not answered.
       * @type {{ on: boolean, tab: string } | null} */
      this.pending = null;
      /** @type {VizDataViewState | null} */
      this.state = null;
      /** The table's rows of the last data revision R sent.
       * @type {{ rev: number, data: unknown } | null} */
      this.rows = null;
      /** The data revision rows were asked for, to ask once. @type {number | null} */
      this.asked = null;
      this.tool = /** @type {HTMLButtonElement} */ (el('button', 'blockr-tool vz-data-tool'));
      this.tool.type = 'button';
      this.tool.innerHTML = ICON;
      frame.tools.insertBefore(this.tool, frame.downloadSlot);
      scope.on(this.tool, 'click', () => this.toggle());
      this.panel = el('div', 'vz-data', frame.body);
      this.bar = el('div', 'vz-data__bar', this.panel);
      this.host = el('div', 'vz-data__table', this.panel);
      this.host.id = frame.id + '-data';
      /** The table view, built when a table first arrives.
       * @type {VizView | null} */
      this.table = null;
      this._label();
    }

    _label() {
      const t = this.on ? 'Hide table' : 'Show as table';
      this.tool.setAttribute('aria-label', t);
      this.tool.setAttribute('data-blockr-tooltip', t);
      this.tool.setAttribute('aria-pressed', String(this.on));
    }

    /** @param {boolean} on */
    _show(on) {
      if (on === this.on) return;
      this.on = on;
      this.frame.el.classList.toggle('vz-data-open', on);
      this._label();
      this.onToggle();
    }

    _send() {
      this.pending = { on: this.on, tab: this.tab };
      V.util.send(this.frame.id, 'data', { on: this.on, tab: this.tab, n: ++requests });
    }

    /** Open or close the table. */
    toggle() {
      this._show(!this.on);
      this._send();
    }

    /** Show a tab; R sends its table. @param {'summary' | 'rows'} tab */
    pick(tab) {
      if (tab === this.tab) return;
      this.tab = tab;
      this._bar();
      this._send();
    }

    /**
     * R's `derived.data_view`, or none while the view is closed. A payload
     * that does not answer the reader's last request yet is skipped: R sends
     * the answer next.
     * @param {VizDataViewState | null | undefined} dv
     */
    update(dv) {
      const p = this.pending;
      if (p) {
        const answers = p.on ? !!dv && (dv.tab === p.tab || dv.summary == null) : !dv;
        if (!answers) return;
        this.pending = null;
      }
      if (!dv) {
        this._show(false);
        return;
      }
      this._show(true);
      this.state = dv;
      this.tab = dv.tab;
      this._bar();
      if (dv.table) this._draw(dv.table);
    }

    /** The table, with the rows of its data revision; without them, ask R.
     * @param {VizPayload} p */
    _draw(p) {
      const T = V.table;
      if (!T) return;
      if (p.data != null) {
        this.rows = { rev: p.data_rev, data: p.data };
      } else if (this.rows && this.rows.rev === p.data_rev) {
        p = Object.assign({}, p, { data: this.rows.data });
      } else {
        if (this.asked !== p.data_rev) {
          this.asked = p.data_rev;
          this._send();
        }
        return;
      }
      this.asked = null;
      if (!this.table) {
        const View = /** @type {new (el: HTMLElement, scope: VizScope, o: object) => VizView} */ (
          /** @type {unknown} */ (T.View));
        const scope = this.scope.own(new V.Scope());
        this.table = scope.own(new View(this.host, scope, { gear: false, still: false }));
      }
      this.table.render(p);
    }

    // The tabs with their counts; with no Summary, the count of rows alone.
    _bar() {
      const dv = this.state;
      this.bar.textContent = '';
      if (!dv) return;
      if (dv.summary == null) {
        el('span', 'vz-data__meta', this.bar).textContent = rowsCount(dv) + ' rows';
        return;
      }
      const opts = [
        { value: 'summary', label: 'Summary', count: String(dv.summary) },
        { value: 'rows', label: 'Rows', count: rowsCount(dv) }
      ];
      const ui = /** @type {VizUi} */ (root.Blockr);
      const seg = ui.segmented(opts, this.tab,
        (/** @type {string} */ v) => this.pick(v === 'rows' ? 'rows' : 'summary'),
        { size: 'xs', label: 'Table' });
      seg.el.querySelectorAll('.blockr-segmented__seg').forEach((b, i) => {
        if (opts[i].count) el('span', 'vz-data__n', /** @type {HTMLElement} */ (b)).textContent = opts[i].count;
      });
      this.bar.appendChild(seg.el);
    }
  }

  V.DataView = DataView;
})();
