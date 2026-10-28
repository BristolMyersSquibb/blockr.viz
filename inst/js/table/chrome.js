// @ts-check
/**
 * The table block's frame, the same for every layout: a header with the
 * titles on the left and the tools (search, download) and the gear on the
 * right, the scroll box the layout draws into, the caption, and a footer
 * with the row count and the drill receipt ("Filtered: Arm = Placebo", with
 * a clear button).
 *
 * The search and download stay hidden until there is a table to search or
 * download. The header lays out like the chart's (chart.css `.vz-head`); in
 * a narrow block the tools take their own row (table-view.css).
 *
 * Titles carry the title slots (core/titles.js) when the view has a gear,
 * plain text when it has none (a printed page). The download menu is
 * core/download.js's.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  /** Wait this long after the last key before searching. */
  const SEARCH_MS = 150;

  /**
   * @param {string} tag
   * @param {string} cls
   * @param {HTMLElement} [parent]
   */
  function el(tag, cls, parent) {
    const e = document.createElement(tag);
    e.className = cls;
    if (parent) parent.appendChild(e);
    return e;
  }

  class Frame {
    /**
     * @param {HTMLElement} host The output element; the frame fills it.
     * @param {VizScope} scope
     */
    constructor(host, scope) {
      this.id = host.id;
      this.scope = scope;
      this.el = el('div', 'blockr-html-table-container drilldown-table-container vz-table');
      host.replaceChildren(this.el);
      scope.own({ dispose: () => this.el.remove() });
      this.head = el('div', 'blockr-gear-header vz-head', this.el);
      this.titleBox = el('div', 'dd-table-titles vz-head__titles', this.head);
      this.titleEl = el('div', 'dd-table-title', this.titleBox);
      this.subEl = el('div', 'dd-table-subtitle', this.titleBox);
      this.tools = el('div', 'vz-head__tools', this.head);
      this.searchInput = /** @type {HTMLInputElement} */ (el('input', 'blockr-search', this.tools));
      this.searchInput.type = 'search';
      this.searchInput.placeholder = 'Search…';
      this.searchInput.setAttribute('aria-label', 'Search table');
      this.downloadSlot = el('div', 'dt-download-slot', this.tools);
      this.box = el('div', 'blockr-table-wrapper', this.el);
      this.box.style.overflow = 'auto';
      this.capEl = el('div', 'dd-table-caption', this.el);
      this.foot = el('div', 'dt-table-footer', this.el);
      this.countEl = el('span', 'dt-row-count', this.foot);
      this.countEl.setAttribute('aria-live', 'polite');
      this.statusEl = el('div', 'dd-status-footer', this.foot);
      this.statusText = el('span', 'dd-status-text', this.statusEl);
      this.clearBtn = /** @type {HTMLButtonElement} */ (el('button', 'dd-status-clear', this.statusEl));
      this.clearBtn.type = 'button';
      this.clearBtn.textContent = '×';
      this.clearBtn.setAttribute('aria-label', 'Clear filter');
      this.clearBtn.setAttribute('data-blockr-tooltip', 'Clear filter');
      /** @type {(() => void) | null} */
      this._onClear = null;
      /** @type {VizSlots | null} */
      this.slots = null;
      scope.on(this.clearBtn, 'click', () => { if (this._onClear) this._onClear(); });
      /** @type {Array<(q: string) => void>} */
      this._search = [];
      const run = scope.debounce(() => this._search.forEach((f) => f(this.query())), SEARCH_MS);
      scope.on(this.searchInput, 'input', run);
      this.titles(null);
      this.receipt(null, null);
      this.showTools({ search: false, download: false });
    }

    /** The search text, trimmed and lower case. */
    query() {
      return this.searchInput.hidden ? '' : this.searchInput.value.trim().toLowerCase();
    }

    /** @param {(q: string) => void} fn Called after a pause in typing. */
    onSearch(fn) {
      this._search.push(fn);
    }

    /**
     * The title, subtitle and caption. With the title slots loaded, words
     * that name an argument open its control.
     * @param {TableDerived['titles']} t
     */
    titles(t) {
      const x = t || {};
      /** @param {VizTitleBand | string | null | undefined} b */
      const band = (b) => (typeof b === 'string' ? { text: b } : b || null);
      const text = (/** @type {VizTitleBand | null} */ b) => (b && b.text) || '';
      const [title, sub, cap] = [band(x.title), band(x.subtitle), band(x.caption)];
      if (this.slots) {
        this.slots.paint(this.titleEl, title);
        this.slots.paint(this.subEl, sub);
      } else {
        this.titleEl.textContent = text(title);
        this.subEl.textContent = text(sub);
      }
      this.capEl.textContent = text(cap);
      this.titleEl.hidden = !text(title);
      this.subEl.hidden = !text(sub) && !(sub && sub.offers && sub.offers.length);
      this.titleBox.hidden = this.titleEl.hidden && this.subEl.hidden;
      this.capEl.hidden = !text(cap);
    }

    /**
     * Search and download, each only when on and there is a table. The
     * download control is core/download.js's; `onOpen` runs when the reader
     * reaches for it.
     * @param {{ search: boolean, download: boolean, downloads?: unknown,
     *           onOpen?: () => void }} o
     */
    showTools(o) {
      const was = !this.searchInput.hidden;
      this.searchInput.hidden = !o.search;
      if (was && !o.search && this.searchInput.value) {
        this.searchInput.value = '';
        this._search.forEach((f) => f(''));
      }
      this.downloadSlot.hidden = !o.download;
      V.downloads(this.downloadSlot, /** @type {VizDownload[]} */ (o.download ? o.downloads : null),
                  this.scope, o.onOpen ? { onOpen: o.onOpen } : {});
    }

    /**
     * Say why there is no table, or take the message away.
     * @param {TableDerived['empty']} e
     * @returns {boolean} Whether a message is shown.
     */
    empty(e) {
      let msg = /** @type {HTMLElement | null} */ (this.box.querySelector(':scope > .vd-empty-state'));
      if (!e) {
        if (msg) msg.remove();
        return false;
      }
      if (!msg) {
        msg = el('div', 'vd-empty-state', this.box);
        el('div', 'vd-empty-text', msg);
      }
      msg.setAttribute('data-kind', e.kind || 'empty');
      /** @type {HTMLElement} */ (msg.firstElementChild).textContent = e.message || '';
      return true;
    }

    /** @param {boolean} on A table with sections or keyed rows. */
    structured(on) {
      this.el.classList.toggle('drilldown-table-structured', on);
    }

    /** @param {string} text */
    count(text) {
      this.countEl.textContent = text;
    }

    /**
     * The receipt line: what the drill holds, with a button that clears it.
     * @param {string | null} text
     * @param {(() => void) | null} onClear No button without it.
     */
    receipt(text, onClear) {
      this.statusText.textContent = text || '';
      this.statusEl.hidden = !text;
      this.clearBtn.hidden = !onClear;
      this._onClear = onClear;
    }
  }

  V.table.Frame = Frame;
})();
