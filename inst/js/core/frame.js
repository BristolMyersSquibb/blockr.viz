// @ts-check
/**
 * The frame every block view draws in: a header with the titles on the left
 * and the tools (the download control, a view's own tools) and the gear on
 * the right, the gear's tray under the header, the body the view draws
 * into, the caption, and a footer with a count and the status line.
 *
 * The status line says what the selection holds ("Filtered: Arm = Placebo")
 * with a button that clears it, a note on the picture after it, and for a
 * moment after a drill sent to another block the receipt of that drill
 * instead. The frame owns the words' place and timing; the view writes them.
 *
 * The header's layout is in frame.css (`.vz-head`); in a narrow block the
 * tools take their own row.
 */
(function () {
  'use strict';
  const root = /** @type {any} */ (typeof window !== 'undefined' ? window : globalThis);
  const V = /** @type {VizNamespace} */ (root.Blockr.viz);

  // How long the footer holds a drill receipt, and how long it takes to
  // leave. The receipt's age drives the animation, so a repainted footer
  // resumes it. The grace is a backstop for an off-screen block, where
  // animationend never fires.
  const RECEIPT_HOLD_MS = 1400;
  const RECEIPT_FADE_MS = 1100;
  const RECEIPT_GRACE_MS = 600;

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

  /**
   * Say why there is nothing to draw in `parent`, or take the message away.
   * The message is the parent's first child.
   * @param {HTMLElement} parent
   * @param {VizEmpty | null | undefined} e
   * @returns {boolean} Whether a message is shown.
   */
  function message(parent, e) {
    let msg = /** @type {HTMLElement | null} */ (parent.querySelector(':scope > .vz-empty'));
    if (!e) {
      if (msg) msg.remove();
      return false;
    }
    if (!msg) {
      msg = el('div', 'vz-empty');
      el('p', 'vz-empty-text', msg);
      parent.prepend(msg);
    }
    msg.setAttribute('data-kind', e.kind || 'empty');
    /** @type {HTMLElement} */ (msg.firstElementChild).textContent = e.message || '';
    return true;
  }

  /** A title band as R sends it: a band, a plain string, or nothing.
   * @param {VizTitleBand | string | null | undefined} b @returns {VizTitleBand | null} */
  const band = (b) => (typeof b === 'string' ? { text: b } : b || null);

  /** A band shows when it has text, or settings to offer where a clause dropped.
   * @param {VizTitleBand | null} b */
  const shows = (b) => !!b && (!!b.text || (Array.isArray(b.offers) && b.offers.length > 0));

  class Frame {
    /**
     * @param {HTMLElement} host The output element; the frame fills it.
     * @param {VizScope} scope
     * @param {{ cls?: string }} [opts] `cls`: the block's own classes on the frame.
     */
    constructor(host, scope, opts = {}) {
      this.id = host.id;
      this.scope = scope;
      this.el = el('div', 'vz-frame' + (opts.cls ? ' ' + opts.cls : ''));
      host.replaceChildren(this.el);
      scope.own({ dispose: () => this.el.remove() });
      this.head = el('div', 'blockr-gear-header vz-head', this.el);
      this.titleBox = el('div', 'vz-head__titles', this.head);
      this.titleEl = el('div', 'vz-title', this.titleBox);
      this.subEl = el('div', 'vz-subtitle', this.titleBox);
      this.tools = el('div', 'vz-head__tools', this.head);
      this.downloadSlot = el('div', 'vz-download', this.tools);
      el('div', 'vz-busy-line', this.el).setAttribute('aria-live', 'polite');
      this.body = el('div', 'vz-body', this.el);
      this.capEl = el('div', 'vz-caption', this.el);
      this.foot = el('div', 'vz-foot', this.el);
      this.countEl = el('span', 'vz-count', this.foot);
      this.countEl.setAttribute('aria-live', 'polite');
      this.statusEl = el('div', 'vz-status', this.foot);
      this.clearBtn = /** @type {HTMLButtonElement} */ (el('button', 'vz-status-clear'));
      this.clearBtn.type = 'button';
      this.clearBtn.textContent = '×';
      this.clearBtn.setAttribute('aria-label', 'Clear filter');
      this.clearBtn.setAttribute('data-blockr-tooltip', 'Clear filter');
      scope.on(this.clearBtn, 'click', () => { if (this._line.onClear) this._line.onClear(); });
      /** The resting line. @type {{ text: string | null, onClear: (() => void) | null }} */
      this._line = { text: null, onClear: null };
      /** @type {string | null} */
      this._note = null;
      /** The drill receipt and when it was taken. @type {{ text: string, at: number } | null} */
      this._receipt = null;
      /** @type {(() => void) | null} */
      this._receiptTimer = null;
      this._returning = false;
      /** Title words that open the gear; none without a gear. @type {VizSlots | null} */
      this.slots = null;
      /** @type {VizGear | null} */
      this.gear = null;
      this.openGear = () => {};
      this.titles(null);
      this.downloads(null);
      this._paintStatus();
    }

    /**
     * Mount the gear in a tray under the header, and the title words that
     * open its controls.
     * @param {{ block: string, typeKey: string, title: string, host: VizGearHost }} o
     * @returns {VizGear}
     */
    mountGear(o) {
      const t = V.gear.tray(this.head, this.scope, o.title);
      this.tray = t;
      this.openGear = () => t.set(true);
      this.gear = this.scope.own(V.gear.mount(t.band, {
        block: o.block, id: this.id, scope: this.scope, typeKey: o.typeKey,
        title: o.title, host: o.host
      }));
      this.slots = this.scope.own(V.titles.slots({
        gear: this.gear, scope: this.scope, openGear: this.openGear
      }));
      return this.gear;
    }

    /**
     * The title, subtitle and caption, each hidden while empty. With a gear,
     * words that name an argument open its control.
     * @param {VizTitles | Record<string, VizTitleBand | string | null> | null | undefined} t
     */
    titles(t) {
      const x = /** @type {Record<string, VizTitleBand | string | null | undefined>} */ (t || {});
      const [title, sub, cap] = [band(x.title), band(x.subtitle), band(x.caption)];
      /** @param {HTMLElement} e @param {VizTitleBand | null} b */
      const paint = (e, b) => {
        if (this.slots) this.slots.paint(e, b);
        else e.textContent = (b && b.text) || '';
        e.hidden = !shows(b);
      };
      paint(this.titleEl, title);
      paint(this.subEl, sub);
      paint(this.capEl, cap);
      this.titleBox.hidden = this.titleEl.hidden && this.subEl.hidden;
    }

    /**
     * The download control (core/download.js) with the formats R offers,
     * hidden without any. `onOpen` runs when the reader reaches for it; a
     * promise it returns (the picture on its way to R) holds a click on a
     * format until it settles.
     * @param {VizDownload[] | null | undefined} list
     * @param {() => unknown} [onOpen]
     */
    downloads(list, onOpen) {
      const offer = Array.isArray(list) ? list : [];
      this.downloadSlot.hidden = offer.length === 0;
      V.downloads(this.downloadSlot, offer, this.scope, onOpen ? { onOpen } : {});
    }

    /**
     * Say in the body why there is nothing to draw, or take the message away.
     * @param {VizEmpty | null | undefined} e
     * @returns {boolean} Whether a message is shown.
     */
    empty(e) {
      return message(this.body, e);
    }

    /** @param {string} text The count left of the status line ("86 rows"). */
    count(text) {
      this.countEl.textContent = text;
    }

    /**
     * The resting status line: what the selection holds, with a clear
     * button when `onClear` is given; null for none.
     * @param {string | null} text
     * @param {(() => void) | null} [onClear]
     */
    status(text, onClear) {
      this._line = { text: text || null, onClear: onClear || null };
      this._paintStatus();
    }

    /** A note after the line, warning about the picture; null for none.
     * @param {string | null} text */
    note(text) {
      this._note = text || null;
      this._paintStatus();
    }

    /**
     * The receipt of a drill sent to another block: it takes the line's place
     * for a moment and fades.
     * @param {string} text
     */
    flash(text) {
      this._receipt = { text, at: performance.now() };
      this._paintStatus();
      if (this._receiptTimer) this._receiptTimer();
      this._receiptTimer = this.scope.timeout(() => this._dropReceipt(),
        RECEIPT_HOLD_MS + RECEIPT_FADE_MS + RECEIPT_GRACE_MS);
    }

    _dropReceipt() {
      if (!this._receipt) return;
      this._receipt = null;
      this._returning = true;
      this._paintStatus();
      this._returning = false;
    }

    // Repainted whole on every change. A live receipt wins the line.
    _paintStatus() {
      const bar = this.statusEl;
      bar.textContent = '';
      this.clearBtn.hidden = true;
      if (this._receipt) {
        const age = performance.now() - this._receipt.at;
        if (age < RECEIPT_HOLD_MS + RECEIPT_FADE_MS) {
          const rec = el('span', 'vz-status-text vz-status-receipt', bar);
          rec.textContent = this._receipt.text;
          // A negative delay resumes the fade where the receipt is.
          rec.style.animation = `vz-receipt-out ${RECEIPT_FADE_MS}ms linear ` +
            `${RECEIPT_HOLD_MS - age}ms both`;
          this.scope.on(rec, 'animationend', () => this._dropReceipt());
          return;
        }
        this._receipt = null;
      }
      const line = this._line;
      if (line.text) {
        el('span', 'vz-status-text' + (this._returning ? ' vz-status-returning' : ''), bar)
          .textContent = line.text;
        if (line.onClear) {
          bar.appendChild(this.clearBtn);
          this.clearBtn.hidden = false;
        }
      }
      if (this._note) el('span', 'vz-status-note', bar).textContent = this._note;
    }

    /** End R's busy cue once the view is on screen: two frames, so the
     * browser has painted. */
    busyDone() {
      const busy = root.Blockr.busy;
      if (!busy || !this.id) return;
      this.scope.frame(() => this.scope.frame(() => busy.stop(this.id)));
    }
  }

  V.Frame = Object.assign(Frame, { message });
})();
