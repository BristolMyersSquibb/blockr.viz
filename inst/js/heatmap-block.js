// @ts-check
/**
 * heatmap-block.js — wiring for new_heatmap_block(). R mounts the hmb-*
 * chrome once (R/heatmap-html.R: hmb_chrome) and pushes the body over a
 * custom message; this script:
 *   - paints the header row: title, sentence and caption, whose `{@arg}`
 *     words are live (Blockr.SentenceSlots, shared with the chart),
 *   - builds the gear tray from the shared Blockr.DrilldownConfig engine and
 *     blockr.ui's Blockr.gearTray,
 *   - assembles the matrix rows from the sparse cell model, with a data
 *     tooltip per cell and the header following the panel's scroll,
 *   - wires the search tool and the row click, which is an event sent over
 *     the control bridge to the board's drill filter (as the composer
 *     table's): nothing latches here, and a board with no drill filter leaves
 *     the rows inert.
 * Mirrors tile-block.js (scan + MutationObserver init, idempotent per root).
 */
(function () {
  // Where this file was served from: snapdom.js sits next to it and is loaded
  // only when a picture is first asked for.
  var JS_BASE = (function () {
    var sc = /** @type {HTMLScriptElement|null} */ (document.currentScript);
    return sc && sc.src ? sc.src.replace(/[^\/]*$/, '') : '';
  })();

  /** @param {string} elemId @param {string} param @param {*} value */
  function sendConfig(elemId, param, value) {
    if (!elemId || !window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(elemId + '_action',
      { action: 'config', param: param, value: value }, { priority: 'event' });
  }
  /**
   * Per-root client state. The chrome is mounted once and never rebuilt, so
   * everything that changes with a payload lives here.
   * @typedef {{ elemId: string, cfg: Record<string, any>, cols: any[],
   *   ddc: any, slots: any, tray: any, model: any, pos: Record<number, number>,
   *   groupOf: string[], capture?: boolean, captureRatio?: number,
   *   capturePage?: { w: number, h: number, minPt?: number } | null }} HmbState
   */
  /** @type {WeakMap<Element, HmbState>} */
  var states = new WeakMap();

  // ---- search ---------------------------------------------------------
  // The row filter, re-applied after every body swap -- the rows are new
  // nodes, so a search typed before the data changed would otherwise show
  // everything again. A search hides the group header rows: their counts
  // would no longer match what is shown.
  /** @param {Element} root */
  function applySearch(root) {
    var search = /** @type {HTMLInputElement|null} */ (
      root.querySelector('.hmb-search'));
    var q = search ? search.value.trim().toLowerCase() : '';
    root.querySelectorAll('tr.hmb-r').forEach(function (tr) {
      var id = (tr.getAttribute('data-hmb-id') || '').toLowerCase();
      (/** @type {HTMLElement} */ (tr)).style.display =
        (q === '' || id.indexOf(q) !== -1) ? '' : 'none';
    });
    root.querySelectorAll('tr.hmb-grp').forEach(function (tr) {
      (/** @type {HTMLElement} */ (tr)).style.display = q === '' ? '' : 'none';
    });
  }

  /** @param {Element} root */
  function wireSearch(root) {
    var btn0 = /** @type {HTMLButtonElement|null} */ (
      root.querySelector('.hmb-search-btn'));
    var input0 = /** @type {HTMLInputElement|null} */ (
      root.querySelector('.hmb-search'));
    if (!btn0 || !input0) return;
    var btn = btn0, input = input0;
    btn.addEventListener('click', function () {
      var on = btn.getAttribute('aria-pressed') !== 'true';
      btn.setAttribute('aria-pressed', on ? 'true' : 'false');
      root.classList.toggle('hmb-search-open', on);
      if (on) {
        input.focus();
      } else {
        input.value = '';
        applySearch(root);
      }
    });
    input.addEventListener('input', function () { applySearch(root); });
    input.addEventListener('keydown', function (e) {
      if (e.key !== 'Escape') return;
      e.stopPropagation();
      btn.click();
      btn.focus();
    });
  }

  // ---- drill ----------------------------------------------------------
  // Click counter: it changes on a real click and NOT on a board update, which
  // is the distinction the server's send-once skip has to make.
  var drillSeq = 0;

  // Light the clicked row, then release it. One at a time: two lit rows would
  // read as two selections in a model where only the last click counts.
  /** @param {Element} root @param {Element} tr */
  function flashRow(root, tr) {
    root.querySelectorAll('tr.hmb-flash').forEach(function (n) {
      n.classList.remove('hmb-flash');
    });
    tr.classList.add('hmb-flash');
    var drop = function () {
      tr.classList.remove('hmb-flash');
      tr.removeEventListener('animationend', drop);
    };
    tr.addEventListener('animationend', drop);
  }

  /** @param {Element} root @param {string} elemId */
  function wireDrill(root, elemId) {
    root.addEventListener('click', function (e) {
      // data-hmb-drill is "1" while the board has a drill filter to send to.
      if (root.getAttribute('data-hmb-drill') !== '1') return;
      var t = /** @type {Element|null} */ (e.target);
      var tr = t && t.closest('tr.hmb-r');
      if (!tr || !root.contains(tr)) return;
      var id = tr.getAttribute('data-hmb-id');
      var col = root.getAttribute('data-hmb-row-col');
      if (!id || !col) return;
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(elemId + '_action', {
          action: 'filter', column: col, values: [id], nonce: ++drillSeq
        }, { priority: 'event' });
      }
      flashRow(root, tr);
    });
  }

  // ---- the gear -------------------------------------------------------
  var HM_ROLES = {
    row:   { label: 'Row', kind: 'column', colType: 'any' },
    col:   { label: 'Column', kind: 'column', colType: 'any' },
    color: { label: 'Paint by worst level of', kind: 'column', colType: 'any' },
    group: { label: 'Group rows by', kind: 'column', colType: 'cat' },
    cell_numbers: { label: 'Cell numbers', kind: 'segmented',
                    options: [{ value: 'on', label: 'Cell numbers' },
                              { value: 'off', label: 'Cell numbers' }] },
    download: { label: 'Download', kind: 'segmented',
                options: [{ value: 'on', label: 'Download' },
                          { value: 'off', label: 'Download' }] },
    // Three-tier text (R/title-template.R): null = the input's own label,
    // "" = none, anything else a template.
    title:    { label: 'Title', kind: 'text', ph: 'e.g. Adverse events',
                autoValue: (/** @type {any} */ cfg) =>
                  (cfg.title == null && cfg.title_resolved) ? cfg.title_resolved : '' },
    subtitle: { label: 'Subtitle', kind: 'text',
                ph: 'e.g. Top {@top_n} {label(@col)}[, by {label(@group)}]',
                hint: 'A setting becomes a control on the block. [ ] drops its clause when the setting is empty.',
                autoValue: (/** @type {any} */ cfg) =>
                  (cfg.subtitle == null && cfg.subtitle_resolved) ? cfg.subtitle_resolved : '' },
    caption:  { label: 'Caption', kind: 'text', ph: 'e.g. {filters}',
                autoValue: (/** @type {any} */ cfg) =>
                  (cfg.caption == null && cfg.caption_resolved) ? cfg.caption_resolved : '' }
  };
  /** @param {boolean} hasCols */
  function hmSections(hasCols) {
    return {
      requiredMap: hasCols ? ['row', 'col'] : [],
      optionalMap: hasCols ? ['color', 'group'] : [],
      mapping: [],
      summaries: false,
      aggregatable: false,
      colorSection: null,
      ctrlSection: false,
      presentation: ['cell_numbers', 'download'],
      titles: ['title', 'subtitle', 'caption']
    };
  }

  /** @param {Element} root @param {HmbState} st */
  function buildGear(root, st) {
    var tools = root.querySelector('.hmb-tools');
    var head = root.querySelector('.hmb-head');
    if (!tools || !head) return;

    var btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'blockr-gear-btn';
    btn.innerHTML = (typeof Blockr !== 'undefined' && Blockr.icons)
      ? Blockr.icons.gear : '';
    tools.appendChild(btn);

    var band = document.createElement('div');
    band.className = 'blockr-settings blockr-settings--beak dd-popover hmb-tray';
    band.setAttribute('data-dd-pop-for', st.elemId);
    head.after(band);

    var B = (typeof Blockr !== 'undefined') ? Blockr : null;
    if (!B || !B.gearTray || !B.DrilldownConfig || !B.SentenceSlots) {
      // LOUD on purpose, as in the chart: blockr.viz and blockr.ui ship
      // together, and a gear that does nothing says nothing about why.
      throw new Error('heatmap block: blockr.ui is too old for this ' +
        'blockr.viz (Blockr.gearTray / Blockr.SentenceSlots missing).');
    }
    st.tray = B.gearTray(band, btn, { label: 'Heatmap settings' });

    var elemId = st.elemId;
    st.ddc = new B.DrilldownConfig({
      popoverEl: function () { return band; },
      roles: HM_ROLES,
      config: function () { return st.cfg; },
      columns: function () { return st.cols; },
      context: function () { return 'all'; },
      currentType: function () { return null; },
      sections: function () { return hmSections(st.cols.length > 0); },
      sectionsForFamily: function () { return hmSections(st.cols.length > 0); },
      secondary: new Set(),
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function (/** @type {string} */ role) {
        return role === 'row' || role === 'col';
      },
      metricsList: function () { return []; },
      onMetricsChange: function () {},
      title: 'Heatmap settings',
      onChange: function (/** @type {string} */ key) {
        var v = st.cfg[key];
        if (key === 'cell_numbers') {
          root.classList.toggle('hmb-nonum', v === 'off');
        }
        sendConfig(elemId, key, v);
      },
      onMults: function () {},
      onClearFilter: function () {},
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () { return st.tray.isOpen(); },
      reopen: function () { st.tray.set(true); }
    });
    st.slots = new B.SentenceSlots({
      ddc: function () { return st.ddc; },
      config: function () { return st.cfg; },
      openGear: function () { st.tray.set(true); }
    });
  }

  // ---- header: title, sentence, caption --------------------------------
  /** @param {Element} root @param {HmbState} st */
  function paintHeader(root, st) {
    var cfg = st.cfg;
    var titleEl = /** @type {HTMLElement|null} */ (root.querySelector('.hmb-title'));
    var subEl = /** @type {HTMLElement|null} */ (root.querySelector('.hmb-subtitle'));
    var capEl = /** @type {HTMLElement|null} */ (root.querySelector('.hmb-caption'));
    var titles = root.querySelector('.hmb-titles');
    if (!titleEl || !subEl || !capEl || !titles || !st.slots) return;
    var offersEl = /** @type {HTMLElement|null} */ (titles.querySelector('.hmb-offers'));
    if (!offersEl) {
      offersEl = document.createElement('div');
      offersEl.className = 'hmb-offers';
      titles.appendChild(offersEl);
    }
    // A repaint replaces the very node an open slot menu is anchored to.
    st.slots.close();
    st.slots.paint(titleEl, cfg.title_resolved || '', cfg.title_parts);
    st.slots.paint(subEl, cfg.subtitle_resolved || '', cfg.subtitle_parts);
    st.slots.paint(capEl, cfg.caption_resolved || '', cfg.caption_parts);
    // Offers sit on their own line under the sentence; the title's and the
    // caption's join them there.
    offersEl.textContent = '';
    /** @type {string[]} */
    var offers = [].concat(cfg.subtitle_offers || [], cfg.title_offers || [],
                           cfg.caption_offers || []);
    st.slots.paintOffers(offersEl, offers);
    titleEl.style.display = titleEl.textContent ? '' : 'none';
    subEl.style.display = subEl.textContent ? '' : 'none';
    offersEl.style.display = offersEl.childNodes.length ? '' : 'none';
    capEl.style.display = capEl.textContent ? '' : 'none';
  }

  // ---- body assembly ---------------------------------------------------
  // The client half of the cell model (R/heatmap-html.R): R sends the
  // <table> shell with its rotated header and a sparse model, this pastes
  // the rows. The markup MUST match hmb_assemble_rows() byte for byte: same
  // classes, same attribute order, same escaping (& < > escaped, quotes not,
  // which is htmltools' own rule for text and non-attribute content).
  /** @param {string} x */
  function esc(x) {
    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;')
      .replace(/>/g, '&gt;');
  }

  /** @param {any} m @returns {string} */
  function assembleRows(m) {
    var n = m.n, k = m.k, i, j;
    // Column-major, the layout R's matrix already has: idx %% n is the row.
    var cells = new Array(n * k);
    for (i = 0; i < n * k; i++) cells[i] = '<td class="hmb-c"></td>';
    for (j = 0; j < m.idx.length; j++) {
      var slot = m.pal[j] - 1;
      cells[m.idx[j]] = '<td class="hmb-c" style="background:' + m.bg[slot] +
        ';color:' + m.fg[slot] + '"><span>' + m.cnt[j] + '</span></td>';
    }

    // A group opens with a section-title row spanning the matrix.
    /** @type {Record<number, string>} */
    var heads = {};
    var groups = m.groups || [];
    var at = 0;
    for (i = 0; i < groups.length; i++) {
      heads[at] = '<tr class="hmb-grp"><td colspan="' + (k + 1) +
        '"><span class="hmb-gt">' + esc(groups[i].label) +
        '</span><span class="hmb-gn">' + groups[i].n + '</span></td></tr>';
      at += groups[i].n;
    }

    var out = new Array(n);
    for (i = 0; i < n; i++) {
      var id = esc(m.rows[i]);
      var row = (heads[i] || '') + '<tr class="hmb-r" data-hmb-i="' + i +
        '" data-hmb-id="' + id + '"><td class="hmb-stub">' + id + '</td>';
      for (j = 0; j < k; j++) row += cells[j * n + i];
      out[i] = row + '</tr>';
    }
    return out.join('');
  }

  // ---- data tooltip ----------------------------------------------------
  // The chart's data tooltip, drawn for a cell (design system, "Charts:
  // data tooltip"): the term with the cell's swatch, then label / value
  // rows. One card for the page, placed by the pointer.
  /** @type {HTMLElement|null} */
  var tipEl = null;
  function tip() {
    if (!tipEl) {
      tipEl = document.createElement('div');
      tipEl.className = 'hmb-tip';
      tipEl.setAttribute('role', 'tooltip');
      document.body.appendChild(tipEl);
    }
    return tipEl;
  }
  function hideTip() { if (tipEl) tipEl.classList.remove('hmb-tip--on'); }

  /** @param {HTMLElement} el @param {string} label @param {string} value */
  function tipRow(el, label, value) {
    var r = document.createElement('div');
    r.className = 'hmb-tip-r';
    var l = document.createElement('span');
    l.textContent = label;
    var v = document.createElement('b');
    v.textContent = value;
    r.appendChild(l);
    r.appendChild(v);
    el.appendChild(r);
  }

  /** @param {Element} root */
  function wireTooltip(root) {
    root.addEventListener('mousemove', function (ev) {
      var e = /** @type {MouseEvent} */ (ev);
      var t = /** @type {Element|null} */ (e.target);
      var td = t && /** @type {HTMLTableCellElement|null} */ (t.closest('td.hmb-c'));
      var st = states.get(root);
      if (!td || !td.firstChild || !st || !st.model) { hideTip(); return; }
      var tr = td.parentElement;
      var i = parseInt((tr && tr.getAttribute('data-hmb-i')) || '', 10);
      var j = td.cellIndex - 1;
      var m = st.model;
      var at = st.pos[j * m.n + i];
      if (!isFinite(i) || at == null) { hideTip(); return; }
      var el = tip();
      el.textContent = '';
      var h = document.createElement('div');
      h.className = 'hmb-tip-h';
      var sw = document.createElement('i');
      sw.style.background = m.bg[m.pal[at] - 1];
      h.appendChild(sw);
      h.appendChild(document.createTextNode(m.terms[j]));
      el.appendChild(h);
      tipRow(el, m.rowLabel || m.rowCol, m.rows[i]);
      tipRow(el, 'Events', String(m.cnt[at]));
      if (m.levels && m.levels.length) {
        var lv = m.lvl[at];
        tipRow(el, 'Worst ' + m.colorLabel,
               lv > 0 ? m.levels[lv - 1] : 'Not recorded');
      }
      if (m.groupLabel && st.groupOf[i] != null) {
        tipRow(el, m.groupLabel, st.groupOf[i]);
      }
      el.classList.add('hmb-tip--on');
      var w = el.offsetWidth, hh = el.offsetHeight;
      var x = e.clientX + 14, y = e.clientY + 14;
      if (x + w > window.innerWidth - 8) x = e.clientX - w - 14;
      if (y + hh > window.innerHeight - 8) y = e.clientY - hh - 14;
      el.style.left = x + 'px';
      el.style.top = y + 'px';
    });
    root.addEventListener('mouseleave', hideTip);
    root.addEventListener('mousedown', hideTip);
  }

  // ---- the header follows the panel's scroll ---------------------------
  // The matrix box scrolls sideways only and runs its full length, so up and
  // down belongs to the dock panel. A sticky header cannot follow that (a
  // box that scrolls in one axis is a scroll container in both), so the
  // header is moved down by as much of the table as has scrolled out of
  // view, and stops at the last row. table.js does the same for the composer
  // table (followHeader).
  /** @param {Element} el @returns {Element | null} */
  function scrollParent(el) {
    for (var n = el.parentElement; n; n = n.parentElement) {
      var oy = getComputedStyle(n).overflowY;
      if ((oy === 'auto' || oy === 'scroll') &&
          n.scrollHeight > n.clientHeight) return n;
    }
    return null;
  }
  /** @param {Element} box */
  function followHeader(box) {
    var thead = /** @type {HTMLElement | null} */ (
      box.querySelector('table.hmb-table > thead'));
    var table = thead && thead.parentElement;
    if (!thead || !table) return;
    var sp = scrollParent(box);
    var top = sp ? sp.getBoundingClientRect().top : 0;
    var max = table.offsetHeight - thead.offsetHeight;
    var y = Math.max(0, Math.min(top - table.getBoundingClientRect().top, max));
    thead.style.transform = y > 0 ? 'translateY(' + y + 'px)' : '';
    box.classList.toggle('hmb-scrolled', y > 0);
  }
  /** @param {Event} e */
  function followHeaders(e) {
    var t = e && e.target;
    var scope = t && /** @type {Element} */ (t).querySelectorAll
      ? /** @type {Element} */ (t) : document;
    scope.querySelectorAll('.hmb-block .hmb-scroll').forEach(followHeader);
  }
  document.addEventListener('scroll', followHeaders,
                            { capture: true, passive: true });
  window.addEventListener('resize', followHeaders);

  // ---- body payloads ---------------------------------------------------
  // A PERSISTENT store, not a one-shot queue (the table and rank blocks'
  // shape): a payload that arrives before its chrome exists waits here, and
  // a chrome re-created later -- dock panel re-mount, view switch -- paints
  // from the store with no R round trip.
  /** @type {Record<string, any>} */
  var store = {};

  /** @param {Element} root @param {any} p */
  function applyPayload(root, p) {
    var st = states.get(root);
    if (!p || !st) return;

    if (p.cols != null) {
      root.setAttribute('data-hmb-cols', p.cols);
      try { st.cols = JSON.parse(p.cols); } catch (e) { st.cols = []; }
    }
    if (p.config != null) {
      root.setAttribute('data-hmb-config', p.config);
      try { st.cfg = JSON.parse(p.config); } catch (e) { st.cfg = {}; }
    }
    root.setAttribute('data-hmb-drill', p.drill ? '1' : '0');
    st.capture = !!p.capture;
    st.captureRatio = Number(p.captureRatio) || 2;
    st.capturePage = p.capturePage || null;
    root.setAttribute('data-hmb-row-col', p.rowCol || '');
    root.classList.toggle('hmb-nonum', !p.cellNumbers);

    var legend = root.querySelector('.hmb-legend-slot');
    var scroll = root.querySelector('.hmb-scroll');
    var count = root.querySelector('.hmb-count');
    st.model = null;
    if (p.err) {
      if (legend) legend.innerHTML = '';
      if (scroll) {
        var msg = document.createElement(p.errKind === 'danger' ? 'div' : 'p');
        msg.className = p.errKind === 'danger'
          ? 'hmb-msg hmb-msg--danger' : 'blockr-empty blockr-empty--block';
        msg.textContent = p.err;
        scroll.innerHTML = '';
        scroll.appendChild(msg);
      }
      if (count) count.textContent = '';
    } else {
      if (legend) legend.innerHTML = p.legend || '';
      if (scroll) {
        scroll.innerHTML = p.head || '';
        var tbody = scroll.querySelector('tbody');
        if (tbody && p.model) tbody.innerHTML = assembleRows(p.model);
        // A term the rotated header cuts off gets its full text as the
        // tooltip; a term that fits has none.
        scroll.querySelectorAll('th.hmb-rot > span').forEach(function (s) {
          if (s.scrollHeight > s.clientHeight + 1) {
            s.setAttribute('data-blockr-tooltip', s.textContent || '');
          }
        });
        followHeader(scroll);
      }
      if (count) count.textContent = p.count || '';
      if (p.model) {
        st.model = p.model;
        /** @type {Record<number, number>} */
        var pos = {};
        for (var t = 0; t < p.model.idx.length; t++) pos[p.model.idx[t]] = t;
        st.pos = pos;
        /** @type {string[]} */
        var groupOf = [];
        (p.model.groups || []).forEach(function (/** @type {any} */ g) {
          for (var r = 0; r < g.n; r++) groupOf.push(g.label);
        });
        st.groupOf = groupOf;
      }
      applySearch(root);
    }

    // A failed prepare script is a block-level error, shown above the
    // output (design system, "Messages"); the gear's script section says it
    // too.
    var serr = root.querySelector('.hmb-script-error');
    if (st.cfg.script_error) {
      if (!serr) {
        serr = document.createElement('div');
        serr.className = 'hmb-msg hmb-msg--danger hmb-script-error';
        var slot = root.querySelector('.hmb-legend-slot');
        if (slot) slot.before(serr);
      }
      serr.textContent = 'Script failed: ' + st.cfg.script_error;
    } else if (serr) {
      serr.remove();
    }

    paintHeader(root, st);
    if (st.ddc) st.ddc.render();
  }

  // ---- the export picture ---------------------------------------------
  // The heatmap a download carries is the one the browser drew (the chart's
  // and the summarize table's rule, R/chart-capture.R): the block cloned
  // offscreen without its tools, every row, the sideways scroll opened up,
  // the sentence as plain text, turned into one PNG by snapdom.
  /** @type {Promise<any> | null} */
  var snapdomLoading = null;
  function loadSnapdom() {
    var w = /** @type {any} */ (window);
    if (w.snapdom) return Promise.resolve(w.snapdom);
    if (!snapdomLoading) {
      snapdomLoading = new Promise(function (resolve, reject) {
        var sc = document.createElement('script');
        sc.src = JS_BASE + 'snapdom.js';
        sc.onload = function () {
          if (w.snapdom) resolve(w.snapdom);
          else reject(new Error('snapdom.js loaded but defined nothing'));
        };
        sc.onerror = function () {
          snapdomLoading = null;
          reject(new Error('snapdom.js could not be loaded'));
        };
        document.head.appendChild(sc);
      });
    }
    return snapdomLoading;
  }

  /** The picture shows every column label in full: the rotated header
   * grows to the longest term (up to 320px) instead of cutting it off.
   * @param {HTMLElement} clone */
  function openColumnLabels(clone) {
    var spans = clone.querySelectorAll('th.hmb-rot > span');
    if (!spans.length) return;
    var tall = 0;
    spans.forEach(function (s) {
      var el = /** @type {HTMLElement} */ (s);
      el.style.maxHeight = '314px';
      tall = Math.max(tall, el.scrollHeight);
    });
    var h = Math.min(320, Math.ceil(tall) + 6);
    if (h <= 150) return;
    clone.querySelectorAll('th.hmb-rot').forEach(function (th) {
      (/** @type {HTMLElement} */ (th)).style.height = h + 'px';
    });
  }

  /** The picture is as wide as the matrix, whatever the panel's width: the
   * host grows past the panel's width until no table runs past the clone's
   * right edge (snapdom draws only the clone's box).
   * @param {HTMLElement} host @param {HTMLElement} clone */
  function widenToContent(host, clone) {
    for (var i = 0; i < 4; i++) {
      var box = clone.getBoundingClientRect();
      var edge = box.right - (parseFloat(getComputedStyle(clone).paddingRight) || 0);
      var over = 0;
      clone.querySelectorAll('table').forEach(function (t) {
        over = Math.max(over, t.getBoundingClientRect().right - edge);
      });
      if (over < 0.5) return;
      var grown = Math.ceil(host.offsetWidth + over);
      host.style.maxWidth = 'none';
      host.style.width = grown + 'px';
    }
  }

  /** One picture of the drawn block: {png, width, height}, the size in CSS
   * px (R turns it into inches at 96 dpi).
   * @param {Element} root @param {HmbState} st */
  function heatmapPicture(root, st) {
    return loadSnapdom().then(function (snap) {
      // As wide as what it shows: no wider than the panel, so the sentence
      // wraps as on screen, but no blank either when the matrix is narrower
      // (widenToContent() takes it past the panel for a wider one).
      var width = Math.ceil(root.getBoundingClientRect().width) || 900;
      var host = document.createElement('div');
      host.style.cssText = 'position:fixed;left:-20000px;top:0;' +
        'width:fit-content;max-width:' + width + 'px;background:#ffffff;';
      var clone = /** @type {HTMLElement} */ (root.cloneNode(true));
      clone.removeAttribute('data-hmb-elem-id');
      clone.classList.remove('hmb-search-open');
      clone.style.padding = '12px';
      clone.querySelectorAll('.hmb-tools, .hmb-tray, .hmb-search-row, ' +
        '.hmb-offers, .hmb-footer').forEach(function (n) { n.remove(); });
      clone.querySelectorAll('[id]').forEach(function (n) {
        n.removeAttribute('id');
      });
      /** @type {[string, string][]} */
      var texts = [['.hmb-title', 'title_resolved'],
                   ['.hmb-subtitle', 'subtitle_resolved'],
                   ['.hmb-caption', 'caption_resolved']];
      texts.forEach(function (t) {
        var el = /** @type {HTMLElement|null} */ (clone.querySelector(t[0]));
        if (!el) return;
        el.textContent = st.cfg[t[1]] || '';
        el.style.display = el.textContent ? '' : 'none';
      });
      var sc = /** @type {HTMLElement|null} */ (clone.querySelector('.hmb-scroll'));
      if (sc) {
        sc.style.overflow = 'visible';
        sc.classList.remove('hmb-scrolled');
      }
      var thead = /** @type {HTMLElement|null} */ (clone.querySelector('thead'));
      if (thead) thead.style.transform = '';
      clone.querySelectorAll('tr').forEach(function (tr) {
        tr.classList.remove('hmb-flash');
        (/** @type {HTMLElement} */ (tr)).style.display = '';
      });
      host.appendChild(clone);
      document.body.appendChild(host);
      var drop = function () {
        if (host.parentNode) host.parentNode.removeChild(host);
      };
      var fonts = (document.fonts && document.fonts.ready) ?
        document.fonts.ready : Promise.resolve();
      return fonts.then(function () {
        return new Promise(function (r) {
          requestAnimationFrame(function () { requestAnimationFrame(r); });
        });
      }).then(function () {
        openColumnLabels(clone);
        widenToContent(host, clone);
        var w = Math.ceil(clone.offsetWidth);
        var h = Math.ceil(clone.offsetHeight);
        // dpr 1: snapdom multiplies the scale by the device pixel ratio, and
        // the ratio is already the export's to choose.
        return snap.toCanvas(clone, {
          scale: st.captureRatio || 2, dpr: 1, embedFonts: true,
          backgroundColor: '#ffffff'
        }).then(function (/** @type {HTMLCanvasElement} */ canvas) {
          // Too long for one slide: the pages the deck puts on several
          // (capture-pages.js), cut while the clone is still laid out.
          var cut = /** @type {any} */ (window).BlockrCapturePages;
          var pages = (st.capturePage && cut) ?
            cut(canvas, clone, st.capturePage, 'tr.hmb-grp') : [];
          drop();
          return { png: canvas.toDataURL('image/png'), width: w, height: h,
                   pages: pages, title: st.cfg.title_resolved || '' };
        });
      }).catch(function (/** @type {any} */ e) { drop(); throw e; });
    });
  }

  // Opening the download menu posts the picture, so it is in R before a
  // format is picked (the chart and the summarize table do the same).
  /** @param {Element} root @param {HmbState} st */
  function wireCapture(root, st) {
    var tools = root.querySelector('.hmb-tools');
    if (!tools) return;
    tools.addEventListener('click', function (e) {
      var t = /** @type {Element|null} */ (e.target);
      if (!t || !t.closest || !st.capture || !st.model ||
          !t.closest('.blockr-action-menu__trigger, a.shiny-download-link')) {
        return;
      }
      heatmapPicture(root, st).then(function (r) {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(st.elemId + '_capture', r, { priority: 'event' });
        }
      }, function (err) {
        if (window.console) console.warn('heatmap picture:', err);
      });
    }, true);
  }

  /** @param {Element} root */
  function init(root) {
    if (!root || root.getAttribute('data-hmb-initialized') === '1') return;
    root.setAttribute('data-hmb-initialized', '1');
    var elemId = root.getAttribute('data-hmb-elem-id');
    if (!elemId) return;
    /** @type {HmbState} */
    var st = {
      elemId: elemId, cfg: {}, cols: [], ddc: null, slots: null, tray: null,
      model: null, pos: {}, groupOf: []
    };
    try { st.cfg = JSON.parse(root.getAttribute('data-hmb-config') || '{}'); }
    catch (e) { st.cfg = {}; }
    try { st.cols = JSON.parse(root.getAttribute('data-hmb-cols') || '[]'); }
    catch (e) { st.cols = []; }
    states.set(root, st);
    buildGear(root, st);
    paintHeader(root, st);
    if (st.ddc) st.ddc.render();
    wireSearch(root);
    wireDrill(root, elemId);
    wireTooltip(root);
    wireCapture(root, st);
    if (store[elemId]) {
      applyPayload(root, store[elemId]);
    } else if (window.Shiny && Shiny.setInputValue) {
      // Nothing to paint: tell the server this chrome is up. Shiny DROPS a
      // custom message with no handler registered, and this script only
      // loads with the first heatmap in the page -- on a board that opens on
      // a view without one, the startup payload would be lost.
      Shiny.setInputValue(elemId + '_ready', Date.now(), { priority: 'event' });
    }
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('blockr-viz-heatmap-data',
      function (/** @type {any} */ msg) {
        if (!msg || !msg.id) return;
        /** @type {any} */
        var p = null;
        try { p = JSON.parse(msg.payload); } catch (e) { return; }
        store[msg.id] = p;
        var root = document.querySelector(
          '.hmb-block[data-hmb-elem-id="' + msg.id + '"]');
        if (root) applyPayload(root, p);
      });
  }

  var SCAN_SEL = '.hmb-block[data-hmb-elem-id]';
  /** @param {Document | Element} [ctx] */
  function scan(ctx) {
    var nodes = (ctx || document)
      .querySelectorAll(SCAN_SEL + ':not([data-hmb-initialized])');
    Array.prototype.forEach.call(nodes, init);
  }
  /** @param {EventTarget | Element | null | undefined} el */
  function scanAround(el) {
    var e = /** @type {Element | null} */ (
      el && /** @type {Node} */ (el).nodeType === 1 ? el : null);
    if (!e) { scan(); return; }
    var root = e.closest(SCAN_SEL);
    if (root) init(root);
    else scan(e);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function () { scan(); });
  } else {
    scan();
  }
  if (typeof window.jQuery === 'function') {
    jQuery(document).on('shiny:value shiny:bound', function (/** @type {any} */ e) {
      var t = e.target;
      setTimeout(function () { scanAround(t); }, 0);
    });
  }
  /** @type {Element[]} */
  var pendingNodes = [];
  var flushScheduled = false;
  /** @param {Element} n */
  function queueWire(n) {
    pendingNodes.push(n);
    if (flushScheduled) return;
    flushScheduled = true;
    window.requestAnimationFrame(function () {
      flushScheduled = false;
      var nodes = pendingNodes;
      pendingNodes = [];
      nodes.forEach(scanAround);
    });
  }
  var mo = new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      var added = muts[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        var n = /** @type {Element} */ (added[j]);
        if (n.nodeType !== 1) continue;
        if (n.matches(SCAN_SEL) || n.querySelector(SCAN_SEL)) queueWire(n);
      }
    }
  });
  mo.observe(document.documentElement, { childList: true, subtree: true });
})();
