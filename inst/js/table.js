// @ts-check
(function () {
  // Shared aggregation vocabulary (group/value/func roles + AGG_FNS +
  // value-follows-agg reconcile) — the identical control the chart/tile use.
  var DAgg = /** @type {VizDrilldownAgg} */ (
    (typeof Blockr !== "undefined" && Blockr.DrilldownAgg) || window.DrilldownAgg);

  /** @param {string | number | null | undefined} s */
  function parseNum(s) {
    if (s == null) return null;
    var m = String(s).match(/-?\d[\d,]*(\.\d+)?/);
    if (!m) return null;
    return parseFloat(m[0].replace(/,/g, ""));
  }

  // Escapers matching htmltools::htmlEscape byte-for-byte, so the rows this
  // script assembles from the data-push cell model are identical to what
  // dt_flat_assemble_tag() pastes server-side: text escapes & < > (quotes
  // stay literal), attributes additionally escape both quote kinds.
  /** @param {string} s */
  function escHtml(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }
  /** @param {string} s */
  function escAttr(s) {
    return escHtml(s).replace(/"/g, "&quot;").replace(/'/g, "&#39;");
  }

  // Latest body payload per block (data-dt-elem-id), kept for the block's
  // lifetime -- NOT a one-shot queue. A payload can arrive before the chrome
  // renders (the container is a renderUI output), and dock panels tear down
  // and re-create the chrome on re-mounts: in both cases the fresh container
  // re-renders from here with no R round trip (wireRoot checks the store).
  /** @type {Record<string, { rev: number, payload: VizTablePayload }>} */
  var payloadStore = {};

  // Windowed ("virtual") rendering bounds for flat cell-model tables: above
  // VIRT_MIN rows only the viewport's rows (plus OVERSCAN each side) enter
  // the DOM, between two spacer rows. Below it the same code path renders
  // every row -- small tables gain nothing from windowing.
  var VIRT_MIN = 400;
  var OVERSCAN = 40;

  // Column widths are computed server-side (blockr.ui::column_widths_px,
  // carried by each table's <colgroup>) and the table renders with
  // table-layout: fixed from the first paint, so nothing in this script
  // measures or mutates layout: sort-arrow reveal, section collapse and
  // search filtering cannot reflow the columns by construction. (The old
  // lockTableWidths() measured the DOM lazily before the first reflow -
  // it skipped hidden tables and never covered the search path at all, so
  // filtering reflowed columns live while typing.)

  // Sort-state header feedback (aria-sort + arrow icon), shared by the DOM
  // sorter below and the cell-model sorter: reset every sortable header,
  // then mark `idx` with `dir` (0 = no sort).
  /** @param {HTMLElement} scope @param {number | null} idx @param {number} dir */
  function updateSortIndicators(scope, idx, dir) {
    scope.querySelectorAll("th.blockr-sortable").forEach(function (h) {
      h.setAttribute("aria-sort", "none");
      var i = h.querySelector(".blockr-sort-icon");
      if (i) i.classList.remove("blockr-sort-icon-asc", "blockr-sort-icon-desc");
    });
    if (idx == null || dir === 0) return;
    var th = scope.querySelector(
      'th.blockr-sortable[data-col-index="' + idx + '"]'
    );
    if (!th) return;
    th.setAttribute("aria-sort", dir === 1 ? "ascending" : "descending");
    var ic = th.querySelector(".blockr-sort-icon");
    if (ic) {
      ic.classList.add(
        dir === 1 ? "blockr-sort-icon-asc" : "blockr-sort-icon-desc"
      );
    }
  }

  /** @param {HTMLElement} root @param {HTMLElement} table @param {HTMLElement} tbody */
  function wireSort(root, table, tbody) {
    var structured = root.getAttribute("data-dt-structured") === "1";
    /** @type {{ col: number | null, dir: number }} */
    var state = { col: null, dir: 0 };
    var origin = Array.prototype.slice.call(tbody.children);
    origin.forEach(function (r, i) { r.setAttribute("data-dt-o", i); });

    function reset() {
      var all = Array.prototype.slice.call(tbody.children);
      all.sort(function (a, b) {
        return (+a.getAttribute("data-dt-o")) - (+b.getAttribute("data-dt-o"));
      });
      var f = document.createDocumentFragment();
      all.forEach(function (r) { f.appendChild(r); });
      tbody.appendChild(f);
    }

    // Decorate-sort-undecorate: read + parse each row's sort key ONCE (n
    // extractions), then sort the keyed array — a comparator that hit the DOM
    // did ~n log n textContent reads and regex parses per header click.
    // Comparator semantics are unchanged (numeric when both keys parse, else
    // locale compare; native sort stability keeps ties in document order).
    /** @param {Element[]} rows @param {number} idx @param {number} dir */
    function sortRows(rows, idx, dir) {
      var keyed = rows.map(function (r) {
        var v = r.children[idx] ? (r.children[idx].textContent || "").trim() : "";
        return { row: r, str: v, num: parseNum(v) };
      });
      keyed.sort(function (a, b) {
        var cmp;
        if (a.num !== null && b.num !== null) cmp = a.num - b.num;
        else cmp = a.str.localeCompare(b.str);
        return dir * cmp;
      });
      return keyed.map(function (k) { return k.row; });
    }

    // Structured tables keep their section grouping: sort the data rows
    // *within* each section block, leaving the section-header rows (and the
    // grouping order) in place — same contract as html_table().
    /** @param {number} idx @param {number} dir */
    function sortStructured(idx, dir) {
      var rows = Array.prototype.slice.call(tbody.children);
      /** @type {Array<{ headers: Element[], rows: Element[] }>} */
      var groups = [];
      /** @type {{ headers: Element[], rows: Element[] }} */
      var cur = { headers: [], rows: [] };
      rows.forEach(function (r) {
        if (r.classList.contains("blockr-section-header")) {
          if (cur.rows.length > 0) { groups.push(cur); cur = { headers: [], rows: [] }; }
          cur.headers.push(r);
        } else if (r.classList.contains("blockr-data-row")) {
          cur.rows.push(r);
        }
      });
      if (cur.headers.length > 0 || cur.rows.length > 0) groups.push(cur);
      groups.forEach(function (g) {
        g.rows = sortRows(g.rows, idx, dir);
      });
      var f = document.createDocumentFragment();
      groups.forEach(function (g) {
        g.headers.forEach(function (h) { f.appendChild(h); });
        g.rows.forEach(function (r) { f.appendChild(r); });
      });
      tbody.appendChild(f);
    }

    /** @param {number} idx @param {number} dir */
    function sortFlat(idx, dir) {
      var rows = sortRows(Array.prototype.slice.call(tbody.children), idx, dir);
      var f = document.createDocumentFragment();
      rows.forEach(function (r) { f.appendChild(r); });
      tbody.appendChild(f);
    }

    /** @param {number} idx */
    function sortBy(idx) {
      if (state.col === idx) {
        state.dir = state.dir === 1 ? -1 : (state.dir === -1 ? 0 : 1);
      } else {
        state.col = idx; state.dir = 1;
      }
      if (state.dir === 0) {
        state.col = null;
        updateSortIndicators(root, null, 0);
        reset();
        return;
      }
      updateSortIndicators(root, idx, state.dir);
      if (structured) sortStructured(idx, state.dir);
      else sortFlat(idx, state.dir);
    }

    root.querySelectorAll("th.blockr-sortable").forEach(function (th) {
      // Keyboard parity: the header is focusable and Enter/Space sort like a
      // click; aria-sort starts at "none" and tracks the state in sortBy.
      th.setAttribute("tabindex", "0");
      th.setAttribute("aria-sort", "none");
      /** @param {Event} e */
      var activate = function (e) {
        // A header the producer could name (data-dd-colkeys, e.g. an arm) is
        // a DRILL target first: its label claims the column, its arrow still
        // sorts. Sorting a Table 1 by an arm only reorders the stat rows
        // inside each section, which is worth less than the claim. Bubble
        // rather than stopPropagation, or the drill listener on <thead>
        // never hears the click.
        var t = /** @type {any} */ (e.target);
        if (e.type === "click" && th.hasAttribute("data-dd-colkeys") &&
            !(t && t.closest && t.closest(".blockr-sort-icon"))) {
          return;
        }
        e.stopPropagation();
        var idx = parseInt(th.getAttribute("data-col-index") || "", 10);
        if (!isNaN(idx)) sortBy(idx);
      };
      th.addEventListener("click", activate);
      th.addEventListener("keydown", function (e) {
        var k = /** @type {KeyboardEvent} */ (e).key;
        if (k !== "Enter" && k !== " ") return;
        e.preventDefault();   // Space must sort, not scroll the panel
        activate(e);
      });
    });
  }

  // Row-side section collapse/expand for structured ("Table 1") tables.
  // Ported from html_table()'s inline script: each section header toggles a
  // `.collapsed` class; visibility of every row is recomputed from the
  // section-header stack so nested collapse is honoured.
  /** @param {HTMLElement} root @param {HTMLElement} tbody */
  function wireCollapse(root, tbody) {
    if (root.getAttribute("data-dt-structured") !== "1") return;
    var table = /** @type {HTMLElement | null} */ (tbody.closest("table"));
    // Collapsing turned off: leave section headers as static labels.
    if (table && table.getAttribute("data-dt-collapsible") === "off") return;
    // Two collapse mechanisms coexist: explicit `.section_*` section headers
    // (data-level) and indent-derived toggles (a row whose data-indent parents
    // deeper rows). A row is hidden iff any ancestor of EITHER kind is collapsed.
    function recompute() {
      /** @type {Array<{ level: number, collapsed: boolean }>} */
      var secStack = [];
      /** @type {Array<{ indent: number, collapsed: boolean }>} */
      var indStack = [];
      Array.prototype.slice.call(tbody.children).forEach(function (r) {
        var isSec = r.classList.contains("blockr-section-header");
        var isData = r.classList.contains("blockr-data-row");
        var ind = parseInt(r.getAttribute("data-indent"), 10);
        // Drop indent-ancestors this row is not nested under (>= its own indent).
        if (isData && !isNaN(ind)) {
          while (indStack.length > 0 &&
                 indStack[indStack.length - 1].indent >= ind) indStack.pop();
        }
        if (isSec) {
          var lvl = parseInt(r.getAttribute("data-level"), 10);
          while (secStack.length > 0 &&
                 secStack[secStack.length - 1].level >= lvl) secStack.pop();
        }
        var hidden = secStack.some(function (s) { return s.collapsed; }) ||
                     indStack.some(function (s) { return s.collapsed; });
        if (hidden) r.classList.add("blockr-hidden-collapse");
        else r.classList.remove("blockr-hidden-collapse");
        if (isSec) {
          secStack.push({
            level: parseInt(r.getAttribute("data-level"), 10),
            collapsed: r.classList.contains("collapsed")
          });
        }
        if (isData && r.classList.contains("blockr-indent-toggle")) {
          indStack.push({ indent: ind, collapsed: r.classList.contains("collapsed") });
        }
      });
    }
    // Keep the group-button's aria-expanded in sync with the row's collapsed
    // state (the chevron rotation is purely CSS off `.collapsed`).
    /** @param {Element} h */
    function syncAria(h) {
      var btn = h.querySelector(".blockr-section-btn");
      if (btn) {
        btn.setAttribute(
          "aria-expanded",
          h.classList.contains("collapsed") ? "false" : "true"
        );
      }
    }
    var heads = function () {
      return Array.prototype.slice.call(
        tbody.querySelectorAll("tr.blockr-section-header"));
    };
    var indentHeads = function () {
      return Array.prototype.slice.call(
        tbody.querySelectorAll("tr.blockr-indent-toggle"));
    };
    var groups = function () {
      return heads().concat(indentHeads());
    };
    /** @type {HTMLButtonElement | null} */
    var foldAll = null;
    // Fold or open every group at once (the corner chevron, Alt+click on a
    // heading).
    /** @param {boolean} shut */
    function setAll(shut) {
      heads().forEach(function (h) {
        h.classList.toggle("collapsed", shut);
        syncAria(h);
      });
      indentHeads().forEach(function (h) {
        h.classList.toggle("collapsed", shut);
        var btn = h.querySelector(".blockr-indent-btn");
        if (btn) {
          btn.setAttribute("aria-expanded", shut ? "false" : "true");
        }
      });
      recompute();
      syncFoldAll();
    }
    function syncFoldAll() {
      if (!foldAll) return;
      var open = groups().every(function (h) {
        return !h.classList.contains("collapsed");
      });
      var act = open ? "Collapse all groups" : "Expand all groups";
      foldAll.setAttribute("aria-expanded", open ? "true" : "false");
      foldAll.setAttribute("aria-label", act);
      foldAll.setAttribute("data-blockr-tooltip", act);
    }
    foldAll = buildFoldAll(tbody, function () {
      setAll(foldAll != null && foldAll.getAttribute("aria-expanded") === "true");
    });
    root.querySelectorAll("tr.blockr-section-header").forEach(function (h) {
      // The Direction-01 group label is a <button> inside the row; its click
      // bubbles to this row-level handler, so a single listener covers both
      // the button and any bare-cell click.
      h.addEventListener("click", function (ev) {
        // Alt+click on any heading folds or opens every group, the way the
        // corner chevron does. Ahead of the drill test: an Alt-click is
        // never a claim.
        if (/** @type {MouseEvent} */ (ev).altKey) {
          ev.stopPropagation();
          ev.preventDefault();
          setAll(!h.classList.contains("collapsed"));
          return;
        }
        // Structured drill: the section VALUE text is the drill target
        // (wireClick emits the section's filter) -- but only on headers
        // that carry an identity claim (data-dd-keys: row-group headers).
        // Variable-block headers ("Sex") have no claim, so their value
        // text falls through to the collapse toggle like the rest of the
        // row. Read the attrs at click time -- the <table> re-renders on
        // config changes while these rows persist.
        var tbl = h.closest("table");
        var t = /** @type {Element | null} */ (ev.target);
        if (tbl && tbl.getAttribute("data-dt-structured-drill") === "1" &&
            t && t.closest(".blockr-section-value") &&
            h.getAttribute("data-dd-keys")) {
          return;
        }
        ev.stopPropagation();
        h.classList.toggle("collapsed");
        syncAria(h);
        recompute();
        syncFoldAll();
      });
    });
    // Indent-derived toggles: listen on the chevron button only, so clicking the
    // label / cells of a parent row still drills (the button never competes).
    root.querySelectorAll("tr.blockr-indent-toggle .blockr-indent-btn").forEach(
      function (btn) {
        btn.addEventListener("click", function (ev) {
          ev.stopPropagation();
          ev.preventDefault();
          var h = btn.closest("tr.blockr-indent-toggle");
          if (!h) return;
          h.classList.toggle("collapsed");
          btn.setAttribute(
            "aria-expanded", h.classList.contains("collapsed") ? "false" : "true"
          );
          recompute();
          syncFoldAll();
        });
      }
    );
    if (root.getAttribute("data-initial-expanded") === "0") {
      root.querySelectorAll("tr.blockr-section-header").forEach(function (h) {
        h.classList.add("collapsed");
        syncAria(h);
      });
      root.querySelectorAll("tr.blockr-indent-toggle").forEach(function (h) {
        h.classList.add("collapsed");
        var btn = h.querySelector(".blockr-indent-btn");
        if (btn) btn.setAttribute("aria-expanded", "false");
      });
      recompute();
    }
    syncFoldAll();
  }

  // The fold-all chevron in the top-left header cell, above the row labels.
  // Only drawn when the table folds and has groups. The producer's
  // first-column title (attr(.label, "label"), rendered by R into the cell)
  // stays exactly as it is and moves beside the chevron; only the chevron is
  // the button. Without a title the button reads "All". Built here, not in R,
  // so the exports -- which draw the title from the attribute -- never carry
  // it. The glyph is a clone of the group chevron, so the two are one svg.
  /** @param {HTMLElement} tbody @param {() => void} onClick */
  function buildFoldAll(tbody, onClick) {
    var table = tbody.closest("table");
    if (!table) return null;
    var chev = tbody.querySelector(
      "tr.blockr-section-header .blockr-chev, " +
      "tr.blockr-indent-toggle .blockr-chev"
    );
    if (!chev) return null;
    /** @param {HTMLButtonElement} btn */
    function bindFoldAll(btn) {
      btn.onclick = function (ev) {
        ev.stopPropagation();
        ev.preventDefault();
        onClick();
      };
      return btn;
    }
    var rendered = /** @type {HTMLButtonElement | null} */ (
      table.querySelector("thead th.blockr-stub-header .dt-foldall")
    );
    if (rendered) return bindFoldAll(rendered);

    var ths = table.querySelectorAll("thead th.blockr-stub-header");
    var first = /** @type {HTMLElement | null} */ (ths.length ? ths[0] : null);
    var firstTitle = first ? (first.textContent || "").trim() : "";
    var multiBlankStub = ths.length > 1 && !firstTitle;
    var th = /** @type {HTMLElement | null} */ (
      multiBlankStub ? first : (ths.length === 1 ? ths[0] : null)
    );
    if (!th) return null;
    var existing = /** @type {HTMLButtonElement | null} */ (
      th.querySelector(".dt-foldall")
    );
    if (existing) return bindFoldAll(existing);
    var title = (th.textContent || "").trim();
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "dt-foldall";
    btn.appendChild(chev.cloneNode(true));
    bindFoldAll(btn);
    var wrap = document.createElement("div");
    wrap.className = "dt-foldall-wrap";
    wrap.appendChild(btn);
    if (title) {
      var lab = document.createElement("span");
      lab.className = "dt-stub-title";
      while (th.firstChild) lab.appendChild(th.firstChild);
      wrap.appendChild(lab);
    } else {
      var all = document.createElement("span");
      all.className = "dt-foldall-all";
      all.textContent = "All";
      btn.appendChild(all);
      th.textContent = "";
    }
    th.appendChild(wrap);
    th.classList.add("dt-has-foldall");
    return btn;
  }

  // Sticky-header scroll shadow: toggle `.scrolled` on the scroll container
  // once it's scrolled away from the top, so the sticky header detaches with
  // a soft shadow rather than a hard line (Direction-01).
  /** @param {HTMLElement} root */
  function wireScrollShadow(root) {
    // const (not var) so the post-guard non-null narrowing holds inside onScroll.
    const sc = root.querySelector(".blockr-table-wrapper");
    if (!sc) return;
    // A page-scrolled box never scrolls vertically; followHeader() owns
    // `.scrolled` there.
    if (sc.classList.contains("dt-scroll-page")) return;
    // Arrow defined after the guard so the non-null narrowing of `sc` is captured.
    const onScroll = () => {
      if (sc.scrollTop > 2) sc.classList.add("scrolled");
      else sc.classList.remove("scrolled");
    };
    sc.addEventListener("scroll", onScroll, { passive: true });
    onScroll();
  }

  // scroll = "page" (dt_chrome, `.dt-scroll-page`): the table box scrolls
  // sideways only and runs its full length, so up and down belongs to
  // whatever scrolls around it (a dock panel, the page). A sticky header
  // cannot follow that: a box that scrolls in one axis is a scroll container
  // in both, so the header would stick to the box. Instead the header is
  // moved down by as much of the table as has scrolled out of the
  // scroller's view, and stops at the last row. The listener is one capture
  // listener on the document, which sees every element's scroll: the dock
  // moves a card between containers after it is wired, so a listener bound
  // to one scroller would go stale.
  var PAGE_SEL = ".blockr-table-wrapper.dt-scroll-page";

  // The nearest ancestor that actually scrolls up and down. An ancestor that
  // only scrolls sideways computes overflow-y to auto as well (the dock's
  // outputs section does), so the style alone would stop there.
  /** @param {Element} el @returns {Element | null} */
  function scrollParent(el) {
    for (var n = el.parentElement; n; n = n.parentElement) {
      var oy = getComputedStyle(n).overflowY;
      if ((oy === "auto" || oy === "scroll") &&
          n.scrollHeight > n.clientHeight) return n;
    }
    return null;
  }

  /** @param {Element} wrapper */
  function followHeader(wrapper) {
    var thead = /** @type {HTMLElement | null} */ (
      wrapper.querySelector("table.blockr-table > thead"));
    var table = thead && thead.parentElement;
    if (!thead || !table) return;
    var sp = scrollParent(wrapper);
    var top = sp ? sp.getBoundingClientRect().top : 0;
    // offsetHeight, not a rect: a rect would include the transform set here.
    var max = table.offsetHeight - thead.offsetHeight;
    var y = Math.max(0, Math.min(top - table.getBoundingClientRect().top, max));
    thead.style.transform = y > 0 ? "translateY(" + y + "px)" : "";
    wrapper.classList.toggle("scrolled", y > 0);
  }

  /** @param {Event} e */
  function followHeaders(e) {
    var t = e && e.target;
    var scope = t && /** @type {Element} */ (t).querySelectorAll
      ? /** @type {Element} */ (t) : document;
    scope.querySelectorAll(PAGE_SEL).forEach(followHeader);
  }

  document.addEventListener("scroll", followHeaders,
                            { capture: true, passive: true });
  window.addEventListener("resize", followHeaders);

  // Per-row lowercase search text, read + lowered ONCE per rendered row
  // (per-keystroke filtering used to re-extract textContent of the whole
  // tbody — on a 10k-row table every typed character walked the entire DOM).
  // Keyed on the row ELEMENT: a re-render mints fresh rows, so the cache can
  // never go stale and the old entries fall out with the old DOM.
  /** @type {WeakMap<Element, string>} */
  var searchText = new WeakMap();

  /** @param {Element} r */
  function rowText(r) {
    var t = searchText.get(r);
    if (t == null) {
      t = (r.textContent || "").toLowerCase();
      searchText.set(r, t);
    }
    return t;
  }

  // Zero-match feedback: with a query active and every data row hidden the
  // table is just a header over nothing — say so. The message lives next to
  // the <table> (inside the scroll wrapper), so it re-derives per filter pass
  // and vanishes with the old table on a re-render.
  /** @param {HTMLElement} table @param {boolean} show */
  function updateNoMatch(table, show) {
    var parent = table.parentElement;
    if (!parent) return;
    var msg = parent.querySelector(".blockr-search-empty");
    if (!show) {
      if (msg) parent.removeChild(msg);
      return;
    }
    if (msg) return;
    msg = document.createElement("div");
    msg.className = "blockr-search-empty";
    msg.setAttribute("role", "status");
    msg.textContent = "No matching rows";
    parent.insertBefore(msg, table.nextSibling);
  }

  // Row count in the footer: "3262 rows", or "12 of 3262 rows" while a search
  // narrows it. The client owns this because the client owns the search — the
  // server never sees the query, so a server-rendered count would go stale the
  // moment someone types. Read off the cell model where there is one, else off
  // the rendered rows (the exported static table, and the small html-kind
  // payloads). Plain digits, no thousands separator, so a count here and the
  // title's `{n}` token cannot disagree.
  //
  // Blank for a structured "Table 1" (section headers and stub rows are not
  // "rows") and for the 0-row / message states, which already say so in the
  // table body. `.dt-row-count:empty` then takes the footer box away.
  /** @param {HTMLElement} root */
  function updateCount(root) {
    var el = /** @type {HTMLElement | null} */ (
      root.querySelector(".dt-row-count"));
    if (!el) return;
    var total = 0;
    var shown = 0;
    var model = getModel(root);
    if (model) {
      total = model.p.n || 0;
      shown = model.view.length;
    } else if (root.getAttribute("data-dt-structured") === "1") {
      total = 0;
    } else {
      var tbody = /** @type {HTMLElement | null} */ (
        root.querySelector("table.blockr-table tbody"));
      var rows = tbody ? tbody.querySelectorAll("tr.blockr-data-row") : [];
      total = rows.length;
      for (var i = 0; i < rows.length; i++) {
        if (!rows[i].classList.contains("blockr-hidden-search")) shown++;
      }
    }
    if (!total) { el.textContent = ""; return; }
    var noun = total === 1 ? " row" : " rows";
    el.textContent = shown === total
      ? total + noun
      : shown + " of " + total + noun;
  }

  // One filter pass over the current table (also run directly by wireTable to
  // re-apply an active query to freshly rendered rows, skipping the debounce).
  /** @param {HTMLElement} root */
  function applySearch(root) {
    // Cell-model table: search filters the model's index view and re-renders
    // the window — O(matches) DOM work instead of class-toggling every row.
    var m = getModel(root);
    if (m) {
      m.query = currentQuery(root);
      computeView(m);
      renderWindow(root, m);
      updateCount(root);
      return;
    }
    var inp = /** @type {HTMLInputElement | null} */ (root.querySelector("input.blockr-search"));
    // The <table>/<tbody> re-renders on each filter while the input (in the
    // chrome) persists, so query them live rather than closing over them.
    var table = /** @type {HTMLElement | null} */ (root.querySelector("table.blockr-table"));
    var tbody = table && table.querySelector("tbody");
    if (!inp || !table || !tbody) return;
    var structured = root.getAttribute("data-dt-structured") === "1";
    var q = inp.value.trim().toLowerCase();
    var rows = Array.prototype.slice.call(tbody.children);
    var anyMatch = false;
    rows.forEach(function (r) {
      if (!r.classList.contains("blockr-data-row")) return;
      if (!q || rowText(r).indexOf(q) !== -1) {
        r.classList.remove("blockr-hidden-search");
        anyMatch = true;
      } else {
        r.classList.add("blockr-hidden-search");
      }
    });
    updateNoMatch(table, !!q && !anyMatch);
    updateCount(root);
    if (!structured) return;
    // A section header stays visible iff any descendant row (up to the next
    // header of equal/shallower level) is still visible — same as
    // html_table()'s search.
    for (var i = rows.length - 1; i >= 0; i--) {
      var r = rows[i];
      if (!r.classList.contains("blockr-section-header")) continue;
      if (!q) { r.classList.remove("blockr-hidden-search"); continue; }
      var level = parseInt(r.getAttribute("data-level"), 10);
      var anyVisible = false;
      for (var j = i + 1; j < rows.length; j++) {
        var n = rows[j];
        if (n.classList.contains("blockr-section-header")) {
          if (parseInt(n.getAttribute("data-level"), 10) <= level) break;
        }
        if (!n.classList.contains("blockr-hidden-search")) { anyVisible = true; break; }
      }
      if (anyVisible) r.classList.remove("blockr-hidden-search");
      else r.classList.add("blockr-hidden-search");
    }
  }

  /** @param {HTMLElement} root */
  function wireSearch(root) {
    var inp = /** @type {HTMLInputElement | null} */ (root.querySelector("input.blockr-search"));
    if (!inp || inp.getAttribute("data-dt-search-wired") === "1") return;
    inp.setAttribute("data-dt-search-wired", "1");
    // Debounced: coalesce a typing burst into one filter pass per 150 ms
    // pause — each pass touches every row, so per-keystroke passes scale
    // with table size.
    /** @type {number | undefined} */
    var timer;
    inp.addEventListener("input", function () {
      if (timer != null) clearTimeout(timer);
      timer = window.setTimeout(function () {
        timer = undefined;
        applySearch(root);
      }, 150);
    });
  }

  // Emit the filter-clear action (chart parity: _sendClearFilter). The R
  // side's filter branch reads the null column/values as "clear" and resets
  // the filter reactiveVals, so downstream recovers.
  /** @param {string} elemId */
  function sendClearFilter(elemId) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(elemId + "_action",
      { action: "filter", column: null, values: null,
        filter_type: "categorical" },
      { priority: "event" });
  }

  /** @param {HTMLElement} root */
  function clearActiveRows(root) {
    root.querySelectorAll("tr.dt-row-active").forEach(function (r) {
      r.classList.remove("dt-row-active");
    });
  }

  // ==========================================================================
  // Transient drill. With a ctrl_target the click is an EVENT sent to that
  // block, not a selection this table holds: nothing latches, nothing toggles
  // off, and the claim carries a click counter so re-clicking one row sends
  // again. The undo lives at the target, because the control channel has no
  // back-edge -- a selection held here would keep showing a cohort anyone else
  // can reset. Same rule as the chart (chart/common.js `transientDrill`). The R side stops stamping
  // `data-dt-active` in this mode, so the restore walks below simply never
  // fire.
  // ==========================================================================

  var FLASH_CLS = ["dt-flash-row", "dt-flash-col", "dt-flash-colhead",
                   "dt-flash-cross"];

  /** @param {HTMLElement} table */
  function transientDrill(table) {
    var t = table.getAttribute("data-dt-ctrl-target");
    return !!(t && t.trim());
  }

  // Click counter: it changes on a real click and NOT on a board update, which
  // is the distinction the server's send-once skip has to make.
  var drillSeq = 0;

  /** @param {HTMLElement} root */
  function clearFlash(root) {
    root.querySelectorAll("." + FLASH_CLS.join(", .")).forEach(function (el) {
      el.classList.remove.apply(el.classList, FLASH_CLS);
    });
  }

  // Light what was clicked, then release it. One signal at a time: two lit
  // rows would read as two selections in a model where only the last click
  // counts.
  /** @param {HTMLElement} root @param {Array<{el: Element, cls: string}>} parts */
  function flash(root, parts) {
    clearFlash(root);
    parts.forEach(function (p) {
      if (!p.el) return;
      p.el.classList.add(p.cls);
      var drop = function () {
        p.el.classList.remove.apply(p.el.classList, FLASH_CLS);
        p.el.removeEventListener("animationend", drop);
      };
      // The row's animation runs on its cells, so animationend arrives by
      // bubbling; a cell's own animation ends on the cell itself.
      p.el.addEventListener("animationend", drop);
    });
  }

  /** @param {HTMLElement} root @param {HTMLElement} table @param {HTMLElement} tbody */
  function wireClick(root, table, tbody) {
    var elemIdAttr = root.getAttribute("data-dt-elem-id");
    if (!elemIdAttr) return;
    // Rebound after the guard so the closures below see a plain string.
    const elemId = elemIdAttr;
    // Grouped table: a row click ANDs the clicked row's group-key values into a
    // downstream filter (raw input -> that group's members). Otherwise the
    // legacy single-column drill (data-dt-onclick-col/idx).
    var groupCols = (table.getAttribute("data-dt-group-cols") || "")
      .split(",").filter(function (n) { return !!n; });
    var grouped = groupCols.length > 0;
    // Structured drill: rows / section headers carry their own filter keys
    // (data-dd-keys, stamped by dd_row_drill_attrs) -- no column mapping at
    // all; a click just forwards the row's keys. Rows without keys (stat
    // rows, variable-block headers -- no identity claim) are inert.
    var structured = table.getAttribute("data-dt-structured-drill") === "1";
    // Column identity, when a producer stamped it (composer's colgroup axis).
    // One map for the whole table, indexed the way cell.cellIndex counts, so
    // the stub sits at 0 and unidentified columns are null. Absent map = the
    // table has no column axis and every click means "this row", as before.
    /** @type {Array<Array<{column: string, values: string[]}>|null>|null} */
    var colMap = null;
    try {
      var cmj = table.getAttribute("data-dd-colkeys-map");
      colMap = cmj ? JSON.parse(cmj) : null;
    } catch (e) { colMap = null; }
    if (!Array.isArray(colMap)) colMap = null;
    var col = table.getAttribute("data-dt-onclick-col");
    var idx = table.getAttribute("data-dt-onclick-idx");
    if (!structured && !grouped && (!col || idx == null)) return;

    // Map each group column name to its rendered column index (header order ==
    // cell order; the stub is column 0).
    /** @type {Array<{ column: string, idx: number }>} */
    var groupIdx = [];
    if (grouped) {
      /** @type {string[]} */
      var names = [];
      table.querySelectorAll("thead th .blockr-col-name")
        .forEach(function (s) { names.push((s.textContent || "").trim()); });
      groupCols.forEach(function (gc) {
        var i = names.indexOf(gc);
        if (i >= 0) groupIdx.push({ column: gc, idx: i });
      });
    }
    var idxN = grouped ? -1 : parseInt(idx || "", 10);

    // A drill cell carries its RAW value on data-raw (emitted by the R
    // renderer): numeric cells display rounded and NA renders as an em-dash,
    // so textContent would emit a filter that matches zero rows and silently
    // empty downstream. No data-raw = an NA cell -> the click is a no-op
    // (the row also renders .dt-row-nodrill, so it never looks clickable);
    // we never emit an is.na() filter — the simplest never-empties policy.
    /** @param {Element} row @param {number} i */
    function rawAt(row, i) {
      var cell = row.children[i];
      return cell ? cell.getAttribute("data-raw") : null;
    }

    /** @param {number} i */
    function colKeysAt(i) {
      if (!colMap || i < 0 || i >= colMap.length) return null;
      var k = colMap[i];
      return (k && k.length) ? k : null;
    }
    // Is `sub` the leading part of `full`? A spanner ("Placebo") is the
    // prefix of every leaf below it ("Placebo / F", "Placebo / M"), which is
    // how a spanner click finds the cells it covers.
    /** @param {any} sub @param {any} full */
    function keysPrefixOf(sub, full) {
      if (!sub || !full || sub.length > full.length) return false;
      for (var i = 0; i < sub.length; i++) {
        if (sub[i].column !== full[i].column) return false;
        if (String(sub[i].values) !== String(full[i].values)) return false;
      }
      return true;
    }
    function clearColActive() {
      table.querySelectorAll(".dt-col-active").forEach(function (el) {
        el.classList.remove("dt-col-active");
      });
    }
    // Paint every cell of the claimed column(s). A spanner covers several
    // leaves, so the match is by key prefix rather than by index.
    /** @param {any} keys */
    // The elements a column claim covers: its header(s) and every body cell
    // under them. Split out of markCol so the transient path can paint the
    // same set with its own classes.
    /** @param {any} keys @returns {{head: Element[], cells: Element[]}} */
    function colEls(keys) {
      /** @type {{head: Element[], cells: Element[]}} */
      var out = { head: [], cells: [] };
      if (!keys || !colMap) return out;
      /** @type {number[]} */
      var idx = [];
      for (var i = 0; i < colMap.length; i++) {
        if (keysPrefixOf(keys, colMap[i])) idx.push(i);
      }
      if (!idx.length) return out;
      table.querySelectorAll("thead th[data-dd-colkeys]").forEach(function (th) {
        /** @type {any} */
        var k = null;
        try { k = JSON.parse(th.getAttribute("data-dd-colkeys") || ""); }
        catch (err) { k = null; }
        // Only what the claim COVERS, not what covers the claim. The reverse
        // test used to be here too, so clicking one leaf also lit every
        // header whose claim contains it: the spanner above it, and any
        // sibling claiming less (an "Any Grade" column claims the arm alone,
        // so it contains every grade in that arm). Three headers went active
        // for a one-cell click, and with the spanner drawn as a tile the arm
        // read as selected when it was not. Clicking the SPANNER still lights
        // its leaves -- that direction is the forward test.
        if (k && keysPrefixOf(keys, k)) out.head.push(th);
      });
      Array.prototype.slice.call(tbody.children).forEach(function (r) {
        idx.forEach(function (i) {
          var c = r.children[i];
          if (c) out.cells.push(c);
        });
      });
      return out;
    }

    /** @param {any} keys */
    function markCol(keys) {
      clearColActive();
      var e = colEls(keys);
      e.head.forEach(function (th) { th.classList.add("dt-col-active"); });
      e.cells.forEach(function (c) { c.classList.add("dt-col-active"); });
    }

    // The transient twin of markCol + the row highlight: light the row, the
    // column, and one step darker where the two cross.
    /** @param {Element | null} tr @param {any} colKeys @param {Element | null} cell */
    function flashClaim(tr, colKeys, cell) {
      /** @type {Array<{el: Element, cls: string}>} */
      var parts = [];
      if (tr) parts.push({ el: tr, cls: "dt-flash-row" });
      var e = colEls(colKeys);
      e.head.forEach(function (th) {
        parts.push({ el: th, cls: "dt-flash-colhead" });
      });
      e.cells.forEach(function (c) {
        if (c !== cell) parts.push({ el: c, cls: "dt-flash-col" });
      });
      if (cell && e.cells.indexOf(cell) !== -1) {
        parts.push({ el: cell, cls: "dt-flash-cross" });
      }
      flash(root, parts);
    }
    // The one send. Row keys and column keys travel in separate fields: the
    // row half has to be resolved against the display frame (its columns are
    // the dotted identity ones), the column half already names real source
    // columns and goes straight out as a claim.
    /** @param {any} rowKeys @param {any} colKeys */
    function sendDrill(rowKeys, colKeys) {
      if (!window.Shiny || !Shiny.setInputValue) return;
      Shiny.setInputValue(elemId + "_action",
        { action: "filter",
          filters: rowKeys || [],
          col_filters: colKeys || [],
          filter_type: "categorical",
          nonce: ++drillSeq },
        { priority: "event" });
    }

    // Header click = the column alone. Only headers the producer could name
    // carry keys, so a Total or a stats spanner is inert and looks it.
    if (structured && colMap) {
      var thead = table.querySelector("thead");
      if (thead) {
        thead.addEventListener("click", function (e) {
          var t = /** @type {Element | null} */ (e.target);
          var th = t && t.closest("th[data-dd-colkeys]");
          if (!th) return;
          // Sorting owns the leaf headers too; a click on the arrow sorts.
          if (t && t.closest(".blockr-sort-icon")) return;
          /** @type {any} */
          var keys = null;
          try { keys = JSON.parse(th.getAttribute("data-dd-colkeys") || ""); }
          catch (err) { keys = null; }
          if (!keys || !keys.length) return;
          // Transient: no toggle. A second click on the same header means
          // "send it again", never "un-drill" -- that is the target's job.
          if (transientDrill(table)) {
            sendDrill([], keys);
            flashClaim(null, keys, null);
            return;
          }
          if (th.classList.contains("dt-col-active")) {
            clearColActive();
            clearActiveRows(root);
            sendClearFilter(elemId);
            return;
          }
          clearActiveRows(root);
          markCol(keys);
          sendDrill([], keys);
        });
      }
    }

    root.classList.add("dt-clickable");

    // Hover previews what a click would claim, following the click handlers
    // below exactly: a keyed header lights the column it claims (a spanner
    // its leaves), a row label lights its row, a value cell lights its row
    // and column faintly and itself a step stronger -- or its row alone when
    // its column has no keys, since that click claims the row alone. A keyed
    // group heading's value text claims the heading row. A row with no keys
    // claims nothing and gets the plain row wash. Classes, not :hover, since
    // a column is not one element.
    var HOT = ["dt-hot-row", "dt-hot-col", "dt-hot-cell", "dt-hot-soft"];
    /** @type {Element | null} */
    var hotAt = null;
    function clearHot() {
      table.querySelectorAll("." + HOT.join(", .")).forEach(function (el) {
        el.classList.remove.apply(el.classList, HOT);
      });
    }
    /** @param {Element} el @param {string[]} cls */
    function hot(el, cls) {
      el.classList.add.apply(el.classList, cls);
    }
    function wireHover() {
      table.addEventListener("mouseover", function (e) {
        var t = /** @type {Element | null} */ (e.target);
        var at = t && (t.closest(".blockr-section-value") || t.closest("td, th"));
        if (at === hotAt) return;
        hotAt = at;
        clearHot();
        if (!at || !t) return;
        if (at.matches(".blockr-section-value")) {
          var sh = at.closest("tr.blockr-section-header");
          if (sh && sh.getAttribute("data-dd-keys")) hot(sh, ["dt-hot-row"]);
          return;
        }
        if (at.matches("th")) {
          if (!colMap || !at.hasAttribute("data-dd-colkeys")) return;
          /** @type {any} */
          var keys = null;
          try { keys = JSON.parse(at.getAttribute("data-dd-colkeys") || ""); }
          catch (err) { keys = null; }
          if (!keys || !keys.length) return;
          var ce = colEls(keys);
          ce.head.concat(ce.cells).forEach(function (c) { hot(c, ["dt-hot-col"]); });
          return;
        }
        var tr = at.closest("tr.blockr-data-row");
        if (!tr) return;
        if (!tr.getAttribute("data-dd-keys") || at.matches(".blockr-stub")) {
          hot(tr, ["dt-hot-row"]);
          return;
        }
        var ck = colKeysAt(/** @type {HTMLTableCellElement} */ (at).cellIndex);
        if (!ck) {
          hot(tr, ["dt-hot-row"]);
          return;
        }
        var e2 = colEls(ck);
        hot(tr, ["dt-hot-row", "dt-hot-soft"]);
        e2.head.concat(e2.cells).forEach(function (c) {
          if (c !== at) hot(c, ["dt-hot-col", "dt-hot-soft"]);
        });
        hot(at, ["dt-hot-cell"]);
      });
      table.addEventListener("mouseleave", function () {
        hotAt = null;
        clearHot();
      });
    }
    if (structured) wireHover();

    tbody.addEventListener("click", function (e) {
      var t = /** @type {Element | null} */ (e.target);
      if (structured) {
        // Drill target: a data row, or a section header's VALUE text (the
        // chevron / label area keeps toggling collapse -- see wireCollapse).
        var secVal = t && t.closest(".blockr-section-value");
        var srcTr = secVal
          ? secVal.closest("tr.blockr-section-header")
          : (t && t.closest("tr.blockr-data-row"));
        if (!srcTr) return;
        var keysJson = srcTr.getAttribute("data-dd-keys");
        if (!keysJson) return;                       // no identity -> no-op
        if (!window.Shiny || !Shiny.setInputValue) return;
        // Which cell was hit: the stub means the row alone, a data cell means
        // the row AND its column. With no column axis the two are the same
        // click, which is why this is backwards compatible by construction.
        var hitCell = t && t.closest("td");
        var cellKeys = (hitCell && !hitCell.classList.contains("blockr-stub"))
          ? colKeysAt(hitCell.cellIndex)
          : null;
        // Transient: send the claim, light what was clicked, keep nothing.
        // No toggle branch, so a re-click of the same cell sends again.
        if (transientDrill(table)) {
          /** @type {any} */
          var tkeys = null;
          try { tkeys = JSON.parse(keysJson); } catch (err) { tkeys = null; }
          if (!tkeys || !tkeys.length) return;
          sendDrill(tkeys, cellKeys);
          flashClaim(srcTr, cellKeys, hitCell);
          return;
        }
        var wasActive = srcTr.classList.contains("dt-row-active");
        // A click toggles-clear ONLY when it re-hits the claim that is
        // active: the very cell (this cell's column is the active column),
        // or the stub while NO column is active (a row-only claim). A stub
        // click while a cell claim is active is a NEW, wider claim -- the
        // row alone -- not a re-click; reading it as a toggle cleared the
        // whole drill and threw the cohort open. With no column axis
        // colActive is always false and this is exactly the old row toggle.
        var colActive = table.querySelector(".dt-col-active") != null;
        var sameCell = wasActive &&
          (cellKeys
            ? !!(hitCell && hitCell.classList.contains("dt-col-active"))
            : !colActive);
        if (wasActive && sameCell) {
          srcTr.classList.remove("dt-row-active");
          clearColActive();
          sendClearFilter(elemId);
          return;
        }
        /** @type {any} */
        var keys = null;
        try { keys = JSON.parse(keysJson); } catch (err) { keys = null; }
        if (!keys || !keys.length) return;
        sendDrill(keys, cellKeys);
        Array.prototype.slice.call(tbody.children).forEach(function (r) {
          r.classList.remove("dt-row-active");
        });
        markCol(cellKeys);
        srcTr.classList.add("dt-row-active");
        return;
      }
      var tr = t && t.closest("tr.blockr-data-row");
      if (!tr) return;
      var row = tr;
      if (!window.Shiny || !Shiny.setInputValue) return;
      var transient = transientDrill(table);
      // Click-to-toggle (chart parity): re-clicking the active row clears
      // the filter and the highlight. Not in transient mode, where a second
      // click on the same row means "send it again".
      if (!transient && tr.classList.contains("dt-row-active")) {
        tr.classList.remove("dt-row-active");
        sendClearFilter(elemId);
        return;
      }
      if (grouped) {
        var filters = /** @type {Array<{ column: string, value: string }>} */ ([]);
        var incomplete = false;
        groupIdx.forEach(function (g) {
          var raw = rawAt(row, g.idx);
          if (raw == null) { incomplete = true; return; }
          filters.push({ column: g.column, value: raw });
        });
        if (incomplete || !filters.length) return;   // NA group key -> no-op
        Shiny.setInputValue(elemId + "_action",
          { action: "filter", filters: filters, filter_type: "categorical",
            nonce: ++drillSeq },
          { priority: "event" });
      } else {
        var val = rawAt(tr, idxN);
        if (val == null) return;                     // NA drill cell -> no-op
        Shiny.setInputValue(elemId + "_action",
          { action: "filter", column: col, values: [val],
            filter_type: "categorical", nonce: ++drillSeq },
          { priority: "event" });
      }
      if (transient) {
        flash(root, [{ el: tr, cls: "dt-flash-row" }]);
        return;
      }
      Array.prototype.slice.call(tbody.children).forEach(function (r) {
        r.classList.remove("dt-row-active");
      });
      tr.classList.add("dt-row-active");
    });

    // Restored / re-rendered active filter -> row highlight. The renderer
    // stamps the block's current filter state on the <table>
    // (data-dt-active, JSON) at render time; match rows by RAW value so a
    // restored board shows which row drives the downstream filter.
    var activeJson = table.getAttribute("data-dt-active");
    if (activeJson) {
      /** @type {any} */
      var af = null;
      try { af = JSON.parse(activeJson); } catch (e) { af = null; }
      // The column half of a restored structured click: mark its column.
      if (af && structured && af.col_filters && af.col_filters.length) {
        markCol(af.col_filters);
      }
      if (af && structured && af.filters && af.filters.length) {
        // Structured rows/headers self-describe via data-dd-keys: the active
        // element is the one whose keys equal the restored filter set.
        /** @type {Record<string, string>} */
        var want = {};
        af.filters.forEach(function (/** @type {any} */ f) {
          want[f.column] = String(f.value);
        });
        var wantN = af.filters.length;
        tbody.querySelectorAll("tr[data-dd-keys]").forEach(function (r) {
          /** @type {any} */
          var ks = null;
          try { ks = JSON.parse(r.getAttribute("data-dd-keys") || ""); }
          catch (err) { ks = null; }
          if (ks && ks.length === wantN &&
              ks.every(function (/** @type {any} */ k) {
                return want[k.column] === String(k.value);
              })) {
            r.classList.add("dt-row-active");
          }
        });
      } else if (af) {
        Array.prototype.slice.call(tbody.children).forEach(function (r) {
          if (!r.classList.contains("blockr-data-row")) return;
          var hit = false;
          if (grouped && af.filters && af.filters.length) {
            hit = af.filters.every(function (/** @type {any} */ f) {
              var g = groupIdx.filter(function (x) {
                return x.column === f.column;
              })[0];
              return !!g && rawAt(r, g.idx) === String(f.value);
            });
          } else if (!grouped && af.column === col && af.values) {
            var rv = rawAt(r, idxN);
            hit = rv != null && af.values.indexOf(rv) !== -1;
          }
          if (hit) r.classList.add("dt-row-active");
        });
      }
    }
  }

  // The table block's layout by engine element id: the server pushes it on
  // every change ("blockr-viz-table-layout", R/table-block.R), to this engine
  // and to the groups engine (summarize-table.js reads the same object). The
  // layout switch in either gear draws from it, never from its own last
  // click: a stale highlight on the wanted segment would swallow the click.
  /** @type {Record<string, string>} */
  var layoutOf = /** @type {any} */ (window).blockrTableLayouts =
    /** @type {any} */ (window).blockrTableLayouts || {};
  // A gear that should open on the engine taking over, by element id. An
  // engine drawing for the first time has no gear yet when the push comes
  // (its chrome is a renderUI that waits for the layout); it opens the gear
  // when it builds it.
  /** @type {Record<string, boolean>} */
  var gearPending = /** @type {any} */ (window).blockrTableGearPending =
    /** @type {any} */ (window).blockrTableGearPending || {};

  // The table block's layout switch (R/table-block.R): three tiles, List,
  // Per group and Per variable, defined with the gear engine
  // (drilldown-config.js). The block's server reads the pick off this
  // engine's action input.
  /** @returns {Array<{value: string, label: string, icon?: string}>} */
  function tableLayouts() {
    return (typeof Blockr !== "undefined" && Blockr.TABLE_LAYOUTS) || [
      { value: "records", label: "List" },
      { value: "groups", label: "Per group" },
      { value: "variables", label: "Per variable" }
    ];
  }

  // What the chosen layout gives, in a line under the tiles: text parts, a
  // { strong } part a column name. `by` is the layout's grouping.
  /** @param {string} layout @param {string[]} by @returns {Array<string | {strong: string}>} */
  function layoutHint(layout, by) {
    var b = (by || []).filter(Boolean);
    var who = { strong: b.join(" and ") };
    if (layout === "records") {
      return b.length ? ["Each input row as it is, nested under each ", who, "."]
        : ["Each input row as it is."];
    }
    if (layout === "groups") {
      return b.length ? ["One row per ", who, ", each column a summary."]
        : ["One row for all rows, each column a summary."];
    }
    return b.length ? ["One row per statistic of each variable, one column per ", who, "."]
      : ["One row per statistic of each variable."];
  }

  /** @param {*} spec @param {string} current @param {string | null} elemId @param {string[]} [by] */
  function withLayoutSwitch(spec, current, elemId, by) {
    // The hint's grouping: the records layout's `by`, or the variables
    // layout's own (its settings widget).
    var hintBy = by || [];
    // The records engine also draws the variables layout; that layout's
    // settings widget sits beside it and is shown only then.
    if (elemId) {
      var blk = elemId.replace(/-records-drilldown_table_block$/, "");
      var vw = document.getElementById(blk + "-variables_wrap");
      // Before the server's first push: which wrap is on screen.
      var lay = layoutOf[elemId] ||
        (vw && getComputedStyle(vw).display !== "none" ? "variables" : current);
      current = lay;
      if (vw && lay === "variables") {
        // The table is the block's own Table 1, not a finished input.
        delete spec.badge;
        delete spec.badgeTitle;
        // One gear per block. The variables layout's own settings (what to
        // summarise, the split, the statistics) come from the summary table
        // widget; its pickers and settings move into this gear as the first
        // section, and the widget's own card (with its gear) stays hidden.
        var host = vw.querySelector(".summary-table-block-container");
        var stb = host && host._block;
        if (stb && stb._state && stb._state.by) hintBy = stb._state.by;
        if (stb && stb.grid) {
          stb.card.classList.add("stb-card--in-gear");
          spec.customSections = (spec.customSections || []).concat([{
            title: "Variables",
            render: function (/** @type {HTMLElement} */ sec) {
              var body = document.createElement("div");
              body.className = "stb-in-gear";
              body.appendChild(stb.grid);
              body.appendChild(stb._bandGrid);
              sec.appendChild(body);
            }
          }]);
        }
      }
    }
    // What a row is sits under the switch, as in the groups gear: the
    // grouping, the row names and the columns.
    spec.layoutSwitch = {
      title: "Table",
      fields: spec.mapping || [],
      value: current,
      options: tableLayouts(),
      hint: layoutHint(current, hintBy),
      onPick: function (/** @type {string} */ v) {
        if (elemId) /** @type {any} */ (window).blockrTableHold(elemId);
        sendConfig(elemId, "layout", v);
      }
    };
    spec.mapping = [];
    // A Table 1 (the variables layout) has no card form, and a finished
    // table keeps the layout it came with.
    var finished = !!spec.finished;
    TABLE_ROLES.look.options = LOOK_OPT.map(function (o) {
      var off = (o.value === "cards" && (current === "variables" || finished)) ||
        (o.value === "compact" && finished);
      return off ? {
        value: o.value, label: o.label, icon: o.icon, disabled: true,
        title: current === "variables"
          ? "The variables layout has no card form"
          : "A finished table keeps its own layout"
      } : o;
    });
    return spec;
  }

  /** @param {string | null} elemId @param {string} param @param {*} value */
  function sendConfig(elemId, param, value) {
    if (!elemId || !window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(
      elemId + "_action",
      { action: "config", param: param, value: value },
      { priority: "event" }
    );
  }

  // A chrome-affecting config edit (search / sortable / export toggles)
  // re-renders the whole container, band included; remember the band's open
  // state per element id so it survives the rebuild.
  /** @type {Record<string, boolean>} */
  var bandOpen = {};

  // Table role-spec for the shared DrilldownConfig engine. The table has no
  // chart families and no add-as-needed mapping, so a single Presentation
  // section holds everything; `drill` is a plain column picker (the column a
  // row-click filters on). Keys match the R config params, so onChange(key) ->
  // sendConfig(key, value) round-trips directly.
  // Click-through toggle options: the pill states what the CURRENT setting means
  // (not a bare On/Off) — see blockr.docs design-system/components/blockr-row.md.
  // The groups gear's words (summarize-table.js), so the trays read alike.
  var SORTABLE_OPT    = [{ value: "on", label: "Header sorting" },
                         { value: "off", label: "No header sorting" }];
  var COLLAPSIBLE_OPT = [{ value: "on", label: "Collapsible" },
                         { value: "off", label: "Not collapsible" }];
  var SEARCH_OPT      = [{ value: "on", label: "Search bar" },
                         { value: "off", label: "No search bar" }];
  var DOWNLOAD_OPT    = [{ value: "on", label: "Download" },
                         { value: "off", label: "No download" }];
  // The table block's three looks (R: TABLE_LOOKS). withLayoutSwitch()
  // greys out what the current layout cannot draw.
  var LOOK_OPT        = (typeof Blockr !== "undefined" && Blockr.TABLE_LOOKS) || [{ value: "table", label: "Table" },
                         { value: "compact", label: "Compact" },
                         { value: "cards", label: "Cards" }];
  var TABLE_ROLES = Object.assign({}, DAgg.aggRoles({ multiple: true }), {
    // "Filter on" matches the chart's drill picker label (one vocabulary).
    drill:      { label: "Filter on", kind: "column", colType: "any" },
    // Categorical identity color ("Color by") — the chart's color aesthetic
    // applied to rows (scale-map tint). "(none)" = no tint.
    color:      { label: "Color by",   kind: "column", colType: "cat" },
    // Mode vocabulary for the repeatable "Shade cells" rules (rendered by the
    // engine's _renderShadings, not as a standalone role row).
    shade_mode: { label: "Mode",       kind: "select",
                  options: ["diverging", "sequential", "bar"] },
    // How numbers print: the chart's set (chart/format.js), with the
    // table's own example numbers (no thousands separator in the cells).
    format:     { label: "Numbers",    kind: "select",
                  options: [{ value: "number", label: "Auto" },
                            { value: "0", label: "1235" },
                            { value: "1", label: "1234.6" },
                            { value: "2", label: "1234.57" },
                            { value: "3", label: "1234.568" },
                            { value: "4", label: "1234.5679" },
                            { value: "compact", label: "1.2K" },
                            { value: "percent", label: "12.3%" }] },
    unit:       { label: "Unit",       kind: "text", ph: "e.g. CHF" },
    // The records layout's rows: the stub column and the columns beside it
    // (R: `rowname`, `columns`). Empty is the default: the first column,
    // and every other column.
    rowname:    { label: "Row names", kind: "column", colType: "any",
                  placeholder: "First column" },
    columns:    { label: "Columns", kind: "columns", colType: "any",
                  placeholder: "All columns" },
    // What a missing `by` value is, in the words of the groups gear
    // (summarize-table.js): shared by the three layouts.
    missing:    { label: "Missing", kind: "segmented", options: [
                  { value: "drop", label: "Drop" }, { value: "level", label: "Show" }
                ] },
    // Display toggles (column-free): click-through pills labelled by meaning.
    // Keys match the R config params so onChange(key) round-trips to the block
    // reactiveVals; the row label names the dimension, the pill the state.
    sortable:    { label: "Header sorting", kind: "segmented", options: SORTABLE_OPT },
    collapsible: { label: "Sections", kind: "segmented", options: COLLAPSIBLE_OPT },
    search:      { label: "Search",   kind: "segmented", options: SEARCH_OPT },
    // One row, not one per format: "may the reader take this table away" is a
    // single decision, and which file they want is theirs. R offers every
    // format it can write (xlsx, html, pptx) -- a button for one, a menu for
    // several.
    download:    { label: "Download", kind: "segmented", options: DOWNLOAD_OPT },
    // The look: the table, the compact matrix (value-column titles stand
    // upright and columns size on cell content, so a wide subject x term
    // matrix fits several times more columns per viewport), or cards (one
    // card per column, one row of cards per row).
    look: { label: "Look", kind: "segmented", options: LOOK_OPT },
    // Table text (chart parity; three-tier contract, R side
    // R/title-template.R): null = auto — the title inherits the data frame's
    // label attribute; "" = explicitly none; other text renders with {...}
    // tokens resolved against the current data BY R. `autoValue` surfaces the
    // inherited auto title as the input's content, so clearing the field
    // turns the auto title off (commits "").
    title:    { label: "Title", kind: "text", ph: "e.g. AEs by {ARM}",
                autoValue: function (/** @type {any} */ cfg) {
                  return (cfg.title == null && cfg.title_auto) ? cfg.title_auto : "";
                } },
    subtitle: { label: "Subtitle", kind: "text", ph: "e.g. Treatment: {ARM}",
                autoValue: function (/** @type {any} */ cfg) {
                  return (cfg.subtitle == null && cfg.subtitle_auto) ? cfg.subtitle_auto : "";
                } },
    caption:  { label: "Caption", kind: "text", ph: "e.g. N = {n} records",
                autoValue: function (/** @type {any} */ cfg) {
                  return (cfg.caption == null && cfg.caption_auto) ? cfg.caption_auto : "";
                } }
  });
  // The summarize word, as in the groups layout and dplyr's `.by`.
  // The records layout's grouping nests the rows under a header per group.
  TABLE_ROLES.group = Object.assign({}, TABLE_ROLES.group, { label: "Nest by" });
  // The records and variables layouts' gear, in the groups gear's order:
  // Table (the layout tiles and their grouping), Presentation, Color, Titles, Drill-down,
  // Prepare script. `hasCols` is false for a structured frame (a Table 1, a
  // nested table, a finished input): its cells are not picked by column, so
  // Color leaves. A control shows only where it changes the table.
  /** @param {Record<string, any>} cfg @param {boolean} hasCols @param {string} [stubCol] */
  function tableSections(cfg, hasCols, stubCol) {
    // The records layout inside the table block: the payload carries the
    // rows' own settings (rows_state), and "Group by" is the block's `by`.
    var rows = (cfg && cfg.rows) || null;
    var byMode = !!(cfg && cfg.byMode);
    var nested = byMode && cfg.group && cfg.group.length > 0;
    var variables = !!(cfg && cfg.variablesLayout);
    // Numbers the format changes: a flat table's columns, or a nested
    // table's number cells.
    var numbers = hasCols || (nested && !!rows && !!rows.numbers);
    var pres = [];
    if (numbers) pres.push("format", "unit");
    pres.push("sortable", "look");
    // Collapsing needs header rows: a nested table or a Table 1.
    if (!hasCols) pres.push("collapsible");
    // A missing `by` value is a header of its own or dropped: nesting and
    // Table 1 only.
    if (nested || variables) pres.push("missing");
    pres.push("search", "download");
    // A finished input (gtsummary, composer, annotated frame) has no
    // columns to pick and nothing to nest: it is shown as it came.
    var finished = !hasCols && !nested && !variables;
    var spec = /** @type {Record<string, any>} */ ({
      finished: finished,
      requiredMap: [], optionalMap: [],
      mapping: (byMode && !finished) ? ["group", "rowname", "columns"] : [],
      // Drill-down: a flat table picks the column a click filters on, the
      // row's own column by default.
      drillToggle: hasCols ? "drill" : null,
      drillDefault: stubCol || "",
      drillSourceOption: !hasCols,
      colorSection: hasCols ? { colorKey: "color", shadings: true } : null,
      ctrlSection: true,
      presentation: pres,
      titles: ["title", "subtitle", "caption"]
    });
    // A finished input (gtsummary, composer, annotated frame) has no
    // pickable columns. A badge says so; the tooltip carries the why.
    if (spec.finished) {
      spec.badge = "Finished table";
      spec.badgeTitle = "The input is already a table (from gtsummary, a " +
        "table composer or an annotated data frame) and is shown as it is. " +
        "Its rows and statistics are set where it was made; titles, " +
        "drill-down and the display options apply here.";
    }
    return spec;
  }

  // Parse the gear's working state — the pickable columns and the current
  // config — off a rendered <table>'s data-attributes. Pure read (no DOM
  // writes), so the gear can re-run it against the FRESHLY rendered table
  // whenever it opens (see buildCogwheel): the chrome (and the gear with it)
  // is built once, while the inner table re-renders on every data / config
  // change with these attributes re-stamped by R (dt_table_attrs).
  /** @param {HTMLElement} table */
  function readGearState(table) {
    /** @type {VizColumn[]} */
    var cols = [];
    var colsAttr = table.getAttribute("data-dt-cols");
    if (colsAttr) {
      // Raw input schema stamped by R (dt_gear_cols_json) — correct even
      // while the table displays an aggregated projection. "[]" for
      // structured ("Table 1") frames: no pickable columns.
      try { cols = JSON.parse(colsAttr) || []; } catch (e) { cols = []; }
    } else {
      // Legacy markup (no data-dt-cols): scrape the rendered header. Numeric
      // columns (from R) drive the colType:"num" filter on the colour / bar
      // scope picker, so it only offers shadeable columns.
      /** @type {Record<string, boolean>} */
      var numSet = {};
      (table.getAttribute("data-dt-num-cols") || "").split(",")
        .forEach(function (n) { if (n) numSet[n] = true; });
      table.querySelectorAll("thead th .blockr-col-name")
        .forEach(function (s) {
          var nm = (s.textContent || "").trim();
          // Non-numeric columns are "categorical" (not "any") so the Group /
          // aggregate picker (colType "cat") offers them — the columns you can
          // aggregate over. "any" pickers (drill, row color) still match either.
          cols.push({ name: nm, type: numSet[nm] ? "numeric" : "categorical" });
        });
    }

    var onClick = table.getAttribute("data-dt-onclick-col");
    /** @type {Record<string, any>} */
    var cfg = {
      // "Group by": the block's `by`, read off the payload below.
      group: [],
      // Drill state. Raw table: the filter column (data-dt-onclick-col).
      // Grouped/aggregated table: the keys drill is a boolean — ON iff the
      // renderer wired the group-cols (data-dt-group-cols is emitted only
      // when drill is enabled); represented as 'auto' for the engine's
      // checkbox section.
      drill: (function () {
        // Structured / aggregated drill: ON iff the renderer stamped the row
        // keys. There is no column to pick, so the value is a MODE word --
        // 'auto' (hand over the clicked display subset) or 'source' (hand
        // over the records behind it). Read the stamped mode, defaulting to
        // 'auto'; returning a flat 'auto' here would clobber 'source' on the
        // next config write.
        var mode = table.getAttribute("data-dt-drill-mode") || "";
        if (table.getAttribute("data-dt-structured-drill") === "1") {
          return mode || "auto";
        }
        if ((table.getAttribute("data-dt-group-cols") || "") !== "") {
          return mode || "auto";
        }
        return (onClick && onClick !== "(none)") ? onClick : "";
      })(),
      // Identity color ("Color by") + value-encoding rules ({mode, cols}
      // JSON) — same transports as group / summaries.
      color: table.getAttribute("data-dt-color") || "",
      shadings: (function () {
        try { return JSON.parse(table.getAttribute("data-dt-shadings") || "[]"); }
        catch (e) { return []; }
      })(),
      format:     table.getAttribute("data-dt-format") || "number",
      unit:       table.getAttribute("data-dt-unit") || "",
      // Display toggles (segmented on/off). Default on, except download.
      sortable:    table.getAttribute("data-dt-sortable") || "on",
      collapsible: table.getAttribute("data-dt-collapsible") || "on",
      search:      table.getAttribute("data-dt-search") || "on",
      download:    table.getAttribute("data-dt-download") || "off",
      // The look; the payload carries it (look_state), the rotated-titles
      // attribute is the fallback for a table drawn without one.
      look: (table.getAttribute("data-dt-rotate-titles") === "on") ? "compact" : "table",
      // External-control send (beta): target value filter block + dm table,
      // and the board's candidate targets ([{value,label}] JSON, stamped by R).
      ctrl_target: table.getAttribute("data-dt-ctrl-target") || "",
      ctrl_table:  table.getAttribute("data-dt-ctrl-table") || "",
      ctrl_choices: (function () {
        try { return JSON.parse(table.getAttribute("data-dt-ctrl-choices") || "[]"); }
        catch (e) { return []; }
      })()
    };
    // Table text state rides on the PAYLOAD, not the table attributes: the
    // gear needs null (auto) vs "" (explicitly none), and an HTML attribute
    // cannot say null. Absent keys (R drops NULLs before serializing) read
    // back as null via the undefined check. While a slot's state is null,
    // its RESOLVED value is by construction the inherited auto text (the
    // data's label/subtitle/caption attribute) — that becomes the gear
    // input's clearable content (role autoValue).
    var rootEl = table.closest ? table.closest("[data-dt-elem-id]") : null;
    var eid = rootEl ? rootEl.getAttribute("data-dt-elem-id") : null;
    var stored = (eid && payloadStore[eid] && payloadStore[eid].payload) || null;
    var tl = (stored && stored.titles) || {};
    /** @param {string} slot */
    function tlState(slot) {
      var v = tl[slot + "_state"];
      cfg[slot] = (v === undefined) ? null : v;
      cfg[slot + "_auto"] = (v === undefined && tl[slot]) ? tl[slot] : "";
    }
    tlState("title");
    tlState("subtitle");
    tlState("caption");
    if (stored && stored.look_state) cfg.look = stored.look_state;
    // The table block's records layout: "Group by" is the block's `by`. The
    // payload carries its value and the input's columns, since a table
    // nested by `by` has no pickable columns of its own.
    /** @type {VizColumn[]} */
    var byCols = [];
    if (stored && stored.by_choices !== undefined) {
      cfg.byMode = true;
      cfg.group = [].concat(stored.by_state || []);
      try { byCols = JSON.parse(stored.by_choices) || []; } catch (e) { byCols = []; }
    }
    // The rows' settings (records: row names, columns, missing; variables:
    // missing), which a nested table or a Table 1 does not show as columns.
    var rs = stored && stored.rows_state;
    if (rs) {
      cfg.rows = rs;
      cfg.rowname = rs.rowname || "";
      cfg.columns = [].concat(rs.columns || []);
      if (rs.missing) cfg.missing = rs.missing;
      if (!cfg.byMode) cfg.variablesLayout = true;
    }
    // The block's prepare script: its text, the controls it declares, its
    // error, and each control's value (sv_<name>), where the engine's script
    // roles read them. Absent outside the table block, which hides the
    // section.
    var prep = stored && stored.prep;
    if (prep) {
      cfg.script = prep.script == null ? "" : prep.script;
      cfg.script_inputs = prep.script_inputs || [];
      cfg.script_error = prep.script_error || null;
      var sv = prep.script_values || {};
      Object.keys(sv).forEach(function (k) { cfg[k] = sv[k]; });
    }
    return { cols: cols, cfg: cfg, byCols: byCols };
  }

  /** @param {HTMLElement} root @param {HTMLElement} table */
  function buildCogwheel(root, table) {
    // Opt out (data-dt-gear="0"): a table whose options are FIXED by whoever
    // drew it. The gear needs somewhere to store an edit, and a renderer
    // called outside a block -- blockr.sandbox drawing a composer table in a
    // function block -- has no state slot to put one in: that block's state is
    // its code and nothing else. A gear whose every edit is swallowed on the
    // way back is worse than no gear.
    if (root.getAttribute("data-dt-gear") === "0") return;
    var elemIdAttr = root.getAttribute("data-dt-elem-id");
    if (!elemIdAttr) return;
    // Rebound after the guard so the closures below see a plain string.
    const elemId = elemIdAttr;

    // Working state, initially off the table rendered at chrome-init time.
    // `cfg` / `cols` are REASSIGNED by refreshState() below, so every engine
    // callback reads them through the closure (never captures the objects).
    var st = readGearState(table);
    var cols = st.cols;
    var cfg = st.cfg;
    var byCols = st.byCols;

    // Re-read state off the CURRENT table. The chrome — and this gear —
    // outlives table re-renders, so the state captured at init goes stale as
    // soon as the config changes anywhere else (state-restore race, AI /
    // external_ctrl edits, upstream schema changes). Called on every popover
    // OPEN (with engine.refresh()), never mid-interaction: while the popover
    // is open its own edits are already in cfg, and re-parsing then could
    // resurrect pre-round-trip server state.
    function refreshState() {
      var t = /** @type {HTMLElement | null} */ (
        root.querySelector("table.blockr-table"));
      if (!t) return;
      var s = readGearState(t);
      cols = s.cols;
      cfg = s.cfg;
      byCols = s.byCols;
    }

    // Structured ("Table 1") tables expose no pickable columns (the header is
    // section spanners, the cells are pre-formatted strings), so the column-based
    // controls (drill / colour / decimals) are no-ops and are dropped from the
    // section list below (`hasCols`). The gear still renders for the column-free
    // display toggles (sortable / collapsible / search / download) — keyed on
    // "no columns" (not a hard-coded "structured" flag) so it covers any future
    // no-column case too.
    function hasCols() { return cols.length > 0; }

    // The header, the chart's (layout E, chart.css .vz-head): titles, tools
    // (search, download), gear and settings tray, placed by state. Closed, one
    // row; open, the gear alone on top, the tray, then titles and tools.
    var header = document.createElement("div");
    header.className = "blockr-gear-header vz-head";
    var tools = document.createElement("div");
    tools.className = "vz-head__tools";
    header.appendChild(tools);
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-gear-btn";
    btn.setAttribute("data-blockr-tooltip", "Settings");
    btn.setAttribute("aria-label", "Table settings");
    btn.setAttribute("aria-haspopup", "dialog");
    btn.setAttribute("aria-expanded", "false");
    btn.innerHTML = (typeof Blockr !== "undefined" && Blockr.icons)
      ? Blockr.icons.gear : "⚙";
    header.appendChild(btn);

    // In-flow settings band (design-system pilot — blockr.ui/dev/
    // gear-panel-proposals.html, variant B). Lives inside the container right
    // below the gear header: no <body> portal, no fixed positioning, no
    // z-index/clipping concerns in dock panels. Opening pushes the table down
    // so the content being configured stays visible.
    // --beak: gear connector T1 (blockr.ui's blockr-settings-band.css) — the open band grows a
    // notch pointing at the gear that opened it.
    var pop = document.createElement("div");
    pop.className = "blockr-settings blockr-settings--beak dd-popover";
    pop.setAttribute("data-dd-pop-for", elemId);

    // Populate the popover with the shared config engine — the same
    // DrilldownConfig the chart uses. cfg keys are the R config params, so a
    // change round-trips via sendConfig(key, value).
    var DDC = /** @type {typeof VizDrilldownConfig} */ (
      (typeof Blockr !== "undefined" && Blockr.DrilldownConfig) || window.DrilldownConfig);
    var engine = new DDC({
      popoverEl: function () { return pop; },
      roles: TABLE_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols.length ? cols : byCols; },
      context: function () { return "all"; },
      currentType: function () { return cfg.transform; },
      sections: function () {
        return withLayoutSwitch(
          tableSections(cfg, hasCols(), cols.length ? cols[0].name : ""),
          "records", elemId, cfg.group
        );
      },
      sectionsForFamily: function () {
        return withLayoutSwitch(
          tableSections(cfg, hasCols(), cols.length ? cols[0].name : ""),
          "records", elemId, cfg.group
        );
      },
      // Paired-tail roles (e.g. func behind value) are rendered inside their
      // partner's row, never standalone — mirror the chart's DD_SECONDARY.
      secondary: new Set(Object.keys(TABLE_ROLES)
        .map(function (k) { return TABLE_ROLES[k].pairedWith; })
        .filter(Boolean)),
      mappingTitle: function () { return "Mapping"; },
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function () { return false; },
      drillAutoLabel: null,
      // A flat table: null, its drill is the drillToggle column picker (a
      // raw row is multi-column; the target is a genuine choice).
      drillHint: function () {
        // Structured (annotated) table: the drill target is the row's own
        // identity columns -- structurally determined, so the section is the
        // picker-less checkbox (same as the grouped table / tile).
        if (root.getAttribute("data-dt-structured") === "1") {
          return "Clicking a row filters downstream to that row's identity " +
            "(variable and value, plus its sections). The output is a pure " +
            "subset of this table \u2014 the identity columns carry the " +
            "selection, so a downstream block can read them directly.";
        }
        return null;
      },
      title: "Table settings",
      // Repeatable "Shade cells" rules, sent as one JSON string.
      // No floor row: an empty list IS "no shading".
      shadingsList: function () { return cfg.shadings || []; },
      onShadingsChange: function (/** @type {any[]} */ ss) {
        cfg.shadings = ss;
        sendConfig(elemId, "shadings", JSON.stringify(ss));
      },
      onChange: function (/** @type {string} */ key) {
        sendConfig(elemId, key, cfg[key]);
      },
      onMults: function () {},
      // Engine hook: a drill re-aim / section-uncheck must drop the emitted
      // filter (or downstream stays filtered forever with clicks inert).
      onClearFilter: function () {
        clearActiveRows(root);
        sendClearFilter(elemId);
      },
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () { return pop.classList.contains("blockr-settings--open"); },
      reopen: function () { openPop(); }
    });
    engine.render();

    // The band is in flow: opening is a class toggle, no positioning, no
    // scroll/resize listeners, and no outside-click dismissal — it is a
    // panel, not a menu; the gear is the only toggle.
    function openPop() {
      pop.classList.add("blockr-settings--open");
      header.classList.add("vz-head--open");
      btn.classList.add("blockr-gear-active");
      btn.setAttribute("aria-expanded", "true");
      bandOpen[elemId] = true;
    }
    function closePop() {
      pop.classList.remove("blockr-settings--open");
      header.classList.remove("vz-head--open");
      btn.classList.remove("blockr-gear-active");
      btn.setAttribute("aria-expanded", "false");
      bandOpen[elemId] = false;
    }
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (pop.classList.contains("blockr-settings--open")) { closePop(); return; }
      // The gear opens on CURRENT state: re-read the freshly rendered
      // table's attributes and rebuild the popover from them, so a config
      // that changed server-side while the gear was closed (restore race,
      // AI / external_ctrl, upstream schema change) is what the user sees —
      // and what the change paths (sendConfig) then write back.
      refreshState();
      engine.refresh();
      openPop();
    });

    // A layout switch (layoutOf): the groups engine taking over closes this
    // gear; this engine drawing the new layout redraws it, open if a gear in
    // the block was open. The new layout's table arrives after the switch,
    // so an open gear redraws once more when it does (root._dtAfterPayload).
    var relayout = false;
    // Until the new layout's table arrives, the tray keeps the height it had,
    // so it does not shrink to the old table's gear and grow back.
    var holdHeight = function (/** @type {number} */ h) {
      pop.style.minHeight = h ? h + "px" : "";
      if (h) setTimeout(function () { pop.style.minHeight = ""; }, 3000);
    };
    document.addEventListener("blockr-table-layout", function (e) {
      var d = /** @type {CustomEvent} */ (e).detail;
      if (!d || d.id !== elemId || !root.isConnected) return;
      relayout = false;
      delete gearPending[elemId];
      if (d.layout === "groups") { closeWhenHidden(root, closePop); return; }
      if (!d.gearOpen) return;
      var held = pop.classList.contains("blockr-settings--open") ? pop.offsetHeight : 0;
      refreshState();
      engine.refresh();
      openPop();
      holdHeight(held);
      relayout = true;
    });
    /** @type {any} */ (root)._dtAfterPayload = function () {
      if (!relayout || !pop.classList.contains("blockr-settings--open")) return;
      relayout = false;
      refreshState();
      engine.refresh();
      holdHeight(0);
    };

    root.insertBefore(header, root.firstChild);
    header.appendChild(pop);
    // The title band joins the header, on its left (applyTitles fills it).
    header.insertBefore(titlesBand(root), header.firstChild);
    // The chrome's search + download MOVE UP into the header's tools, left
    // of the gear, which keeps its top-right spot. The vacated chrome header
    // row would leave a stray padded border, so it is hidden once emptied.
    var toolbar = root.querySelector(".blockr-html-table-toolbar");
    if (toolbar) {
      tools.appendChild(toolbar);
      var hdrRow = /** @type {HTMLElement | null} */ (
        root.querySelector(".blockr-html-table-header"));
      if (hdrRow) hdrRow.style.display = "none";
    }
    // Restore the remembered open state across chrome rebuilds (the state
    // was parsed just above, so no refresh needed here), or open the gear a
    // layout switch carried over (gearPending).
    var carried = gearPending[elemId] && layoutOf[elemId] !== "groups";
    delete gearPending[elemId];
    if (bandOpen[elemId] || carried) openPop();
  }

  // Chrome wiring — once per container. The gear, search input and scroll
  // container live in the chrome, which is rendered once and never rebuilt, so
  // these never need re-wiring.
  /** @param {HTMLElement} root @param {HTMLElement} table */
  function initContainer(root, table) {
    if (root.getAttribute("data-dt-initialized") === "1") return;
    root.setAttribute("data-dt-initialized", "1");
    buildCogwheel(root, table);
    wireSearch(root);
    wireScrollShadow(root);
  }

  // Table wiring — for each freshly-rendered <table> (sort, row-click drill,
  // section collapse, all bound to elements inside the table). Only the table
  // re-renders on a filter / config edit, so this re-runs against the new
  // table; the per-table flag keeps re-scans idempotent.
  /** @param {HTMLElement} root @param {HTMLElement} table */
  function wireTable(root, table) {
    if (table.getAttribute("data-dt-table-wired") === "1") return;
    var tbody = /** @type {HTMLElement | null} */ (table.querySelector("tbody"));
    if (!tbody) return;
    table.setAttribute("data-dt-table-wired", "1");
    wireSort(root, table, tbody);
    wireClick(root, table, tbody);
    wireCollapse(root, tbody);
    // Re-apply any active search filter to the freshly rendered rows (the
    // input persists in the chrome but the new rows start unfiltered).
    // Directly, not via an input event: the listener debounces, and 150 ms
    // of unfiltered rows after every re-render would flash.
    var inp = /** @type {HTMLInputElement | null} */ (root.querySelector("input.blockr-search"));
    if (inp && inp.value.trim()) applySearch(root);
    else updateCount(root);
    // A fresh <thead> starts untransformed; place it for the current scroll.
    var page = root.querySelector(PAGE_SEL);
    if (page) followHeader(page);
  }

  // ==========================================================================
  // Data-push body (dev/table-data-push-design.md). The block server ships
  // the table body over the "blockr-viz-table-data" custom message instead
  // of rendering it through Shiny. Small tables (structured "Table 1",
  // message/error states) arrive as ready HTML and take the pre-existing
  // DOM wiring path above; flat tables arrive as a column-oriented cell
  // model, are assembled here (same markup bytes as the R renderer) and are
  // WINDOWED: above VIRT_MIN rows only the viewport's rows live in the DOM,
  // and sort/search operate on the model arrays instead of walking rows.
  // ==========================================================================

  /**
   * Client-side state for one flat cell-model table.
   * @typedef {{
   *   p: VizTablePayload,
   *   nodrill: Set<number>,
   *   activeSet: Set<number>,
   *   query: string,
   *   sortCol: number | null,
   *   sortDir: number,
   *   view: number[],
   *   searchCache: string[] | null,
   *   keys: Record<number, { num: (number|null)[], str: string[] }>,
   *   groupIdx: Array<{ column: string, idx: number }>,
   *   onclickIdx: number,
   *   onclickCol: string | null,
   *   rowH: number,
   *   measured: boolean,
   *   winStart: number,
   *   winEnd: number,
   *   cards: any
   * }} DtModel
   */

  /** @param {HTMLElement} root @returns {DtModel | null} */
  function getModel(root) {
    return /** @type {any} */ (root)._dtModel || null;
  }

  /** @param {HTMLElement} root */
  function currentQuery(root) {
    var inp = /** @type {HTMLInputElement | null} */ (
      root.querySelector("input.blockr-search"));
    return inp ? inp.value.trim().toLowerCase() : "";
  }

  // Per-row lowercase search text over the model's display strings — the
  // cell-model twin of rowText()'s textContent cache (NA cells contribute
  // nothing here; in the DOM they contribute the em-dash glyph).
  /** @param {DtModel} model */
  function ensureSearchCache(model) {
    if (model.searchCache) return;
    var cols = model.p.cols || [];
    var n = model.p.n || 0;
    var out = new Array(n);
    for (var i = 0; i < n; i++) {
      var txt = "";
      for (var j = 0; j < cols.length; j++) {
        var d = cols[j].disp[i];
        if (d != null) txt += d;
      }
      out[i] = txt.toLowerCase();
    }
    model.searchCache = out;
  }

  // Sort keys for one column, computed once from the display strings —
  // decorate-sort-undecorate over the model, with wireSort's comparator
  // semantics (displayed values: numeric when both keys parse, else locale
  // compare; stable sort keeps ties in row order).
  /** @param {DtModel} model @param {number} idx */
  function ensureKeys(model, idx) {
    var have = model.keys[idx];
    if (have) return have;
    var col = (model.p.cols || [])[idx];
    var n = model.p.n || 0;
    /** @type {{ num: (number|null)[], str: string[] }} */
    var k = { num: new Array(n), str: new Array(n) };
    for (var i = 0; i < n; i++) {
      var d = col && col.disp[i] != null ? String(col.disp[i]) : "";
      var s = d.trim();
      k.str[i] = s;
      k.num[i] = parseNum(s);
    }
    model.keys[idx] = k;
    return k;
  }

  // The display order: search filter first, then the active sort. Row
  // indexes, not DOM nodes — renderWindow() materializes a slice of this.
  /** @param {DtModel} model */
  function computeView(model) {
    var n = model.p.n || 0;
    /** @type {number[]} */
    var idxs = [];
    var i;
    if (model.query) {
      ensureSearchCache(model);
      var sc = /** @type {string[]} */ (model.searchCache);
      for (i = 0; i < n; i++) {
        if (sc[i].indexOf(model.query) !== -1) idxs.push(i);
      }
    } else {
      for (i = 0; i < n; i++) idxs.push(i);
    }
    if (model.sortCol != null && model.sortDir !== 0) {
      var keys = ensureKeys(model, model.sortCol);
      var dir = model.sortDir;
      idxs.sort(function (a, b) {
        var an = keys.num[a];
        var bn = keys.num[b];
        var cmp;
        if (an !== null && bn !== null) cmp = an - bn;
        else cmp = keys.str[a].localeCompare(keys.str[b]);
        return dir * cmp;
      });
    }
    model.view = idxs;
  }

  // One <tr> from the cell model. The markup must stay byte-compatible with
  // dt_flat_assemble_tag() (classes, data-raw, style chunks, the em-dash NA
  // cell) — everything downstream (CSS, the click handlers, the gear) reads
  // the same shapes off both.
  /** @param {DtModel} model @param {number} i */
  function rowHtml(model, i) {
    var cols = /** @type {VizTableCol[]} */ (model.p.cols);
    var cls = "blockr-data-row";
    if (model.nodrill.has(i)) cls += " dt-row-nodrill";
    if (model.activeSet.has(i)) cls += " dt-row-active";
    var out = '<tr class="' + cls + '" data-i="' + i + '">';
    for (var j = 0; j < cols.length; j++) {
      var c = cols[j];
      var d = c.disp[i];
      if (d == null) {
        out += '<td class="' + c.cls + '">&mdash;</td>';
        continue;
      }
      out += '<td class="' + c.cls + '"';
      var rv = c.raw ? c.raw[i] : null;
      if (rv != null) out += ' data-raw="' + escAttr(rv) + '"';
      var st = c.style ? c.style[i] : null;
      if (st) out += st;
      out += ">" + escHtml(d) + "</td>";
    }
    return out + "</tr>";
  }

  // The cards look: the rows of `view` (search applied) as rows of cards,
  // drawn by table-cards.js, which loads only with a table in that look and
  // may arrive after the payload (the "blockr-table-cards-ready" listener
  // below draws then). Card rows carry data-i, the row classes and nothing
  // else the table rows do not: the click, the highlight and the count read
  // them alike.
  /** @param {HTMLElement} root @param {DtModel} model */
  function renderCards(root, model) {
    var host = /** @type {HTMLElement | null} */ (
      root.querySelector(".blockr-cards-host"));
    var TC = /** @type {any} */ (window).BlockrTableCards;
    if (!host) return;
    if (!TC) {
      /** @type {any} */ (root)._tcPending = true;
      return;
    }
    /** @type {any} */ (root)._tcPending = false;
    var cards = model.cards;
    TC.render(host, cards, {
      order: model.view.filter(function (i) { return i < cards.n; }),
      row: function (/** @type {number} */ i) {
        return {
          cls: (model.nodrill.has(i) ? "dt-row-nodrill" : "") +
            (model.activeSet.has(i) ? " dt-row-active" : ""),
          attrs: ' data-i="' + i + '"'
        };
      }
    });
  }

  document.addEventListener("blockr-table-cards-ready", function () {
    document.querySelectorAll(".drilldown-table-container").forEach(function (el) {
      var root = /** @type {HTMLElement} */ (el);
      var m = getModel(root);
      if (/** @type {any} */ (root)._tcPending && m && m.cards) renderCards(root, m);
    });
  });

  /** @param {number} h @param {number} ncols */
  function spacerRow(h, ncols) {
    return '<tr class="dt-vspacer" aria-hidden="true"><td colspan="' + ncols +
      '" style="padding:0;border:0;height:' + h + 'px"></td></tr>';
  }

  // Materialize the window of `view` around the wrapper's scrollTop into the
  // tbody, between two spacer rows that keep the scrollbar geometry. Under
  // VIRT_MIN rows this renders everything (start=0, end=n, no spacers).
  /** @param {HTMLElement} root @param {DtModel} model */
  function renderWindow(root, model) {
    if (model.cards) {
      renderCards(root, model);
      return;
    }
    var table = /** @type {HTMLElement | null} */ (
      root.querySelector("table.blockr-table"));
    var tbody = table && /** @type {HTMLElement | null} */ (
      table.querySelector("tbody"));
    if (!table || !tbody) return;
    var wrapper = /** @type {HTMLElement | null} */ (
      root.querySelector(".blockr-table-wrapper"));
    var view = model.view;
    var n = view.length;
    var ncols = (model.p.cols || []).length;
    var virt = n > VIRT_MIN;
    var start = 0;
    var end = n;
    var rh = model.rowH;
    if (virt) {
      var st = wrapper ? wrapper.scrollTop : 0;
      var vh = wrapper ? wrapper.clientHeight : 600;
      var vis = Math.max(1, Math.ceil(vh / rh));
      start = Math.max(0, Math.floor(st / rh) - OVERSCAN);
      end = Math.min(n, start + vis + 2 * OVERSCAN);
    }
    var parts = [];
    if (virt && start > 0) parts.push(spacerRow(Math.round(start * rh), ncols));
    for (var k = start; k < end; k++) parts.push(rowHtml(model, view[k]));
    if (virt && end < n) {
      parts.push(spacerRow(Math.round((n - end) * rh), ncols));
    }
    tbody.innerHTML = parts.join("");
    model.winStart = start;
    model.winEnd = end;
    updateNoMatch(table, !!model.query && n === 0);
    // Measure the real row height off the first rendered window, once per
    // payload: the spacer heights (scrollbar geometry) use it. A wrong
    // estimate only mis-sizes the scrollbar — the window itself always
    // follows scrollTop.
    if (virt && !model.measured) {
      model.measured = true;
      var r0 = /** @type {HTMLElement | null} */ (
        tbody.querySelector("tr.blockr-data-row"));
      if (r0 && r0.offsetHeight > 0 && Math.abs(r0.offsetHeight - rh) > 0.5) {
        model.rowH = r0.offsetHeight;
        renderWindow(root, model);
      }
    }
  }

  // Scroll -> re-window, wired ONCE per container (the wrapper lives in the
  // chrome and survives payload updates; a chrome rebuild mints a new
  // container element and re-enters here). rAF-coalesced, and inert while
  // the model is small enough to be fully rendered.
  /** @param {HTMLElement} root */
  function wireModelScroll(root) {
    if (root.getAttribute("data-dt-vscroll") === "1") return;
    root.setAttribute("data-dt-vscroll", "1");
    var wrapper = /** @type {HTMLElement | null} */ (
      root.querySelector(".blockr-table-wrapper"));
    if (!wrapper) return;
    // const (not var) so the post-guard non-null narrowing holds below.
    const sc = wrapper;
    var ticking = false;
    sc.addEventListener("scroll", function () {
      if (ticking) return;
      ticking = true;
      window.requestAnimationFrame(function () {
        ticking = false;
        var m = getModel(root);
        if (!m || m.cards || m.view.length <= VIRT_MIN) return;
        // Re-render once scrolling nears the rendered window's edge.
        var first = Math.floor(sc.scrollTop / m.rowH);
        var last = first + Math.ceil(sc.clientHeight / m.rowH);
        if ((first - m.winStart < OVERSCAN / 2 && m.winStart > 0) ||
            (m.winEnd - last < OVERSCAN / 2 && m.winEnd < m.view.length)) {
          renderWindow(root, m);
        }
      });
    }, { passive: true });
  }

  /** @param {HTMLElement} root @param {DtModel} model */
  function repaintActive(root, model) {
    root.querySelectorAll("tbody tr[data-i], .tc-row[data-i]").forEach(function (r) {
      var i = parseInt(r.getAttribute("data-i") || "", 10);
      if (!isNaN(i) && model.activeSet.has(i)) {
        r.classList.add("dt-row-active");
      } else {
        r.classList.remove("dt-row-active");
      }
    });
  }

  // Restored / re-built active filter -> model row set (the cell-model twin
  // of wireClick's data-dt-active restore walk): match rows by their RAW
  // values so a restored board highlights the row that drives the
  // downstream filter — applied at every window render, so it survives
  // scrolling, search and sort.
  /** @param {HTMLElement} table @param {DtModel} model */
  function seedActive(table, model) {
    var activeJson = table.getAttribute("data-dt-active");
    if (!activeJson) return;
    /** @type {any} */
    var af = null;
    try { af = JSON.parse(activeJson); } catch (e) { af = null; }
    if (!af) return;
    var cols = model.p.cols || [];
    var n = model.p.n || 0;
    var i;
    if (af.filters && af.filters.length && model.groupIdx.length) {
      for (i = 0; i < n; i++) {
        var row = i;
        var hit = af.filters.every(function (/** @type {any} */ f) {
          var g = model.groupIdx.filter(function (x) {
            return x.column === f.column;
          })[0];
          var raw = g && cols[g.idx] ? cols[g.idx].raw : null;
          var rv = raw ? raw[row] : null;
          return rv != null && rv === String(f.value);
        });
        if (hit) model.activeSet.add(i);
      }
    } else if (af.column && af.values && af.column === model.onclickCol &&
               !isNaN(model.onclickIdx) && cols[model.onclickIdx] &&
               cols[model.onclickIdx].raw) {
      var rawCol = /** @type {(string|null)[]} */ (cols[model.onclickIdx].raw);
      for (i = 0; i < n; i++) {
        var v = rawCol[i];
        if (v != null && af.values.indexOf(v) !== -1) model.activeSet.add(i);
      }
    }
  }

  // Header sort for cell-model tables: same tri-state cycle and indicators
  // as wireSort, but the order lives in the model and only the window
  // re-renders — no data-dt-o stamping, no DOM reads.
  /** @param {HTMLElement} root @param {HTMLElement} table @param {DtModel} model */
  function wireFlatSort(root, table, model) {
    table.querySelectorAll("th.blockr-sortable").forEach(function (th) {
      th.setAttribute("tabindex", "0");
      th.setAttribute("aria-sort", "none");
      /** @param {Event} e */
      var activate = function (e) {
        e.stopPropagation();
        var idx = parseInt(th.getAttribute("data-col-index") || "", 10);
        if (isNaN(idx)) return;
        if (model.sortCol === idx) {
          model.sortDir =
            model.sortDir === 1 ? -1 : (model.sortDir === -1 ? 0 : 1);
        } else {
          model.sortCol = idx;
          model.sortDir = 1;
        }
        if (model.sortDir === 0) model.sortCol = null;
        updateSortIndicators(root, model.sortCol, model.sortDir);
        computeView(model);
        renderWindow(root, model);
      };
      th.addEventListener("click", activate);
      th.addEventListener("keydown", function (e) {
        var k = /** @type {KeyboardEvent} */ (e).key;
        if (k !== "Enter" && k !== " ") return;
        e.preventDefault();   // Space must sort, not scroll the panel
        activate(e);
      });
    });
  }

  // Row-click drill for cell-model tables: wireClick's contract (toggle,
  // grouped keys, NA no-ops, the `_action` payload shapes) with the raw
  // values read from the model instead of cell attributes.
  /** @param {HTMLElement} root @param {HTMLElement} table @param {DtModel} model */
  function wireFlatClick(root, table, model) {
    var elemIdAttr = root.getAttribute("data-dt-elem-id");
    if (!elemIdAttr) return;
    // Rebound after the guard so the closures below see a plain string.
    const elemId = elemIdAttr;
    // The rows a click lands on: the table's, or the cards' (same data-i).
    var tbody = /** @type {HTMLElement | null} */ (model.cards ?
      root.querySelector(".blockr-cards-host") : table.querySelector("tbody"));
    if (!tbody) return;
    var grouped = model.groupIdx.length > 0;
    var single = !!model.onclickCol && !isNaN(model.onclickIdx);
    if (!grouped && !single) return;
    var cols = /** @type {VizTableCol[]} */ (model.p.cols);

    tbody.addEventListener("click", function (e) {
      var t = /** @type {Element | null} */ (e.target);
      var tr = t && t.closest("tr[data-i], .tc-row[data-i]");
      if (!tr) return;
      if (!window.Shiny || !Shiny.setInputValue) return;
      var i = parseInt(tr.getAttribute("data-i") || "", 10);
      if (isNaN(i)) return;
      var transient = transientDrill(table);
      // Click-to-toggle (chart parity): re-clicking the active row clears
      // the filter and the highlight. Not in transient mode, where a second
      // click on the same row means "send it again".
      if (!transient && tr.classList.contains("dt-row-active")) {
        model.activeSet.clear();
        sendClearFilter(elemId);
        repaintActive(root, model);
        return;
      }
      if (grouped) {
        /** @type {Array<{ column: string, value: string }>} */
        var filters = [];
        var incomplete = false;
        model.groupIdx.forEach(function (g) {
          var raw = cols[g.idx] ? cols[g.idx].raw : null;
          var rv = raw ? raw[i] : null;
          if (rv == null) { incomplete = true; return; }
          filters.push({ column: g.column, value: rv });
        });
        if (incomplete || !filters.length) return;   // NA group key -> no-op
        Shiny.setInputValue(elemId + "_action",
          { action: "filter", filters: filters, filter_type: "categorical",
            nonce: ++drillSeq },
          { priority: "event" });
      } else {
        var rawCol = cols[model.onclickIdx] ? cols[model.onclickIdx].raw : null;
        var val = rawCol ? rawCol[i] : null;
        if (val == null) return;                     // NA drill cell -> no-op
        Shiny.setInputValue(elemId + "_action",
          { action: "filter", column: model.onclickCol, values: [val],
            filter_type: "categorical", nonce: ++drillSeq },
          { priority: "event" });
      }
      // Transient: the flash is on the DOM row, never in the model. A windowed
      // table rebuilds its rows on scroll, sort and search, and a signal that
      // survived that would be the latch this mode exists to remove.
      if (transient) {
        flash(root, [{ el: tr, cls: "dt-flash-row" }]);
        return;
      }
      model.activeSet.clear();
      model.activeSet.add(i);
      repaintActive(root, model);
    });
  }

  // Title / caption bands (chart parity). Filled from the RESOLVED text on
  // the payload (R substitutes the {...} tokens and applies the auto tier);
  // ensured lazily around the scroll wrapper so the chrome — which outlives
  // payloads — needs no rebuild. Hidden while empty: a title-less table is
  // pixel-identical to before. textContent, never innerHTML.
  // The title and subtitle band: the container's own, or a new one, hidden
  // until applyTitles has text for it.
  /** @param {HTMLElement} root @returns {HTMLElement} */
  function titlesBand(root) {
    var band = /** @type {HTMLElement | null} */ (
      root.querySelector(".dd-table-titles"));
    if (band) { band.classList.add("vz-head__titles"); return band; }
    band = document.createElement("div");
    band.className = "dd-table-titles vz-head__titles";
    band.style.display = "none";
    var te = document.createElement("div");
    te.className = "dd-table-title";
    var se = document.createElement("div");
    se.className = "dd-table-subtitle";
    band.appendChild(te);
    band.appendChild(se);
    return band;
  }

  // A layout switch shows the other engine's container (the server's
  // layout_css). The first time, that container is still empty: the server
  // draws an engine's table only once its output is on screen. Until the new
  // container has its table, and its tray if a gear was open, the old one
  // stays on screen and the new one is laid out out of sight (hidden, out of
  // the flow, so Shiny still renders into it); the block never shows
  // nothing. Inline !important outranks the server's rule; it comes off when
  // the new view is ready, or after two seconds.
  // The element that holds a block's three engine containers.
  /** @param {string[]} ids @returns {HTMLElement | null} */
  function engineBox(ids) {
    for (var i = 0; i < ids.length; i++) {
      var sel = (window.CSS && CSS.escape) ? CSS.escape(ids[i]) : ids[i];
      var c = document.querySelector('[data-dt-elem-id="' + sel + '"], [data-summarize-elem-id="' + sel + '"]');
      var w = c && c.closest(".blockr-table-engine");
      if (w) return /** @type {HTMLElement | null} */ (w.parentElement);
    }
    return null;
  }

  // While the engines swap, the block can be shorter for a moment (the
  // records engine draws the new layout's table before the layout message,
  // a tray is redrawn), and a scrolled panel would clamp its scroll and keep
  // it there. From the click on a layout tile, the block keeps its height
  // until the server is idle and its content has not changed for 300 ms, at
  // most five seconds. One hold per block: a second call extends it.
  /** @param {HTMLElement} box */
  function holdBlockHeight(box) {
    var held = /** @type {any} */ (box)._dtHold;
    if (held) { held.extend(); return; }
    var h0 = box.offsetHeight;
    if (!h0) return;
    var prev = box.style.minHeight;
    box.style.minHeight = h0 + "px";
    var t0 = Date.now(), since = t0, last = -1;
    var natural = function () {
      var sum = 0;
      Array.prototype.forEach.call(box.children, function (/** @type {HTMLElement} */ c) {
        if (c.offsetParent) sum += c.offsetHeight;
      });
      return sum;
    };
    /** @type {any} */ (box)._dtHold = {
      extend: function () { t0 = Date.now(); since = t0; }
    };
    var check = function () {
      var n = natural(), now = Date.now();
      if (n !== last) { last = n; since = now; }
      if (document.documentElement.classList.contains("shiny-busy")) since = now;
      if ((now - since > 300 && now - t0 > 300) || now - t0 > 5000) {
        box.style.minHeight = prev;
        delete /** @type {any} */ (box)._dtHold;
        return;
      }
      setTimeout(check, 50);
    };
    setTimeout(check, 50);
  }
  // For the groups engine's switch (summarize-table.js).
  /** @type {any} */ (window).blockrTableHold = function (/** @type {string} */ id) {
    var box = engineBox([id]);
    if (box) holdBlockHeight(box);
  };

  /** @param {string[]} ids @param {string} layout @param {boolean} open */
  function bridgeSwitch(ids, layout, open) {
    var box = engineBox(ids);
    if (!box) return;
    var wraps = /** @type {HTMLElement[]} */ (Array.prototype.slice.call(
      box.querySelectorAll(":scope > .blockr-table-engine")));
    var kind = function (/** @type {HTMLElement} */ w) {
      var m = /(records|groups|variables)_wrap$/.exec(w.id);
      return m ? m[1] : "";
    };
    var shown = wraps.filter(function (w) { return !!w.offsetParent; });
    if (!shown.length) return;
    var engine = wraps.filter(function (w) {
      return kind(w) === (layout === "groups" ? "groups" : "records");
    })[0];
    if (!engine) return;
    var ready = function () {
      return !!engine.querySelector("table") &&
        (!open || !!engine.querySelector(".blockr-settings--open"));
    };
    if (ready()) return;
    var hold = box;
    var pos = hold.style.position;
    if (getComputedStyle(hold).position === "static") hold.style.position = "relative";
    var off = ["position", "visibility", "top", "left", "right", "pointer-events"];
    wraps.forEach(function (w) {
      if (shown.indexOf(w) >= 0) {
        w.style.setProperty("display", "block", "important");
      } else if (w === engine) {
        w.style.setProperty("display", "block", "important");
        w.style.setProperty("position", "absolute");
        w.style.setProperty("visibility", "hidden");
        w.style.setProperty("top", "0");
        w.style.setProperty("left", "0");
        w.style.setProperty("right", "0");
        w.style.setProperty("pointer-events", "none");
      } else {
        w.style.setProperty("display", "none", "important");
      }
    });
    var t0 = Date.now();
    var check = function () {
      if (ready() || Date.now() - t0 > 2000) {
        wraps.forEach(function (w) {
          w.style.removeProperty("display");
          off.forEach(function (k) { w.style.removeProperty(k); });
        });
        hold.style.position = pos;
        return;
      }
      setTimeout(check, 16);
    };
    setTimeout(check, 16);
  }

  // A layout switch hands the gear to the other engine. The server shows
  // the other engine's container a moment after the layout message, so the
  // tray closes only once its own container is off screen: closed earlier,
  // the table would jump up for a few frames and back down. Gives up after
  // three seconds.
  /** @param {HTMLElement} root @param {() => void} close */
  function closeWhenHidden(root, close) {
    var t0 = Date.now();
    var check = function () {
      if (!root.isConnected || !root.offsetParent || Date.now() - t0 > 3000) {
        close();
        return;
      }
      // A timer, not requestAnimationFrame: a tab in the background runs
      // no frames, and the tray would stay open behind the scenes.
      setTimeout(check, 16);
    };
    check();
  }
  /** @type {any} */ (window).blockrCloseWhenHidden = closeWhenHidden;

  /** @param {HTMLElement} root @param {VizTablePayload} p */
  function applyTitles(root, p) {
    // No `titles` field at all: the sender paints its own band (the composer
    // block's sentence, rendered by R as its own output inside this
    // container). Leave it alone; `titles: {}` still means "hide the band".
    if (/** @type {any} */ (p).titles == null) return;
    var wrap = /** @type {HTMLElement | null} */ (
      root.querySelector(".blockr-table-wrapper"));
    if (!wrap || !wrap.parentNode) return;
    var tl = /** @type {any} */ (p).titles || {};
    var band = titlesBand(root);
    if (!band.parentNode) wrap.parentNode.insertBefore(band, wrap);
    var capEl = /** @type {HTMLElement | null} */ (
      root.querySelector(".dd-table-caption"));
    if (!capEl) {
      capEl = document.createElement("div");
      capEl.className = "dd-table-caption";
      wrap.parentNode.insertBefore(capEl, wrap.nextSibling);
    }
    var t = tl.title || "";
    var s = tl.subtitle || "";
    var cap = tl.caption || "";
    var tEl = /** @type {HTMLElement} */ (band.children[0]);
    var sEl = /** @type {HTMLElement} */ (band.children[1]);
    tEl.textContent = t;
    sEl.textContent = s;
    tEl.style.display = t ? "" : "none";
    sEl.style.display = s ? "" : "none";
    band.style.display = (t || s) ? "" : "none";
    capEl.textContent = cap;
    capEl.style.display = cap ? "" : "none";
  }

  // Apply one payload to its container: inject, wire, render. Runs on every
  // payload arrival AND whenever a payload-less container turns up with a
  // stored payload (chrome after payload, dock re-mounts — see wireRoot).
  /** @param {HTMLElement} root @param {VizTablePayload} p */
  function applyPayload(root, p) {
    drawPayload(root, p);
    var after = /** @type {any} */ (root)._dtAfterPayload;
    if (after) after();
  }

  /** @param {HTMLElement} root @param {VizTablePayload} p */
  function drawPayload(root, p) {
    var slot = /** @type {HTMLElement | null} */ (
      root.querySelector(".dt-table-slot") ||
      root.querySelector(".blockr-table-wrapper"));
    if (!slot) return;
    applyTitles(root, p);
    root.setAttribute("data-dt-body-applied", "1");
    // Structured promotion is re-derived per payload: upstream can flip a
    // block between structured and flat.
    root.removeAttribute("data-dt-structured");
    root.classList.remove("drilldown-table-structured");
    /** @type {any} */ (root)._dtModel = null;

    if (p.kind === "html") {
      // Structured "Table 1" / message / error markup, rendered whole by R:
      // inject and take the pre-existing DOM wiring path.
      slot.innerHTML = p.html || "";
      wireRoot(root);
      updateCount(root);
      return;
    }
    if (!p.head || !p.cols) return;
    // The cards look (R/table-cards.R): the table stays, empty and hidden,
    // for the gear and the drill to read their state off; the cards draw
    // beside it from the same rows.
    var cards = p.look === "cards" && /** @type {any} */ (p).cards;
    slot.innerHTML = p.head + (cards ? '<div class="blockr-cards-host"></div>' : "");
    var wrapEl = root.querySelector(".blockr-table-wrapper");
    if (wrapEl) wrapEl.classList.toggle("tc-on", !!cards);
    var table = /** @type {HTMLElement | null} */ (
      slot.querySelector("table.blockr-table"));
    if (!table) return;
    // The cell model owns this table: block the DOM wiring path.
    table.setAttribute("data-dt-table-wired", "1");
    initContainer(root, table);

    /** @type {DtModel} */
    var model = {
      p: p,
      nodrill: new Set(p.nodrill || []),
      activeSet: new Set(),
      query: currentQuery(root),
      sortCol: null,
      sortDir: 0,
      view: [],
      searchCache: null,
      keys: {},
      groupIdx: [],
      onclickIdx: parseInt(
        table.getAttribute("data-dt-onclick-idx") || "", 10),
      onclickCol: table.getAttribute("data-dt-onclick-col"),
      rowH: 30,
      measured: false,
      winStart: 0,
      winEnd: 0,
      cards: cards || null
    };
    /** @type {any} */ (root)._dtModel = model;

    // Grouped drill: map each group column name to its rendered column
    // index (header order == cell order; the stub is column 0) — the same
    // lookup the DOM path does in wireClick.
    var groupCols = (table.getAttribute("data-dt-group-cols") || "")
      .split(",").filter(function (n) { return !!n; });
    if (groupCols.length) {
      /** @type {string[]} */
      var names = [];
      table.querySelectorAll("thead th .blockr-col-name")
        .forEach(function (s) { names.push((s.textContent || "").trim()); });
      groupCols.forEach(function (gc) {
        var gi = names.indexOf(gc);
        if (gi >= 0) model.groupIdx.push({ column: gc, idx: gi });
      });
    }
    seedActive(table, model);
    root.classList.toggle(
      "dt-clickable",
      model.groupIdx.length > 0 ||
        (!!model.onclickCol && !isNaN(model.onclickIdx))
    );
    wireFlatSort(root, table, model);
    wireFlatClick(root, table, model);
    wireModelScroll(root);
    computeView(model);
    renderWindow(root, model);
    updateCount(root);
  }

  /** @param {HTMLElement} root */
  function wireRoot(root) {
    var table = /** @type {HTMLElement | null} */ (root.querySelector("table.blockr-table"));
    if (!table) {
      // A data-push container whose payload arrived before the chrome, or a
      // re-created chrome (dock re-mount, search-toggle rebuild): render
      // from the payload store, with no R round trip. The applied-flag
      // keeps the html-kind error/empty case (which leaves no <table>) from
      // re-entering through the wireRoot call inside applyPayload.
      var eid = root.getAttribute("data-dt-elem-id");
      var stored = eid ? payloadStore[eid] : null;
      if (stored && !root.hasAttribute("data-dt-body-applied")) {
        applyPayload(root, stored.payload);
      }
      return;
    }
    // The chrome can be rendered before the data is known (the block server
    // paints it immediately so the control section does not wait on the
    // pipeline), so it may lack the structured class. The <table> always
    // carries the verdict — promote it before anything reads the container.
    if (table.getAttribute("data-dt-structured") === "1" &&
        root.getAttribute("data-dt-structured") !== "1") {
      root.setAttribute("data-dt-structured", "1");
      root.classList.add("drilldown-table-structured");
    }
    initContainer(root, table);
    wireTable(root, table);
  }

  /** @param {Document | Element} [ctx] */
  function scan(ctx) {
    var nodes = (ctx || document)
      .querySelectorAll(".drilldown-table-container");
    Array.prototype.forEach.call(nodes, wireRoot);
  }

  // Wire exactly the part of the DOM an event touched. A table (re)renders
  // *inside* its container (the renderUI output sits within the chrome), so
  // the enclosing container is found via closest(); a subtree that itself
  // contains containers (initial UI, insertUI) is scanned scoped. Everything
  // else costs one querySelectorAll over a small unrelated subtree.
  /** @param {EventTarget | Element | null | undefined} el */
  function scanAround(el) {
    var e = /** @type {Element | null} */ (
      el && /** @type {Node} */ (el).nodeType === 1 ? el : null);
    if (!e) { scan(); return; }
    var root = e.closest(".drilldown-table-container");
    if (root) wireRoot(/** @type {HTMLElement} */ (root));
    else scan(e);
  }

  // Fallback path only (see below): queue the relevant added nodes and wire
  // just those subtrees, coalesced to one flush per animation frame. Wiring
  // is idempotent (data-attribute guards), so overlap with the shiny:value
  // path costs an attribute check, never a rewire.
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

  var SCAN_SEL = ".drilldown-table-container, table.blockr-table";

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { scan(); });
  } else {
    scan();
  }

  // R -> JS: the block body (see applyPayload). The payload travels as ONE
  // pre-serialized JSON string; parses are cached by rev, so a re-send of an
  // unchanged rev (session reconnect) skips JSON.parse. A message with no
  // container yet just parks in the store — wireRoot delivers it when the
  // chrome turns up (no timers, no delivery window that can expire).
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    // R -> JS: the table block's layout, to both engines (see layoutOf). A
    // gear open in either stays open across the switch, on the engine that
    // draws the new layout; the engines act on the "blockr-table-layout"
    // event.
    Shiny.addCustomMessageHandler("blockr-viz-table-layout",
      function (/** @type {any} */ msg) {
        /** @type {string[]} */
        var ids = [].concat(msg.ids || []);
        var open = ids.some(function (id) {
          var sel = (window.CSS && CSS.escape) ? CSS.escape(id) : id;
          var pop = document.querySelector('[data-dd-pop-for="' + sel + '"]');
          return !!pop && pop.classList.contains("blockr-settings--open");
        });
        var box = engineBox(ids);
        if (box) holdBlockHeight(box);
        bridgeSwitch(ids, msg.layout, open);
        ids.forEach(function (id) {
          layoutOf[id] = msg.layout;
          gearPending[id] = open;
          document.dispatchEvent(new CustomEvent("blockr-table-layout", {
            detail: { id: id, layout: msg.layout, gearOpen: open }
          }));
        });
      });

    Shiny.addCustomMessageHandler("blockr-viz-table-data",
      function (/** @type {any} */ msg) {
        var entry = payloadStore[msg.id];
        /** @type {VizTablePayload | null} */
        var payload = null;
        if (entry && entry.rev === msg.rev) {
          payload = entry.payload;
        } else {
          try { payload = JSON.parse(msg.payload); } catch (err) { payload = null; }
          if (!payload) return;
          payloadStore[msg.id] = { rev: msg.rev, payload: payload };
        }
        var eid = (window.CSS && CSS.escape)
          ? CSS.escape(msg.id)
          : String(msg.id).replace(/"/g, '\\"');
        var root = /** @type {HTMLElement | null} */ (document.querySelector(
          '.drilldown-table-container[data-dt-elem-id="' + eid + '"]'));
        if (root) applyPayload(root, payload);
      });
  }

  // Primary wiring path: shiny:value / shiny:bound fire on the output element
  // that just (re)rendered — wire only that subtree. The setTimeout defers
  // past Shiny's DOM swap (shiny:value fires before the new HTML is applied).
  if (typeof window.jQuery === "function") {
    jQuery(document).on("shiny:value shiny:bound", function (/** @type {any} */ e) {
      var t = e.target;
      setTimeout(function () { scanAround(t); }, 0);
    });
  }

  // Fallback for tables that enter the DOM outside a Shiny render event.
  // Gated on relevance so tooltip/relayout churn never wakes it: only a
  // mutation that adds a container (or a table inside one) queues its node.
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
