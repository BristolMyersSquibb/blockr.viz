// @ts-check
(function () {
  /** @param {string | number | null | undefined} s */
  function parseNum(s) {
    if (s == null) return null;
    var m = String(s).match(/-?\d[\d,]*(\.\d+)?/);
    if (!m) return null;
    return parseFloat(m[0].replace(/,/g, ""));
  }

  // Column widths are computed server-side (blockr.ui::column_widths_px,
  // carried by each table's <colgroup>) and the table renders with
  // table-layout: fixed from the first paint, so nothing in this script
  // measures or mutates layout: sort-arrow reveal, section collapse and
  // search filtering cannot reflow the columns by construction. (The old
  // lockTableWidths() measured the DOM lazily before the first reflow -
  // it skipped hidden tables and never covered the search path at all, so
  // filtering reflowed columns live while typing.)

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

    /** @param {Element} a @param {Element} b @param {number} idx @param {number} dir */
    function cmpRows(a, b, idx, dir) {
      var av = a.children[idx] ? (a.children[idx].textContent || "").trim() : "";
      var bv = b.children[idx] ? (b.children[idx].textContent || "").trim() : "";
      var an = parseNum(av), bn = parseNum(bv), cmp;
      if (an !== null && bn !== null) cmp = an - bn;
      else cmp = av.localeCompare(bv);
      return dir * cmp;
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
        g.rows.sort(function (a, b) { return cmpRows(a, b, idx, dir); });
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
      var rows = Array.prototype.slice.call(tbody.children);
      rows.sort(function (a, b) { return cmpRows(a, b, idx, dir); });
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
      root.querySelectorAll("th.blockr-sortable .blockr-sort-icon")
        .forEach(function (ic) {
          ic.classList.remove("blockr-sort-icon-asc", "blockr-sort-icon-desc");
        });
      if (state.dir === 0) { state.col = null; reset(); return; }
      var th = root.querySelector(
        'th.blockr-sortable[data-col-index="' + idx + '"]'
      );
      if (th) {
        var ic = th.querySelector(".blockr-sort-icon");
        if (ic) {
          ic.classList.add(
            state.dir === 1 ? "blockr-sort-icon-asc" : "blockr-sort-icon-desc"
          );
        }
      }
      if (structured) sortStructured(idx, state.dir);
      else sortFlat(idx, state.dir);
    }

    root.querySelectorAll("th.blockr-sortable").forEach(function (th) {
      th.addEventListener("click", function (e) {
        e.stopPropagation();
        var idx = parseInt(th.getAttribute("data-col-index") || "", 10);
        if (!isNaN(idx)) sortBy(idx);
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
    root.querySelectorAll("tr.blockr-section-header").forEach(function (h) {
      // The Direction-01 group label is a <button> inside the row; its click
      // bubbles to this row-level handler, so a single listener covers both
      // the button and any bare-cell click.
      h.addEventListener("click", function (ev) {
        ev.stopPropagation();
        h.classList.toggle("collapsed");
        syncAria(h);
        recompute();
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
        });
      }
    );
    if (root.getAttribute("data-initial-expanded") === "0") {
      root.querySelectorAll("tr.blockr-section-header").forEach(function (h) {
        h.classList.add("collapsed");
        syncAria(h);
      });
      recompute();
    }
  }

  // Sticky-header scroll shadow: toggle `.scrolled` on the scroll container
  // once it's scrolled away from the top, so the sticky header detaches with
  // a soft shadow rather than a hard line (Direction-01).
  /** @param {HTMLElement} root */
  function wireScrollShadow(root) {
    // const (not var) so the post-guard non-null narrowing holds inside onScroll.
    const sc = root.querySelector(".blockr-table-wrapper");
    if (!sc) return;
    // Arrow defined after the guard so the non-null narrowing of `sc` is captured.
    const onScroll = () => {
      if (sc.scrollTop > 2) sc.classList.add("scrolled");
      else sc.classList.remove("scrolled");
    };
    sc.addEventListener("scroll", onScroll, { passive: true });
    onScroll();
  }

  /** @param {HTMLElement} root */
  function wireSearch(root) {
    var inp = /** @type {HTMLInputElement | null} */ (root.querySelector("input.blockr-search"));
    if (!inp || inp.getAttribute("data-dt-search-wired") === "1") return;
    inp.setAttribute("data-dt-search-wired", "1");
    inp.addEventListener("input", function () {
      // The <table>/<tbody> re-renders on each filter while this input (in the
      // chrome) persists, so query it live rather than closing over it.
      var table = root.querySelector("table.blockr-table");
      var tbody = table && table.querySelector("tbody");
      if (!tbody) return;
      var structured = root.getAttribute("data-dt-structured") === "1";
      var q = /** @type {HTMLInputElement} */ (inp).value.trim().toLowerCase();
      var rows = Array.prototype.slice.call(tbody.children);
      rows.forEach(function (r) {
        if (!r.classList.contains("blockr-data-row")) return;
        if (!q || r.textContent.toLowerCase().indexOf(q) !== -1) {
          r.classList.remove("blockr-hidden-search");
        } else {
          r.classList.add("blockr-hidden-search");
        }
      });
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
    });
  }

  /** @param {HTMLElement} root @param {HTMLElement} table @param {HTMLElement} tbody */
  function wireClick(root, table, tbody) {
    var elemId = root.getAttribute("data-dt-elem-id");
    var col = table.getAttribute("data-dt-onclick-col");
    var idx = table.getAttribute("data-dt-onclick-idx");
    if (!elemId || !col || idx == null) return;
    var idxN = parseInt(idx, 10);
    root.classList.add("dt-clickable");
    tbody.addEventListener("click", function (e) {
      var t = /** @type {Element | null} */ (e.target);
      var tr = t && t.closest("tr.blockr-data-row");
      if (!tr || !tr.children[idxN]) return;
      var val = (tr.children[idxN].textContent || "").trim();
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(
          elemId + "_action",
          {
            action: "filter",
            column: col,
            values: [val],
            filter_type: "categorical"
          },
          { priority: "event" }
        );
      }
      Array.prototype.slice.call(tbody.children).forEach(function (r) {
        r.classList.remove("dt-row-active");
      });
      tr.classList.add("dt-row-active");
    });
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

  // Table role-spec for the shared DrilldownConfig engine. The table has no
  // chart families and no add-as-needed mapping, so a single Presentation
  // section holds everything; `drill` is a plain column picker (the column a
  // row-click filters on). Keys match the R config params, so onChange(key) ->
  // sendConfig(key, value) round-trips directly.
  // Click-through toggle options: the pill states what the CURRENT setting means
  // (not a bare On/Off) — see blockr.docs design-system/components/blockr-row.md.
  var SORTABLE_OPT    = [{ value: "on", label: "Sortable" },
                         { value: "off", label: "Not sortable" }];
  var COLLAPSIBLE_OPT = [{ value: "on", label: "Collapsible" },
                         { value: "off", label: "Not collapsible" }];
  var SEARCH_OPT      = [{ value: "on", label: "Search bar" },
                         { value: "off", label: "No search bar" }];
  var EXPORT_OPT      = [{ value: "on", label: "Excel export" },
                         { value: "off", label: "No Excel export" }];
  var TABLE_ROLES = {
    drill:      { label: "Drill-down", kind: "column", colType: "any" },
    row_color:  { label: "Row color",  kind: "column", colType: "any" },
    color_mode: { label: "Coloring",   kind: "select", rerender: true,
                  options: ["off", "diverging", "sequential", "bar"] },
    // Column-scope multi-select: empty = ALL numeric columns (the common case,
    // e.g. a correlation heatmap), so the placeholder spells that out. Picking
    // restricts to those columns (e.g. a single count column for data bars).
    color_columns: { label: "Columns", kind: "columns", colType: "num",
                     placeholder: "All numeric columns" },
    digits:     { label: "Decimals",   kind: "select", options: ["0", "1", "2", "3", "4"] },
    // Display toggles (column-free): click-through pills labelled by meaning.
    // Keys match the R config params so onChange(key) round-trips to the block
    // reactiveVals; the row label names the dimension, the pill the state.
    sortable:    { label: "Sorting",  kind: "segmented", options: SORTABLE_OPT },
    collapsible: { label: "Sections", kind: "segmented", options: COLLAPSIBLE_OPT },
    search:      { label: "Search",   kind: "segmented", options: SEARCH_OPT },
    excel_download: { label: "Export", kind: "segmented", options: EXPORT_OPT }
  };
  // Dynamic: the column-scope picker only appears once a coloring mode is on
  // (it is meaningless for a plain table). color_mode has rerender:true so
  // toggling it rebuilds the section list live. `hasCols` is false for a
  // structured ("Table 1") frame — there the column-based controls are no-ops,
  // so only the display toggles show (and Collapsible, which needs sections).
  /** @param {Record<string, any>} cfg @param {boolean} hasCols */
  function tableSections(cfg, hasCols) {
    var pres = [];
    if (hasCols) {
      pres.push("drill", "row_color", "color_mode");
      if (cfg && cfg.color_mode && cfg.color_mode !== "off") pres.push("color_columns");
      pres.push("digits");
    }
    pres.push("sortable");
    if (!hasCols) pres.push("collapsible");   // only sectioned tables collapse
    pres.push("search", "excel_download");
    return { requiredMap: [], optionalMap: [], mapping: [], presentation: pres };
  }

  /** @param {HTMLElement} root @param {HTMLElement} table */
  function buildCogwheel(root, table) {
    var elemId = root.getAttribute("data-dt-elem-id");
    if (!elemId) return;

    // Numeric columns (from R) drive the colType:"num" filter on the colour /
    // bar scope picker, so it only offers shadeable columns.
    /** @type {Record<string, boolean>} */
    var numSet = {};
    (table.getAttribute("data-dt-num-cols") || "").split(",")
      .forEach(function (n) { if (n) numSet[n] = true; });
    /** @type {VizColumn[]} */
    var cols = [];
    table.querySelectorAll("thead th .blockr-col-name")
      .forEach(function (s) {
        var nm = (s.textContent || "").trim();
        cols.push({ name: nm, type: numSet[nm] ? "numeric" : "any" });
      });

    // Structured ("Table 1") tables expose no pickable columns (the header is
    // section spanners, the cells are pre-formatted strings), so the column-based
    // controls (drill / colour / decimals) are no-ops and are dropped from the
    // section list below (`hasCols`). The gear still renders for the column-free
    // display toggles (sortable / collapsible / search / Excel export) — keyed on
    // "no columns" (not a hard-coded "structured" flag) so it covers any future
    // no-column case too.
    var hasCols = cols.length > 0;

    var onClick = table.getAttribute("data-dt-onclick-col");
    var colorCols = (table.getAttribute("data-dt-color-cols") || "")
      .split(",").filter(function (n) { return !!n; });
    /** @type {Record<string, any>} */
    var cfg = {
      drill:      (onClick && onClick !== "(none)") ? onClick : "",
      row_color:  table.getAttribute("data-dt-row-color") || "",
      color_mode: table.getAttribute("data-dt-color-mode") || "off",
      color_columns: colorCols,            // [] = all numeric
      digits:     table.getAttribute("data-dt-digits") || "2",
      // Display toggles (segmented on/off). Default on, except export.
      sortable:    table.getAttribute("data-dt-sortable") || "on",
      collapsible: table.getAttribute("data-dt-collapsible") || "on",
      search:      table.getAttribute("data-dt-search") || "on",
      excel_download: table.getAttribute("data-dt-excel") || "off"
    };

    var header = document.createElement("div");
    header.className = "blockr-gear-header";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-gear-btn";
    btn.title = "Table settings";
    btn.setAttribute("aria-label", "Table settings");
    btn.setAttribute("aria-haspopup", "dialog");
    btn.setAttribute("aria-expanded", "false");
    btn.innerHTML = (typeof Blockr !== "undefined" && Blockr.icons)
      ? Blockr.icons.gear : "⚙";
    header.appendChild(btn);

    // Remove a stale popover orphaned on <body> by a previous MOUNT of this
    // element (the popover is portaled to <body>). The gear is built once per
    // container now — the chrome never re-renders on a filter or config edit —
    // so there is no per-edit rebuild to restore (the old wasOpen hack is gone).
    var staleP = document.querySelector(
      '.dd-popover[data-dd-pop-for="' + (window.CSS && CSS.escape
        ? CSS.escape(elemId) : elemId) + '"]');
    if (staleP && staleP.parentNode) staleP.parentNode.removeChild(staleP);

    var pop = document.createElement("div");
    pop.className = "blockr-popover dd-popover";
    pop.setAttribute("data-dd-pop-for", elemId);
    pop.style.display = "none";

    // Populate the popover with the shared config engine — the same
    // DrilldownConfig the chart uses. cfg keys are the R config params, so a
    // change round-trips via sendConfig(key, value).
    var DDC = /** @type {typeof VizDrilldownConfig} */ (
      (typeof Blockr !== "undefined" && Blockr.DrilldownConfig) || window.DrilldownConfig);
    new DDC({
      popoverEl: function () { return pop; },
      roles: TABLE_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return "all"; },
      currentType: function () { return cfg.transform; },
      sections: function () { return tableSections(cfg, hasCols); },
      sectionsForFamily: function () { return tableSections(cfg, hasCols); },
      secondary: new Set(),
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function () { return false; },
      drillAutoLabel: null,
      title: "Table settings",
      onChange: function (key) { sendConfig(elemId, key, cfg[key]); },
      onMults: function () {},
      onClearFilter: function () {},
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () { return pop.style.display === "block"; },
      reopen: function () { openPop(); }
    }).render();

    // Anchor the popover to the gear in viewport coords. The shared CSS
    // anchors .blockr-popover absolute/right:0 to its offset parent, so
    // inside a table card it lands below the table, not the gear, and a
    // Dockview panel's overflow:auto / transform clips or traps it.
    // Portaling to <body> + position:fixed (same as the drilldown chart
    // and blockr-select) escapes both.
    function positionPop() {
      var g = btn.getBoundingClientRect();
      var vw = window.innerWidth, vh = window.innerHeight;
      pop.style.position = "fixed";
      pop.style.right = "auto";
      pop.style.maxHeight = (vh - 16) + "px";
      var pw = pop.offsetWidth, ph = pop.offsetHeight;
      var left = Math.min(g.right, vw - 8) - pw;
      left = Math.max(8, Math.min(left, vw - pw - 8));
      var top = g.bottom + 6;
      if (top + ph > vh - 8) top = Math.max(8, vh - 8 - ph);
      pop.style.left = left + "px";
      pop.style.top = top + "px";
      pop.style.maxHeight = (vh - top - 8) + "px";
    }
    var reposition = function () {
      if (pop.style.display === "block") positionPop();
    };
    function openPop() {
      pop.style.display = "block";
      btn.classList.add("blockr-gear-active");
      btn.setAttribute("aria-expanded", "true");
      positionPop();
      requestAnimationFrame(positionPop);
      window.addEventListener("scroll", reposition, true);
      window.addEventListener("resize", reposition);
    }
    function closePop() {
      pop.style.display = "none";
      btn.classList.remove("blockr-gear-active");
      btn.setAttribute("aria-expanded", "false");
      window.removeEventListener("scroll", reposition, true);
      window.removeEventListener("resize", reposition);
    }
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (pop.style.display === "block") closePop(); else openPop();
    });
    // Decide on mousedown, not click: a Blockr.Select dropdown is portaled to
    // <body> (outside the popover) and tears itself down on the option click,
    // so at click time the exclusion below would miss and picking a value would
    // dismiss the settings form. At mousedown the dropdown is still attached.
    document.addEventListener("mousedown", function (e) {
      if (pop.style.display !== "block") return;
      var t = /** @type {HTMLElement | null} */ (e.target);
      if (pop.contains(t) || btn.contains(t)) return;
      if (t && t.closest(".blockr-select__dropdown")) return;
      closePop();
    });

    document.body.appendChild(pop);
    root.insertBefore(header, root.firstChild);
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
    var inp = /** @type {HTMLInputElement | null} */ (root.querySelector("input.blockr-search"));
    if (inp && inp.value.trim()) inp.dispatchEvent(new Event("input"));
  }

  /** @param {Document | Element} [ctx] */
  function scan(ctx) {
    var nodes = (ctx || document)
      .querySelectorAll(".drilldown-table-container");
    Array.prototype.forEach.call(nodes, function (/** @type {HTMLElement} */ root) {
      var table = /** @type {HTMLElement | null} */ (root.querySelector("table.blockr-table"));
      if (!table) return;
      initContainer(root, table);
      wireTable(root, table);
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { scan(); });
  } else {
    scan();
  }

  var mo = new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      if (muts[i].addedNodes && muts[i].addedNodes.length) { scan(); break; }
    }
  });
  mo.observe(document.documentElement, { childList: true, subtree: true });

  if (typeof window.jQuery === "function") {
    jQuery(document).on("shiny:value shiny:bound", function () {
      setTimeout(scan, 0);
    });
  }
})();
