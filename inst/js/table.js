// @ts-check
(function () {
  // Shared aggregation vocabulary (group/value/func roles + AGG_FNS +
  // value-follows-agg reconcile) — the identical control the chart/tile use.
  var DAgg = (typeof Blockr !== "undefined" && Blockr.DrilldownAgg) ||
    window.DrilldownAgg;

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
    if (!elemId) return;
    // Grouped table: a row click ANDs the clicked row's group-key values into a
    // downstream filter (raw input -> that group's members). Otherwise the
    // legacy single-column drill (data-dt-onclick-col/idx).
    var groupCols = (table.getAttribute("data-dt-group-cols") || "")
      .split(",").filter(function (n) { return !!n; });
    var grouped = groupCols.length > 0;
    var col = table.getAttribute("data-dt-onclick-col");
    var idx = table.getAttribute("data-dt-onclick-idx");
    if (!grouped && (!col || idx == null)) return;

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
    root.classList.add("dt-clickable");
    tbody.addEventListener("click", function (e) {
      var t = /** @type {Element | null} */ (e.target);
      var tr = t && t.closest("tr.blockr-data-row");
      if (!tr) return;
      if (window.Shiny && Shiny.setInputValue) {
        if (grouped) {
          var filters = [];
          groupIdx.forEach(function (g) {
            var cell = tr.children[g.idx];
            if (cell) filters.push({
              column: g.column, value: (cell.textContent || "").trim()
            });
          });
          if (!filters.length) return;
          Shiny.setInputValue(elemId + "_action",
            { action: "filter", filters: filters, filter_type: "categorical" },
            { priority: "event" });
        } else {
          if (!tr.children[idxN]) return;
          var val = (tr.children[idxN].textContent || "").trim();
          Shiny.setInputValue(elemId + "_action",
            { action: "filter", column: col, values: [val],
              filter_type: "categorical" },
            { priority: "event" });
        }
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
  var SORTABLE_OPT    = [{ value: "on", label: "Sortable" },
                         { value: "off", label: "Not sortable" }];
  var COLLAPSIBLE_OPT = [{ value: "on", label: "Collapsible" },
                         { value: "off", label: "Not collapsible" }];
  var SEARCH_OPT      = [{ value: "on", label: "Search bar" },
                         { value: "off", label: "No search bar" }];
  var EXPORT_OPT      = [{ value: "on", label: "Excel export" },
                         { value: "off", label: "No Excel export" }];
  var TABLE_ROLES = Object.assign({}, DAgg.aggRoles({ multiple: true }), {
    drill:      { label: "Filter column", kind: "column", colType: "any" },
    // Categorical identity color ("Color by") — the chart's color aesthetic
    // applied to rows (scale-map tint). "(none)" = no tint.
    color:      { label: "Color by",   kind: "column", colType: "cat" },
    // Mode vocabulary for the repeatable "Shade cells" rules (rendered by the
    // engine's _renderShadings, not as a standalone role row).
    shade_mode: { label: "Mode",       kind: "select",
                  options: ["diverging", "sequential", "bar"] },
    digits:     { label: "Decimals",   kind: "select", options: ["0", "1", "2", "3", "4"] },
    // Display toggles (column-free): click-through pills labelled by meaning.
    // Keys match the R config params so onChange(key) round-trips to the block
    // reactiveVals; the row label names the dimension, the pill the state.
    sortable:    { label: "Sorting",  kind: "segmented", options: SORTABLE_OPT },
    collapsible: { label: "Sections", kind: "segmented", options: COLLAPSIBLE_OPT },
    search:      { label: "Search",   kind: "segmented", options: SEARCH_OPT },
    excel_download: { label: "Export", kind: "segmented", options: EXPORT_OPT }
  });
  // Variant A: Aggregation, Drill-down, Coloring and Row color are each a
  // checkbox capability section (off by default, revealing their controls when
  // checked); Presentation keeps only the always-on display options (decimals,
  // sorting, search, export). `hasCols` is false for a structured ("Table 1")
  // frame — there the column-based capabilities are no-ops, so only the display
  // toggles show (and Collapsible, which needs sections).
  /** @param {Record<string, any>} cfg @param {boolean} hasCols @param {string} [stubCol] */
  function tableSections(cfg, hasCols, stubCol) {
    // Aggregation lives under the "Aggregation" header: `group` is always
    // offered; the repeatable summaries list (spec.summaries) appears once a group
    // is set. `value`/`func` are no longer standalone roles here — the
    // summaries list owns them.
    var hasGroup = hasCols && cfg && cfg.group && cfg.group.length > 0;
    // Aggregation is "on" when a value is carried (checking the box seeds a
    // count). Grand totals = on with no group -> a single totals row.
    var aggOn = hasCols && cfg && cfg.summaries && cfg.summaries.length > 0;
    var pres = [];
    if (hasCols) pres.push("digits");
    pres.push("sortable");
    if (!hasCols) pres.push("collapsible");   // only sectioned tables collapse
    pres.push("search", "excel_download");
    var spec = { requiredMap: [], optionalMap: [],
                 mapping: hasCols ? ["group"] : [],
                 summaries: hasCols,         // offer the summaries list whenever the box is on
                 aggregatable: hasCols,    // Variant A: Aggregation checkbox section
                 // Empty cols on a numeric aggregation = all numeric columns
                 // (override rule, dd_metric_plan) — placeholder promises it.
                 metricsDefaultAll: true,
                 // Drill-down checkbox section only for a raw (non-aggregated)
                 // table; a grouped table drills on its group keys, and a
                 // totals row (aggregating, no group) has nothing to drill.
                 drillToggle: (hasCols && !hasGroup && !aggOn) ? "drill" : null,
                 // Checking the box pre-fills the filter column with the
                 // rowname/stub column (the row's identity — the natural
                 // click target), so drill works in one click; the picker
                 // stays for re-aiming (e.g. AETERM instead of USUBJID).
                 drillDefault: stubCol || "",
                 // Coloring checkbox section: the mode select + numeric column
                 // COLOR — one plain section (no checkbox: its activation
                 // lives in the picks): "Color by" identity tint + the
                 // repeatable "Shade cells" value-encoding rules.
                 colorSection: hasCols ? { colorKey: "color", shadings: true } : null,
                 presentation: pres };
    // Structured ("Table 1") input is an already-summarized annotated data
    // frame with no pickable columns, so grouping / aggregation / drill / colour
    // don't apply. A small badge names it; the tooltip carries the why.
    if (!hasCols) {
      spec.badge = "Annotated data frame";
      spec.badgeTitle = "Dot-prefixed columns (.label, .section, .indent, …) " +
        "were detected and interpreted as layout — they build the section " +
        "structure and are not shown as data columns. Grouping and aggregation " +
        "are set upstream, so only display options apply here.";
    }
    return spec;
  }

  /** @param {HTMLElement} root @param {HTMLElement} table */
  function buildCogwheel(root, table) {
    var elemIdAttr = root.getAttribute("data-dt-elem-id");
    if (!elemIdAttr) return;
    // Rebound after the guard so the closures below see a plain string.
    const elemId = elemIdAttr;

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
        // Non-numeric columns are "categorical" (not "any") so the Group /
        // aggregate picker (colType "cat") offers them — the columns you can
        // aggregate over. "any" pickers (drill, row color) still match either.
        cols.push({ name: nm, type: numSet[nm] ? "numeric" : "categorical" });
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
    /** @type {Record<string, any>} */
    var cfg = {
      // Aggregation config: group (comma-joined -> array), and a summaries list
      // (JSON array of {func, cols}) parsed off the table element.
      group: (table.getAttribute("data-dt-group") || "")
        .split(",").filter(function (n) { return !!n; }),
      summaries: (function () {
        try { return JSON.parse(table.getAttribute("data-dt-summaries") || "[]"); }
        catch (e) { return []; }
      })(),
      // Drill state. Raw table: the filter column (data-dt-onclick-col).
      // Grouped/aggregated table: the keys drill is a boolean — ON iff the
      // renderer wired the group-cols (data-dt-group-cols is emitted only
      // when drill is enabled); represented as 'auto' for the engine's
      // checkbox section.
      drill: (function () {
        if ((table.getAttribute("data-dt-group-cols") || "") !== "") return "auto";
        return (onClick && onClick !== "(none)") ? onClick : "";
      })(),
      // Identity color ("Color by") + value-encoding rules ({mode, cols}
      // JSON) — same transports as group / summaries.
      color: table.getAttribute("data-dt-color") || "",
      shadings: (function () {
        try { return JSON.parse(table.getAttribute("data-dt-shadings") || "[]"); }
        catch (e) { return []; }
      })(),
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

    // In-flow settings band (design-system pilot — blockr.ui/dev/
    // gear-panel-proposals.html, variant B). Lives inside the container right
    // below the gear header: no <body> portal, no fixed positioning, no
    // z-index/clipping concerns in dock panels. Opening pushes the table down
    // so the content being configured stays visible.
    // --beak: gear connector T1 (settings-band.css) — the open band grows a
    // notch pointing at the gear that opened it.
    var pop = document.createElement("div");
    pop.className = "blockr-settings blockr-settings--beak dd-popover";
    pop.setAttribute("data-dd-pop-for", elemId);

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
      sections: function () {
        return tableSections(cfg, hasCols, cols.length ? cols[0].name : "");
      },
      sectionsForFamily: function () {
        return tableSections(cfg, hasCols, cols.length ? cols[0].name : "");
      },
      // Paired-tail roles (e.g. func behind value) are rendered inside their
      // partner's row, never standalone — mirror the chart's DD_SECONDARY.
      secondary: new Set(Object.keys(TABLE_ROLES)
        .map(function (k) { return TABLE_ROLES[k].pairedWith; })
        .filter(Boolean)),
      // The table's mapping section is purely the aggregation step (group +
      // aggregate), so name it "Aggregation" — clearer than "Mapping" that it
      // transforms the data before display.
      mappingTitle: "Aggregation",
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function () { return false; },
      drillAutoLabel: null,
      // Grouped table: the drill target is the group keys — structurally
      // determined, so the section is a picker-less checkbox (same as the
      // tile). Raw table: null — its drill is the drillToggle column picker
      // (a raw row is multi-column; the target is a genuine choice). No
      // columns (annotated frame) or grand totals: null (section hidden).
      drillHint: function () {
        var hasGroup = cfg.group && cfg.group.length > 0;
        if (!hasCols || !hasGroup) return null;
        return "Clicking a row filters downstream on the group key" +
          (cfg.group.length > 1 ? "s" : "") + " (" + cfg.group.join(", ") + ").";
      },
      title: "Table settings",
      // Repeatable aggregation list: the engine renders one "[agg] of [cols]"
      // row per value. A grouped table always shows at least a count, so an
      // empty list surfaces as a single count row.
      metricsList: function () {
        return (cfg.summaries && cfg.summaries.length)
          ? cfg.summaries : [{ func: "count", cols: [] }];
      },
      onMetricsChange: function (/** @type {any[]} */ ms) {
        cfg.summaries = ms;
        sendConfig(elemId, "summaries", JSON.stringify(ms));
      },
      // Repeatable "Shade cells" rules — same JSON transport as summaries.
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
      onClearFilter: function () {},
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () { return pop.classList.contains("blockr-settings--open"); },
      reopen: function () { openPop(); }
    }).render();

    // The band is in flow: opening is a class toggle, no positioning, no
    // scroll/resize listeners, and no outside-click dismissal — it is a
    // panel, not a menu; the gear is the only toggle.
    function openPop() {
      pop.classList.add("blockr-settings--open");
      btn.classList.add("blockr-gear-active");
      btn.setAttribute("aria-expanded", "true");
      bandOpen[elemId] = true;
    }
    function closePop() {
      pop.classList.remove("blockr-settings--open");
      btn.classList.remove("blockr-gear-active");
      btn.setAttribute("aria-expanded", "false");
      bandOpen[elemId] = false;
    }
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (pop.classList.contains("blockr-settings--open")) closePop(); else openPop();
    });

    root.insertBefore(header, root.firstChild);
    root.insertBefore(pop, header.nextSibling);
    // Restore the remembered open state across chrome rebuilds.
    if (bandOpen[elemId]) openPop();
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
