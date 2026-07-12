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
      root.querySelectorAll("th.blockr-sortable").forEach(function (h) {
        h.setAttribute("aria-sort", "none");
        var i = h.querySelector(".blockr-sort-icon");
        if (i) i.classList.remove("blockr-sort-icon-asc", "blockr-sort-icon-desc");
      });
      if (state.dir === 0) { state.col = null; reset(); return; }
      var th = root.querySelector(
        'th.blockr-sortable[data-col-index="' + idx + '"]'
      );
      if (th) {
        th.setAttribute(
          "aria-sort", state.dir === 1 ? "ascending" : "descending"
        );
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
      // Keyboard parity: the header is focusable and Enter/Space sort like a
      // click; aria-sort starts at "none" and tracks the state in sortBy.
      th.setAttribute("tabindex", "0");
      th.setAttribute("aria-sort", "none");
      /** @param {Event} e */
      var activate = function (e) {
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
    root.querySelectorAll("tr.blockr-section-header").forEach(function (h) {
      // The Direction-01 group label is a <button> inside the row; its click
      // bubbles to this row-level handler, so a single listener covers both
      // the button and any bare-cell click.
      h.addEventListener("click", function (ev) {
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

  // One filter pass over the current table (also run directly by wireTable to
  // re-apply an active query to freshly rendered rows, skipping the debounce).
  /** @param {HTMLElement} root */
  function applySearch(root) {
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

    root.classList.add("dt-clickable");
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
        if (srcTr.classList.contains("dt-row-active")) {
          srcTr.classList.remove("dt-row-active");
          sendClearFilter(elemId);
          return;
        }
        /** @type {any} */
        var keys = null;
        try { keys = JSON.parse(keysJson); } catch (err) { keys = null; }
        if (!keys || !keys.length) return;
        Shiny.setInputValue(elemId + "_action",
          { action: "filter", filters: keys,
            filter_type: "categorical" },
          { priority: "event" });
        Array.prototype.slice.call(tbody.children).forEach(function (r) {
          r.classList.remove("dt-row-active");
        });
        srcTr.classList.add("dt-row-active");
        return;
      }
      var tr = t && t.closest("tr.blockr-data-row");
      if (!tr) return;
      var row = tr;
      if (!window.Shiny || !Shiny.setInputValue) return;
      // Click-to-toggle (chart parity): re-clicking the active row clears
      // the filter and the highlight.
      if (tr.classList.contains("dt-row-active")) {
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
          { action: "filter", filters: filters, filter_type: "categorical" },
          { priority: "event" });
      } else {
        var val = rawAt(tr, idxN);
        if (val == null) return;                     // NA drill cell -> no-op
        Shiny.setInputValue(elemId + "_action",
          { action: "filter", column: col, values: [val],
            filter_type: "categorical" },
          { priority: "event" });
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
    // "Filter on" matches the chart's drill picker label (one vocabulary).
    drill:      { label: "Filter on", kind: "column", colType: "any" },
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
    var spec = /** @type {Record<string, any>} */ ({ requiredMap: [], optionalMap: [],
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
                 // COLOR — one plain section (no checkbox: its activation
                 // lives in the picks): "Color by" identity tint + the
                 // repeatable "Shade cells" value-encoding rules.
                 colorSection: hasCols ? { colorKey: "color", shadings: true } : null,
                 presentation: pres });
    // Structured ("Table 1") input is an already-summarized annotated data
    // frame with no pickable columns, so grouping / aggregation / drill / colour
    // don't apply. A small badge names it; the tooltip carries the why.
    if (!hasCols) {
      spec.badge = "Annotated data frame";
      spec.badgeTitle = "Dot-prefixed columns (.label, .variable, .group1, …) " +
        "were detected and interpreted as structure — they build the section " +
        "hierarchy and row identity and are not shown as data columns. " +
        "Grouping and aggregation are set upstream; Drill-down and the " +
        "display options apply here.";
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
        // Structured drill: ON iff the renderer stamped the row keys
        // (data-dt-structured-drill), represented as 'auto' like the grouped
        // table's keys drill.
        if (table.getAttribute("data-dt-structured-drill") === "1") return "auto";
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
    return { cols: cols, cfg: cfg };
  }

  /** @param {HTMLElement} root @param {HTMLElement} table */
  function buildCogwheel(root, table) {
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
    }

    // Structured ("Table 1") tables expose no pickable columns (the header is
    // section spanners, the cells are pre-formatted strings), so the column-based
    // controls (drill / colour / decimals) are no-ops and are dropped from the
    // section list below (`hasCols`). The gear still renders for the column-free
    // display toggles (sortable / collapsible / search / Excel export) — keyed on
    // "no columns" (not a hard-coded "structured" flag) so it covers any future
    // no-column case too.
    function hasCols() { return cols.length > 0; }

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
    var engine = new DDC({
      popoverEl: function () { return pop; },
      roles: TABLE_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return "all"; },
      currentType: function () { return cfg.transform; },
      sections: function () {
        return tableSections(cfg, hasCols(), cols.length ? cols[0].name : "");
      },
      sectionsForFamily: function () {
        return tableSections(cfg, hasCols(), cols.length ? cols[0].name : "");
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
        // Structured (annotated) table: the drill target is the row's own
        // identity columns -- structurally determined, so the section is the
        // picker-less checkbox (same as the grouped table / tile).
        if (root.getAttribute("data-dt-structured") === "1") {
          return "Clicking a row filters downstream to that row's identity " +
            "(variable and value, plus its sections); the selection is also " +
            "named as a real column, so a dm filter-by-data block can " +
            "cascade it to the source data.";
        }
        var hasGroup = cfg.group && cfg.group.length > 0;
        if (!hasCols() || !hasGroup) return null;
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

    root.insertBefore(header, root.firstChild);
    root.insertBefore(pop, header.nextSibling);
    // Restore the remembered open state across chrome rebuilds (the state
    // was parsed just above, so no refresh needed here).
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
    // Status-footer Reset (the footer is a server-rendered output inside the
    // chrome and re-renders per filter change, so delegate from the
    // container rather than wiring the button itself).
    var elemIdAttr = root.getAttribute("data-dt-elem-id");
    if (elemIdAttr) {
      // Rebound after the guard so the closure sees a plain string.
      const elemId = elemIdAttr;
      root.addEventListener("click", function (e) {
        var t = /** @type {Element | null} */ (e.target);
        if (!t || !t.closest(".dd-status-reset")) return;
        clearActiveRows(root);
        sendClearFilter(elemId);
      });
    }
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
  }

  /** @param {HTMLElement} root */
  function wireRoot(root) {
    var table = /** @type {HTMLElement | null} */ (root.querySelector("table.blockr-table"));
    if (!table) return;
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
