/**
 * Ranked bar table: search, sort, expand, row-click drill.
 *
 * Self-contained on purpose. table.js owns `.drilldown-table-container` and
 * its payload-push render path; the rank table is server-rendered HTML in
 * `.blockr-rank-container`, so it binds its own handlers rather than reaching
 * into table.js internals.
 *
 * Contract with R (R/rank-table-html.R):
 *   container  data-rank-elem-id  the ns()-ed id the `_action` input hangs off
 *              data-rank-drill    the drill column, absent = display only
 *   table      data-rank-nested   "1" when parent rows are present
 *   row        data-rank-label / data-rank-parent / data-rank-level
 *   num cell   data-v             the raw number to sort on
 */
(function () {
  "use strict";

  var BOUND = "rankTableBound";

  /** @param {Element} root */
  function rows(root) {
    return Array.prototype.slice.call(
      root.querySelectorAll("tbody tr.blockr-rank-row")
    );
  }

  /** Parent rows start collapsed; a child is visible only when its parent is
   * expanded AND it survives the search. Both conditions are recomputed here
   * so the two features cannot fight over the same class. */
  function applyVisibility(root) {
    var open = {};
    rows(root).forEach(function (r) {
      if (r.classList.contains("is-parent")) {
        // The table block's contract: the ROW carries `collapsed`, which is
        // what rotates the shared chevron.
        open[r.getAttribute("data-rank-label")] = !r.classList.contains("collapsed");
      }
    });
    rows(root).forEach(function (r) {
      if (!r.classList.contains("is-child")) return;
      var vis = open[r.getAttribute("data-rank-parent")] === true;
      if (vis) r.classList.remove("collapsed-hidden");
      else r.classList.add("collapsed-hidden");
    });
  }

  // ---------- search ----------
  // Row text read once per row (the body is static between renders), and a
  // parent stays visible when any of its children match -- the same rule the
  // table block's search follows.
  function bindSearch(root) {
    var input = root.querySelector("input.blockr-search");
    if (!input) return;
    var cache = new WeakMap();
    function text(r) {
      var t = cache.get(r);
      if (t == null) {
        t = r.textContent.toLowerCase();
        cache.set(r, t);
      }
      return t;
    }
    var timer = null;
    function run() {
      var q = (input.value || "").trim().toLowerCase();
      var all = rows(root);
      var matched = {};
      all.forEach(function (r) {
        var hit = !q || text(r).indexOf(q) !== -1;
        if (hit) {
          r.classList.remove("blockr-rank-hidden-search");
          var p = r.getAttribute("data-rank-parent");
          if (p) matched[p] = true;
        } else {
          r.classList.add("blockr-rank-hidden-search");
        }
      });
      // A matching child keeps its parent on screen, and auto-expands it, so
      // searching a leaf never returns an empty-looking table.
      all.forEach(function (r) {
        if (!r.classList.contains("is-parent")) return;
        var label = r.getAttribute("data-rank-label");
        if (matched[label]) {
          r.classList.remove("blockr-rank-hidden-search");
          if (q) setOpen(r, true);
        }
      });
      applyVisibility(root);
      var foldRow = root.querySelector("tr.blockr-rank-fold");
      if (foldRow) foldRow.style.display = q ? "none" : "";
    }
    input.addEventListener("input", function () {
      if (timer) clearTimeout(timer);
      timer = setTimeout(run, 120);
    });
  }

  // ---------- sort ----------
  // Client-side, on `data-v` for numbers and the label text otherwise. In a
  // nested table only whole parent blocks move, so a child never leaves its
  // parent; within a block the children sort too.
  function bindSort(root) {
    var table = root.querySelector("table.blockr-rank-table");
    if (!table) return;
    var tbody = table.querySelector("tbody");
    if (!tbody) return;
    var nested = table.getAttribute("data-rank-nested") === "1";
    var state = { key: null, dir: 0 };

    // Same contract as the table block's wireSort(): the header carries
    // data-col-index, the cell carries the raw number in data-v. Index 0 is
    // the stub (label) column, which sorts on its text.
    function cellValue(r, th) {
      var idx = parseInt(th.getAttribute("data-col-index"), 10);
      if (!idx) return r.getAttribute("data-rank-label") || "";
      var td = r.children[idx];
      if (!td) return null;
      var raw = td.getAttribute("data-v");
      if (raw === null || raw === "") return null;
      var n = parseFloat(raw);
      return isNaN(n) ? null : n;
    }

    function cmp(a, b, th, dir) {
      var av = cellValue(a, th);
      var bv = cellValue(b, th);
      if (typeof av === "string" || typeof bv === "string") {
        return dir * String(av).localeCompare(String(bv));
      }
      if (av === null && bv === null) return 0;
      if (av === null) return 1;
      if (bv === null) return -1;
      return dir * (av - bv);
    }

    function blocks() {
      var out = [];
      var current = null;
      rows(root).forEach(function (r) {
        if (nested && r.classList.contains("is-child")) {
          if (current) current.kids.push(r);
          return;
        }
        current = { head: r, kids: [] };
        out.push(current);
      });
      return out;
    }

    function sortBy(th) {
      var key = th.getAttribute("data-col-index");
      if (state.key === key) {
        state.dir = state.dir === -1 ? 1 : -1;
      } else {
        state.key = key;
        state.dir = key === "0" ? 1 : -1;
      }
      // The arrow is the table block's .blockr-sort-icon, driven by the same
      // asc/desc classes, so the two headers behave and read identically.
      table.querySelectorAll("th .blockr-sort-icon").forEach(function (ic) {
        ic.classList.remove("blockr-sort-icon-asc", "blockr-sort-icon-desc");
      });
      var icon = th.querySelector(".blockr-sort-icon");
      if (icon) {
        icon.classList.add(state.dir === 1
          ? "blockr-sort-icon-asc" : "blockr-sort-icon-desc");
      }

      var bs = blocks();
      bs.sort(function (x, y) { return cmp(x.head, y.head, th, state.dir); });
      var frag = document.createDocumentFragment();
      bs.forEach(function (b) {
        frag.appendChild(b.head);
        b.kids.sort(function (x, y) { return cmp(x, y, th, state.dir); });
        b.kids.forEach(function (k) { frag.appendChild(k); });
      });
      var fold = tbody.querySelector("tr.blockr-rank-fold");
      tbody.appendChild(frag);
      if (fold) tbody.appendChild(fold);
    }

    table.querySelectorAll("th.blockr-sortable[data-col-index]")
      .forEach(function (th) {
        th.addEventListener("click", function (e) {
          e.stopPropagation();
          sortBy(th);
        });
      });
  }

  // ---------- expand / collapse ----------
  /** @param {Element} row @param {boolean} open */
  function setOpen(row, open) {
    if (open) row.classList.remove("collapsed");
    else row.classList.add("collapsed");
    var btn = row.querySelector(".blockr-indent-btn");
    if (btn) btn.setAttribute("aria-expanded", open ? "true" : "false");
  }

  function bindToggle(root) {
    root.addEventListener("click", function (e) {
      var btn = e.target.closest(".blockr-indent-btn");
      if (!btn || !root.contains(btn)) return;
      e.stopPropagation();
      e.preventDefault();
      var row = btn.closest("tr.blockr-rank-row");
      if (!row) return;
      setOpen(row, row.classList.contains("collapsed"));
      applyVisibility(root);
    });
  }

  // ---------- drill ----------
  // Row click emits the same categorical filter contract as the chart and
  // table blocks: {type, column, values}. Clicking the active row clears it.
  function bindDrill(root) {
    var elemId = root.getAttribute("data-rank-elem-id");
    var col = root.getAttribute("data-rank-drill");
    if (!elemId || !col) return;

    function send(values) {
      if (!window.Shiny || !Shiny.setInputValue) return;
      Shiny.setInputValue(
        elemId + "_action",
        {
          action: values === null ? "clear_filter" : "filter",
          type: "categorical",
          column: col,
          values: values
        },
        { priority: "event" }
      );
    }

    function status(text) {
      var box = root.querySelector(".blockr-rank-status");
      if (!box) return;
      var txt = box.querySelector(".blockr-rank-status-text");
      if (txt) txt.textContent = text ? "Filtering downstream: " + text : "";
      box.style.display = text ? "" : "none";
    }

    root.addEventListener("click", function (e) {
      var tr = e.target.closest("tr.blockr-rank-row.is-pick");
      if (!tr || !root.contains(tr)) return;
      if (e.target.closest(".blockr-indent-btn")) return;
      var label = tr.getAttribute("data-rank-label");
      var was = tr.classList.contains("is-on");
      rows(root).forEach(function (r) { r.classList.remove("is-on"); });
      if (was) {
        send(null);
        status("");
      } else {
        tr.classList.add("is-on");
        send([label]);
        status(label);
      }
    });

    var reset = root.querySelector(".blockr-rank-reset");
    if (reset) {
      reset.addEventListener("click", function (e) {
        e.stopPropagation();
        rows(root).forEach(function (r) { r.classList.remove("is-on"); });
        send(null);
        status("");
      });
    }
  }


  // ---------- gear ----------
  // Same engine, same structure, same vocabulary as the chart and table blocks:
  // Blockr.DrilldownConfig renders the Mapping / Presentation sections plus the
  // Drill-down capability section from this role spec. Keys are the block's R
  // config params, so onChange(key) round-trips straight to the reactiveVals.
  var FUNC_OPT = ["count", "count_distinct", "sum", "mean", "median",
                  "min", "max"];
  var BAR_MODE_OPT = [{ value: "stacked", label: "Stacked" },
                      { value: "grouped", label: "Grouped" },
                      { value: "percent", label: "100%" }];
  var SORT_DIR_OPT = [{ value: "desc", label: "Largest first" },
                      { value: "asc", label: "Smallest first" }];
  var SEARCH_OPT = [{ value: "on", label: "Search bar" },
                    { value: "off", label: "No search bar" }];

  var RANK_ROLES = {
    // Mapping — the chart's aesthetic vocabulary, one row per role.
    group:  { label: "Rank by", kind: "column", colType: "cat" },
    parent: { label: "Group into", kind: "column", colType: "cat" },
    color:  { label: "Color by", kind: "column", colType: "cat" },
    facet:  { label: "One column per", kind: "column", colType: "cat" },
    // Compare offers LEVELS of the facet column, not columns, so it is a
    // plain select whose options are refreshed from data-rank-cfg.
    compare: { label: "Compare to", kind: "select", options: [] },
    func:   { label: "Aggregate", kind: "select", options: FUNC_OPT,
              rerender: true },
    value:  { label: "Of column", kind: "column", colType: "num" },
    id_var: { label: "Count distinct", kind: "column", colType: "any" },
    // Presentation.
    sort_by:  { label: "Sort", kind: "select", options: [] },
    bar_mode: { label: "Split layout", kind: "segmented", options: BAR_MODE_OPT },
    sort_dir: { label: "Order", kind: "segmented", options: SORT_DIR_OPT },
    search:   { label: "Search", kind: "segmented", options: SEARCH_OPT },
    // Drill-down: a plain column role, like the table block's.
    drill:    { label: "Filter on", kind: "column", colType: "any" },
    title:    { label: "Title", kind: "text", ph: "e.g. AEs by {ARM}",
                autoValue: function (cfg) {
                  return (cfg.title == null && cfg.title_auto) ? cfg.title_auto : "";
                } },
    subtitle: { label: "Subtitle", kind: "text", ph: "e.g. N = {n_distinct(USUBJID)}",
                autoValue: function (cfg) {
                  return (cfg.subtitle == null && cfg.subtitle_auto) ? cfg.subtitle_auto : "";
                } },
    caption:  { label: "Caption", kind: "text", ph: "e.g. Source: ADAE",
                autoValue: function (cfg) {
                  return (cfg.caption == null && cfg.caption_auto) ? cfg.caption_auto : "";
                } }
  };

  // Which controls apply depends on the picks: `Of column` only for the
  // aggregations that reduce a column, `Count distinct` only for
  // count_distinct, the split layout only with a colour split, Compare only
  // with a facet. Conditional rows beat a wall of inert ones.
  function rankSections(cfg) {
    var needsValue = ["sum", "mean", "median", "min", "max"]
      .indexOf(cfg.func) > -1;
    var mapping = ["func"];
    if (needsValue) mapping.push("value");
    if (cfg.func === "count_distinct") mapping.push("id_var");

    var optional = ["parent", "color", "facet"];
    if (cfg.facet) optional.push("compare");

    var pres = ["sort_by", "sort_dir"];
    if (cfg.color && !cfg.facet) pres.push("bar_mode");
    pres.push("search");

    return {
      requiredMap: ["group"],
      optionalMap: optional,
      mapping: mapping,
      // The measure is the aggregation step, so it gets the chart's trailing
      // "Aggregation" section rather than sitting inside Mapping.
      aggTitle: "Aggregation",
      presentation: pres,
      drillToggle: "drill",
      drillDefault: cfg.group || "",
      // Spec-level hint (the table block's shape). NOT a host-level drillHint:
      // that one triggers the chart/tile drill section too and rendered a
      // second, empty "Drill-down" heading.
      drillHint: cfg.drill
        ? "Clicking a row filters downstream on " + cfg.drill + "."
        : "Clicking a row filters downstream blocks to that row.",
      titles: ["title", "subtitle", "caption"]
    };
  }

  /** Read the gear's working state off the rendered table. */
  function readGearState(table) {
    var cfg = {};
    try { cfg = JSON.parse(table.getAttribute("data-rank-cfg") || "{}"); }
    catch (e) { cfg = {}; }
    var cols = cfg.columns || [];
    var levels = cfg.facet_levels || [];
    var titles = cfg.titles || {};
    delete cfg.columns;
    delete cfg.facet_levels;
    delete cfg.titles;
    // The three text slots need null (auto) vs "" (explicitly none), which the
    // JSON carries; `*_auto` surfaces the inherited text so clearing the field
    // commits "" and turns the auto title off.
    cfg.title = titles.title_state === undefined ? null : titles.title_state;
    cfg.subtitle = titles.subtitle_state === undefined ? null : titles.subtitle_state;
    cfg.caption = titles.caption_state === undefined ? null : titles.caption_state;
    cfg.title_auto = titles.title || "";
    cfg.subtitle_auto = titles.subtitle || "";
    cfg.caption_auto = titles.caption || "";
    // A `null` column pick reads as "" for the pickers (the engine's no-value).
    ["group", "parent", "color", "facet", "compare", "value", "id_var", "drill"]
      .forEach(function (k) { if (cfg[k] == null) cfg[k] = ""; });
    RANK_ROLES.compare.options = levels;
    RANK_ROLES.sort_by.options = [
      { value: "value", label: "Measure" },
      { value: "label", label: "Name" }
    ].concat(levels.map(function (lv) { return { value: lv, label: lv }; }));
    return { cfg: cfg, cols: cols };
  }

  function sendConfig(elemId, param, value) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(
      elemId + "_action",
      { action: "config", param: param, value: value },
      { priority: "event" }
    );
  }

  function buildGear(root) {
    var elemId = root.getAttribute("data-rank-elem-id");
    var table = root.querySelector("table.blockr-rank-table");
    if (!elemId || !table) return;

    var st = readGearState(table);
    var cfg = st.cfg;
    var cols = st.cols;

    var header = document.createElement("div");
    header.className = "blockr-gear-header";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-gear-btn";
    btn.title = "Rank settings";
    btn.setAttribute("aria-label", "Rank settings");
    btn.setAttribute("aria-haspopup", "dialog");
    btn.setAttribute("aria-expanded", "false");
    btn.innerHTML = (typeof Blockr !== "undefined" && Blockr.icons)
      ? Blockr.icons.gear : "\u2699";
    header.appendChild(btn);

    // In-flow settings band (not a floating popover): opening pushes the table
    // down, so what is being configured stays visible.
    var pop = document.createElement("div");
    pop.className = "blockr-settings blockr-settings--beak dd-popover";
    pop.setAttribute("data-dd-pop-for", elemId);

    var DDC = (typeof Blockr !== "undefined" && Blockr.DrilldownConfig) ||
      window.DrilldownConfig;
    if (!DDC) return;
    var engine = new DDC({
      popoverEl: function () { return pop; },
      roles: RANK_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return "all"; },
      currentType: function () { return null; },
      sections: function () { return rankSections(cfg); },
      sectionsForFamily: function () { return rankSections(cfg); },
      secondary: new Set(),
      mappingTitle: "Mapping",
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function (role) { return role === "group"; },
      drillAutoLabel: null,
      title: "Rank settings",
      onChange: function (key) { sendConfig(elemId, key, cfg[key]); },
      onMults: function () {},
      onClearFilter: function () {
        rows(root).forEach(function (r) { r.classList.remove("is-on"); });
        sendConfig(elemId, "filter_values", null);
      },
      ensureDefaults: function () {},
      afterTypeChange: function () {},
      isOpen: function () {
        return pop.classList.contains("blockr-settings--open");
      },
      reopen: function () { openPop(); }
    });
    engine.render();

    function openPop() {
      pop.classList.add("blockr-settings--open");
      btn.setAttribute("aria-expanded", "true");
    }
    function closePop() {
      pop.classList.remove("blockr-settings--open");
      btn.setAttribute("aria-expanded", "false");
    }
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (pop.classList.contains("blockr-settings--open")) { closePop(); return; }
      // Re-read state before opening: the config may have moved server-side
      // (state restore, AI / external_ctrl edit) since the gear was built.
      var t = root.querySelector("table.blockr-rank-table");
      if (t) {
        var s2 = readGearState(t);
        cfg = s2.cfg;
        cols = s2.cols;
      }
      engine.refresh();
      openPop();
    });

    root.insertBefore(header, root.firstChild);
    root.insertBefore(pop, header.nextSibling);
    // One control row, chart / table parity: the search box MOVES UP into the
    // gear row and sits LEFT of the gear, which keeps its canonical top-right
    // spot (the cross-block anchor). The emptied chrome row is hidden so it
    // leaves no stray padded border. The legend is NOT in this row -- it has
    // its own row below, so it can never push the search box around.
    var toolbar = root.querySelector(".blockr-html-table-toolbar");
    if (toolbar) {
      header.insertBefore(toolbar, btn);
      var hdrRow = root.querySelector(".blockr-html-table-header");
      if (hdrRow) hdrRow.style.display = "none";
    }
  }

  function bind(root) {
    if (root.dataset[BOUND] === "1") return;
    root.dataset[BOUND] = "1";
    bindSearch(root);
    bindSort(root);
    bindToggle(root);
    bindDrill(root);
    buildGear(root);
    applyVisibility(root);
  }

  function scan(scope) {
    var host = scope && scope.querySelectorAll ? scope : document;
    host.querySelectorAll(".blockr-rank-container").forEach(bind);
  }

  // The container arrives with the block's UI, and again on every re-render
  // (a gear edit, new upstream data), so watch rather than bind once.
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { scan(document); });
  } else {
    scan(document);
  }
  new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      var added = muts[i].addedNodes;
      for (var j = 0; j < added.length; j++) {
        var n = added[j];
        if (n.nodeType !== 1) continue;
        if (n.classList && n.classList.contains("blockr-rank-container")) bind(n);
        else scan(n);
      }
    }
  }).observe(document.documentElement, { childList: true, subtree: true });
})();
