(function () {
  function parseNum(s) {
    if (s == null) return null;
    var m = String(s).match(/-?\d[\d,]*(\.\d+)?/);
    if (!m) return null;
    return parseFloat(m[0].replace(/,/g, ""));
  }

  function wireSort(root, table, tbody) {
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
      var rows = Array.prototype.slice.call(tbody.children);
      var dir = state.dir;
      rows.sort(function (a, b) {
        var av = a.children[idx] ? a.children[idx].textContent.trim() : "";
        var bv = b.children[idx] ? b.children[idx].textContent.trim() : "";
        var an = parseNum(av), bn = parseNum(bv), cmp;
        if (an !== null && bn !== null) cmp = an - bn;
        else cmp = av.localeCompare(bv);
        return dir * cmp;
      });
      var f = document.createDocumentFragment();
      rows.forEach(function (r) { f.appendChild(r); });
      tbody.appendChild(f);
    }

    root.querySelectorAll("th.blockr-sortable").forEach(function (th) {
      th.addEventListener("click", function (e) {
        e.stopPropagation();
        var idx = parseInt(th.getAttribute("data-col-index"), 10);
        if (!isNaN(idx)) sortBy(idx);
      });
    });
  }

  function wireSearch(root, tbody) {
    var inp = root.querySelector("input.blockr-search");
    if (!inp) return;
    inp.addEventListener("input", function () {
      var q = inp.value.trim().toLowerCase();
      Array.prototype.slice.call(tbody.children).forEach(function (r) {
        if (!r.classList.contains("blockr-data-row")) return;
        if (!q || r.textContent.toLowerCase().indexOf(q) !== -1) {
          r.classList.remove("blockr-hidden-search");
        } else {
          r.classList.add("blockr-hidden-search");
        }
      });
    });
  }

  function wireClick(root, tbody) {
    var elemId = root.getAttribute("data-dt-elem-id");
    var col = root.getAttribute("data-dt-onclick-col");
    var idx = root.getAttribute("data-dt-onclick-idx");
    if (!elemId || !col || idx == null) return;
    idx = parseInt(idx, 10);
    root.classList.add("dt-clickable");
    tbody.addEventListener("click", function (e) {
      var tr = e.target.closest("tr.blockr-data-row");
      if (!tr || !tr.children[idx]) return;
      var val = tr.children[idx].textContent.trim();
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
  // section holds everything; `cor_method` shows only for a correlation
  // transform (currentType() == cfg.transform); `drill` is a plain column
  // picker (the column a row-click filters on). Keys match the R config
  // params, so onChange(key) -> sendConfig(key, value) round-trips directly.
  var TABLE_ROLES = {
    drill:      { label: "Drill-down", kind: "column", colType: "any" },
    transform:  { label: "Transform",  kind: "select", options: ["none", "correlation"] },
    cor_method: { label: "Correlation method", kind: "select", options: ["pearson", "spearman", "kendall"] },
    color_mode: { label: "Coloring",   kind: "select", options: ["off", "diverging", "sequential"] },
    digits:     { label: "Decimals",   kind: "select", options: ["0", "1", "2", "3", "4"] }
  };
  var TABLE_SECTIONS = {
    requiredMap: [], optionalMap: [], encoding: [],
    presentation: [
      "drill", "transform",
      { role: "cor_method", types: ["correlation"] },
      "color_mode", "digits"
    ]
  };

  function buildCogwheel(root, table) {
    var elemId = root.getAttribute("data-dt-elem-id");
    if (!elemId) return;

    var cols = [];
    table.querySelectorAll("thead th .blockr-col-name")
      .forEach(function (s) { cols.push({ name: s.textContent.trim(), type: "any" }); });

    var onClick = root.getAttribute("data-dt-onclick-col");
    var cfg = {
      drill:      (onClick && onClick !== "(none)") ? onClick : "",
      transform:  root.getAttribute("data-dt-transform") || "none",
      cor_method: root.getAttribute("data-dt-cor-method") || "pearson",
      color_mode: root.getAttribute("data-dt-color-mode") || "off",
      digits:     root.getAttribute("data-dt-digits") || "2"
    };

    var header = document.createElement("div");
    header.className = "blockr-gear-header";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-gear-btn";
    btn.title = "Table settings";
    btn.innerHTML = (typeof Blockr !== "undefined" && Blockr.icons)
      ? Blockr.icons.gear : "⚙";
    header.appendChild(btn);

    // Remove a stale popover orphaned on <body> by a previous render of
    // this element (the popover is portaled to <body>, see below).
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
    var DDC = (typeof Blockr !== "undefined" && Blockr.DrilldownConfig) || window.DrilldownConfig;
    new DDC({
      popoverEl: function () { return pop; },
      roles: TABLE_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return "all"; },
      currentType: function () { return cfg.transform; },
      sections: function () { return TABLE_SECTIONS; },
      sectionsForFamily: function () { return TABLE_SECTIONS; },
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
      reopen: function () {}
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
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      var open = pop.style.display === "block";
      if (open) {
        pop.style.display = "none";
        btn.classList.remove("blockr-gear-active");
        window.removeEventListener("scroll", reposition, true);
        window.removeEventListener("resize", reposition);
      } else {
        pop.style.display = "block";
        btn.classList.add("blockr-gear-active");
        positionPop();
        requestAnimationFrame(positionPop);
        window.addEventListener("scroll", reposition, true);
        window.addEventListener("resize", reposition);
      }
    });
    document.addEventListener("click", function (e) {
      if (!pop.contains(e.target) && !btn.contains(e.target)) {
        pop.style.display = "none";
        btn.classList.remove("blockr-gear-active");
        window.removeEventListener("scroll", reposition, true);
        window.removeEventListener("resize", reposition);
      }
    });

    document.body.appendChild(pop);
    root.insertBefore(header, root.firstChild);
  }

  function init(root) {
    if (!root || root.getAttribute("data-dt-initialized") === "1") return;
    var table = root.querySelector("table.blockr-table");
    if (!table) return;
    var tbody = table.querySelector("tbody");
    if (!tbody) return;
    root.setAttribute("data-dt-initialized", "1");
    buildCogwheel(root, table);
    wireSort(root, table, tbody);
    wireSearch(root, tbody);
    wireClick(root, tbody);
  }

  function scan(ctx) {
    var nodes = (ctx || document)
      .querySelectorAll(".drilldown-table-container:not([data-dt-initialized])");
    Array.prototype.forEach.call(nodes, init);
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

  if (window.jQuery) {
    jQuery(document).on("shiny:value shiny:bound", function () {
      setTimeout(scan, 0);
    });
  }
})();
