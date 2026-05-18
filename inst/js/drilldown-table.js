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

  function mkSelect(cls, options, selected, onChange) {
    var sel = document.createElement("select");
    sel.className = cls;
    options.forEach(function (o) {
      var op = document.createElement("option");
      op.value = o;
      op.textContent = o;
      if (o === selected) op.selected = true;
      sel.appendChild(op);
    });
    sel.addEventListener("click", function (e) { e.stopPropagation(); });
    sel.addEventListener("change", function () { onChange(sel.value); });
    return sel;
  }

  function popRow(popover, labelText, control, desc) {
    // Vertical stack: label, full-width control, muted help text below.
    // (A right-side description column gets squished — keep it under
    // the control like a normal settings form.)
    var row = document.createElement("div");
    row.className = "blockr-popover-row";
    row.style.display = "flex";
    row.style.flexDirection = "column";
    row.style.alignItems = "stretch";
    row.style.gap = "4px";
    row.style.marginBottom = "12px";

    var lbl = document.createElement("span");
    lbl.className = "blockr-popover-label";
    lbl.textContent = labelText;
    lbl.style.marginBottom = "0";
    row.appendChild(lbl);

    control.style.width = "100%";
    control.style.boxSizing = "border-box";
    row.appendChild(control);

    if (desc) {
      var m = document.createElement("span");
      m.textContent = desc;
      m.style.fontSize = "0.75rem";
      m.style.color = "#9ca3af";
      m.style.lineHeight = "1.3";
      row.appendChild(m);
    }
    popover.appendChild(row);
  }

  function buildCogwheel(root, table) {
    var elemId = root.getAttribute("data-dt-elem-id");
    if (!elemId) return;

    var cols = [];
    table.querySelectorAll("thead th .blockr-col-name")
      .forEach(function (s) { cols.push(s.textContent.trim()); });

    var colorMode = root.getAttribute("data-dt-color-mode") || "off";
    var onClick = root.getAttribute("data-dt-onclick-col") || "(none)";
    var digits = root.getAttribute("data-dt-digits") || "2";
    var transform = root.getAttribute("data-dt-transform") || "none";

    var header = document.createElement("div");
    header.className = "blockr-gear-header";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-gear-btn";
    btn.title = "Advanced settings";
    btn.innerHTML = (typeof Blockr !== "undefined" && Blockr.icons)
      ? Blockr.icons.gear : "⚙";
    header.appendChild(btn);

    var pop = document.createElement("div");
    pop.className = "blockr-popover";
    pop.style.display = "none";
    var title = document.createElement("div");
    title.className = "blockr-popover-label";
    title.style.fontWeight = "600";
    title.style.marginBottom = "10px";
    title.textContent = "Advanced";
    pop.appendChild(title);

    popRow(pop, "Transform",
      mkSelect("blockr-popover-input",
        ["none", "correlation"], transform,
        function (v) { sendConfig(elemId, "transform", v); }),
      "Render a pairwise correlation matrix of numeric columns");

    popRow(pop, "Coloring",
      mkSelect("blockr-popover-input",
        ["off", "diverging", "sequential"], colorMode,
        function (v) { sendConfig(elemId, "color_mode", v); }),
      "Cell background color scale");

    popRow(pop, "Drill-down",
      mkSelect("blockr-popover-input",
        ["(none)"].concat(cols), onClick,
        function (v) { sendConfig(elemId, "drill", v); }),
      "Column whose value a row click filters on");

    popRow(pop, "Decimals",
      mkSelect("blockr-popover-input",
        ["0", "1", "2", "3", "4"], digits,
        function (v) { sendConfig(elemId, "digits", v); }),
      "Numeric rounding");

    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      var open = pop.style.display === "block";
      pop.style.display = open ? "none" : "block";
      btn.classList.toggle("blockr-gear-active", !open);
    });
    document.addEventListener("click", function (e) {
      if (!root.contains(e.target)) {
        pop.style.display = "none";
        btn.classList.remove("blockr-gear-active");
      }
    });

    root.insertBefore(pop, root.firstChild);
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
