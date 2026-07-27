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
  function rowText(root, r) {
    if (!root._rankCache) root._rankCache = new WeakMap();
    var t = root._rankCache.get(r);
    if (t == null) {
      // Segment tooltips (the swimlane's event labels) live in attributes,
      // not text -- fold them in so searching "pruritus" finds the
      // subjects who had it.
      var tips = [];
      r.querySelectorAll("[data-tip]").forEach(function (e) {
        tips.push(e.getAttribute("data-tip"));
      });
      t = (r.textContent + " " + tips.join(" ")).toLowerCase();
      root._rankCache.set(r, t);
    }
    return t;
  }

  /** Re-apply the search box's current query to the rows now in the DOM. */
  function runSearch(root) {
    var input = root.querySelector("input.blockr-search");
    var q = input ? (input.value || "").trim().toLowerCase() : "";
    var all = rows(root);
    var matched = {};
    all.forEach(function (r) {
      var hit = !q || rowText(root, r).indexOf(q) !== -1;
      if (hit) {
        r.classList.remove("blockr-rank-hidden-search");
        var pr = r.getAttribute("data-rank-parent");
        if (pr) matched[pr] = true;
      } else {
        r.classList.add("blockr-rank-hidden-search");
      }
    });
    // A matching child keeps its parent on screen, and auto-expands it, so
    // searching a leaf never returns an empty-looking table.
    all.forEach(function (r) {
      if (!r.classList.contains("is-parent")) return;
      if (matched[r.getAttribute("data-rank-label")]) {
        r.classList.remove("blockr-rank-hidden-search");
        if (q) setOpen(r, true);
      }
    });
    applyVisibility(root);
    var foldRow = root.querySelector("tr.blockr-rank-fold");
    if (foldRow) foldRow.style.display = q ? "none" : "";
    // Segment emphasis: a query that matches segment tooltips also LIGHTS
    // the matching segments (same dimming as the hover highlight), so a
    // term search shows WHERE in each timeline the event sits -- not just
    // which rows survive. Only armed when at least one segment matches;
    // a subject-id query must not dim everything.
    var anyHit = false;
    root.querySelectorAll(".lane-seg").forEach(function (s) {
      var hit = !!q &&
        (s.getAttribute("data-tip") || "").toLowerCase().indexOf(q) !== -1;
      s.classList.toggle("is-hit", hit);
      if (hit) anyHit = true;
    });
    root.classList.toggle("seg-search", anyHit);
  }

  function bindSearch(root) {
    var input = root.querySelector("input.blockr-search");
    if (!input) return;
    var timer = null;
    function run() {
      runSearch(root);
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
    // Looked up per interaction: with the data-push transport the <table> is
    // replaced on every payload, so nothing may be captured here.
    var tbl = function () { return root.querySelector("table.blockr-rank-table"); };
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
      // A text field column carries its text in data-v: sort it as text.
      return isNaN(n) ? raw : n;
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

    function blocks(nested) {
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
      var table = tbl();
      if (!table) return;
      var tbody = table.querySelector("tbody");
      if (!tbody) return;
      var nested = table.getAttribute("data-rank-nested") === "1";
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

      var bs = blocks(nested);
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

    root.addEventListener("click", function (e) {
      var th = e.target.closest("th.blockr-sortable[data-col-index]");
      if (!th || !root.contains(th)) return;
      e.stopPropagation();
      sortBy(th);
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



  // ==========================================================================
  // Data-push body (dev/table-data-push-design.md, the table block's shape).
  // The server ships the body as a column-oriented cell model over the
  // "blockr-viz-rank-data" custom message instead of rendering it through
  // Shiny: ~93-95% smaller than the equivalent HTML at 790 rows, and a payload
  // cached per elem id re-renders a re-mounted dock panel with no R round trip.
  //
  // The markup assembled here must match R/rank-push.R's rank_cells_html()
  // byte for byte (test-rank-push.R pins them), including the escaping rules
  // htmltools applies: & < > and the attribute quote.
  // ==========================================================================

  /** @type {Record<string, {rev: number, payload: any}>} */
  var payloadStore = {};

  function esc(x) {
    return String(x == null ? "" : x)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;")
      .replace(/>/g, "&gt;").replace(/"/g, "&quot;");
  }
  // R prints widths with format(trim=TRUE): an integral value has no decimals,
  // so match that rather than emitting "57.00".
  function w(x) {
    var n = Number(x) || 0;
    return String(n);
  }
  function dataV(v) {
    return " data-v=\"" + (v == null || v === "" ? "" : esc(String(v))) + "\"";
  }

  var CHEV = '<svg class="blockr-chev" viewBox="0 0 24 24" fill="none"' +
    ' stroke="currentColor" stroke-width="2.4" stroke-linecap="round"' +
    ' stroke-linejoin="round" aria-hidden="true"><path d="M6 9l6 6 6-6"/></svg>';

  // An NA width ships as null: a no-value cell renders an EMPTY track (no
  // fill, no zero sliver), where 0 keeps the visible sliver.
  function trackHtml(width, fill, sub) {
    return '<div class="blockr-rank-track' + (sub ? " is-sub" : "") + '">' +
      (width == null ? "" :
        '<div class="blockr-rank-fill" style="width:' + w(width) + "%" +
        (fill ? ";background:" + fill : "") + '"></div>') +
      "</div>";
  }

  // The in-bar value label (R: rank_barwrap): track left, the value in a
  // fixed-width right-aligned slot -- one width per column so tracks align.
  function barWrap(inner, c, i) {
    if (!c.disp) return inner;
    var pct = c.pct && c.pct[i] ?
      ' <span class="blockr-rank-pct">' + c.pct[i] + "</span>" : "";
    return '<div class="blockr-rank-barwrap">' + inner +
      '<span class="blockr-rank-barval" style="width:' + c.dw + 'ch">' +
      c.disp[i] + pct + "</span></div>";
  }

  function splitHtml(c, i) {
    var grouped = c.mode === "grouped";
    var out = "";
    for (var j = 0; j < c.names.length; j++) {
      var body = '<div class="blockr-rank-fill" style="width:' +
        w(c.seg[j][i]) + "%;background:" + c.fills[j] + '" title="' +
        esc(c.names[j]) + ": " + c.segv[j][i] + '"></div>';
      if (grouped) out += '<div class="blockr-rank-row3">' + body + "</div>";
      else if (c.segv[j][i] > 0) out += body;
    }
    return '<div class="blockr-rank-track' + (grouped ? " is-tall" : "") +
      '">' + out + "</div>";
  }

  function dvHtml(width, pos) {
    return '<div class="blockr-rank-dv"><div class="blockr-rank-fill ' +
      (pos ? "is-pos" : "is-neg") + '" style="width:' + w(width) +
      '%"></div></div>';
  }

  // ---- lane mark emitters ----
  // Byte-identical twins of rank_box_html / rank_pr_html / rank_iv_html /
  // rank_sp_html in R/rank-push.R (test-rank-push.R pins the pair). Every
  // geometric number arrives pre-rounded; emission conditions key on shipped
  // nulls, never re-derived arithmetic. `p(x)` is String(): positions and
  // widths were rounded R-side so the two prints agree.
  function p(x) { return String(x); }

  function boxHtml(c, i) {
    if (c.bc[i] == null) {
      return '<div class="blockr-rank-lane blockr-rank-boxcell"></div>';
    }
    var s = '<div class="blockr-rank-lane blockr-rank-boxcell" title="' +
      c.tip[i] + '">';
    if (c.w1[i] != null) {
      s += '<i class="lane-wh" style="left:' + p(c.wl[i]) + "%;width:" +
        p(c.w1[i]) + '%"></i>';
    }
    if (c.w2[i] != null) {
      s += '<i class="lane-wh" style="left:' + p(c.b2[i]) + "%;width:" +
        p(c.w2[i]) + '%"></i>';
    }
    if (c.w1[i] != null) {
      s += '<i class="lane-cap" style="left:' + p(c.wl[i]) + '%"></i>';
    }
    if (c.w2[i] != null) {
      s += '<i class="lane-cap" style="left:' + p(c.wh[i]) + '%"></i>';
    }
    if (c.bw[i] != null) {
      s += '<i class="lane-box" style="left:' + p(c.bl[i]) + "%;width:" +
        p(c.bw[i]) + '%"></i>';
    }
    return s + '<i class="lane-med" style="left:' + p(c.bc[i]) +
      '%"></i></div>';
  }

  function prHtml(c, i) {
    if (c.c[i] == null) {
      return '<div class="blockr-rank-lane blockr-rank-prcell"></div>';
    }
    var s = '<div class="blockr-rank-lane blockr-rank-prcell" title="' +
      c.tip[i] + '">';
    if (c.rw[i] != null) {
      s += '<i class="lane-rng" style="left:' + p(c.l[i]) + "%;width:" +
        p(c.rw[i]) + '%"></i>';
    }
    return s + '<i class="lane-ctr" style="left:' + p(c.c[i]) +
      '%"></i></div>';
  }

  function ivHtml(c, i) {
    var s = '<div class="blockr-rank-lane blockr-rank-ivcell" data-d0="' +
      p(c.d0) + '" data-d1="' + p(c.d1) + '"' +
      (c.dd ? ' data-dd="1"' : "") + ">";
    var segs = c.segs[i] || [];
    for (var j = 0; j < segs.length; j++) {
      s += '<i class="lane-seg" style="left:' + p(segs[j][0]) + "%;width:" +
        p(segs[j][1]) + "%;background:" + c.fills[segs[j][2] - 1] + '"' +
        (segs[j].length > 3 ? ' data-l="' + segs[j][3] + '"' : "") +
        ' data-tip="' + c.tips[i][j] + '"></i>';
    }
    return s + "</div>";
  }

  function spHtml(c, i) {
    var s = '<div class="blockr-rank-lane blockr-rank-spcell" data-xs="' +
      c.xs[i] + '" data-ys="' + c.ys[i] + '">' +
      '<svg viewBox="0 0 100 36" preserveAspectRatio="none">';
    if (c.rby != null) {
      s += '<rect class="lane-refband" x="0" y="' + p(c.rby) +
        '" width="100" height="' + p(c.rbh) + '"></rect>';
    }
    if (c.bd[i] != null) {
      s += '<polygon class="lane-band" points="' + c.bd[i] + '"></polygon>';
    }
    if (c.rc != null) {
      s += '<line class="lane-refline" x1="0" y1="' + p(c.rc) +
        '" x2="100" y2="' + p(c.rc) +
        '" vector-effect="non-scaling-stroke"></line>';
    }
    if (c.pl[i] !== "") {
      s += '<polyline class="lane-ln" points="' + c.pl[i] +
        '" vector-effect="non-scaling-stroke"></polyline>';
    }
    s += "</svg>";
    if (c.dx[i] != null) {
      s += '<i class="lane-dot" style="left:' + p(c.dx[i]) + "%;top:" +
        p(c.dy[i]) + '%"></i>';
    }
    return s + "</div>";
  }

  /** Assemble the whole tbody from the cell model. */
  function assembleBody(p) {
    var out = [];
    for (var i = 0; i < p.n; i++) {
      var parent = !!(p.parent_row && p.parent_row[i]);
      var child = !!(p.level && p.level[i] > 0);
      var cls = "blockr-rank-row" +
        (parent ? " is-parent blockr-indent-toggle collapsed" : "") +
        (child ? " is-child collapsed-hidden" : "") +
        (p.pick ? " is-pick" : "") +
        (p.on && p.on[i] ? " is-on" : "");
      var row = '<tr class="' + cls + '" data-rank-label="' +
        esc(p.label[i]) + '"' +
        (child ? ' data-rank-parent="' + esc(p.parent[i]) + '"' : "") +
        ' data-rank-level="' + (child ? p.level[i] : 0) + '">';
      row += '<td class="blockr-rank-label-col blockr-stub' +
        (parent ? " blockr-has-toggle" : "") + '"' +
        (child ? ' style="padding-left:40px;"' : "") + ">" +
        (parent ? '<button class="blockr-indent-btn" type="button"' +
          ' tabindex="-1" aria-expanded="false">' + CHEV + "</button>" : "") +
        '<span class="blockr-rank-label">' + esc(p.label[i]) + "</span></td>";
      for (var k = 0; k < p.cols.length; k++) {
        var c = p.cols[k];
        if (c.kind === "num" && c.text) {
          row += '<td class="blockr-rank-txt"' + dataV(c.v[i]) + ">" +
            c.disp[i] + "</td>";
        } else if (c.kind === "num") {
          row += '<td class="blockr-rank-num dt-col-num"' + dataV(c.v[i]) +
            ">" + c.disp[i] +
            (c.pct ? ' <span class="blockr-rank-pct">' + c.pct[i] + "</span>" : "") +
            "</td>";
        } else if (c.kind === "barsplit") {
          row += '<td class="blockr-rank-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(splitHtml(c, i), c, i) + "</td>";
        } else if (c.kind === "bardiv") {
          row += '<td class="blockr-rank-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(dvHtml(c.w[i], c.pos[i]), c, i) + "</td>";
        } else if (c.kind === "box") {
          row += '<td class="blockr-rank-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(boxHtml(c, i), c, i) + "</td>";
        } else if (c.kind === "pointrange") {
          row += '<td class="blockr-rank-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(prHtml(c, i), c, i) + "</td>";
        } else if (c.kind === "interval") {
          row += '<td class="blockr-rank-bar-col' +
            (c.lg ? " blockr-rank-wide" : "") + '"' + dataV(c.v[i]) + ">" +
            ivHtml(c, i) + "</td>";
        } else if (c.kind === "sparkline") {
          row += '<td class="blockr-rank-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(spHtml(c, i), c, i) + "</td>";
        } else {
          row += '<td class="blockr-rank-bar-col"' + dataV(c.v[i]) + ">" +
            barWrap(trackHtml(c.w[i], c.fill, c.sub && c.sub[i]), c, i) +
            "</td>";
        }
      }
      out.push(row + "</tr>");
    }
    if (p.fold) {
      out.push('<tr class="blockr-rank-fold"><td colspan="' + p.ncol + '">' +
        esc(p.fold) + "</td></tr>");
    }
    return out.join("");
  }

  /** Title / subtitle / caption / legend / footer, refreshed in place. */
  function applyChrome(root, ch) {
    if (!ch) return;
    var band = root.querySelector(".dd-table-titles");
    if (band) {
      var t = ch.title || "";
      var st = ch.subtitle || "";
      var tEl = band.querySelector(".dd-table-title");
      var sEl = band.querySelector(".dd-table-subtitle");
      if (tEl) { tEl.textContent = t; tEl.style.display = t ? "" : "none"; }
      if (sEl) { sEl.textContent = st; sEl.style.display = st ? "" : "none"; }
      band.style.display = (t || st) ? "" : "none";
    }
    var cap = root.querySelector(".dd-table-caption");
    if (cap) {
      cap.textContent = ch.caption || "";
      cap.style.display = ch.caption ? "" : "none";
    }
    var lg = root.querySelector(".blockr-rank-legend");
    if (lg) {
      if (!ch.legend) {
        lg.style.display = "none";
        lg.innerHTML = "";
      } else {
        var h = '<span class="blockr-rank-legend-title">' +
          esc(ch.legend.title) + "</span>";
        (ch.legend.items || []).forEach(function (it) {
          h += '<span class="blockr-rank-legend-item"><i style="background:' +
            it.color + '"></i>' + esc(it.label) + "</span>";
        });
        lg.innerHTML = h;
        lg.style.display = "";
      }
    }
    var f = ch.foot || {};
    var cnt = root.querySelector(".blockr-rank-count");
    if (cnt) cnt.textContent = f.count || "";
    var note = root.querySelector(".blockr-rank-note");
    if (note) note.textContent = f.note || "";
    var st2 = root.querySelector(".blockr-rank-status");
    if (st2) {
      var txt = st2.querySelector(".blockr-rank-status-text");
      if (txt) {
        txt.textContent = f.filter ? "Filtering downstream: " + f.filter : "";
      }
      st2.style.display = f.filter ? "" : "none";
    }
  }

  /** Apply one payload: inject the body, refresh the bands, re-arm the JS. */
  function applyPayload(root, p) {
    var wrap = root.querySelector(".blockr-table-wrapper");
    if (!wrap) return;
    if (p.kind === "html") {
      wrap.innerHTML = p.html;
    } else {
      wrap.innerHTML = p.head;
      var tb = wrap.querySelector("tbody");
      if (tb) tb.innerHTML = assembleBody(p);
    }
    applyChrome(root, p.chrome);
    // The row set changed: drop the search text cache and re-apply the current
    // query + collapse state to the fresh rows.
    root._rankCache = null;
    var input = root.querySelector("input.blockr-search");
    if (input && input.value) runSearch(root);
    else applyVisibility(root);
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("blockr-viz-rank-data",
      function (msg) {
        var entry = payloadStore[msg.id];
        var payload = null;
        if (entry && entry.rev === msg.rev) {
          payload = entry.payload;
        } else {
          try { payload = JSON.parse(msg.payload); } catch (e) { payload = null; }
          if (!payload) return;
          payloadStore[msg.id] = { rev: msg.rev, payload: payload };
        }
        var eid = (window.CSS && CSS.escape)
          ? CSS.escape(msg.id)
          : String(msg.id).replace(/"/g, '\\"');
        var root = document.querySelector(
          '.blockr-rank-container[data-rank-elem-id="' + eid + '"]');
        // No container yet: the payload waits in the store, and bind() picks it
        // up when the chrome turns up (no timers, no expiring delivery window).
        if (root) applyPayload(root, payload);
      });
  }

  /** A stored payload for this container, or null. */
  function storedFor(root) {
    var id = root.getAttribute("data-rank-elem-id");
    var e = id ? payloadStore[id] : null;
    return e ? e.payload : null;
  }

  // ---------- gear ----------
  // Same engine, same structure, same vocabulary as the chart and table blocks:
  // Blockr.DrilldownConfig renders the Mapping / Presentation sections plus the
  // Drill-down capability section from this role spec. Keys are the block's R
  // config params, so onChange(key) round-trips straight to the reactiveVals.
  // The chart's aggregate picker verbatim: the shared labeled AGG_FNS plus the
  // chart-only "None (as is)" identity (drilldown-agg.js documents it) -- for
  // pre-aggregated data such as one value per subject.
  var DAggR = (typeof Blockr !== "undefined" && Blockr.DrilldownAgg) ||
    window.DrilldownAgg;
  var FUNC_OPT = ((DAggR && DAggR.AGG_FNS) ||
    ["count", "count_distinct", "sum", "mean", "median", "min", "max"])
    .concat([{ value: "identity", label: "None (as is)" }]);
  var BAR_MODE_OPT = [{ value: "stacked", label: "Stacked" },
                      { value: "grouped", label: "Grouped" },
                      { value: "percent", label: "100%" }];
  var SORT_DIR_OPT = [{ value: "desc", label: "Largest first" },
                      { value: "asc", label: "Smallest first" }];
  var SEARCH_OPT = [{ value: "on", label: "Search bar" },
                    { value: "off", label: "No search bar" }];

  // The lane statistic vocabulary: MUST mirror R's LANE_STATS/LANE_STAT_META
  // (R/lane-stats.R, drift-tested) -- chart.js's SUMMARY_STATS plus
  // mean_ci95, which exists only on this surface because R has qt().
  var LANE_STATS = [
    { value: "median_q1_q3", label: "Median · Q1–Q3" },
    { value: "mean_sd", label: "Mean ± SD" },
    { value: "mean_2sd", label: "Mean ± 2 SD" },
    { value: "mean_se", label: "Mean ± SE" },
    { value: "mean_ci95", label: "Mean · 95% CI" },
    { value: "p5_p95", label: "5th–95th percentile" },
    { value: "min_max", label: "Min–Max" }
  ];
  var LANE_WHISKERS = [{ value: "tukey", label: "Tukey (1.5×IQR)" }]
    .concat(LANE_STATS);

  // Glyphs for the columns editor's display tiles, the chart block's icon
  // style (14px, viewBox 16, currentColor, stroke-width 1.4).
  var TYPE_ICONS = {
    bar:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">' +
      '<rect x="2" y="2.5" width="11" height="3"/>' +
      '<rect x="2" y="6.5" width="8" height="3"/>' +
      '<rect x="2" y="10.5" width="5" height="3"/></svg>',
    box:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M1.5 8h3M11.5 8h3M4.5 4.5h7v7h-7zM8 4.5v7"/></svg>',
    pointrange:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M2 8h12M2 5.5v5M14 5.5v5"/>' +
      '<circle cx="8" cy="8" r="2.4" fill="currentColor" stroke="none"/></svg>',
    interval:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="3">' +
      '<path d="M2 5h6M7 11h7"/></svg>',
    sparkline:
      '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.4">' +
      '<path d="M1.5 11l3.5-3 2.5 1.5L11 5l3.5 3"/></svg>'
  };

  var RANK_ROLES = {
    // Mapping — the chart block's labels verbatim (Group / Color / Facet), so
    // the two gears read as the same system. `parent` is the rank-only extra.
    group:  { label: "Group", kind: "column", colType: "cat" },
    // The summarize-table path's grouping vector (outer -> inner, at most
    // two): one row per key combo, the outer becomes the expandable parent.
    by:     { label: "Group by", kind: "columns", colType: "cat",
              placeholder: "add columns…" },
    parent: { label: "Nest under", kind: "column", colType: "cat" },
    color:  { label: "Color", kind: "column", colType: "cat" },
    facet:  { label: "Facet", kind: "column", colType: "cat" },
    // Compare offers LEVELS of the facet column, not columns, so it is a
    // plain select whose options are refreshed from data-rank-cfg.
    compare: { label: "Compare to", kind: "select", options: [] },
    // Extra row columns beside the bar — the chart's tooltip fields, shown
    // as real columns. Offered only for the as-is measure (rankSections):
    // any aggregation has no underlying row to read a column from.
    fields: { label: "More columns", kind: "columns", colType: "any",
              placeholder: "add columns…" },
    func:   { label: "Aggregate", kind: "select", options: FUNC_OPT,
              rerender: true },
    value:  { label: "Of column", kind: "column", colType: "num" },
    id_var: { label: "Count distinct", kind: "column", colType: "any" },
    // Facet column order in the summarize-table mode: each summary's level
    // copies adjacent (the comparison affordance), or level groups spanning
    // the summaries (the Table-1 reading).
    facet_layout: { label: "Facet layout", kind: "segmented", options: [
      { value: "by_summary", label: "By summary" },
      { value: "by_level", label: "By level" }
    ] },
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

  // ==========================================================================
  // The summarize-table columns editor (_blockr.design/open/summarize-table/):
  // one row per summary, typed (simple / dist / field / series / spans /
  // expr), each with its own display and scope. Rendered into the engine's
  // customSections seam; every edit sends the FULL array as one config
  // value. Presets reproduce the single-mark blocks in one click.
  // ==========================================================================

  var SUMMARY_TYPES = {
    simple: { label: "simple", shows: ["bar", "number", "dot"] },
    dist:   { label: "dist", shows: ["box", "pointrange", "text"] },
    field:  { label: "field", shows: ["text"] },
    series: { label: "series", shows: ["sparkline"] },
    spans:  { label: "spans", shows: ["interval"] },
    expr:   { label: "expr", shows: ["text"] }
  };
  var SHOW_ICONS = {
    number: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<text x="1" y="12" font-size="9" fill="currentColor">123</text></svg>',
    text: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<text x="0" y="12" font-size="8" fill="currentColor">1 (2)</text></svg>',
    dot: '<svg width="14" height="14" viewBox="0 0 16 16">' +
      '<circle cx="8" cy="8" r="2.6" fill="currentColor"/></svg>',
    bar: TYPE_ICONS.bar,
    box: TYPE_ICONS.box,
    pointrange: TYPE_ICONS.pointrange,
    interval: TYPE_ICONS.interval,
    sparkline: TYPE_ICONS.sparkline
  };
  // Tile captions: friendlier than the raw enum where it helps.
  var SHOW_LABELS = { pointrange: "dot range", interval: "swimlane" };

  function summaryLine(s) {
    switch (s.type) {
      case "simple": return (s.func || "count") +
        (s.col ? "(" + s.col + ")" : "()") + " · " + (s.show || "bar");
      case "dist": return (s.stat || "median_q1_q3") + "(" + (s.col || "?") +
        ")" + (s.show === "box" ? ", " + (s.whiskers || "tukey") + " whiskers" : "") +
        " · " + (s.show || "box");
      case "field": return (s.col || "?") + " (distinct values)";
      case "series": return (s.col || "?") + " over " + (s.x || "?") +
        (s.band && s.band.length === 2 ? ", band " + s.band[0] + "–" + s.band[1] : "") +
        (s.ref && s.ref !== "none" ? ", ref " + s.ref : "");
      case "spans": return (s.x || "?") + " → " + (s.xend || "?") +
        (s.color ? ", color " + s.color : "") +
        (s.label ? ", label " + s.label : "") +
        (s.size === "lg" ? ", wide" : "");
      case "expr": return s.expr || "";
      default: return "";
    }
  }

  /** First column of a type, for preset seeds. */
  function firstCol(cols, want, skip) {
    for (var i = 0; i < cols.length; i++) {
      var ok = want === "any" || (want === "num"
        ? cols[i].type === "numeric" : cols[i].type !== "numeric");
      if (ok && (!skip || skip.indexOf(cols[i].name) === -1)) return cols[i].name;
    }
    return "";
  }

  var SUMMARY_PRESETS = {
    Rank: function (cols) {
      return { type: "simple", name: "Rows", func: "count", show: "bar" };
    },
    Distribution: function (cols) {
      var c = firstCol(cols, "num");
      return { type: "dist", name: c || "Value", col: c,
               stat: "median_q1_q3", whiskers: "tukey", show: "box" };
    },
    Trajectory: function (cols) {
      var x = firstCol(cols, "num");
      var c = firstCol(cols, "num", [x]);
      return { type: "series", name: c || "Value", x: x, col: c || x };
    },
    Swimlane: function (cols) {
      var x = firstCol(cols, "num");
      var e = firstCol(cols, "num", [x]);
      return { type: "spans", name: "Spans", x: x, xend: e || x };
    }
  };

  /** The editor body. ctx: { cfg(), cols(), send(list), rerender(), open:Set } */
  function renderSummariesEditor(sec, ctx) {
    var cfg = ctx.cfg();
    var cols = ctx.cols();
    var list = Array.isArray(cfg.summaries) ? cfg.summaries : (cfg.summaries = []);
    var S = (typeof Blockr !== "undefined" && Blockr.Select) || null;
    var commit = function () { ctx.send(list.slice()); };

    function colOpts(want) {
      return cols.filter(function (c) {
        return want === "any" ||
          (want === "num" ? c.type === "numeric" : c.type !== "numeric");
      }).map(function (c) { return { value: c.name, label: c.label || c.name }; });
    }
    function selectCtl(parent, label, want, selected, onChange) {
      var ctl = document.createElement("div");
      ctl.className = "lane-sum-ctl";
      var l = document.createElement("span");
      l.className = "blockr-popover-label";
      l.textContent = label;
      ctl.appendChild(l);
      var wrap = document.createElement("div");
      wrap.className = "blockr-popover-select-wrap dd-picker-wrap";
      var opts = typeof want === "string" ? colOpts(want) : want;
      if (S && S.single) {
        S.single(wrap, { options: opts, selected: selected || "",
                         onChange: onChange });
      }
      ctl.appendChild(wrap);
      parent.appendChild(ctl);
    }
    function multiCtl(parent, label, selected, onChange) {
      var ctl = document.createElement("div");
      ctl.className = "lane-sum-ctl";
      var l = document.createElement("span");
      l.className = "blockr-popover-label";
      l.textContent = label;
      ctl.appendChild(l);
      var wrap = document.createElement("div");
      wrap.className = "blockr-popover-select-wrap dd-picker-wrap";
      if (S && S.multi) {
        S.multi(wrap, {
          options: colOpts("any"),
          selected: (selected || []).slice(),
          placeholder: "add columns…",
          onChange: onChange
        });
      }
      ctl.appendChild(wrap);
      parent.appendChild(ctl);
    }
    function segCtl(parent, label, options, selected, onChange) {
      var ctl = document.createElement("div");
      ctl.className = "lane-sum-ctl";
      var l = document.createElement("span");
      l.className = "blockr-popover-label";
      l.textContent = label;
      ctl.appendChild(l);
      var seg = document.createElement("div");
      seg.className = "lane-sum-seg";
      options.forEach(function (o) {
        var b = document.createElement("button");
        b.type = "button";
        b.className = "lane-sum-seg-btn" + (o.value === selected ? " is-on" : "");
        b.textContent = o.label;
        b.addEventListener("click", function () {
          onChange(o.value);
        });
        seg.appendChild(b);
      });
      ctl.appendChild(seg);
      parent.appendChild(ctl);
    }

    // dd-summaries: the engine's full-width escape from the section's
    // narrow grid cells (the metrics editor's own trick).
    var wrap = document.createElement("div");
    wrap.className = "dd-summaries lane-summaries";

    list.forEach(function (s, i) {
      var row = document.createElement("div");
      row.className = "lane-sum-row" + (ctx.open.has(i) ? " is-open" : "");

      var head = document.createElement("div");
      head.className = "lane-sum-head";
      head.innerHTML =
        '<span class="lane-sum-chip lane-sum-chip-' + (s.type || "simple") +
        '">' + (SUMMARY_TYPES[s.type] || SUMMARY_TYPES.simple).label + "</span>" +
        '<span class="lane-sum-name">' + esc(s.name || "") + "</span>" +
        '<span class="lane-sum-line">' + esc(summaryLine(s)) + "</span>";
      var rm = document.createElement("button");
      rm.type = "button";
      rm.className = "dd-role-remove lane-sum-rm";
      rm.title = "Remove column";
      rm.innerHTML = "✕";
      rm.addEventListener("click", function (e) {
        e.stopPropagation();
        list.splice(i, 1);
        ctx.open.delete(i);
        commit();
        ctx.rerender();
      });
      head.appendChild(rm);
      var up = document.createElement("button");
      up.type = "button";
      up.className = "lane-sum-move";
      up.title = "Move up";
      up.innerHTML = "↑";
      up.disabled = i === 0;
      up.addEventListener("click", function (e) {
        e.stopPropagation();
        list.splice(i - 1, 0, list.splice(i, 1)[0]);
        commit();
        ctx.rerender();
      });
      head.appendChild(up);
      head.addEventListener("click", function () {
        if (ctx.open.has(i)) ctx.open.delete(i);
        else ctx.open.add(i);
        ctx.rerender();
      });
      row.appendChild(head);

      if (ctx.open.has(i)) {
        var body = document.createElement("div");
        body.className = "lane-sum-body";
        // Name.
        var nameCtl = document.createElement("div");
        nameCtl.className = "lane-sum-ctl";
        nameCtl.innerHTML = '<span class="blockr-popover-label">Name</span>';
        var nameIn = document.createElement("input");
        nameIn.type = "text";
        nameIn.className = "lane-sum-name-input";
        nameIn.value = s.name || "";
        nameIn.addEventListener("change", function () {
          s.name = nameIn.value;
          commit();
          ctx.rerender();
        });
        nameCtl.appendChild(nameIn);
        body.appendChild(nameCtl);

        var t = s.type || "simple";
        if (t === "simple") {
          // The full aggregate set INCLUDING identity ("None (as is)"): a
          // value already reduced upstream — one row per group — drawn
          // as-is, as a bar or a number. The chart block's duality.
          selectCtl(body, "Function", FUNC_OPT, s.func || "count",
            function (v) {
              s.func = v;
              commit();
              ctx.rerender();
            });
          if ((s.func || "count") !== "count") {
            selectCtl(body,
              s.func === "count_distinct" ? "Distinct of" : "Of column",
              s.func === "count_distinct" ? "any" : "num",
              s.col, function (v) { s.col = v; commit(); ctx.rerender(); });
          }
        } else if (t === "dist") {
          selectCtl(body, "Value", "num", s.col, function (v) {
            s.col = v;
            commit();
            ctx.rerender();
          });
          selectCtl(body, "Statistic", LANE_STATS, s.stat || "median_q1_q3",
            function (v) { s.stat = v; commit(); ctx.rerender(); });
          if ((s.show || "box") === "box") {
            selectCtl(body, "Whiskers", LANE_WHISKERS, s.whiskers || "tukey",
              function (v) { s.whiskers = v; commit(); ctx.rerender(); });
          }
        } else if (t === "field") {
          selectCtl(body, "Column", "any", s.col, function (v) {
            s.col = v;
            commit();
            ctx.rerender();
          });
        } else if (t === "series") {
          selectCtl(body, "Order (x)", "num", s.x, function (v) {
            s.x = v;
            commit();
            ctx.rerender();
          });
          selectCtl(body, "Value (y)", "num", s.col, function (v) {
            s.col = v;
            commit();
            ctx.rerender();
          });
          var band = (s.band && s.band.length === 2) ? s.band : ["", ""];
          selectCtl(body, "Band low", "num", band[0], function (v) {
            s.band = [v, band[1]];
            commit();
            ctx.rerender();
          });
          selectCtl(body, "Band high", "num", band[1], function (v) {
            s.band = [band[0], v];
            commit();
            ctx.rerender();
          });
          // The computed reference: pooled orientation line/band, computed
          // in R over the column's values (per facet level under facet).
          selectCtl(body, "Reference", [
            { value: "none", label: "None" },
            { value: "mean", label: "Mean" },
            { value: "mean_sd", label: "Mean ± SD" },
            { value: "median_iqr", label: "Median · IQR" }
          ], s.ref || "none", function (v) {
            s.ref = v;
            commit();
            ctx.rerender();
          });
        } else if (t === "spans") {
          selectCtl(body, "Start", "num", s.x, function (v) {
            s.x = v;
            commit();
            ctx.rerender();
          });
          selectCtl(body, "End", "num", s.xend, function (v) {
            s.xend = v;
            commit();
            ctx.rerender();
          });
          selectCtl(body, "Color", "cat", s.color, function (v) {
            s.color = v;
            commit();
            ctx.rerender();
          });
          // Event identity on hover: label headlines the segment tooltip
          // and keys the same-event highlight; fields append extras.
          selectCtl(body, "Label (hover)", "any", s.label, function (v) {
            s.label = v;
            commit();
            ctx.rerender();
          });
          multiCtl(body, "Tooltip fields", s.fields, function (vals) {
            s.fields = vals;
            commit();
            ctx.rerender();
          });
          segCtl(body, "Width", [
            { value: "md", label: "Regular" },
            { value: "lg", label: "Wide" }
          ], s.size || "md", function (v) {
            s.size = v;
            commit();
            ctx.rerender();
          });
        } else if (t === "expr") {
          var exCtl = document.createElement("div");
          exCtl.className = "lane-sum-ctl lane-sum-ctl-wide";
          exCtl.innerHTML =
            '<span class="blockr-popover-label">Expression</span>';
          var exIn = document.createElement("input");
          exIn.type = "text";
          exIn.className = "lane-sum-name-input";
          exIn.value = s.expr || "";
          exIn.placeholder = "e.g. sd(DUR)/mean(DUR) * 100";
          exIn.addEventListener("change", function () {
            s.expr = exIn.value;
            commit();
            ctx.rerender();
          });
          exCtl.appendChild(exIn);
          body.appendChild(exCtl);
        }

        // Display tiles (only where the type offers a choice).
        var shows = (SUMMARY_TYPES[t] || {}).shows || [];
        if (shows.length > 1) {
          var dCtl = document.createElement("div");
          dCtl.className = "lane-sum-ctl";
          dCtl.innerHTML = '<span class="blockr-popover-label">Display</span>';
          var tiles = document.createElement("div");
          tiles.className = "dd-type-grid lane-sum-tiles";
          shows.forEach(function (sh) {
            var b = document.createElement("button");
            b.type = "button";
            b.className = "dd-type-tile" +
              ((s.show || shows[0]) === sh ? " dd-type-active" : "");
            b.innerHTML = '<span class="dd-type-tile-icon">' +
              (SHOW_ICONS[sh] || "") + '</span>' +
              '<span class="dd-type-tile-label">' +
              (SHOW_LABELS[sh] || sh) + "</span>";
            b.addEventListener("click", function () {
              s.show = sh;
              commit();
              ctx.rerender();
            });
            tiles.appendChild(b);
          });
          dCtl.appendChild(tiles);
          body.appendChild(dCtl);
        }

        // Scope: fields are group facts (always pooled), everything else
        // picks. Inert without a facet, semantic with one.
        if (t !== "field") {
          segCtl(body, "Scope", [
            { value: "cell", label: "Per level" },
            { value: "pooled", label: "Overall" }
          ], s.scope || "cell", function (v) {
            s.scope = v;
            commit();
            ctx.rerender();
          });
        }
        row.appendChild(body);
      }
      wrap.appendChild(row);
    });

    // Add + presets.
    var addRow = document.createElement("div");
    addRow.className = "lane-sum-addrow";
    var addWrap = document.createElement("div");
    addWrap.className = "lane-sum-add-types";
    Object.keys(SUMMARY_TYPES).forEach(function (t) {
      var b = document.createElement("button");
      b.type = "button";
      b.className = "lane-sum-add";
      b.textContent = "+ " + t;
      b.addEventListener("click", function () {
        var s = { type: t, name: "", scope: "cell",
                  show: SUMMARY_TYPES[t].shows[0] };
        if (t === "simple") s.func = "count";
        if (t === "dist") {
          s.col = firstCol(cols, "num");
          s.stat = "median_q1_q3";
          s.whiskers = "tukey";
        }
        if (t === "field") s.col = firstCol(cols, "cat");
        if (t === "series") {
          s.x = firstCol(cols, "num");
          s.col = firstCol(cols, "num", [s.x]);
        }
        if (t === "spans") {
          s.x = firstCol(cols, "num");
          s.xend = firstCol(cols, "num", [s.x]);
        }
        list.push(s);
        ctx.open.add(list.length - 1);
        commit();
        ctx.rerender();
      });
      addWrap.appendChild(b);
    });
    addRow.appendChild(addWrap);
    var presets = document.createElement("div");
    presets.className = "lane-sum-presets";
    Object.keys(SUMMARY_PRESETS).forEach(function (nm) {
      var b = document.createElement("button");
      b.type = "button";
      b.className = "lane-sum-preset";
      b.textContent = nm;
      b.title = "Append a preconfigured " + nm.toLowerCase() + " column";
      b.addEventListener("click", function () {
        list.push(SUMMARY_PRESETS[nm](cols));
        commit();
        ctx.rerender();
      });
      presets.appendChild(b);
    });
    addRow.appendChild(presets);
    wrap.appendChild(addRow);

    if (!list.length) {
      var hint = document.createElement("div");
      hint.className = "lane-sum-hint";
      hint.textContent = "Adding columns switches the block to the " +
        "multi-column table and replaces the single mark above.";
      wrap.appendChild(hint);
    }
    sec.appendChild(wrap);
  }

  // Which controls apply depends on the picks: `Of column` only for the
  // aggregations that reduce a column, `Count distinct` only for
  // count_distinct, the split layout only with a colour split, Compare only
  // with a facet. Conditional rows beat a wall of inert ones.
  function rankSections(cfg, ctx) {
    var mapping = [];
    var optional = [];
    var pres = ["sort_by", "sort_dir"];
    var aggTitle = null;

    // The summarize-table mode: the column list IS the config. Grouping
    // (`by`) and facet below it; the flat mark mappings never render.
    if (Array.isArray(cfg.summaries) && cfg.summaries.length) {
      return {
        requiredMap: ["by"],
        optionalMap: ["facet"],
        mapping: [],
        aggTitle: null,
        customSections: ctx ? [{
          title: "Columns",
          render: function (sec) { renderSummariesEditor(sec, ctx); }
        }] : [],
        presentation: cfg.facet
          ? ["sort_by", "sort_dir", "facet_layout", "search"]
          : ["sort_by", "sort_dir", "search"],
        drillToggle: "drill",
        drillDefault: (cfg.by && cfg.by.length)
          ? cfg.by[cfg.by.length - 1] : (cfg.group || ""),
        ctrlSection: true,
        drillHint: cfg.drill
          ? "Clicking a row filters downstream on " + cfg.drill + "."
          : "Clicking a row filters downstream blocks to that row.",
        titles: ["title", "subtitle", "caption"]
      };
    }

    // The ranked-bar surface (the block's original form, kept for boards
    // that never opt into the column list): func / value / id_var, colour
    // split, facet, comparison. Every other glyph lives in the columns
    // editor above -- the type belongs to the COLUMN.
    var needsValue = ["identity", "sum", "mean", "median", "min", "max"]
      .indexOf(cfg.func) > -1;
    mapping = ["func"];
    if (needsValue) mapping.push("value");
    if (cfg.func === "count_distinct") mapping.push("id_var");
    optional = ["parent", "color", "facet"];
    if (cfg.facet) optional.push("compare");
    if (cfg.func === "identity") optional.push("fields");
    // color + facet compose (split bars inside each facet column), so the
    // split layout applies whenever a colour split exists — except under a
    // comparison, which owns the colour slot.
    if (cfg.color && !cfg.compare) pres.push("bar_mode");
    // The measure is the aggregation step, so it gets the chart's trailing
    // "Aggregation" section rather than sitting inside Mapping.
    aggTitle = "Aggregation";
    pres.push("search");

    return {
      requiredMap: ["group"],
      optionalMap: optional,
      mapping: mapping,
      aggTitle: aggTitle,
      // The entry point into the summarize-table mode: an empty column
      // list with the add/preset row. Adding the first column commits a
      // non-empty `summaries` and this spec switches to the branch above.
      customSections: ctx ? [{
        title: "Columns",
        render: function (sec) { renderSummariesEditor(sec, ctx); }
      }] : [],
      presentation: pres,
      drillToggle: "drill",
      drillDefault: cfg.group || "",
      // "Send to filter (beta)" inside the open Drill-down section, chart /
      // table parity: the engine reads cfg.ctrl_target / cfg.ctrl_choices.
      ctrlSection: true,
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
    ["group", "parent", "color", "facet", "compare", "value", "id_var",
     "drill"]
      .forEach(function (k) { if (cfg[k] == null) cfg[k] = ""; });
    // The summarize-table config: the column list and the grouping vector.
    if (!Array.isArray(cfg.summaries)) {
      cfg.summaries = cfg.summaries ? [cfg.summaries] : [];
    }
    if (!Array.isArray(cfg.by)) cfg.by = cfg.by ? [String(cfg.by)] : [];
    if (!cfg.facet_layout) cfg.facet_layout = "by_summary";
    // The multi-column picker wants an array.
    if (!Array.isArray(cfg.fields)) {
      cfg.fields = cfg.fields ? [String(cfg.fields)] : [];
    }
    // The ctrl-send tail wants a string target and an array of choices.
    if (cfg.ctrl_target == null) cfg.ctrl_target = "";
    if (!Array.isArray(cfg.ctrl_choices)) cfg.ctrl_choices = [];
    RANK_ROLES.compare.options = levels;
    // Sort targets: the measure / the name, plus facet levels, plus (in the
    // summarize-table mode) each summary column by its name.
    var sumNames = (cfg.summaries || []).map(function (s) {
      return s && s.name ? { value: s.name, label: s.name } : null;
    }).filter(Boolean);
    RANK_ROLES.sort_by.options = [
      { value: "value", label: "Measure" },
      // The order the data itself carries -- factor levels, else
      // first-appearance in the rows (chart parity: "Data order"). A visit
      // or dose-group table reads wrong alphabetically.
      { value: "data", label: "Data order" },
      { value: "label", label: "Name" }
    ].concat(sumNames)
      .concat(levels.map(function (lv) { return { value: lv, label: lv }; }))
      // Numeric columns of the input (the engine expands "#num"): the row's
      // MINIMUM of that column orders the table. AVISITN is the ordering a
      // character AVISIT does not carry.
      .concat(["#num"]);
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
    if (!elemId) return;
    // The table arrives with the first payload, after this chrome: start from
    // whatever is there (possibly nothing). State is re-read ONLY on popover
    // open (followed by engine.refresh(), which rebuilds every control
    // against the fresh objects) -- the table block's contract. NEVER on a
    // payload: the engine's controls capture the cfg OBJECT at build time
    // (drilldown-config.js `const cfg = this._cfg()`), so reassigning it
    // between builds orphans them -- their edits then land in the old object
    // while onChange reads the new one and transmits the STALE value (the
    // second-gear-edit-after-a-render bug).
    var table = root.querySelector("table.blockr-rank-table");
    var st = table ? readGearState(table) : { cfg: {}, cols: [] };
    var cfg = st.cfg;
    var cols = st.cols;

    // Context for the summaries editor (the customSections seam): live
    // accessors, the transport, a re-render hook (assigned after the
    // engine exists) and the per-row expansion memory.
    var ctx = {
      cfg: function () { return cfg; },
      cols: function () { return cols; },
      send: function (list) { sendConfig(elemId, "summaries", list); },
      rerender: function () {},
      open: new Set()
    };

    var header = document.createElement("div");
    header.className = "blockr-gear-header";
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "blockr-gear-btn";
    btn.title = "Summarize table settings";
    btn.setAttribute("aria-label", "Summarize table settings");
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
    var engine;
    engine = new DDC({
      popoverEl: function () { return pop; },
      roles: RANK_ROLES,
      config: function () { return cfg; },
      columns: function () { return cols; },
      context: function () { return "all"; },
      currentType: function () { return null; },
      sections: function () { return rankSections(cfg, ctx); },
      sectionsForFamily: function () { return rankSections(cfg, ctx); },
      secondary: new Set(),
      mappingTitle: function () {
        return (cfg.summaries && cfg.summaries.length) ? "Grouping" : "Mapping";
      },
      // No block-level type picker: the type belongs to the COLUMN (the
      // columns editor's row types + display tiles).
      typeKey: null,
      typeGroups: null,
      familyFor: null,
      entryRequired: function (role) {
        return role === "by" || role === "group";
      },
      drillAutoLabel: null,
      title: "Summarize table settings",
      onChange: function (key) {
        sendConfig(elemId, key, cfg[key]);
        // The ranked default is descending; the data's own order read
        // backwards is exactly what "data order" is asked for to avoid, so
        // the pick carries the direction with it. One control change, two
        // params sent -- and the direction stays a control, so reverse
        // chronological is still one click away.
        if (key === "sort_by" && cfg.sort_by === "data" &&
              cfg.sort_dir !== "asc") {
          cfg.sort_dir = "asc";
          sendConfig(elemId, "sort_dir", "asc");
          engine.render();
        }
      },
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
    ctx.rerender = function () { engine.render(); };
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
    // A payload that arrived before this container existed waits in the store;
    // a container re-created later (dock panel re-mount, view switch) renders
    // from it with no R round trip.
    var stored = storedFor(root);
    if (stored) {
      applyPayload(root, stored);
    } else {
      applyVisibility(root);
      // Nothing for us yet: either the payload has not been built, or it was
      // pushed before this script existed (Shiny drops a custom message with
      // no registered handler). Announce, and let R re-send.
      var id = root.getAttribute("data-rank-elem-id");
      if (id && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(id + "_ready", Date.now(), { priority: "event" });
      }
    }
  }

  function scan(scope) {
    var host = scope && scope.querySelectorAll ? scope : document;
    host.querySelectorAll(".blockr-rank-container").forEach(bind);
  }

  // ---------- hover readout ----------
  // One fixed tooltip fed by ONE delegated listener for every lane cell on
  // the page: the inverse of the percentage layout. Over an interval segment
  // the payload's exact span wins (data-tip); over the empty track it is the
  // approximate domain value under the cursor (day = fraction × domain).
  // Sparklines snap to the nearest point and read the shipped display
  // values. No per-row handlers, so cost is flat in the row count.
  var laneTip = null;
  // Same-event hover state: the currently highlighted label, cleared when
  // the cursor leaves the segments.
  var laneHl = { root: null, label: null };
  function laneHighlight(root, label) {
    if (laneHl.label === label && laneHl.root === root) return;
    if (laneHl.root) {
      laneHl.root.classList.remove("seg-hover");
      laneHl.root.querySelectorAll(".lane-seg.is-same").forEach(function (s) {
        s.classList.remove("is-same");
      });
    }
    laneHl = { root: null, label: null };
    if (root && label) {
      root.classList.add("seg-hover");
      root.querySelectorAll(".lane-seg").forEach(function (s) {
        if (s.getAttribute("data-l") === label) s.classList.add("is-same");
      });
      laneHl = { root: root, label: label };
    }
  }
  function tipEl() {
    if (!laneTip) {
      laneTip = document.createElement("div");
      laneTip.className = "blockr-lane-tip";
      laneTip.hidden = true;
      document.body.appendChild(laneTip);
    }
    return laneTip;
  }
  function fmtDomain(v, isDate) {
    if (isDate) {
      // Days since epoch (R Date semantics).
      return new Date(Math.round(v) * 86400000).toISOString().slice(0, 10);
    }
    return String(Math.round(v * 10) / 10);
  }
  document.addEventListener("mousemove", function (e) {
    var t = /** @type {Element} */ (e.target);
    if (!t || !t.closest) { return; }
    var lane = t.closest(".blockr-rank-ivcell, .blockr-rank-spcell");
    var tip = tipEl();
    if (!lane) {
      tip.hidden = true;
      laneHighlight(null, null);
      return;
    }
    var r = lane.getBoundingClientRect();
    if (!r.width) { tip.hidden = true; return; }
    var fx = Math.min(Math.max((e.clientX - r.left) / r.width, 0), 1);
    var txt = "";
    if (lane.classList.contains("blockr-rank-ivcell")) {
      var seg = t.closest(".lane-seg");
      // Same-event highlight: hovering a labelled segment dims every other
      // segment in the table and lifts the matches -- the table's answer
      // to cross-lane identity emphasis, keyed on data-l.
      laneHighlight(lane.closest(".blockr-rank-container"),
                    seg ? seg.getAttribute("data-l") : null);
      if (seg) {
        txt = seg.getAttribute("data-tip") || "";
      } else {
        var d0 = parseFloat(lane.getAttribute("data-d0"));
        var d1 = parseFloat(lane.getAttribute("data-d1"));
        if (isNaN(d0) || isNaN(d1)) { tip.hidden = true; return; }
        txt = "~" + fmtDomain(d0 + fx * (d1 - d0),
          lane.getAttribute("data-dd") === "1");
      }
    } else {
      var xs = (lane.getAttribute("data-xs") || "").split(",");
      var ys = (lane.getAttribute("data-ys") || "").split(",");
      if (!xs.length || xs[0] === "") { tip.hidden = true; return; }
      var i = Math.round(fx * (xs.length - 1));
      txt = xs[i] + " · " + ys[i];
    }
    if (!txt) { tip.hidden = true; return; }
    // getAttribute already decoded the escaped payload text.
    tip.textContent = txt;
    tip.hidden = false;
    tip.style.left = Math.min(e.clientX + 12,
      window.innerWidth - tip.offsetWidth - 8) + "px";
    tip.style.top = (e.clientY + 16) + "px";
  });
  // Leaving the window (or scrolling the lane away) must not strand the tip
  // or the highlight.
  document.addEventListener("mouseout", function (e) {
    if (!e.relatedTarget) {
      if (laneTip) laneTip.hidden = true;
      laneHighlight(null, null);
    }
  });
  document.addEventListener("scroll", function () {
    if (laneTip) laneTip.hidden = true;
  }, true);

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
