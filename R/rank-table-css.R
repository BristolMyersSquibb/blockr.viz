# Ranked bar table: the CSS delta on top of the shared html-table rules.
#
# Only what the bars, the legend, the expand caret and the footer add. Type,
# padding, hover, sticky header and the scroll shadow all come from
# html_table_shared_css_fallback(), so a rank table and a table block are the
# same object with a different cell.
#
# Colors read blockr.theme's --blockr-* tokens with a fallback, so a themed
# board restyles the bars without touching this file. The diverging pair is a
# warm/cool pair with a neutral zero tick (polarity, not identity).

#' @noRd
rank_table_css <- function() {
  "
.blockr-rank-container {
  --blockr-rank-fill: var(--blockr-color-primary, #2a78d6);
  --blockr-rank-sub: color-mix(in srgb, var(--blockr-rank-fill) 45%, transparent);
  /* The lane track. The design token alone (#eeeeea) is so close to the
     surface that an empty lane reads as nothing at all -- and on a box or a
     dot range the track IS the axis the glyph is read against, so it has to
     be visible. Mixed toward the border token: still recessive, no longer
     invisible. */
  --blockr-rank-track: color-mix(in srgb,
                                 var(--blockr-color-bg-subtle, #eeeeea) 80%,
                                 var(--blockr-color-text-subtle, #898781));
  --blockr-rank-pos: var(--blockr-color-danger, #d03b3b);
  --blockr-rank-neg: var(--blockr-color-primary, #2a78d6);
  --blockr-rank-tick: var(--blockr-color-border, #c3c2b7);
}
/* Title / subtitle / caption: the canonical .dd-table-* bands, styled by
   inst/css/table.css (shipped with the table dep). Nothing to add here. */
.blockr-rank-table { width: 100%; }
/* Column widths ride on the CELLS: the header cells come from the table
   block's dt_th(), so they carry its classes, not ours. */
.blockr-rank-table td.blockr-rank-bar-col {
  width: 26%;
  min-width: 110px;
}
.blockr-rank-table td.blockr-rank-num {
  text-align: right;
  white-space: nowrap;
  font-variant-numeric: tabular-nums;
}
/* Raw field columns (the as-is measure's extra row columns): plain text,
   left-aligned like the label column, never numeric-formatted. */
.blockr-rank-table td.blockr-rank-txt { white-space: nowrap; }
.blockr-rank-table td.blockr-rank-label-col { white-space: nowrap; }
/* The GLYPH columns own the slack. Auto table layout hands leftover width to
   the unconstrained cells, which meant a wide panel only stretched the label
   column while the bars and boxes stayed at their 26%. width:1% is the
   shrink-to-fit idiom (with nowrap, the cell takes its content width and no
   more), so widening the block lengthens the visuals instead. Label and text
   cells still cap out and ellipsis rather than pushing the glyphs off. */
.blockr-rank-table th.blockr-stub-header,
.blockr-rank-table td.blockr-rank-label-col,
.blockr-rank-table td.blockr-rank-txt,
.blockr-rank-table td.blockr-rank-num { width: 1%; }
.blockr-rank-table td.blockr-rank-label-col,
.blockr-rank-table td.blockr-rank-txt {
  max-width: 260px;
  overflow: hidden;
  text-overflow: ellipsis;
}
/* The in-bar value label: track left, value right in a FIXED slot (one width
   per column, in ch) so every row's track spans the same range -- a varying
   label width would silently rescale the bars against each other. */
.blockr-rank-barwrap {
  display: flex;
  align-items: center;
  gap: 8px;
}
.blockr-rank-barwrap .blockr-rank-track,
.blockr-rank-barwrap .blockr-rank-dv,
.blockr-rank-barwrap .blockr-rank-lane { flex: 1 1 auto; min-width: 0; }
.blockr-rank-barval {
  flex: 0 0 auto;
  text-align: right;
  white-space: nowrap;
  font-variant-numeric: tabular-nums;
}
.blockr-rank-table .blockr-rank-pct {
  color: var(--blockr-color-text-subtle, #898781);
}
/* Sorting affordance: none of our own. The header cells are dt_th()'s, so the
   .blockr-sortable cursor and the .blockr-sort-icon arrow come from the shared
   table CSS and read exactly like a table block's. */

/* The by_level facet layout's spanning header row (one cell per facet
   level over its summary group) -- centred, with a hairline under the
   span so the group reads as one unit. */
.blockr-rank-table th.blockr-th-group {
  text-align: center;
  border-bottom: 1px solid var(--blockr-color-border, #e1e0d9);
}

/* Bars: SQUARE, and segments TOUCH -- the house bar style, taken from the chart
   block rather than invented here. echarts sets no borderRadius on any bar
   series (chart.js even records dropping a lone rounded override, which had
   made the waterfall the only rounded chart), stacks
   with `stack:'stack'` and no separating border, and groups with `barGap: 0` so
   a group's bars touch. A rank table sitting next to a chart of the same data
   has to read as the same mark, so: no rounded data-end, no surface gap between
   segments, no gap between grouped bars.

   The grey track stays: it is a table-cell affordance (it says what the row's
   share is against the column max) with no echarts equivalent, and the
   crossfilter block in blockr.dm already draws track + fill. */
.blockr-rank-track {
  display: flex;
  gap: 0;
  /* 12px = the shared lane height: bars, boxes, dot ranges and swimlanes
     line up across columns. */
  height: 12px;
  background: var(--blockr-rank-track);
}
.blockr-rank-track.is-tall {
  height: auto;
  flex-direction: column;
  gap: 2px;
  background: none;
}
.blockr-rank-track.is-tall .blockr-rank-row3 {
  height: 6px;
  background: var(--blockr-rank-track);
}
.blockr-rank-fill {
  height: 100%;
  min-width: 2px;
  border-radius: 0;
  background: var(--blockr-rank-fill);
}
.blockr-rank-track.is-sub .blockr-rank-fill { background: var(--blockr-rank-sub); }

/* Lane marks (box / point range / interval / sparkline): absolutely
   positioned glyphs inside a track-coloured lane, percentage geometry
   computed in R. Same fill/track/sub tokens as the bars, so a themed board
   restyles every mark together. ONE height (matching the bar track) so a
   bar column, a box column and a swimlane read as one system; only the
   sparkline is taller (amplitude needs room). */
.blockr-rank-lane {
  position: relative;
  height: 12px;
  background: var(--blockr-rank-track);
}
.blockr-rank-lane i { position: absolute; }
/* Colour-split distribution cell: the levels stack INSIDE the cell, so the
   column stays one column and the row keeps its height (two 12px lanes plus
   the gap still fit the 40px the sparkline already claims). Three or more
   levels share the same budget by thinning. */
.blockr-rank-multi {
  display: flex;
  flex-direction: column;
  justify-content: center;
  gap: 2px;
}
.blockr-rank-multi .blockr-rank-lv {
  min-width: 0;
  /* The level's colour arrives as --blockr-rank-fill on this element. The
     TRANSLUCENT token is derived from the fill, so it has to be re-derived
     here as well -- otherwise every level's box body keeps the column
     default and only the whiskers and the median tick take the colour. */
  --blockr-rank-sub: color-mix(in srgb, var(--blockr-rank-fill) 45%,
                               transparent);
}
.blockr-rank-multi .blockr-rank-lv:nth-child(n+3) .blockr-rank-lane,
.blockr-rank-multi .blockr-rank-lv:nth-child(n+3) ~ .blockr-rank-lv
  .blockr-rank-lane { height: 8px; }
/* Box: whiskers OUTSIDE the body only (two segments), caps, a translucent
   body, a solid median tick. */
.blockr-rank-boxcell .lane-wh {
  top: 50%;
  height: 1px;
  margin-top: -0.5px;
  background: var(--blockr-rank-fill);
}
.blockr-rank-boxcell .lane-cap {
  top: 3px;
  bottom: 3px;
  width: 1px;
  background: var(--blockr-rank-fill);
}
.blockr-rank-boxcell .lane-box {
  top: 1px;
  bottom: 1px;
  background: var(--blockr-rank-sub);
}
.blockr-rank-boxcell .lane-med {
  top: 0;
  bottom: 0;
  width: 2px;
  background: var(--blockr-rank-fill);
}
/* Point range: a 2px interval line and a ringed center dot (the ring
   separates the dot from its own line). */
.blockr-rank-prcell .lane-rng {
  top: 50%;
  height: 2px;
  margin-top: -1px;
  background: var(--blockr-rank-fill);
}
.blockr-rank-prcell .lane-ctr {
  top: 50%;
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--blockr-rank-fill);
  transform: translate(-50%, -50%);
  box-shadow: 0 0 0 2px var(--blockr-color-bg, #fff);
}
/* Interval: the swimlane. Square segments, colour = the mapped level. */
.blockr-rank-ivcell .lane-seg {
  top: 0;
  bottom: 0;
  min-width: 2px;
}
/* The exhibit form (a spans row's size = lg): a WIDER column for boards
   where the swimlane is the centerpiece -- more horizontal resolution for
   the spans, not more height. */
.blockr-rank-table td.blockr-rank-wide {
  width: 55%;
  min-width: 320px;
}
/* Same-event emphasis, from hover (seg-hover + is-same) or from a live
   search query (seg-search + is-hit). NOT opacity: translucency stacks
   multiplicatively, so a subject with many overlapping events re-darkens
   however low the alpha. Instead the non-matches are repainted with ONE
   flat opaque near-track shade (!important beats the inline fill) --
   identical opaque rectangles overlap invisibly, so only the matched
   event carries colour no matter how dense the timeline is. */
.blockr-rank-container.seg-hover .lane-seg:not(.is-same),
.blockr-rank-container.seg-search .lane-seg:not(.is-hit) {
  background: color-mix(in srgb, var(--blockr-rank-track) 88%,
                        var(--blockr-rank-tick)) !important;
  transition: background 0.1s ease;
}
/* Sparkline: one inline SVG per cell, band under line, last-value dot.
   Taller than the other lanes -- and the trajectory USES the row: the
   cell keeps a token 1px of vertical padding (a line rarely touches the
   extremes), so a sparkline row stays close to a text row's height
   instead of paying 36px plus full text padding. */
.blockr-rank-table td:has(.blockr-rank-spcell) {
  padding-top: 1px;
  padding-bottom: 1px;
}
.blockr-rank-spcell {
  /* Row height minus the two 1px paddings: the trajectory occupies the
     WHOLE row (the svg stretches freely; viewBox geometry is
     percentage-based and the stroke is non-scaling). */
  height: 40px;
  background: none;
}
.blockr-rank-spcell svg {
  display: block;
  width: 100%;
  height: 100%;
}
.blockr-rank-spcell .lane-band { fill: var(--blockr-rank-track); }
/* The computed reference (a series row's `ref` option): a dashed pooled
   center line, optionally a dispersion band under everything. */
.blockr-rank-spcell .lane-refband {
  fill: color-mix(in srgb, var(--blockr-rank-fill) 10%, transparent);
}
.blockr-rank-spcell .lane-refline {
  stroke: var(--blockr-rank-tick);
  stroke-width: 1;
  stroke-dasharray: 3 2;
}
.blockr-rank-spcell .lane-ln {
  fill: none;
  stroke: var(--blockr-rank-fill);
  stroke-width: 1.6;
}
.blockr-rank-spcell .lane-dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: var(--blockr-rank-fill);
  transform: translate(-50%, -50%);
  box-shadow: 0 0 0 2px var(--blockr-color-bg, #fff);
}
/* The summarize-table columns editor (the gear's custom section): one row
   per summary, expand to edit. Chips are categorical identity of the ROW
   TYPE (muted pastels, not the data palette). */
.lane-summaries { display: flex; flex-direction: column; gap: 5px; width: 100%; }
.lane-sum-row {
  border: 1px solid var(--blockr-color-border, #e1e0d9);
  border-radius: 5px;
  background: var(--blockr-color-bg, #fff);
}
.lane-sum-head {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 6px 9px;
  cursor: pointer;
  min-width: 0;
}
.lane-sum-chip {
  font-size: 0.64rem;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  border-radius: 3px;
  padding: 2px 6px;
  flex: none;
}
.lane-sum-chip-simple { background: #e3edfa; color: #1d5cab; }
.lane-sum-chip-dist { background: #ece5f7; color: #5b3b9e; }
.lane-sum-chip-field { background: #efeee8; color: #6b6a63; }
.lane-sum-chip-series { background: #e0f0ee; color: #17635a; }
.lane-sum-chip-spans { background: #fbeadd; color: #9a5416; }
.lane-sum-chip-expr { background: #f6e8ec; color: #93314f; }
.lane-sum-name { font-weight: 500; font-size: 0.82rem; flex: none; }
.lane-sum-line {
  color: var(--blockr-color-text-subtle, #898781);
  font-size: 0.76rem;
  flex: 1 1 auto;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.lane-sum-rm, .lane-sum-move {
  border: 0;
  background: none;
  color: #b6b4aa;
  cursor: pointer;
  padding: 0;
  flex: none;
  /* A real hit target: the glyphs are small, the button must not be. */
  min-width: 24px;
  min-height: 24px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}
.lane-sum-rm:hover { color: var(--blockr-color-danger, #d03b3b); }
.lane-sum-move:hover { color: var(--blockr-color-text-primary, #111827); }
.lane-sum-move:disabled { opacity: 0.3; cursor: default; }
.lane-sum-body {
  border-top: 1px solid var(--blockr-color-bg-subtle, #f0efe9);
  padding: 9px 11px 10px;
  display: flex;
  flex-wrap: wrap;
  gap: 10px 14px;
}
.lane-sum-ctl { display: flex; flex-direction: column; gap: 3px; }
.lane-sum-ctl-wide { flex: 1 1 100%; }
.lane-sum-name-input {
  height: 28px;
  border: 1px solid var(--blockr-color-border, #e1e0d9);
  border-radius: 4px;
  padding: 0 8px;
  font: inherit;
  font-size: 0.8rem;
  background: var(--blockr-color-bg-input, #f9fafb);
}
.lane-sum-seg {
  display: inline-flex;
  border: 1px solid var(--blockr-color-border, #e1e0d9);
  border-radius: 4px;
  overflow: hidden;
}
.lane-sum-seg-btn {
  border: 0;
  background: var(--blockr-color-bg, #fff);
  color: var(--blockr-color-text-muted, #52514e);
  font-size: 0.76rem;
  padding: 4px 10px;
  cursor: pointer;
  border-left: 1px solid var(--blockr-color-border, #e1e0d9);
}
.lane-sum-seg-btn:first-child { border-left: 0; }
.lane-sum-seg-btn.is-on {
  background: var(--blockr-color-primary, #2a78d6);
  color: #fff;
}
/* Display tiles: outside the engine's type grid the tiles shrink to their
   caption (bar collapsed to 30px), so give them the grid's footprint. */
.lane-sum-tiles { display: flex; gap: 5px; }
.lane-sum-tiles .dd-type-tile { min-width: 64px; }
.lane-sum-addrow {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  align-items: center;
  margin-top: 7px;
  justify-content: space-between;
}
.lane-sum-add-types { display: flex; flex-wrap: wrap; gap: 5px; }
.lane-sum-add {
  border: 1px solid var(--blockr-color-border, #e1e0d9);
  border-radius: 4px;
  background: var(--blockr-color-bg, #fff);
  color: var(--blockr-color-text-muted, #52514e);
  font-size: 0.76rem;
  padding: 3px 8px;
  min-width: 58px;
  cursor: pointer;
}
.lane-sum-add:hover {
  border-color: var(--blockr-color-primary, #2a78d6);
  color: var(--blockr-color-primary, #2a78d6);
}
.lane-sum-presets { display: flex; flex-wrap: wrap; gap: 5px; }
.lane-sum-preset {
  border: 1px dashed var(--blockr-color-border, #e1e0d9);
  border-radius: 4px;
  background: none;
  color: var(--blockr-color-text-subtle, #898781);
  font-size: 0.76rem;
  padding: 3px 9px;
  cursor: pointer;
}
.lane-sum-preset:hover {
  border-color: var(--blockr-color-primary, #2a78d6);
  color: var(--blockr-color-primary, #2a78d6);
}
.lane-sum-hint {
  font-size: 0.74rem;
  color: var(--blockr-color-text-subtle, #898781);
  margin-top: 5px;
}

/* The cursor readout (interval track / sparkline points): one fixed element
   per page, positioned by rank-table.js. */
.blockr-lane-tip {
  position: fixed;
  z-index: 1070;
  pointer-events: none;
  background: var(--blockr-color-text-primary, #111827);
  color: #fff;
  font-size: 0.72rem;
  line-height: 1.35;
  padding: 3px 8px;
  border-radius: 4px;
  white-space: nowrap;
}

/* Zero-centred difference bar. */
.blockr-rank-dv {
  position: relative;
  height: 12px;
  background: var(--blockr-rank-track);
}
.blockr-rank-dv::before {
  content: '';
  position: absolute;
  left: 50%;
  top: -2px;
  bottom: -2px;
  width: 1px;
  background: var(--blockr-rank-tick);
}
.blockr-rank-dv .blockr-rank-fill { position: absolute; top: 0; }
.blockr-rank-dv .blockr-rank-fill.is-pos {
  left: 50%;
  background: var(--blockr-rank-pos);
}
.blockr-rank-dv .blockr-rank-fill.is-neg {
  right: 50%;
  background: var(--blockr-rank-neg);
}

/* Hierarchy. The chevron itself is the table block's -- same button, same svg
   (section_chevron_svg()), same rotation contract (the ROW carries `collapsed`).
   These four rules are the ONLY part of html_table_delta_css() the rank table
   needs; the rest of that delta is the structured Table-1 typography, which
   would restyle every cell, so it is deliberately not injected. Keep in sync
   with the .blockr-indent-btn / .blockr-chev block in R/html-table.R. */
.blockr-rank-container .blockr-indent-btn {
  border: 0;
  background: transparent;
  padding: 0;
  margin-right: 5px;
  margin-left: -18px;
  cursor: pointer;
  display: inline-flex;
  align-items: center;
  vertical-align: baseline;
}
.blockr-rank-container .blockr-chev {
  width: 13px;
  height: 13px;
  flex: none;
  color: var(--blockr-color-text-muted, #9aa3b0);
  transition: transform 0.2s ease, color 0.15s ease;
}
.blockr-rank-container .blockr-indent-btn:hover .blockr-chev {
  color: var(--blockr-color-text-primary, #111827);
}
.blockr-rank-container tr.blockr-indent-toggle.collapsed .blockr-chev {
  transform: rotate(-90deg);
}

/* Hierarchy. */
.blockr-rank-table tr.is-child td.blockr-rank-label-col {
  color: var(--blockr-color-text-muted, #52514e);
}
.blockr-rank-table tr.is-child.collapsed-hidden { display: none; }
.blockr-rank-table tr.is-parent .blockr-rank-label { font-weight: 600; }
.blockr-rank-table tr.is-pick { cursor: pointer; }
.blockr-rank-table tr.is-on {
  background: color-mix(in srgb, var(--blockr-rank-fill) 10%, transparent);
}
.blockr-rank-table tr.blockr-rank-fold td {
  font-style: italic;
  color: var(--blockr-color-text-subtle, #898781);
}
.blockr-rank-table tr.blockr-rank-hidden-search { display: none; }

/* Legend + footer. */
/* The legend is its own row under the control row (search + gear), so a long
   legend can never push the search box around. */
.blockr-rank-legend {
  padding: 0.35rem 0.25rem 0.15rem;
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  align-items: center;
  font-size: 0.8rem;
  color: var(--blockr-color-text-muted, #52514e);
}
.blockr-rank-legend-title {
  font-size: 0.7rem;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: var(--blockr-color-text-subtle, #898781);
}
.blockr-rank-legend-item { display: inline-flex; gap: 0.3rem; align-items: center; }
.blockr-rank-legend-item i {
  width: 10px;
  height: 10px;
  border-radius: 2px;
  display: inline-block;
}
.blockr-rank-footer {
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  align-items: center;
  justify-content: space-between;
  padding: 0.4rem 0.1rem 0;
  font-size: 0.75rem;
  color: var(--blockr-color-text-subtle, #898781);
}
.blockr-rank-note { color: var(--blockr-color-warning, #b45309); }
.blockr-rank-status { display: inline-flex; gap: 0.4rem; align-items: center; }
.blockr-rank-dot {
  width: 7px;
  height: 7px;
  border-radius: 50%;
  background: var(--blockr-rank-fill);
}
.blockr-rank-reset {
  font: inherit;
  cursor: pointer;
  background: none;
  border: 1px solid var(--blockr-color-border, #e1e0d9);
  border-radius: 2px;
  padding: 0.1rem 0.4rem;
  color: inherit;
}
"
}
