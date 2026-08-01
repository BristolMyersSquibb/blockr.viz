# Ranked bar table: the CSS delta on top of the shared html-table rules.
#
# Only what the bars, the legend, the expand caret and the footer add. Type,
# padding, hover, sticky header and the scroll shadow all come from
# html_table_shared_css_fallback(), so a rank table and a table block are the
# same object with a different cell.
#
# Colors read blockr.theme's --blockr-* tokens with a fallback, so a themed
# board restyles the bars without touching this file. The diverging bar is one
# colour both ways: the zero tick carries the direction, not the hue.

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
  --blockr-rank-bar: var(--blockr-color-primary, #2a78d6);
  --blockr-rank-tick: var(--blockr-color-border, #c3c2b7);
  /* The floor under a GLYPH -- see .blockr-rank-barwrap below. A board that
     wants shorter marks and less scrolling overrides it on the container. */
  --blockr-rank-lane-min: 80px;
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
/* The mark has a FLOOR, and it is the mark that carries it, not the cell.
   A cell minimum (the 110px on .blockr-rank-bar-col) is spent on the value
   label first, so a faceted table with a dozen level columns squeezed every
   lane down to the few pixels the label left over -- glyphs too small to
   compare, which is the whole point of the column. Putting the minimum on
   the lane makes the column's minimum label-plus-a-readable-mark instead, and
   a table that no longer fits scrolls sideways in its wrapper (the cheap
   direction: rows stay put, and the reader keeps the labels in view). */
.blockr-rank-barwrap .blockr-rank-track,
.blockr-rank-barwrap .blockr-rank-dv,
.blockr-rank-barwrap .blockr-rank-lane {
  flex: 1 1 auto;
  min-width: var(--blockr-rank-lane-min, 80px);
}
.blockr-rank-barval {
  flex: 0 0 auto;
  text-align: right;
  white-space: nowrap;
  font-variant-numeric: tabular-nums;
}
.blockr-rank-table .blockr-rank-pct {
  color: var(--blockr-color-text-subtle, #898781);
}
/* The column axis, under the header label. Printing the domain ONCE is what
   pays for the empty lanes below it: with a scale named at the top of the
   column, a cell only has to hold its mark. Geometry mirrors
   .blockr-rank-barwrap exactly (flexed strip + the same value slot), so a
   tick and the mark under it are percentages of one box. */
.blockr-rank-axis {
  display: flex;
  gap: 8px;
  align-items: center;
  margin-top: 5px;
  height: 12px;
  font-size: 9.5px;
  font-weight: var(--blockr-font-weight-normal, 400);
  letter-spacing: 0;
  color: var(--blockr-color-text-subtle, #898781);
  font-variant-numeric: tabular-nums;
}
.blockr-rank-axis-in {
  position: relative;
  flex: 1 1 auto;
  min-width: 0;
  height: 100%;
}
.blockr-rank-axis-pad { flex: 0 0 auto; }
.blockr-rank-axis-in span {
  position: absolute;
  top: 0;
  transform: translateX(-50%);
  white-space: nowrap;
}
.blockr-rank-axis-in span.is-first { transform: none; }
.blockr-rank-axis-in span.is-last { transform: translateX(-100%); }

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

/* Bars: segments TOUCH, and only the VALUE end is rounded. The end a bar grows
   to is a measurement and gets the cosmetic --blockr-mark-radius; the end at
   zero is the axis, shared by every row, and stays square because rounding it
   lifts the bar off its baseline. In a stack only the outermost segment has a
   value end, so `:last-child` carries the radius and the inner joins stay
   square -- which is also why segments still touch with no separating border,
   matching the chart block's `stack:'stack'` and `barGap: 0`.

   The radius is cosmetic and means NOTHING. It must stay well under half the
   lane height, where a capsule becomes the SEMANTIC mark for a soft boundary
   (see .blockr-rank-prcell below). On the 6px grouped rows the token would be a
   third of the height, so it is clamped to thickness/4 there.

   The grey track stays: it is a table-cell affordance (it says what the row's
   share is against the column max) with no echarts equivalent, and the
   crossfilter block in blockr.dm draws the same track + fill pair. */
.blockr-rank-track {
  display: flex;
  gap: 0;
  /* 12px = the shared lane height: bars, boxes, dot ranges and swimlanes
     line up across columns. */
  height: 12px;
  background: var(--blockr-rank-track);
  border-radius: 0 var(--blockr-mark-radius, 2px) var(--blockr-mark-radius, 2px) 0;
}
.blockr-rank-track.is-tall {
  height: auto;
  flex-direction: column;
  gap: 2px;
  background: none;
  border-radius: 0;
}
.blockr-rank-track.is-tall .blockr-rank-row3 {
  height: 6px;
  background: var(--blockr-rank-track);
  /* 6px row: thickness/4, so the radius eases down instead of reading as a
     capsule at the token's full 2px. */
  border-radius: 0 min(var(--blockr-mark-radius, 2px), 1.5px)
                 min(var(--blockr-mark-radius, 2px), 1.5px) 0;
}
.blockr-rank-fill {
  height: 100%;
  min-width: 2px;
  border-radius: 0;
  background: var(--blockr-rank-fill);
}
/* The value end. In a plain bar the fill is the only child; in a stack it is
   the outermost segment; in a grouped bar each row3 holds one. Zero-width
   segments are never emitted, so :last-child is always a segment that shows. */
.blockr-rank-track > .blockr-rank-fill:last-child,
.blockr-rank-row3 > .blockr-rank-fill:last-child {
  border-radius: 0 var(--blockr-mark-radius, 2px) var(--blockr-mark-radius, 2px) 0;
}
.blockr-rank-row3 > .blockr-rank-fill:last-child {
  border-radius: 0 min(var(--blockr-mark-radius, 2px), 1.5px)
                 min(var(--blockr-mark-radius, 2px), 1.5px) 0;
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
}
/* The ground, decided by the MARK and not by taste: a glyph that draws an
   outer range (a whisker, or the dot style's fence band) already spans the
   cell, so that band IS the rail the row is read against and a track behind
   it would be a second line saying the same thing. Only a mark with no outer
   range -- an IQR bar, a plain point range, a bare dot -- has nothing
   spanning the cell, and those keep a hairline so they do not float as chips.
   The column axis in the header carries the domain either way
   (_blockr.design/open/summarize-table/mock-box/, card D00). */
.blockr-rank-lane.is-bare::before {
  content: '';
  position: absolute;
  left: 0;
  right: 0;
  top: 50%;
  height: 1px;
  margin-top: -0.5px;
  background: var(--blockr-color-border, #e1e0d9);
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
/* The IQR body is free-standing: neither end sits on an axis and neither abuts
   a sibling, so the cosmetic radius applies to BOTH ends. It is not the pill --
   the box is 10px tall and the radius is 2px, nowhere near the half-height that
   would make it read as a soft boundary. The fence caps, whiskers and median
   tick stay square: at 1-2px a radius would turn them into dots. */
.blockr-rank-boxcell .lane-box {
  top: 1px;
  bottom: 1px;
  background: var(--blockr-rank-sub);
  border-radius: var(--blockr-mark-radius, 2px);
}
.blockr-rank-boxcell .lane-med {
  top: 0;
  bottom: 0;
  width: 2px;
  background: var(--blockr-rank-fill);
}
/* The dot style: three nested weights over one x. The fence band (outer
   range) recedes to a tint, the inner range is a rounded bar, the centre is a
   ringed dot -- so the eye reads centre first, spread second, extent third,
   which is the order the numbers matter in. Both ends rounded because the
   band is a soft boundary, unlike the box's hard fence caps. */
/* 999px, not half the height as a literal. These are CAPSULES: the radius is
   the signal, so it has to stay half the height whatever the height becomes.
   Written as 4px and 2px it only happened to be a capsule at 8px and 4px, and
   a later height change would have quietly demoted it to a rounded rectangle,
   i.e. to the cosmetic --blockr-mark-radius, which means nothing. */
.blockr-rank-prcell .lane-fence {
  top: 50%;
  height: 8px;
  margin-top: -4px;
  border-radius: 999px;
  background: var(--blockr-rank-fill);
  opacity: 0.16;
}
.blockr-rank-prcell .lane-rng {
  top: 50%;
  height: 4px;
  margin-top: -2px;
  border-radius: 999px;
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
/* Interval: the swimlane. Colour = the mapped level, and BOTH ends round.
   A timeline is not a stack. A stack tiles by construction -- its segments
   always share edges, they compose one quantity, and a seam between them
   would read as a gap in that quantity, which is why a stack's inner joins
   stay square. A swimlane's segments are per-event [left, width] pairs that
   overlap, leave gaps, or only incidentally touch. When two of them DO touch,
   the seam is true: it says these are two events and not one long one. So the
   abutment exception belongs to stacking, not to interval marks.

   Narrow segments need no guard: min-width is 2px and CSS scales border-radius
   down proportionally when the corners would not fit the box. */
.blockr-rank-ivcell .lane-seg {
  top: 0;
  bottom: 0;
  min-width: 2px;
  border-radius: var(--blockr-mark-radius, 2px);
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
/* The optional mappings (colour, facet): the add buttons sit on the label
   line's baseline so the row reads as one band of controls, and an added
   mapping carries its ✕ in the label, the way a grouping role does. */
.lane-sum-addmaps { flex-direction: row; gap: 5px; align-self: flex-end; }
.lane-sum-map-rm {
  border: 0;
  background: none;
  padding: 0 0 0 4px;
  font-size: 0.65rem;
  line-height: 1;
  cursor: pointer;
  color: var(--blockr-color-text-subtle, #898781);
}
.lane-sum-map-rm:hover { color: var(--blockr-color-danger, #d03b3b); }
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

/* Zero-centred difference bar. Zero sits in the MIDDLE here, so neither end of
   the rail is an axis and both round; the fill rounds on whichever end points
   away from the zero tick. */
.blockr-rank-dv {
  position: relative;
  height: 12px;
  background: var(--blockr-rank-track);
  border-radius: var(--blockr-mark-radius, 2px);
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
/* One colour both ways. The side of the zero line already says which
   direction; colouring the two apart would only add an opinion about which
   one is good, and nothing tells the block that. */
.blockr-rank-dv .blockr-rank-fill.is-pos {
  left: 50%;
  border-radius: 0 var(--blockr-mark-radius, 2px) var(--blockr-mark-radius, 2px) 0;
}
.blockr-rank-dv .blockr-rank-fill.is-neg {
  right: 50%;
  border-radius: var(--blockr-mark-radius, 2px) 0 0 var(--blockr-mark-radius, 2px);
}
.blockr-rank-dv .blockr-rank-fill {
  background: var(--blockr-rank-bar);
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
  gap: 0.35rem 1.4rem;
  align-items: center;
  font-size: 0.8rem;
  color: var(--blockr-color-text-muted, #52514e);
}
/* One group per colour column (a summarize table maps colour per column, so
   it can carry several). The wider gap BETWEEN groups keeps a title bound to
   the items it decodes. */
.blockr-rank-legend-group {
  display: inline-flex;
  flex-wrap: wrap;
  gap: 0.6rem;
  align-items: center;
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
