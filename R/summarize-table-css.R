# Summarize table: the CSS delta on top of the shared html-table rules.
#
# Only what the bars, the legend, the expand caret and the footer add. Type,
# padding, hover, sticky header and the scroll shadow all come from
# html_table_shared_css_fallback(), so a summarize table and a table block are
# the same object with a different cell.
#
# Colors read blockr.theme's --blockr-* tokens with a fallback, so a themed
# board restyles the bars without touching this file. The diverging bar is one
# colour both ways: the zero tick carries the direction, not the hue.

#' @noRd
summarize_table_css <- function() {
  "
.blockr-summarize-container {
  --blockr-summarize-fill: var(--blockr-color-bg-accent, #2563eb);
  --blockr-summarize-sub: color-mix(in srgb, var(--blockr-summarize-fill) 45%,
                                    transparent);
  /* The lane track. The design token alone is so close to the surface
     that an empty lane reads as nothing at all -- and on a box or a dot
     range the track IS the axis the glyph is read against, so it has to be
     visible. Mixed toward the strong border: still recessive, no longer
     invisible. */
  --blockr-summarize-track: color-mix(in srgb,
                                 var(--blockr-color-bg-subtle, #f9fafb) 55%,
                                 var(--blockr-color-border-strong, #d1d5db));
  --blockr-summarize-bar: var(--blockr-color-bg-accent, #2563eb);
  --blockr-summarize-tick: var(--blockr-color-border-default, #e5e7eb);
  /* The floor under a GLYPH -- see .blockr-summarize-barwrap below. A board
     that wants shorter marks and less scrolling overrides it on the
     container. */
  --blockr-summarize-lane-min: 80px;
  /* The ceiling over it, in the default `bar_width = 'fit'`: past it a wide
     panel's slack stays blank instead of stretching the marks. */
  --blockr-summarize-lane-max: 320px;
}
/* Title / subtitle / caption: the canonical .dd-table-* bands, styled by
   inst/css/table.css (shipped with the table dep). Nothing to add here. */
.blockr-summarize-table { width: 100%; }
/* Column widths ride on the CELLS: the header cells come from the table
   block's dt_th(), so they carry its classes, not ours. */
.blockr-summarize-table td.blockr-summarize-bar-col {
  width: 26%;
  min-width: 110px;
}
.blockr-summarize-table td.blockr-summarize-num {
  text-align: right;
  white-space: nowrap;
  font-variant-numeric: tabular-nums;
}
/* Raw field columns (the as-is measure's extra row columns): plain text,
   left-aligned like the label column, never numeric-formatted. */
.blockr-summarize-table td.blockr-summarize-txt { white-space: nowrap; }
.blockr-summarize-table td.blockr-summarize-label-col { white-space: nowrap; }
/* The GLYPH columns own the slack. Auto table layout hands leftover width to
   the unconstrained cells, which meant a wide panel only stretched the label
   column while the bars and boxes stayed at their 26%. width:1% is the
   shrink-to-fit idiom (with nowrap, the cell takes its content width and no
   more), so widening the block lengthens the visuals instead. Label and text
   cells still cap out and ellipsis rather than pushing the glyphs off. */
.blockr-summarize-table th.blockr-stub-header,
.blockr-summarize-table td.blockr-summarize-label-col,
.blockr-summarize-table td.blockr-summarize-txt,
.blockr-summarize-table td.blockr-summarize-num { width: 1%; }
.blockr-summarize-table td.blockr-summarize-label-col,
.blockr-summarize-table td.blockr-summarize-txt {
  max-width: 260px;
  overflow: hidden;
  text-overflow: ellipsis;
}
/* The in-bar value label: track left, value right in a FIXED slot (one width
   per column, in ch) so every row's track spans the same range -- a varying
   label width would silently rescale the bars against each other. */
.blockr-summarize-barwrap {
  display: flex;
  align-items: center;
  gap: 8px;
}
/* The mark has a FLOOR, and it is the mark that carries it, not the cell.
   A cell minimum (the 110px on .blockr-summarize-bar-col) is spent on the value
   label first, so a faceted table with a dozen level columns squeezed every
   lane down to the few pixels the label left over -- glyphs too small to
   compare, which is the whole point of the column. Putting the minimum on
   the lane makes the column's minimum label-plus-a-readable-mark instead, and
   a table that no longer fits scrolls sideways in its wrapper (the cheap
   direction: rows stay put, and the reader keeps the labels in view). */
.blockr-summarize-barwrap .blockr-summarize-track,
.blockr-summarize-barwrap .blockr-summarize-dv,
.blockr-summarize-barwrap .blockr-summarize-lane {
  flex: 1 1 auto;
  min-width: var(--blockr-summarize-lane-min, 80px);
  max-width: var(--blockr-summarize-lane-max, 320px);
}
/* Bar width (`bar_width`). Fit, the default, is the rule above: the lanes
   take the panel's slack, between the floor and the ceiling. A preset fixes
   the lane instead, and the table drops to its natural width: every cell's
   width goes back to auto, because one percentage cell (the 1% shrink-to-fit
   idiom, the 26% glyph column) stretches an auto-width table to the panel
   again. The slack stays blank on the right; a panel narrower than the table
   scrolls. Only the labelled marks follow it: a swimlane spans its cell. */
.blockr-summarize-table[data-summarize-width='narrow'] {
  --blockr-summarize-lane-w: 90px;
}
.blockr-summarize-table[data-summarize-width='medium'] {
  --blockr-summarize-lane-w: 150px;
}
.blockr-summarize-table[data-summarize-width='wide'] {
  --blockr-summarize-lane-w: 240px;
}
.blockr-summarize-table[data-summarize-width] { width: auto; }
.blockr-summarize-table[data-summarize-width] th,
.blockr-summarize-table[data-summarize-width] td {
  width: auto;
  min-width: 0;
}
.blockr-summarize-table[data-summarize-width]
  .blockr-summarize-barwrap > :not(.blockr-summarize-barval),
.blockr-summarize-table[data-summarize-width]
  .blockr-summarize-axis.has-val .blockr-summarize-axis-in {
  flex: 0 0 var(--blockr-summarize-lane-w);
  min-width: 0;
  max-width: none;
}
/* Grey: the number belongs to the mark beside it and should not outshout
   the label column. */
.blockr-summarize-barval {
  flex: 0 0 auto;
  text-align: right;
  white-space: nowrap;
  font-variant-numeric: tabular-nums;
  color: var(--blockr-color-text-muted, #6b7280);
  /* Smaller than the cells, larger than the axis ticks. The slot is sized
     in ch of THIS type, so the axis pad above it takes the same size. */
  font-size: 11px;
}
.blockr-summarize-table .blockr-summarize-pct {
  color: var(--blockr-color-text-muted, #6b7280);
}
/* The column axis, under the header label. Printing the domain ONCE is what
   pays for the empty lanes below it: with a scale named at the top of the
   column, a cell only has to hold its mark. Geometry mirrors
   .blockr-summarize-barwrap exactly (flexed strip + the same value slot), so a
   tick and the mark under it are percentages of one box. */
.blockr-summarize-axis {
  display: flex;
  gap: 8px;
  align-items: center;
  margin-top: 5px;
  height: 12px;
  font-weight: var(--blockr-font-weight-normal, 400);
  letter-spacing: 0;
  color: var(--blockr-color-text-muted, #6b7280);
  font-variant-numeric: tabular-nums;
}
/* The small type sits on the ticks, not on the strip: the pad is sized in
   ch, and a ch at 9.5px is two thirds of the cell's, so a strip-wide font
   size left the ticked span ~20px longer than the lane under it. The pad
   takes the value labels' size (11px), so it and the slot are one width. */
.blockr-summarize-axis-in {
  position: relative;
  flex: 1 1 auto;
  min-width: 0;
  height: 100%;
  font-size: 9.5px;
}
.blockr-summarize-axis.has-val .blockr-summarize-axis-in {
  max-width: var(--blockr-summarize-lane-max, 320px);
}
.blockr-summarize-axis-pad { flex: 0 0 auto; font-size: 11px; }
.blockr-summarize-axis-in span {
  position: absolute;
  top: 0;
  transform: translateX(-50%);
  white-space: nowrap;
}
.blockr-summarize-axis-in span.is-first { transform: none; }
.blockr-summarize-axis-in span.is-last { transform: translateX(-100%); }

/* Sorting affordance: none of our own. The header cells are dt_th()'s, so the
   .blockr-sortable cursor and the .blockr-sort-icon arrow come from the shared
   table CSS and read exactly like a table block's. */

/* The by_level facet layout's spanning header row (one cell per facet
   level over its summary group) -- centred, with a hairline under the
   span so the group reads as one unit. */
.blockr-summarize-table th.blockr-th-group {
  text-align: center;
  border-bottom: 1px solid var(--blockr-color-border-default, #e5e7eb);
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
   (see .blockr-summarize-prcell below).

   The grey track stays: it is a table-cell affordance (it says what the row's
   share is against the column max) with no echarts equivalent, and the
   crossfilter block in blockr.dm draws the same track + fill pair. */
.blockr-summarize-track {
  display: flex;
  gap: 0;
  /* 12px = the shared lane height: bars, boxes, dot ranges and swimlanes
     line up across columns. */
  height: 12px;
  background: var(--blockr-summarize-track);
  border-radius: 0 var(--blockr-mark-radius, 2px) var(--blockr-mark-radius, 2px) 0;
}
.blockr-summarize-track.is-tall {
  height: auto;
  flex-direction: column;
  /* 5px: the same white between levels as the labelled split, whose 13px
     value line plus the 2px gap puts its 10px lanes 15px apart. */
  gap: 5px;
  background: none;
  border-radius: 0;
}
/* One level of a grouped split, with its value beside it (.is-lv) or
   without (.row3): 10px, a little under the plain bar's 12px so the levels
   keep 5px of white between them. A row grows to hold its levels. */
.blockr-summarize-track.is-lv { height: 10px; }
.blockr-summarize-track.is-tall .blockr-summarize-row3 {
  height: 10px;
  background: var(--blockr-summarize-track);
  border-radius: 0 var(--blockr-mark-radius, 2px) var(--blockr-mark-radius, 2px) 0;
}
.blockr-summarize-fill {
  height: 100%;
  min-width: 2px;
  border-radius: 0;
  background: var(--blockr-summarize-fill);
}
/* A stacked segment's own number, inside it. The segment is a size
   container, so a number it is too narrow for is hidden rather than cut:
   wN is the number's length in characters, about 6px each at 10px type,
   plus a little air. A longer number than w7 never shows inside. */
.blockr-summarize-fill.has-lab {
  container-type: inline-size;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}
.blockr-summarize-seglab {
  display: none;
  font-size: 10px;
  line-height: 1;
  font-weight: 600;
  font-variant-numeric: tabular-nums;
  white-space: nowrap;
}
.blockr-summarize-seglab.w1, .blockr-summarize-seglab.w2,
.blockr-summarize-seglab.w3, .blockr-summarize-seglab.w4,
.blockr-summarize-seglab.w5, .blockr-summarize-seglab.w6,
.blockr-summarize-seglab.w7 { display: inline; }
@container (max-width: 11px) { .blockr-summarize-seglab.w1 { display: none; } }
@container (max-width: 17px) { .blockr-summarize-seglab.w2 { display: none; } }
@container (max-width: 23px) { .blockr-summarize-seglab.w3 { display: none; } }
@container (max-width: 29px) { .blockr-summarize-seglab.w4 { display: none; } }
@container (max-width: 35px) { .blockr-summarize-seglab.w5 { display: none; } }
@container (max-width: 41px) { .blockr-summarize-seglab.w6 { display: none; } }
@container (max-width: 47px) { .blockr-summarize-seglab.w7 { display: none; } }
/* The value end. In a plain bar the fill is the only child; in a stack it is
   the outermost segment; in a grouped bar each row3 holds one. Zero-width
   segments are never emitted, so :last-child is always a segment that shows. */
.blockr-summarize-track > .blockr-summarize-fill:last-child,
.blockr-summarize-row3 > .blockr-summarize-fill:last-child {
  border-radius: 0 var(--blockr-mark-radius, 2px) var(--blockr-mark-radius, 2px) 0;
}
.blockr-summarize-track.is-sub .blockr-summarize-fill {
  background: var(--blockr-summarize-sub);
}

/* Lane marks (box / point range / interval / sparkline): absolutely
   positioned glyphs inside a track-coloured lane, percentage geometry
   computed in R. Same fill/track/sub tokens as the bars, so a themed board
   restyles every mark together. ONE height (matching the bar track) so a
   bar column, a box column and a swimlane read as one system; only the
   sparkline is taller (amplitude needs room). */
.blockr-summarize-lane {
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
.blockr-summarize-lane.is-bare::before {
  content: '';
  position: absolute;
  left: 0;
  right: 0;
  top: 50%;
  height: 1px;
  margin-top: -0.5px;
  background: var(--blockr-color-border-default, #e5e7eb);
}
.blockr-summarize-lane i { position: absolute; }
/* Colour-split distribution cell: the levels stack INSIDE the cell, so the
   column stays one column. Every level keeps the full 12px lane and its
   marks; a row with more levels grows to hold them. (Thinning the third
   level on to fit a fixed row drew one row in two dot sizes.) */
.blockr-summarize-multi {
  display: flex;
  flex-direction: column;
  justify-content: center;
  gap: 2px;
}
.blockr-summarize-multi .blockr-summarize-lv {
  min-width: 0;
  /* The level's colour arrives as --blockr-summarize-fill on this element. The
     TRANSLUCENT token is derived from the fill, so it has to be re-derived
     here as well -- otherwise every level's box body keeps the column
     default and only the whiskers and the median tick take the colour. */
  --blockr-summarize-sub: color-mix(in srgb, var(--blockr-summarize-fill) 45%,
                               transparent);
}
/* A colour-split cell with value labels: one number per level, beside its
   lane. A tight line, so a level is barely taller than its lane and a row of
   five levels grows by what the numbers need. */
.blockr-summarize-multi .blockr-summarize-lv.blockr-summarize-barwrap
  .blockr-summarize-barval {
  line-height: 13px;
}
/* Box: whiskers OUTSIDE the body only (two segments), caps, a translucent
   body, a solid median tick. */
.blockr-summarize-boxcell .lane-wh {
  top: 50%;
  height: 1px;
  margin-top: -0.5px;
  background: var(--blockr-summarize-fill);
}
.blockr-summarize-boxcell .lane-cap {
  top: 3px;
  bottom: 3px;
  width: 1px;
  background: var(--blockr-summarize-fill);
}
/* The IQR body is free-standing: neither end sits on an axis and neither abuts
   a sibling, so the cosmetic radius applies to BOTH ends. It is not the pill --
   the box is 10px tall and the radius is 2px, nowhere near the half-height that
   would make it read as a soft boundary. The fence caps, whiskers and median
   tick stay square: at 1-2px a radius would turn them into dots. */
.blockr-summarize-boxcell .lane-box {
  top: 1px;
  bottom: 1px;
  background: var(--blockr-summarize-sub);
  border-radius: var(--blockr-mark-radius, 2px);
}
.blockr-summarize-boxcell .lane-med {
  top: 0;
  bottom: 0;
  width: 2px;
  background: var(--blockr-summarize-fill);
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
.blockr-summarize-prcell .lane-fence {
  top: 50%;
  height: 8px;
  margin-top: -4px;
  border-radius: 999px;
  background: var(--blockr-summarize-fill);
  opacity: 0.16;
}
.blockr-summarize-prcell .lane-rng {
  top: 50%;
  height: 4px;
  margin-top: -2px;
  border-radius: 999px;
  background: var(--blockr-summarize-fill);
}
.blockr-summarize-prcell .lane-ctr {
  top: 50%;
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--blockr-summarize-fill);
  transform: translate(-50%, -50%);
  box-shadow: 0 0 0 2px var(--blockr-color-bg-surface, #ffffff);
}
/* Pair (dumbbell): two values of one group joined by a segment. The band is
   the range the values are read against, at lane height like every other
   ground; the reference line runs past the lane so rows read as one line. The
   `from` mark is a hollow diamond, the `to` mark a dot, hollow when it falls
   outside the band. A dashed link is the row's `dash` level. */
.blockr-summarize-pacell .lane-band {
  top: 0;
  bottom: 0;
  background: var(--blockr-summarize-track);
  border-radius: var(--blockr-mark-radius, 2px);
}
.blockr-summarize-pacell .lane-ref {
  top: -6px;
  bottom: -6px;
  width: 0;
  border-left: 1px dashed var(--blockr-color-border-strong, #d1d5db);
}
.blockr-summarize-pacell .lane-link {
  top: 50%;
  height: 0;
  border-top: 2px solid var(--blockr-summarize-fill);
  margin-top: -1px;
}
.blockr-summarize-pacell.is-dash .lane-link { border-top-style: dashed; }
.blockr-summarize-pacell .lane-from {
  top: 50%;
  width: 7px;
  height: 7px;
  background: var(--blockr-color-bg-surface, #ffffff);
  border: 1.5px solid var(--blockr-summarize-fill);
  transform: translate(-50%, -50%) rotate(45deg);
  box-sizing: border-box;
}
.blockr-summarize-pacell .lane-to {
  top: 50%;
  width: 9px;
  height: 9px;
  border-radius: 50%;
  background: var(--blockr-summarize-fill);
  border: 1.5px solid var(--blockr-summarize-fill);
  transform: translate(-50%, -50%);
  box-sizing: border-box;
}
.blockr-summarize-pacell .lane-to.is-open {
  background: var(--blockr-color-bg-surface, #ffffff);
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
.blockr-summarize-ivcell .lane-seg {
  top: 0;
  bottom: 0;
  min-width: 2px;
  border-radius: var(--blockr-mark-radius, 2px);
}
/* The exhibit form (a spans row's size = lg): a WIDER column for boards
   where the swimlane is the centerpiece -- more horizontal resolution for
   the spans, not more height. */
.blockr-summarize-table td.blockr-summarize-wide {
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
.blockr-summarize-container.seg-hover .lane-seg:not(.is-same),
.blockr-summarize-container.seg-search .lane-seg:not(.is-hit) {
  background: color-mix(in srgb, var(--blockr-summarize-track) 88%,
                        var(--blockr-summarize-tick)) !important;
  transition: background 0.1s ease;
}
/* Rows: the table style every output table uses (blockr.ui design system,
   Tables): 30px, a 20px line with 5px above and below, no rules in the body.
   The 12px lanes sit inside the line, and the gaps between them separate
   the rows. The sparkline row below keeps its own taller cell. */
.blockr-summarize-table tbody td {
  padding-top: 5px;
  padding-bottom: 5px;
  line-height: 20px;
}
.blockr-summarize-table tbody tr {
  border-bottom: 0;
}

/* Sparkline: one inline SVG per cell, band under line, last-value dot.
   Taller than the other lanes -- and the trajectory USES the row: the
   cell keeps a token 1px of vertical padding (a line rarely touches the
   extremes), so a sparkline row stays close to a text row's height
   instead of paying 36px plus full text padding. */
.blockr-summarize-table td:has(.blockr-summarize-spcell) {
  padding-top: 1px;
  padding-bottom: 1px;
}
.blockr-summarize-spcell {
  /* Row height minus the two 1px paddings: the trajectory occupies the
     WHOLE row (the svg stretches freely; viewBox geometry is
     percentage-based and the stroke is non-scaling). */
  height: 40px;
  background: none;
}
.blockr-summarize-spcell svg {
  display: block;
  width: 100%;
  height: 100%;
}
.blockr-summarize-spcell .lane-band { fill: var(--blockr-summarize-track); }
/* The computed reference (a series row's `ref` option): a dashed pooled
   center line, optionally a dispersion band under everything. */
.blockr-summarize-spcell .lane-refband {
  fill: color-mix(in srgb, var(--blockr-summarize-fill) 10%, transparent);
}
.blockr-summarize-spcell .lane-refline {
  stroke: var(--blockr-summarize-tick);
  stroke-width: 1;
  stroke-dasharray: 3 2;
}
.blockr-summarize-spcell .lane-ln {
  fill: none;
  stroke: var(--blockr-summarize-fill);
  stroke-width: 1.6;
}
.blockr-summarize-spcell .lane-dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: var(--blockr-summarize-fill);
  transform: translate(-50%, -50%);
  box-shadow: 0 0 0 2px var(--blockr-color-bg-surface, #ffffff);
}
/* The summarize-table columns editor (the gear's Columns section), on the
   design system's rows: one 42px row per column that reads as a sentence
   (mark glyph, name, what it computes, badges), one column open at a time as
   a card in place. The drag handle sits in the list's left padding and
   shows on hover, as on every row list. */
.lane-summaries { display: flex; flex-direction: column; gap: 6px; width: 100%; padding-left: 14px; margin-left: -14px; }
.lane-sum-row {
  position: relative;
  border: 1px solid var(--blockr-color-border-default, #e5e7eb);
  border-radius: var(--blockr-radius-lg, 8px);
  background: var(--blockr-color-bg-surface, #ffffff);
  transition: border-color var(--blockr-transition, 0.15s ease);
}
.lane-sum-row:hover { border-color: var(--blockr-color-border-strong, #d1d5db); }
.lane-sum-row.is-open { border-color: var(--blockr-color-border-accent-subtle, #bfdbfe); }
.lane-sum-row.is-dragging { opacity: 0.5; }
.lane-sum-row.is-drop { box-shadow: 0 -2px 0 var(--blockr-color-border-accent, #2563eb); }
.lane-sum-head {
  display: flex;
  align-items: center;
  gap: 8px;
  height: 40px;
  padding: 0 6px 0 12px;
  cursor: pointer;
  min-width: 0;
  font-size: var(--blockr-font-size-base, 14px);
}
.lane-sum-grip {
  position: absolute;
  left: -14px;
  top: 12px;
  width: 10px;
  height: 16px;
  display: inline-flex;
  color: var(--blockr-color-text-muted, #6b7280);
  cursor: grab;
  opacity: 0;
  transition: opacity 0.12s;
}
.lane-sum-row:hover .lane-sum-grip { opacity: 1; }
.lane-sum-glyph { display: inline-flex; flex: none; width: 18px; justify-content: center; color: var(--blockr-color-text-muted, #6b7280); }
.lane-sum-name { font-weight: 500; flex: none; white-space: nowrap; }
.lane-sum-row.is-open .lane-sum-name { font-weight: 600; }
.lane-sum-line {
  color: var(--blockr-color-text-muted, #6b7280);
  font-size: var(--blockr-font-size-sm, 13px);
  flex: 1 1 auto;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.lane-sum-badge {
  flex: none;
  display: inline-flex;
  align-items: center;
  height: 18px;
  padding: 0 7px;
  border-radius: 999px;
  border: 1px solid var(--blockr-color-border-default, #e5e7eb);
  font-size: 11px;
  font-weight: 500;
  color: var(--blockr-color-text-muted, #6b7280);
  white-space: nowrap;
}
.lane-sum-rm, .lane-sum-code {
  flex: none;
  width: 26px;
  height: 26px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 0;
  border: 1px solid transparent;
  border-radius: var(--blockr-radius-sm, 4px);
  background: transparent;
  color: var(--blockr-color-text-muted, #6b7280);
  cursor: pointer;
}
.lane-sum-rm { opacity: 0; transition: opacity 0.12s; }
.lane-sum-row:hover .lane-sum-rm, .lane-sum-rm:focus-visible { opacity: 1; }
.lane-sum-rm:hover { color: var(--blockr-color-text-danger, #b91c1c); }
.lane-sum-code:hover { background: var(--blockr-color-bg-hover, rgba(17, 24, 39, 0.06)); color: var(--blockr-color-text-default, #111827); }
.lane-sum-code[aria-pressed='true'] {
  color: var(--blockr-color-text-accent, #2563eb);
  background: var(--blockr-color-bg-accent-subtle, #eff6ff);
  border-color: var(--blockr-color-border-accent-subtle, #bfdbfe);
}
.lane-sum-code:disabled { opacity: 0.4; cursor: default; background: transparent; }
.lane-sum-chev { flex: none; display: inline-flex; width: 26px; justify-content: center; color: var(--blockr-color-text-muted, #6b7280); transform: rotate(-90deg); transition: transform var(--blockr-transition, 0.15s ease); }
.lane-sum-row.is-open .lane-sum-chev { transform: none; }
.lane-sum-body {
  padding: 4px 12px 16px;
  display: flex;
  flex-wrap: wrap;
  align-items: flex-end;
  gap: 16px 12px;
}
.lane-sum-ctl { display: flex; flex-direction: column; gap: 4px; min-width: 200px; }
.lane-sum-ctl > .blockr-label { font-size: var(--blockr-font-size-xs, 12px); color: var(--blockr-color-text-muted, #6b7280); }
.lane-sum-ctl-wide { flex: 1 1 100%; }
/* The optional mappings (colour, facet, percent): quiet add buttons on the
   controls' line; an added mapping carries its remove button in the label. */
.lane-sum-addmaps { flex-direction: row; gap: 6px; align-self: flex-end; min-width: 0; padding-bottom: 8px; }
.lane-sum-map-rm {
  border: 0;
  background: none;
  padding: 0 0 0 4px;
  font-size: 0.65rem;
  line-height: 1;
  cursor: pointer;
  color: var(--blockr-color-text-muted, #6b7280);
}
.lane-sum-map-rm:hover { color: var(--blockr-color-text-danger, #b91c1c); }
.lane-sum-name-input {
  height: var(--blockr-control-h, 42px);
  border: 1px solid var(--blockr-color-border-default, #e5e7eb);
  border-radius: var(--blockr-radius-lg, 8px);
  padding: 0 12px;
  font: inherit;
  font-size: var(--blockr-font-size-base, 14px);
  background: var(--blockr-color-bg-field, #f9fafb);
}
.lane-sum-name-input:focus { outline: 0; border-color: var(--blockr-color-border-accent, #2563eb); box-shadow: var(--blockr-focus-ring, 0 0 0 3px rgba(37, 99, 235, 0.12)); background: var(--blockr-color-bg-surface, #fff); }
.lane-sum-seg {
  display: inline-grid;
  grid-auto-flow: column;
  height: var(--blockr-control-h, 42px);
  padding: 3px;
  gap: 3px;
  border: 1px solid var(--blockr-color-border-default, #e5e7eb);
  border-radius: var(--blockr-radius-lg, 8px);
  background: var(--blockr-color-bg-field, #f9fafb);
}
.lane-sum-seg-btn {
  border: 1px solid transparent;
  border-radius: var(--blockr-radius-md, 6px);
  background: transparent;
  color: var(--blockr-color-text-muted, #6b7280);
  font-size: var(--blockr-font-size-base, 14px);
  padding: 0 12px;
  cursor: pointer;
}
.lane-sum-seg-btn:hover { color: var(--blockr-color-text-default, #111827); background: var(--blockr-color-bg-hover, rgba(17, 24, 39, 0.06)); }
.lane-sum-seg-btn.is-on {
  color: var(--blockr-color-text-accent, #2563eb);
  background: var(--blockr-color-bg-accent-subtle, #eff6ff);
  border-color: var(--blockr-color-border-accent-subtle, #bfdbfe);
  font-weight: 500;
}
/* Display tiles: outside the engine's type grid the tiles shrink to their
   caption (bar collapsed to 30px), so give them the grid's footprint. */
.lane-sum-tiles { display: flex; gap: 5px; }
.lane-sum-tiles .dd-type-tile { min-width: 64px; }
.lane-sum-addrow { display: flex; align-items: center; margin-top: 2px; }
.lane-sum-addcol {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  height: 26px;
  padding: 0 8px;
  border: 1px solid transparent;
  border-radius: var(--blockr-radius-sm, 4px);
  background: transparent;
  color: var(--blockr-color-text-muted, #6b7280);
  font-size: var(--blockr-font-size-xs, 12px);
  font-weight: 500;
  cursor: pointer;
}
.lane-sum-addcol:hover { background: var(--blockr-color-bg-hover, rgba(17, 24, 39, 0.06)); color: var(--blockr-color-text-default, #111827); }
.lane-sum-add-types { display: flex; flex-wrap: wrap; gap: 5px; }
/* A custom column's function: the prepare script's editor look, an Apply
   button under it (code commits on its own button, design system). */
.lane-sum-fn {
  width: 100%;
  font-family: var(--blockr-font-mono, ui-monospace, monospace);
  font-size: 13px;
}
.lane-sum-fn-foot {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  margin-top: 6px;
}
.lane-sum-apply {
  flex: none;
  height: 26px;
  padding: 0 9px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
  cursor: pointer;
  color: var(--blockr-color-text-accent, #2563eb);
  background: var(--blockr-color-bg-accent-subtle, #eff6ff);
  border: 1px solid var(--blockr-color-border-accent-subtle, #bfdbfe);
}
.lane-sum-apply:hover { border-color: var(--blockr-color-border-accent, #2563eb); }
.lane-sum-add {
  border: 1px solid var(--blockr-color-border-default, #e5e7eb);
  border-radius: 4px;
  background: var(--blockr-color-bg-surface, #ffffff);
  color: var(--blockr-color-text-muted, #6b7280);
  font-size: 0.76rem;
  padding: 3px 8px;
  min-width: 58px;
  cursor: pointer;
}
.lane-sum-add:hover {
  border-color: var(--blockr-color-border-accent, #2563eb);
  color: var(--blockr-color-text-accent, #2563eb);
}
.lane-sum-hint {
  font-size: 0.74rem;
  color: var(--blockr-color-text-muted, #6b7280);
  margin-top: 5px;
}

/* The cursor readout (interval track / sparkline points): one fixed element
   per page, positioned by summarize-table.js. It follows the pointer, so it is
   not a Blockr.tooltip, but it wears the same light card. */
.blockr-lane-tip {
  position: fixed;
  z-index: 1070;
  pointer-events: none;
  background: var(--blockr-color-bg-raised, #ffffff);
  border: 1px solid var(--blockr-color-border-default, #e5e7eb);
  box-shadow: var(--blockr-shadow-md, 0 4px 12px rgba(0, 0, 0, 0.1));
  color: var(--blockr-color-text-default, #111827);
  font-size: var(--blockr-font-size-xs, 0.75rem);
  line-height: 1.4;
  padding: 5px 9px;
  border-radius: var(--blockr-radius-lg, 8px);
  white-space: nowrap;
}
/* The hover card over a bar, box, dot range or dumbbell: the chart tooltip
   card (chart.css .dd-tt-*), at its padding and width. */
.blockr-lane-tip.is-card {
  padding: 6px 10px;
  min-width: 160px;
}

/* Zero-centred difference bar. Zero sits in the MIDDLE here, so neither end of
   the rail is an axis and both round; the fill rounds on whichever end points
   away from the zero tick. */
.blockr-summarize-dv {
  position: relative;
  height: 12px;
  background: var(--blockr-summarize-track);
  border-radius: var(--blockr-mark-radius, 2px);
}
.blockr-summarize-dv::before {
  content: '';
  position: absolute;
  left: 50%;
  top: -2px;
  bottom: -2px;
  width: 1px;
  background: var(--blockr-summarize-tick);
}
.blockr-summarize-dv .blockr-summarize-fill { position: absolute; top: 0; }
/* One colour both ways. The side of the zero line already says which
   direction; colouring the two apart would only add an opinion about which
   one is good, and nothing tells the block that. */
.blockr-summarize-dv .blockr-summarize-fill.is-pos {
  left: 50%;
  border-radius: 0 var(--blockr-mark-radius, 2px) var(--blockr-mark-radius, 2px) 0;
}
.blockr-summarize-dv .blockr-summarize-fill.is-neg {
  right: 50%;
  border-radius: var(--blockr-mark-radius, 2px) 0 0 var(--blockr-mark-radius, 2px);
}
.blockr-summarize-dv .blockr-summarize-fill {
  background: var(--blockr-summarize-bar);
}

/* Hierarchy. The chevron itself is the table block's -- same button, same svg
   (section_chevron_svg()), same rotation contract (the ROW carries `collapsed`).
   These four rules are the ONLY part of html_table_delta_css() the summarize
   table needs; the rest of that delta is the structured Table-1 typography,
   which would restyle every cell, so it is deliberately not injected. Keep in
   sync with the .blockr-indent-btn / .blockr-chev block in R/html-table.R. */
.blockr-summarize-container .blockr-indent-btn {
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
.blockr-summarize-container .blockr-chev {
  width: 13px;
  height: 13px;
  flex: none;
  color: var(--blockr-color-text-muted, #6b7280);
  transition: transform 0.2s ease, color 0.15s ease;
}
.blockr-summarize-container .blockr-indent-btn:hover .blockr-chev {
  color: var(--blockr-color-text-default, #111827);
}
.blockr-summarize-container tr.blockr-indent-toggle.collapsed .blockr-chev {
  transform: rotate(-90deg);
}

/* Fold all: the table block's corner chevron (summarize-table.js
   syncFoldAll), hung into the stub header's padding exactly as the parent
   rows hang theirs, so the title keeps the labels' left edge. */
.blockr-summarize-container .dt-foldall-wrap {
  display: flex;
  align-items: flex-start;
  gap: 5px;
  margin-left: -18px;
}
.blockr-summarize-container .dt-foldall {
  display: inline-flex;
  align-items: center;
  height: 1lh;
  padding: 0;
  border: 0;
  background: none;
  cursor: pointer;
  flex: none;
}
.blockr-summarize-container .dt-foldall:hover .blockr-chev {
  color: var(--blockr-color-text-default, #111827);
}
.blockr-summarize-container .dt-foldall[aria-expanded='false'] .blockr-chev {
  transform: rotate(-90deg);
}
.blockr-summarize-container .dt-foldall:focus-visible {
  outline: var(--blockr-focus-outline, 2px solid #2563eb);
  outline-offset: var(--blockr-focus-offset, 2px);
  border-radius: 2px;
}

/* Hierarchy. */
/* A nested table's chevron hangs into the label cell's left padding
   (margin-left: -18px). The shared 16px left it 2px outside the cell, under
   the 3px bar a clicked row draws at the cell edge. 24px leaves a gap; the
   header moves with it so the labels stay under it, and the children's
   inline 48px keeps them 24px deeper. */
.blockr-summarize-table[data-summarize-nested='1'] td.blockr-summarize-label-col,
.blockr-summarize-table[data-summarize-nested='1'] th.blockr-stub-header {
  padding-left: 24px;
}
.blockr-summarize-table tr.is-child td.blockr-summarize-label-col {
  color: var(--blockr-color-text-muted, #6b7280);
}
.blockr-summarize-table tr.is-child.collapsed-hidden { display: none; }
.blockr-summarize-table tr.is-parent .blockr-summarize-label {
  font-weight: 600;
}
.blockr-summarize-table tr.is-pick { cursor: pointer; }
.blockr-summarize-table tr.is-on {
  background: color-mix(in srgb, var(--blockr-summarize-fill) 10%, transparent);
}
/* Transient drill (a ctrl_target is set): the row is lit, held, released --
   there is nothing to un-set. A LAYER whose opacity fades, not the row's own
   background: the bars behind it are the data, and a background-image fade
   would animate discretely anyway (see the note in table.css). Same clock as
   the table's flash. */
.blockr-summarize-table tr.summarize-flash td {
  position: relative;
  z-index: 0;
}
.blockr-summarize-table tr.summarize-flash td::after {
  /* Single quotes: this stylesheet lives inside an R string literal. */
  content: '';
  position: absolute;
  inset: 0;
  z-index: -1;
  pointer-events: none;
  background: color-mix(in srgb, var(--blockr-summarize-fill) 10%, transparent);
  animation: summarize-flash-out 900ms linear 300ms both;
}
.blockr-summarize-table tr.summarize-flash td:first-child::after {
  box-shadow: inset 3px 0 0 0 var(--blockr-color-border-accent, #2563eb);
}
@keyframes summarize-flash-out {
  from { opacity: 1; }
  to { opacity: 0; }
}
.blockr-summarize-table tr.blockr-summarize-fold td {
  font-style: italic;
  color: var(--blockr-color-text-muted, #6b7280);
}
.blockr-summarize-table tr.blockr-summarize-hidden-search { display: none; }

/* Legend + footer. */
/* The legend is its own row under the control row (search + gear), so a long
   legend can never push the search box around. */
.blockr-summarize-legend {
  padding: 0.35rem 0.25rem 0.15rem;
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem 1.4rem;
  align-items: center;
  font-size: 0.8rem;
  color: var(--blockr-color-text-muted, #6b7280);
}
/* One group per colour column (a summarize table maps colour per column, so
   it can carry several). The wider gap BETWEEN groups keeps a title bound to
   the items it decodes. */
.blockr-summarize-legend-group {
  display: inline-flex;
  flex-wrap: wrap;
  gap: 0.6rem;
  align-items: center;
}
.blockr-summarize-legend-title {
  font-size: 0.7rem;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: var(--blockr-color-text-muted, #6b7280);
}
.blockr-summarize-legend-item {
  display: inline-flex;
  gap: 0.3rem;
  align-items: center;
}
.blockr-summarize-legend-item i {
  width: 10px;
  height: 10px;
  border-radius: 2px;
  display: inline-block;
}
.blockr-summarize-footer {
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  align-items: center;
  justify-content: space-between;
  padding: 0.4rem 0.1rem 0;
  font-size: 0.75rem;
  color: var(--blockr-color-text-muted, #6b7280);
}
.blockr-summarize-note { color: var(--blockr-color-text-warning, #b45309); }
"
}
