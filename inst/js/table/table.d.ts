/**
 * Types for the table block's scripts (inst/js/table/). Dev tooling only:
 * read by tsconfig.core.json, never served to the browser.
 *
 * The table R computes for the browser travels as `payload.data`
 * (TableModel, built in R/table-records.R, R/table-groups-view.R and
 * R/table-structured.R); the shapes here are the contract between the two.
 * Everything is column-oriented: a vector per column, one entry per row.
 */

/** Column name to values; `null` is a missing value. The shape of
 *  `selection.filters` and `selection.columns`. */
type TableClaim = Record<string, (string | null)[]>;

/** The table's selection: `config.selection`, and what a click sends. */
interface TableSelection {
  type: 'categorical';
  filters: TableClaim;
  columns?: TableClaim;
}

/** One header cell. Rows of these, top to bottom, make the header. */
interface TableHeadCell {
  /** Name lines, separated by "\n". Plain text. */
  text: string;
  /** The quiet second line ("N = 86"), or the column's label. */
  sub?: string | null;
  /** Columns the cell spans (a spanner covers several). */
  span?: number;
  /** Header rows the cell spans. */
  rows?: number;
  /** The body column under a leaf header: sorts it and aligns with it. */
  col?: number | null;
  /** What a click on the header claims. */
  keys?: TableClaim | null;
  /** The stub's header, top left. */
  stub?: boolean;
  /** A stub header the producer set in bold. */
  bold?: boolean;
}

/** One body column. */
interface TableCol {
  name: string;
  /** Numeric: right-aligned, sorted by number. */
  num?: boolean;
  /** Display text per row, `null` for a missing value. */
  disp: (string | null)[];
  /** The value a click on the row sends for this column. */
  raw?: (string | null)[] | null;
  /** Sort key per row; without it the display text is parsed. */
  key?: (number | string | null)[] | null;
  /** Cell fill and ink. */
  bg?: (string | null)[] | null;
  fg?: (string | null)[] | null;
  /** Data bar: width as a share of the cell, 0 to 1. */
  bar?: (number | null)[] | null;
  bar_fill?: string | null;
  /** What a click on a cell of this column claims, with the row's claim. */
  keys?: TableClaim | null;
  /** Groups: the percentage after a count, "(12%)". */
  pct?: (string | null)[] | null;
  /** Groups: the mark each cell draws (marks.js). */
  mark?: TableMark | null;
  /** Groups: the column's scale, printed once under its header. */
  axis?: TableAxis | null;
  /** Groups: a custom summary that failed; its cells hold the error. */
  failed?: boolean;
}

/** A column axis: tick positions (0 to 100) and their labels. `foot`: the
 *  strip goes under the table instead (a wide swimlane). */
interface TableAxis {
  at: number[];
  labels: string[];
  foot?: boolean;
}

type TableNums = (number | null)[];
type TableTexts = (string | null)[];

/** The marks of the groups layout, as R/groups-cells*.R computes them and
 *  R/table-groups-cols.R names them. Positions and widths are percentages of
 *  the cell. `valueWidth`: the width of the value slot beside the mark, in
 *  ch. `hover`: what the hover card adds. */
interface TableMarkBase { valueWidth?: number | null; hover?: unknown }

interface TableMarkBar extends TableMarkBase {
  kind: 'bar'; width: TableNums; sub?: boolean[] | null; fill?: string | null;
}

/** A colour split: per level, each row's segment width, value, label inside
 *  the segment and the label's ink; grouped, each level's value text. */
interface TableMarkSplit extends TableMarkBase {
  kind: 'barsplit'; mode?: string; names: string[]; fills: string[];
  segWidths: TableNums[]; segValues: TableNums[]; levelValues?: TableTexts[] | null;
  segLabels?: TableTexts[] | null; segInk?: string[] | null;
}

interface TableMarkDiv extends TableMarkBase { kind: 'bardiv'; width: TableNums; positive: boolean[] }

/** The statistics a hover card prints for a box or point range. */
interface TableLaneRaw {
  center?: TableNums; low?: TableNums; high?: TableNums;
  whiskerLow?: TableNums; whiskerHigh?: TableNums;
}

/** A box or a point range. A box: the low whisker from `whiskerLow`, the box
 *  from `boxLeft`, the median, the high whisker from `boxRight` to
 *  `whiskerHigh`. A point range: the fence, the range, the centre. */
interface TableLaneGeom {
  whiskerLow?: TableNums; whiskerLowWidth?: TableNums; boxLeft?: TableNums;
  boxWidth?: TableNums; median?: TableNums; boxRight?: TableNums;
  whiskerHighWidth?: TableNums; whiskerHigh?: TableNums;
  center?: TableNums; rangeLeft?: TableNums; rangeWidth?: TableNums;
  fenceLeft?: TableNums; fenceWidth?: TableNums; count?: TableNums;
  tip?: TableTexts; raw?: TableLaneRaw; bare?: boolean; disp?: TableTexts;
}

interface TableMarkLane extends TableMarkBase, TableLaneGeom {
  kind: 'box' | 'pointrange'; multi?: boolean; levels?: string[]; fills?: string[];
  lanes?: TableLaneGeom[]; reference?: undefined;
}

/** A dumbbell: from `from` to `to`, the link between them, the band behind. */
interface TablePairGeom {
  from: TableNums; to: TableNums; linkLeft: TableNums; linkWidth: TableNums;
  bandLeft: TableNums; bandWidth: TableNums;
  fill?: TableTexts; dash?: boolean[]; open?: boolean[]; tip?: TableTexts;
  disp?: TableTexts; fromTo?: TableTexts;
}

interface TableMarkPair extends TableMarkBase, Partial<TablePairGeom> {
  kind: 'pair'; reference?: number | null; multi?: boolean; levels?: string[]; fills?: string[];
  lanes?: TablePairGeom[];
}

/** Swimlane: per row, segments [left, width, level from 1, label?]. `dates`:
 *  the domain is in days; `large`: the column takes the width it needs. */
interface TableMarkInterval extends TableMarkBase {
  kind: 'interval'; segs: (number | string)[][][]; tips: string[][]; fills: string[];
  domainStart: number; domainEnd: number; dates?: boolean; large?: boolean;
}

/** Sparkline in a 100 x 36 box: the line's points, the value band, the last
 *  point, the x and y values for the hover, the reference line and band. */
interface TableMarkSpark extends TableMarkBase {
  kind: 'sparkline'; line: string[]; band?: TableTexts | null; dotX: TableNums; dotY: TableNums;
  xValues: string[]; yValues: string[]; count?: TableNums; refY?: number | null;
  refBandY?: number | null; refBandHeight?: number | null;
}

interface TableMarkTile extends TableMarkBase { kind: 'tile' }

type TableMark = TableMarkBar | TableMarkSplit | TableMarkDiv | TableMarkLane | TableMarkPair |
  TableMarkInterval | TableMarkSpark | TableMarkTile;

/** A section header: a row above the rows it groups, from `at` on. */
interface TableSection {
  /** The first row under it. */
  at: number;
  /** 1 is the outermost. */
  level: number;
  value: string;
  /** Written before the value, "Sex: ". */
  label?: string | null;
  /** What a click on its value claims; without keys it only folds. */
  keys?: TableClaim | null;
}

/** Per-row extras of a structured table. */
interface TableRows {
  indent?: number[] | null;
  strong?: boolean[] | null;
  emph?: boolean[] | null;
  /** What a click on the row claims; `null` rows do not click. */
  keys?: (TableClaim | null)[] | null;
  /** Row tint, drawn as a strip on the stub. */
  tint?: (string | null)[] | null;
}

/** One column of cards: a card per row. */
interface TableCardCol {
  label: string;
  main: (string | null)[];
  sub?: (string | null)[] | null;
  /** The statistic's name, "Mean". */
  word?: string | null;
  range?: string | null;
  /** Index of the glyph the layout draws in the card, if any. */
  glyph?: number | null;
  legend?: { color: string; label: string }[] | null;
  text?: boolean;
}

/** The cards look: one row of cards per table row. */
interface TableCards {
  n: number;
  /** Several rows: a row head per row and a head per column. */
  multi?: boolean;
  label?: (string | null)[] | null;
  hex?: (string | null)[] | null;
  cols: TableCardCol[];
  /** The note under the cards about rows not shown. */
  fold?: string | null;
}

/** A table to draw: `payload.data`. */
interface TableModel {
  n: number;
  /** Index of the stub (row names) column, or null. */
  stub?: number | null;
  cols: TableCol[];
  head: TableHeadCell[][];
  /** Column widths in px, stub first. */
  widths?: number[] | null;
  rows?: TableRows | null;
  sections?: TableSection[] | null;
  /** A flat table's drill column: a click claims the row's `raw` value of
   *  the column with this name. */
  drill?: string | null;
  /** Start with every group folded. */
  collapsed?: boolean;
  cards?: TableCards | null;
  /** A line under the rows ("12 more in Other"). */
  note?: string | null;
}

interface TableLegend {
  groups: { title: string; items: { color: string; label: string }[] }[];
}

/** The table block's `derived`, as R builds it. The table itself is
 *  `payload.data`. */
interface TableDerived {
  titles?: Record<string, VizTitleBand | string | null> | null;
  /** Say this instead of drawing a table. */
  empty?: { kind: string; message: string } | null;
  /** The drill receipt line, "Filtered: Arm = Placebo". */
  receipt?: string | null;
  /** Groups: one titled group of colours per colour column. */
  legend?: TableLegend | null;
  downloads?: VizDownload[] | null;
  /** The blocks a drill can be sent to, for the gear's Target filter. */
  ctrl_choices?: VizChoice[] | null;
  /** The prepare script's inputs, values and error (core/script-band.js). */
  script?: VizScript | null;
  /** The pixel ratio of the Image and PowerPoint picture. */
  capture_ratio?: number | null;
}

/** What the Grid works out once per model (dom.js `structure()`). */
interface TableStructure {
  /** Each row's block: the row its innermost section starts at, or -1. */
  blk: number[];
  /** Each block's sections, outermost first, by index into `sections`. */
  path: Map<number, number[]>;
  indent: number[] | null;
  /** Rows that head an indented group. */
  toggle: boolean[];
  /** The smallest indent. */
  base: number;
  /** The deepest section level. */
  levels: number;
}

/** An item of the drawn body: a section header or a row. */
type TableItem = { s: number; i?: undefined } | { i: number; s?: undefined };

/** What the Grid draws a row or section with, besides the model. */
interface TableGridHooks {
  /** Classes added to row `i` (the drill highlight). */
  rowClass?(i: number): string;
  /** Classes added to section `s`. */
  secClass?(s: number): string;
  /** Whether a click on a keyed header or section value drills. */
  drills?(): boolean;
  /** A click that is not the Grid's own (sort, fold). */
  click?(e: MouseEvent): void;
  /** After every draw of the body. */
  drawn?(): void;
  /** The rows and section headers the selection lights; a new model opens
   *  the folds that would hide them. */
  lit?(): { rows: Iterable<number>; sections: Iterable<number> };
  /** A body cell drawn by the layout instead (the groups marks); null for
   *  the plain cell. */
  cell?(c: TableCol, i: number): string | null;
  /** A column's axis strip (groups): under its header, or in the foot row. */
  axis?(c: TableCol): string;
}

interface TableGridFlags {
  sortable?: boolean;
  collapsible?: boolean;
  /** Extra classes and attributes of the <table>, and of the stub cells. */
  cls?: string;
  attrs?: Record<string, string>;
  stubCls?: string;
  /** Draw every row, never a window (a picture). */
  all?: boolean;
}

interface TableGrid {
  readonly box: HTMLElement;
  readonly host: HTMLElement;
  hooks: TableGridHooks;
  table: HTMLTableElement | null;
  model: TableModel | null;
  query: string;
  sortCol: number | null;
  sortDir: number;
  shut: Set<number>;
  shutRows: Set<number>;
  rowH: number;
  win: number[];
  set(model: TableModel, flags?: TableGridFlags): void;
  clear(): void;
  search(q: string): void;
  sort(col: number | null, dir: number): void;
  fold(s: number, shut: boolean): void;
  foldAll(shut: boolean): void;
  reveal(rows: Iterable<number>, sections?: Iterable<number>): void;
  folds(): boolean;
  draw(): void;
  counts(): { shown: number; total: number };
  items(): TableItem[];
  headAt(el: Element | null): { th: Element; h: TableHeadCell } | null;
}

/** How a TableView is built outside the block's binding (static.js). */
interface TableViewOpts {
  /** false: no gear and no title slots. */
  gear?: boolean;
  /** Every row drawn and no tools, as for a printed page. */
  still?: boolean;
}

/** One table layout's view, drawn into the block's frame. */
interface TableLayoutView {
  render(payload: VizPayload): void;
  dispose(): void;
  /** What it draws, for the gear's rules. */
  info?(): TableInfo;
}

/** What a layout draws, as far as the gear's rules ask. */
interface TableInfo {
  /** Sections or keyed rows: a nested table, a Table 1, a finished input. */
  structured: boolean;
  /** A finished input: structured without `by`, shown as it came. */
  finished: boolean;
  /** Numeric columns, which `format` and `unit` change. */
  numbers: boolean;
}

/** The mounted gear: the engine, and a way to open its tray. */
interface TableGearHandle {
  engine: VizGear;
  open(): void;
  update(p: VizPayload): void;
}

/** The title slots (core/titles.js): words of a title that open controls. */
/** How a layout draws: `still` is a picture or a page without Shiny. */
interface TableLayoutOpts {
  still?: boolean;
}

type TableLayoutClass = new (frame: TableFrame, scope: VizScope, opts?: TableLayoutOpts) => TableLayoutView;

/** The block's frame (chrome.js): the core frame with a search box and the
 * scroll box the layout draws into. */
interface TableFrame extends VizFrame {
  readonly box: HTMLElement;
  readonly searchInput: HTMLInputElement;
  query(): string;
  onSearch(fn: (q: string) => void): () => void;
  showTools(o: { search: boolean; download: boolean; downloads?: VizDownload[] | null;
                 onOpen?: () => void }): void;
  structured(on: boolean): void;
  scroller(): Element | null;
}

interface TableCardOpts {
  order?: number[];
  rowClass?(i: number): string;
  glyph?(k: number, i: number): string;
}

/** What the drill needs from its view. */
interface TableDrillContext {
  /** The model drawn, as a table or as cards. */
  model(): TableModel | null;
  /** The selection the view shows: R's, or the click before R answered. */
  selection(): TableSelection | null;
  /** Record a click; the view repaints. */
  select(sel: TableSelection | null): void;
  /** A claim was sent to a drill target. */
  sent(sel: TableSelection): void;
  /** A drill target is set: clicks are events, nothing latches. */
  transient(): boolean;
}

interface TableDrill {
  /** Classes for row `i` and section `s` under the current selection. */
  rowClass(i: number): string;
  secClass(s: number): string;
  /** Light the claimed columns. */
  paint(): void;
  /** A click on card row `i` (cards share the rows' claims). */
  clickRow(i: number, el: Element): void;
}

interface VizTableNamespace {
  /** Table layouts by name; each layout's script adds itself. */
  layouts: Record<string, TableLayoutClass>;
  Grid: new (box: HTMLElement, scope: VizScope, hooks?: TableGridHooks) => TableGrid;
  parseNum(s: unknown): number | null;
  rowText(model: TableModel, i: number): string;
  Frame: new (el: HTMLElement, scope: VizScope) => TableFrame;
  View: VizViewClass;
  /** Gear editors for keys the engine draws no control for, by
   *  "<layout>.<key>" or "<key>". */
  editors: Record<string, (el: HTMLElement, ctx: VizCustomCtx) => void>;
  variables: {
    varOf(s: Record<string, unknown>): string | null;
    withVars(summaries: Record<string, unknown>[], vars: string[],
             columns: VizPayload['columns']): Record<string, unknown>[];
  };
  /** A picture of a groups table drawn off screen (groups.js). */
  picture(payload: VizPayload, width: number, ratio?: number): Promise<VizPicture>;
  /** Draw the tables R wrote into a page without Shiny (static.js). */
  mount(node?: ParentNode): void;
  /** Draw a payload on an element without the output binding, or redraw it. */
  renderStatic(el: HTMLElement, payload: VizPayload, opts?: TableViewOpts): void;
  /** The records layout; groups extends it. */
  Records: new (frame: TableFrame, scope: VizScope, opts?: TableLayoutOpts) => TableLayoutView;
  html: {
    th(model: TableModel, r: number, c: number,
       f: { sortable: boolean; foldAll: boolean; axis?: (c: TableCol) => string }): string;
    td(c: TableCol, i: number): string;
    section(m: TableModel, s: number,
            o: { shut: boolean; fold: boolean; extra: string }): string;
    row(m: TableModel, st: TableStructure, i: number,
        o: { last: boolean; shut: boolean; extra: string; stubCls?: string;
             cell?(c: TableCol, i: number): string | null }): string;
    foot(m: TableModel, axis: (c: TableCol) => string): string;
  };
  drill: {
    same(a: TableSelection | null, b: TableSelection | null): boolean;
    covers(claim: TableClaim, keys: TableClaim | null | undefined): boolean;
    equal(a: TableClaim | null | undefined, b: TableClaim | null | undefined): boolean;
    label(sel: TableSelection | null, labels: Record<string, string>): string;
    clickable(m: TableModel | null): boolean;
    wire(grid: TableGrid, frame: TableFrame, scope: VizScope,
         ctx: TableDrillContext): TableDrill;
  };
  cards: {
    render(host: HTMLElement, cards: TableCards, scope: VizScope,
           opts?: TableCardOpts): void;
  };
  marks: {
    glyph(c: TableMark | null | undefined, i: number, labels: boolean): string;
    cell(c: TableCol, i: number, o: { labels: boolean }): string;
    axis(c: TableCol, labels: boolean): string;
  };
  summaries: {
    shows(s: Record<string, unknown>, f: string): boolean;
    title(s: Record<string, unknown>): string;
    describe(s: Record<string, unknown>): string;
  };
  tooltip: {
    wire(box: HTMLElement, scope: VizScope, model: () => TableModel | null): void;
    card(model: TableModel, k: number, i: number, cell: Element, t: Element): string;
  };
  gear: {
    mount(frame: TableFrame, scope: VizScope, info: () => TableInfo,
          onLayout?: (layout: string) => void): TableGearHandle | null;
    visible(key: string, v: Record<string, unknown>, info: TableInfo): boolean;
    shadings(el: HTMLElement, ctx: VizCustomCtx): void;
  };
  /** A number column of the payload's `columns` (gear.js). */
  /** blockr.ui's controls for the table's own gear rows (gear.js). */
  ui: {
    pick(el: HTMLElement, options: { value: string; label: string }[], value: string,
         o: { label: string; placeholder?: string }, onPick: (v: string) => void,
         scope: VizScope): void;
    check(el: HTMLElement, label: string, on: boolean, onToggle: (on: boolean) => void): void;
    button(cls: string, text: string, label: string, fn: () => void,
           scope: VizScope): HTMLButtonElement;
  };
}

interface VizNamespace {
  table: VizTableNamespace;
}
