/**
 * Types for the table block's scripts (inst/js/table/). Dev tooling only:
 * read by tsconfig.core.json, never served to the browser.
 *
 * `derived.table` is what R computes for the browser (R/table-model.R); the
 * shapes here are the contract between the two. Everything is
 * column-oriented: a vector per column, one entry per row.
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
}

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

/** A table to draw: `derived.table`. */
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
}

/** The table block's `derived`, as R builds it. */
interface TableDerived {
  table?: TableModel | null;
  titles?: { title?: string | null; subtitle?: string | null;
             caption?: string | null } | null;
  /** Say this instead of drawing a table. */
  empty?: { kind: string; message: string } | null;
  /** The drill receipt line, "Filtered: Arm = Placebo". */
  receipt?: string | null;
  downloads?: unknown;
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
}

interface TableGridFlags {
  sortable?: boolean;
  collapsible?: boolean;
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
  folds(): boolean;
  draw(): void;
  counts(): { shown: number; total: number };
  items(): TableItem[];
  headAt(el: Element | null): { th: Element; h: TableHeadCell } | null;
}

/** One table layout's view, drawn into the block's frame. */
interface TableLayoutView {
  render(payload: VizPayload): void;
  dispose(): void;
}

type TableLayoutClass = new (frame: TableFrame, scope: VizScope) => TableLayoutView;

/** The block's frame: header, scroll box, footer (chrome.js). */
interface TableFrame {
  readonly id: string;
  readonly el: HTMLElement;
  readonly head: HTMLElement;
  readonly tools: HTMLElement;
  readonly box: HTMLElement;
  readonly foot: HTMLElement;
  readonly searchInput: HTMLInputElement;
  query(): string;
  onSearch(fn: (q: string) => void): void;
  titles(t: TableDerived['titles']): void;
  showTools(o: { search: boolean; download: boolean; downloads?: unknown }): void;
  empty(e: TableDerived['empty']): boolean;
  structured(on: boolean): void;
  count(text: string): void;
  receipt(text: string | null, onClear: (() => void) | null): void;
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
  html: {
    th(model: TableModel, r: number, c: number,
       f: { sortable: boolean; foldAll: boolean }): string;
    td(c: TableCol, i: number): string;
    section(m: TableModel, s: number,
            o: { shut: boolean; fold: boolean; extra: string }): string;
    row(m: TableModel, st: TableStructure, i: number,
        o: { last: boolean; shut: boolean; extra: string }): string;
  };
  drill: {
    same(a: TableSelection | null, b: TableSelection | null): boolean;
    covers(claim: TableClaim, keys: TableClaim | null | undefined): boolean;
    equal(a: TableClaim | null | undefined, b: TableClaim | null | undefined): boolean;
    label(sel: TableSelection | null, labels: Record<string, string>): string;
    wire(grid: TableGrid, frame: TableFrame, scope: VizScope,
         ctx: TableDrillContext): TableDrill;
  };
  cards: {
    render(host: HTMLElement, cards: TableCards, scope: VizScope,
           opts?: TableCardOpts): void;
  };
}

interface VizNamespace {
  table: VizTableNamespace;
}
