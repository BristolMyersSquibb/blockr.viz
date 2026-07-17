/**
 * Protocol / ambient types for blockr.viz's hand-written JS.
 *
 * This file is the contract between the R side (block servers, *-dep.R) and
 * the JS in inst/js/ (chart.js, table.js, tile-block.js, summary-table-block.js,
 * drilldown-config.js). It is dev-tooling only: type-checked via tsconfig.json /
 * `tsc`, never referenced by an htmlDependency, never run in the browser.
 *
 * blockr.viz reuses blockr.dplyr's shared JS (blockr-core.js + blockr-select.js)
 * at runtime, so the Blockr namespace below declares only the slice this package
 * consumes — it is intentionally a subset of blockr.dplyr's own types.d.ts.
 */

/* --- Column metadata (R pushes these; JS host `columns()` returns them) --- */

interface VizColumn {
  name: string;
  /** A coarse type tag; conventions vary per host ('numeric'/'categorical'/'any'/...). */
  type: string;
  /** attr(col, "label") or "" */
  label?: string;
  /** distinct count, used to decide a column "fits" a categorical role */
  n_unique?: number;
  /** factor-level ordering (chart category sort); absent for non-factor columns */
  levels?: string[];
}

/* --- Table data-push payload (table-block.R -> table.js) ---
   Sent as ONE pre-serialized JSON string over the "blockr-table-data"
   custom message ({id, rev, payload}); pre-serializing dodges Shiny's
   auto_unbox scalar-collapse and lets `rev` gate re-parsing. See
   dev/table-data-push-design.md. */

/** One rendered column of a flat table (entry 0 is the row stub). */
interface VizTableCol {
  /** Constant td class for the column ("blockr-data dt-num", "blockr-stub", ...). */
  cls: string;
  /** PLAIN display strings; null = NA (JS renders the em-dash cell). */
  disp: (string | null)[];
  /** PLAIN raw values for data-raw (drill/group columns only); null = NA. */
  raw?: (string | null)[];
  /** Pre-built ' style="..."' chunks (shading/bar; generated, attr-safe). */
  style?: (string | null)[];
}

interface VizTablePayload {
  /** "flat" = cell model + windowed render; "html" = inject + legacy wire
      (structured tables, message tables, render errors — all small). */
  kind: 'flat' | 'html';
  /** kind "html": the complete <table> (or error <div>) HTML. */
  html?: string;
  /** kind "flat": <table ...data-dt-*><colgroup/><thead/><tbody/></table>
      with an EMPTY tbody; the gear reads its state off these attributes. */
  head?: string;
  /** kind "flat": row count. */
  n?: number;
  /** kind "flat": stub + value columns, rendered order. */
  cols?: VizTableCol[];
  /** kind "flat": row indexes (0-based) rendered with dt-row-nodrill. */
  nodrill?: number[];
}

/* --- Drilldown gear-popover engine (drilldown-config.js) ---
   The engine is host-agnostic: chart.js, table.js and tile-block.js each build
   a `host` object and do `new Blockr.DrilldownConfig(host)`. Role keys are the
   R-side config params. Typed permissively here; the class body refines it. */

interface VizDrilldownRole {
  kind?: 'column' | 'select' | string;
  allowCount?: boolean | ((cfg: Record<string, any>) => boolean);
  /** Placeholder: the empty slot's voice. `phBy` keys it by host context(). */
  ph?: string;
  phBy?: Record<string, string>;
  /** Help line: speaks about the value, and survives the field being filled. */
  hint?: string;
  hintBy?: Record<string, string>;
  // Roles carry many host-specific, dynamically-shaped fields (colTypeBy,
  // optionsBy, options, pairedWith, maxUnique, placeholder, label, ...).
  [field: string]: any;
}

interface VizDrilldownHost {
  /** Current column metadata. */
  columns(): VizColumn[] | null | undefined;
  /** Current persisted config (role -> value); values are dynamic JSON. */
  config(): Record<string, any>;
  /** Role specs, keyed by R config param. */
  roles: Record<string, VizDrilldownRole>;
  /** Host context tag (e.g. chart family) selecting type-conditional sections. */
  context(): string;
  /** Optional type-picker groups (chart only; null when the host has none). */
  typeGroups?: Array<{ label?: string; types: string[] }> | null;
  /** Fires when a config entry commits, with the role key that changed. */
  onChange: (key: string) => void;
  // The host exposes many block-specific callbacks (popoverEl, context,
  // currentType, sections, isOpen, reopen, onClearFilter, ...) — all dynamic.
  [field: string]: any;
}

declare class VizDrilldownConfig {
  constructor(host: VizDrilldownHost);
  /** Rebuilds the popover DOM in place. */
  render(): void;
  /** render() from the host's CURRENT state, re-seeding section checkboxes. */
  refresh(): void;
  [member: string]: any;
}

/* --- Shared aggregation vocabulary (drilldown-agg.js) ---
   The group/value/func role triple + AGG_FNS + value-follows-agg reconcile,
   consumed identically by chart.js, table.js and tile-block.js. Exposed as
   Blockr.DrilldownAgg (and window.DrilldownAgg). */

interface VizDrilldownAgg {
  /** Aggregation-function select options; mirrors R AGG_FNS (drift-tested). */
  AGG_FNS: Array<{ value: string; label: string }>;
  /** One word per aggregation for composed labels ("Mean AGE", axis titles). */
  AGG_WORDS: Record<string, string>;
  /** The group + value + func role-spec triple hosts spread into their ROLES. */
  aggRoles(opts?: { multiple?: boolean }): Record<string, VizDrilldownRole>;
  /** Keep `value` consistent with `func`; mutates cfg in place. */
  reconcileValue(cfg: Record<string, any>, columns: VizColumn[] | null | undefined): void;
}

/* --- Blockr namespace: the subset blockr.viz consumes from blockr.dplyr --- */

/** Option entry: a bare value string, or {value, label} for a muted label. */
type BlockrSelectOption = string | { value: string; label?: string };

interface BlockrSelectHandleBase {
  el: HTMLDivElement;
  setOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  updateOptions(opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined): void;
  setLoading(flag: boolean): void;
  destroy(): void;
}

interface BlockrSelectSingleHandle extends BlockrSelectHandleBase {
  getValue(): string;
}

interface BlockrSelectMultiHandle extends BlockrSelectHandleBase {
  getValue(): string[];
}

interface BlockrSelectSingleConfig {
  options?: BlockrSelectOption[];
  selected?: string | null;
  placeholder?: string;
  onChange?: (value: string) => void;
  [opt: string]: unknown;
}

interface BlockrSelectMultiConfig {
  options?: BlockrSelectOption[];
  selected?: string[];
  placeholder?: string;
  reorderable?: boolean;
  /** Cap on simultaneously-selected values (summary-table uses max 2). */
  max?: number;
  onChange?: (value: string[]) => void;
  [opt: string]: unknown;
}

interface BlockrSelectStatic {
  single(container: HTMLElement, config: BlockrSelectSingleConfig): BlockrSelectSingleHandle;
  multi(container: HTMLElement, config: BlockrSelectMultiConfig): BlockrSelectMultiHandle;
}

interface BlockrNamespace {
  /** Shared select component (blockr-select.js). */
  Select?: BlockrSelectStatic;
  /** SVG icon strings (gear, plus, ...). */
  icons: Record<string, string>;
  /** Document-level click delegate that drops listeners for removed nodes. */
  onDocClick(el: Element, cb: (e: MouseEvent) => void): void;
  uid(prefix?: string): string;
  escapeHtml(s: string): string;
  removeNode(node: Node | null | undefined): void;
  contentWidth(el: Element): number;
  /** The shared drilldown popover engine (defined in this package). */
  DrilldownConfig: typeof VizDrilldownConfig;
  /** Shared aggregation vocabulary (drilldown-agg.js). */
  DrilldownAgg?: VizDrilldownAgg;
  /** Design-system checkbox factory (settings-band.js). */
  checkbox(
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ): {
    el: HTMLLabelElement;
    input: HTMLInputElement;
    set(v: boolean): void;
    get(): boolean;
  };
  [member: string]: unknown;
}

declare var Blockr: BlockrNamespace;

/* --- Ambient third-party globals (no @types dependency) --- */

/** ECharts instance — typed loosely; option shapes stay `any`. */
interface EChartsInstance {
  setOption(option: any, opts?: any): void;
  getOption(): any;
  on(event: string, handler: (params: any) => void): void;
  on(event: string, query: any, handler: (params: any) => void): void;
  off(event: string, handler?: (params: any) => void): void;
  dispatchAction(action: any): void;
  convertToPixel(finder: any, value: any): number[] | number;
  convertFromPixel(finder: any, value: any): number[] | number;
  getZr(): any;
  getWidth(): number;
  getHeight(): number;
  resize(opts?: any): void;
  dispose(): void;
  getModel(): any;
  [member: string]: any;
}

declare const echarts: {
  init(el: HTMLElement, theme?: string | object | null, opts?: any): EChartsInstance;
  getInstanceByDom(el: HTMLElement): EChartsInstance | undefined;
  registerTheme(name: string, theme: object): void;
  connect(group: string | EChartsInstance[]): void;
  [member: string]: any;
};

declare const Shiny: {
  InputBinding: new () => any;
  inputBindings: { register(binding: object, name: string): void };
  addCustomMessageHandler(name: string, handler: (msg: any) => void): void;
  setInputValue(
    name: string,
    value: unknown,
    opts?: { priority?: 'event' | 'immediate' | 'deferred' }
  ): void;
  bindAll?(scope?: unknown): void;
  unbindAll?(scope?: unknown): void;
};

declare function jQuery(selector: unknown): any;
declare const $: typeof jQuery;

interface Window {
  // All optional: code guards on their presence (`window.echarts`,
  // `if (window.jQuery)`, the `|| window.DrilldownConfig` fallback, ...).
  Blockr?: BlockrNamespace;
  DrilldownConfig?: typeof VizDrilldownConfig;
  DrilldownAgg?: VizDrilldownAgg;
  Shiny?: typeof Shiny;
  echarts?: typeof echarts;
  jQuery?: typeof jQuery;
}
