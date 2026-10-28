/**
 * Types for blockr.viz's core scripts (inst/js/core/). Dev tooling only:
 * read by tsconfig.core.json, never served to the browser.
 */

interface VizScope {
  readonly disposed: boolean;
  on(target: EventTarget, type: string, fn: EventListener,
     opts?: boolean | AddEventListenerOptions): () => void;
  resize(el: Element, fn: ResizeObserverCallback): () => void;
  mutate(node: Node, fn: MutationCallback, opts: MutationObserverInit): () => void;
  timeout(fn: () => void, ms: number): () => void;
  interval(fn: () => void, ms: number): () => void;
  frame(fn: () => void): () => void;
  debounce<A extends unknown[]>(fn: (...args: A) => void, ms: number): (...args: A) => void;
  own<T extends { dispose: () => void }>(obj: T): T;
  node<T extends Node>(node: T): T;
  size(): number;
  dispose(): void;
}

/**
 * One column of the block's input (R/wire.R, viz_wire_columns()). `class`
 * is the wire class: number, string, bool, date, datetime or duration.
 * `levels` comes with a factor, `tz` with a date-time, `unit` with a
 * duration, `kind` with a column mark_column_kinds() marked. `label` is the
 * variable label, "" when the column has none.
 */
interface VizColumn {
  name: string;
  class: 'number' | 'string' | 'bool' | 'date' | 'datetime' | 'duration';
  label: string;
  n_unique: number;
  levels?: string[];
  tz?: string;
  unit?: string;
  kind?: 'group' | 'time' | 'value' | 'id';
}

/** What R sends a block view (R/kit-output.R, viz_payload()). */
interface VizPayload {
  state_rev: number;
  data_rev: number;
  columns: VizColumn[];
  config: Record<string, unknown>;
  derived: Record<string, unknown>;
  data?: Record<string, unknown> | null;
}

/** A block view: built by the shared binding on its element's first value. */
interface VizView {
  render(payload: VizPayload): void;
  dispose(): void;
  /** Shows an error R sent; a silent req() never reaches it. */
  error?(message: string): void;
}

type VizViewClass = new (el: HTMLElement, scope: VizScope) => VizView;

/** A mounted view: what the binding keeps per element. */
interface VizMount {
  el: HTMLElement;
  scope: VizScope;
  view: VizView;
  last: VizPayload | null;
  needRev: number | null;
  /** The last value drawn, with its rows. */
  drawn: VizPayload | null;
}

/** One row of a view's download menu (R/export-registry.R). */
interface VizDownload {
  id: string;
  label: string;
  ext: string;
}

/** Why a view has nothing to draw: R's `derived.empty`, or the view's own. */
interface VizEmpty {
  kind?: string;
  message?: string;
}

/** The frame every block view draws in (core/frame.js). */
interface VizFrame {
  readonly id: string;
  readonly scope: VizScope;
  readonly el: HTMLElement;
  readonly head: HTMLElement;
  readonly titleBox: HTMLElement;
  readonly titleEl: HTMLElement;
  readonly subEl: HTMLElement;
  readonly tools: HTMLElement;
  readonly downloadSlot: HTMLElement;
  readonly body: HTMLElement;
  readonly capEl: HTMLElement;
  readonly foot: HTMLElement;
  readonly countEl: HTMLElement;
  readonly statusEl: HTMLElement;
  readonly clearBtn: HTMLButtonElement;
  slots: VizSlots | null;
  gear: VizGear | null;
  tray?: { band: HTMLElement; button: HTMLButtonElement; set(on: boolean): void; isOpen(): boolean };
  openGear(): void;
  mountGear(o: { block: string; typeKey: string; title: string; host: VizGearHost }): VizGear;
  titles(t: VizTitles | Record<string, VizTitleBand | string | null> | null | undefined): void;
  downloads(list: VizDownload[] | null | undefined, onOpen?: () => unknown): void;
  empty(e: VizEmpty | null | undefined): boolean;
  count(text: string): void;
  status(text: string | null, onClear?: (() => void) | null): void;
  note(text: string | null): void;
  flash(text: string): void;
  busyDone(): void;
}

/** The value of one script control, keyed by its `sv_` key in `values`. */
type VizScriptValue = string | number | string[];

/** One option of a flag's control (R/prepare-apply.R, dd_script_roles()). */
interface VizScriptOption {
  value: 'on' | 'off';
  label: string;
}

/** A control the prepare script declares, ready for the gear's builder. */
interface VizScriptControl {
  name: string;
  /** The config key the control writes to: `sv_` and the name. */
  key: string;
  label: string;
  kind: 'select' | 'multi' | 'segmented' | 'number' | 'date' | 'text';
  /** Bare strings for `select` and `multi`, the on and off rows for `segmented`. */
  options?: string[] | VizScriptOption[];
  /** From the declaration's annotation; a number, or a date for a date control. */
  min?: number | string;
  max?: number | string;
  step?: number;
  placeholder?: string;
}

/** A declaration that could not become a control, with its reason. */
interface VizScriptFailed {
  name: string;
  key: string;
  label: string;
  kind: 'error';
  error: string;
}

type VizScriptInput = VizScriptControl | VizScriptFailed;

/**
 * The prepare script's controls and state (`derived.script`, both blocks).
 * `inputs` is `[]` without declarations. `values` holds an entry per working
 * control and is `[]` rather than `{}` when there is none, since R builds it
 * as an unnamed empty list. `error` is the script's message, or null.
 */
interface VizScript {
  inputs: VizScriptInput[];
  values: Record<string, VizScriptValue> | [];
  error: string | null;
}

/** The band above a view that draws the prepare script's controls (core/script-band.js). */
interface VizScriptBand {
  readonly el: HTMLElement;
  update(script: VizScript | null | undefined): void;
}

/** A picture of a view: a PNG data URL and its size in CSS px. */
interface VizPicture {
  png: string;
  width: number;
  height: number;
}

interface VizNamespace {
  /** The block specs, generated from R (inst/js/spec/). */
  spec: Record<string, unknown>;
  util: {
    esc(x: unknown): string;
    send(id: string, kind: 'set' | 'select' | 'capture' | 'need', value: unknown,
         block?: string): boolean;
    select(id: string, selection: Record<string, unknown> | null): boolean;
    isNumeric(c: VizColumn | null | undefined): boolean;
    isTemporal(c: VizColumn | null | undefined): boolean;
    num(v: unknown): unknown;
    num3(v: unknown): unknown;
  };
  tooltip: {
    swatch(color: unknown): string;
    head(html: string, color?: unknown): string;
    row(label: string, value: unknown, color?: unknown, hit?: boolean | null): string;
    note(html: string): string;
    SEP: string;
  };
  Scope: { new (): VizScope; open(): number };
  scriptBand(parent: HTMLElement, scope: VizScope, gear: VizGear): VizScriptBand;
  Frame: {
    new (host: HTMLElement, scope: VizScope, opts?: { cls?: string }): VizFrame;
    message(parent: HTMLElement, e: VizEmpty | null | undefined): boolean;
  };
  binding(name: string, view: string, View: VizViewClass,
          opts?: { grace?: number; busy?: string }): boolean;
  downloads(host: HTMLElement, list: VizDownload[] | null | undefined,
            scope: VizScope, opts?: { onOpen?: () => unknown }): void;
  capture(opts: { width: number; height: number;
                  draw: (host: HTMLElement) => VizPicture | Promise<VizPicture>;
                  timeout?: number }): Promise<VizPicture>;
  captureReply(req: string, p: Promise<VizPicture>): Promise<void>;
}
