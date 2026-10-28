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
    isNumeric(c: VizColumn | null | undefined): boolean;
    isTemporal(c: VizColumn | null | undefined): boolean;
  };
  Scope: { new (): VizScope; open(): number };
  binding(name: string, view: string, View: VizViewClass,
          opts?: { grace?: number; busy?: string }): boolean;
  downloads(host: HTMLElement, list: VizDownload[] | null | undefined,
            scope: VizScope, opts?: { onOpen?: () => void }): void;
  capture(opts: { width: number; height: number;
                  draw: (host: HTMLElement) => VizPicture | Promise<VizPicture>;
                  timeout?: number }): Promise<VizPicture>;
  captureReply(req: string, p: Promise<VizPicture>): Promise<void>;
}
