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

/** What R sends a block view (R/kit-output.R, viz_payload()). */
interface VizPayload {
  state_rev: number;
  data_rev: number;
  columns: Array<{ name: string; class: string; label: string; n_unique: number;
                   [meta: string]: unknown }>;
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
}

interface VizNamespace {
  /** The block specs, generated from R (inst/js/spec/). */
  spec: Record<string, unknown>;
  util: {
    esc(x: unknown): string;
    send(id: string, kind: 'set' | 'select' | 'capture' | 'need', value: unknown,
         block?: string): boolean;
  };
  Scope: { new (): VizScope; open(): number };
  binding(name: string, view: string, View: VizViewClass,
          opts?: { grace?: number }): boolean;
}
