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

interface VizNamespace {
  /** The block specs, generated from R (inst/js/spec/). */
  spec: Record<string, unknown>;
  util: {
    esc(x: unknown): string;
    send(id: string, kind: 'set' | 'select' | 'capture', value: unknown,
         block?: string): boolean;
  };
  Scope: { new (): VizScope; open(): number };
}
