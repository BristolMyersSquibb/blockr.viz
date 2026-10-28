/**
 * Ambient types for the scripts in inst/js/ outside the rewritten core,
 * chart and table (drilldown-theme-register.js), which
 * tsconfig.json checks. The rewritten JS has its own types (core/core.d.ts,
 * table/table.d.ts) under tsconfig.core.json. Dev tooling only: never
 * referenced by an htmlDependency, never run in the browser.
 */

/** ECharts, typed loosely: option shapes stay `any`. */
declare const echarts: {
  init(el: HTMLElement, theme?: string | object | null, opts?: any): any;
  registerTheme(name: string, theme: object): void;
  [member: string]: any;
};

declare const Shiny: {
  addCustomMessageHandler(name: string, handler: (msg: any) => void): void;
  setInputValue(
    name: string,
    value: unknown,
    opts?: { priority?: 'event' | 'immediate' | 'deferred' }
  ): void;
};

interface Window {
  // Optional: code guards on their presence.
  Shiny?: typeof Shiny;
  echarts?: typeof echarts;
}
