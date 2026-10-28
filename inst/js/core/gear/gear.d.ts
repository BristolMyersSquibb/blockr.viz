/**
 * Types for the gear engine (inst/js/core/gear/) and the title slots
 * (inst/js/core/titles.js). Dev tooling only, read by tsconfig.core.json.
 */

/** The `gear` field of a spec argument (R/spec.R, gear()). */
interface VizGearField {
  group: string;
  control: string;
  label: string;
  labels?: string[];
  parse?: string;
  when?: string;
}

/** One argument of a generated spec (inst/js/spec/*.js). */
interface VizArg {
  type: { name: string; of?: string; [k: string]: unknown };
  default?: unknown;
  values?: unknown[];
  nullable: boolean;
  applies?: { only?: string[]; except?: string[] };
  also?: string[];
  bounds?: { min: number | null; max: number | null; minOpen: boolean };
  gear?: VizGearField;
  slot?: boolean;
  ctrl?: boolean;
  option?: string;
}

type VizSpec = Record<string, VizArg>;
type VizValues = Record<string, unknown>;

/** An option. `note` is muted text after the label (a column's variable label). */
interface VizChoice { value: unknown; label: string; note?: string }

/** The view's hooks; every one optional (gear-api.md). */
interface VizGearHost {
  visible?(key: string, values: VizValues): boolean;
  required?(key: string, values: VizValues): boolean;
  label?(key: string, values: VizValues): string | null | undefined;
  choices?(key: string, values: VizValues, columns: VizColumn[]): VizChoice[] | null | undefined;
  columns?(key: string, values: VizValues, columns: VizColumn[]): VizColumn[];
  tiles?(key: string, values: VizValues): { label?: string; values: unknown[] }[] | null | undefined;
  icon?(key: string, value: unknown): string;
  custom?(key: string, el: HTMLElement, ctx: VizCustomCtx): void;
  placeholder?(key: string, values: VizValues): string | null | undefined;
  onChange?(changed: VizValues, values: VizValues): void;
  send?(msg: { rev: number; values: VizValues }): void;
}

interface VizCustomCtx {
  values: VizValues;
  columns: VizColumn[];
  scope: VizScope;
  set(value: unknown): void;
}

interface VizGearOptions {
  block: string;
  id: string;
  scope: VizScope;
  typeKey?: string;
  title?: string;
  host?: VizGearHost;
  /** Defaults to Blockr.viz.spec[block]. */
  spec?: VizSpec;
}

/** What a template row reads from the payload (derived.titles). */
interface VizTitlePart { text: string; arg?: string; by?: string }
interface VizTitleBand { text?: string; parts?: VizTitlePart[]; offers?: string[] }
interface VizTitles {
  title?: VizTitleBand | null;
  subtitle?: VizTitleBand | null;
  caption?: VizTitleBand | null;
  args?: Record<string, unknown>;
  choices?: Record<string, unknown>;
}

/** Everything a control builder gets for one row. */
interface VizRowCtx {
  key: string;
  arg: VizArg;
  label: string;
  value: unknown;
  values: VizValues;
  columns: VizColumn[];
  required: boolean;
  scope: VizScope;
  host: VizGearHost;
  titles: VizTitles;
  /** The label the gear gives any argument (for the `@` menu). */
  labelOf(key: string): string;
  set(value: unknown): void;
}

/** What a click on a sentence word opens. */
type VizSlotOpener =
  | { kind: 'flag'; value: boolean }
  | { kind: 'entry' }
  | { kind: 'number'; min: number; max: number; step: number }
  | { kind: 'menu'; options: VizChoice[]; selected: unknown; multi: boolean };

interface VizGear {
  readonly block: string;
  update(payload: VizPayload): void;
  set(changes: VizValues): void;
  values(): VizValues;
  slot(key: string): VizSlotOpener | null;
  label(key: string): string;
  render(): void;
  dispose(): void;
}

interface VizControlKit {
  /** Build the controls of one row into `el`. */
  build(el: HTMLElement, ctx: VizRowCtx): void;
  /** The options a select-like control offers. */
  options(ctx: VizRowCtx): VizChoice[];
  /** The spec value as one line of text, and back. */
  toText(arg: VizArg, value: unknown): string;
  fromText(arg: VizArg, text: string): unknown;
  isSet(v: unknown): boolean;
  same(a: unknown, b: unknown): boolean;
  emptyChoice(ctx: VizRowCtx): VizChoice | null;
  uiOptions(choices: VizChoice[]): { options: (string | { value: string; label: string })[]; keys: string[] };
}

interface VizSectionPlan { title: string; group: string; ord: number; keys: string[]; adds: string[] }

interface VizSectionKit {
  GROUPS: [string, string][];
  plan(spec: VizSpec, shown: (key: string) => boolean,
       isSet: (key: string) => boolean): VizSectionPlan[];
  sectionEl(title: string): { el: HTMLElement; titleEl: HTMLElement; body: HTMLElement };
  assemble(pop: HTMLElement, built: { plan: VizSectionPlan; el: HTMLElement; titleEl: HTMLElement }[],
           current: string | null, onPick: (g: string) => void, label: string): void;
  applies(arg: VizArg, type: unknown): boolean;
}

interface VizTileOption { value: string; label: string; icon: string; disabled?: boolean; title?: string }

interface VizTileKit {
  build(parent: HTMLElement, options: VizTileOption[], current: string,
        onPick: (v: string) => void, scope: VizScope): HTMLElement;
}

interface VizGearNamespace {
  mount(el: HTMLElement, opts: VizGearOptions): VizGear;
  tray(head: HTMLElement, scope: VizScope, label: string): { band: HTMLElement;
    button: HTMLButtonElement; set(on: boolean): void; isOpen(): boolean };
  sections: VizSectionKit;
  controls: VizControlKit;
  tiles: VizTileKit;
  icons: Record<string, string>;
}

interface VizTitlesNamespace {
  row(el: HTMLElement, ctx: VizRowCtx): void;
  unknownTokens(tpl: string, titles: VizTitles, columns: VizColumn[]): string[];
  paintPreview(el: HTMLElement, band: VizTitleBand | null | undefined, label: (k: string) => string): void;
  menuGroups(titles: VizTitles, columns: VizColumn[], label: (k: string) => string):
    { head: string; note?: string; items: { label: string; tok: string; val: string; indent?: boolean }[] }[];
  slots(opts: { gear: VizGear; scope: VizScope; openGear?: () => void }): VizSlots;
}

interface VizSlots {
  paint(el: HTMLElement, band: VizTitleBand | null | undefined): void;
  close(): void;
  dispose(): void;
}

interface VizNamespace {
  gear: VizGearNamespace;
  titles: VizTitlesNamespace;
}

/** The part of blockr.ui (local main) the gear uses, each part optional. */
interface VizUi {
  icons?: Record<string, string>;
  gearTray?(band: HTMLElement, gear: HTMLButtonElement, opts?: { label?: string; open?: boolean }):
    { set(open: boolean): void; toggle(): void; isOpen(): boolean };
  trayNav?(groups: { key: string; title: string;
                     sections: { el: HTMLElement; title: string; titleEl?: HTMLElement | null }[] }[],
           opts?: { current?: string; onPick?: (key: string) => void; label?: string }):
    { el: HTMLElement; show(key: string): void; current(): string };
  segmented?(options: { value: string; label: string }[], selected: string,
             onChange: (v: string) => void, opts?: { label?: string }):
    { el: HTMLElement; set(v: string): void; get(): string };
  checkbox?(label: string, checked: boolean, onChange: (on: boolean) => void):
    { el: HTMLElement; input: HTMLInputElement; set(v: boolean): void; get(): boolean };
  layer?(el: Element | Element[], opts?: { from?: Element; inPage?: boolean;
         escape?: (e: KeyboardEvent) => void; outside?: (e: PointerEvent) => void }):
    { remove(): void };
  Select?: {
    single(el: HTMLElement, cfg: Record<string, unknown>): { destroy(): void; getValue(): string };
    multi(el: HTMLElement, cfg: Record<string, unknown>): { destroy(): void; getValue(): string[] };
    menu?(anchor: HTMLElement, cfg: Record<string, unknown>): { close(): void };
    menuMulti?: boolean;
  };
}
