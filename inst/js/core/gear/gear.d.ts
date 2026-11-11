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
  /** A segmented control draws each option's icon (icons.js) beside its word. */
  icons?: boolean;
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

/**
 * What a view tells the gear engine beyond its spec. Every hook is
 * optional; `key` is a spec argument, `values` the gear's current values
 * (with edits R has not echoed yet).
 */
interface VizGearHost {
  /** Whether the row shows; false hides it. */
  visible?(key: string, values: VizValues): boolean;
  /** Whether the row must be set: no empty choice, marked as required. */
  required?(key: string, values: VizValues): boolean;
  /** The row's label for these values, in place of the spec's; null keeps it. */
  label?(key: string, values: VizValues): string | null | undefined;
  /** The options of a pick the spec leaves open (values from the data or the
   * board), and the labels of a column pick's `also` words. */
  choices?(key: string, values: VizValues, columns: VizColumn[]): VizChoice[] | null | undefined;
  /** The columns a column pick offers, of all the input's columns. */
  columns?(key: string, values: VizValues, columns: VizColumn[]): VizColumn[];
  /** The groups of a tiles control (the chart's type tiles). */
  tiles?(key: string, values: VizValues): { label?: string; values: unknown[] }[] | null | undefined;
  /** A tile's icon as an SVG string; '' for the engine's own. */
  icon?(key: string, value: unknown): string;
  /** Draws the row of a key whose spec control is "custom": the view's own editor. */
  custom?(key: string, el: HTMLElement, ctx: VizCustomCtx): void;
  /** The hint in an empty text field. */
  placeholder?(key: string, values: VizValues): string | null | undefined;
  /** Called after an edit was sent, with the changed keys and all values. */
  onChange?(changed: VizValues, values: VizValues): void;
  /** Sends an edit in place of the engine (`<id>_set`); for a page without Shiny. */
  send?(msg: { rev: number; values: VizValues }): void;
  /** What a word in the titles opens when the host knows better than the spec: a word
   * naming part of a setting (the groups layout's `{@value}`, its first summary's column).
   * `write` turns the picked value into the settings to send. Null leaves it to the spec. */
  slot?(key: string, values: VizValues): VizHostSlot | null | undefined;
  /** The axis a ticks setting (`x_ticks`, `y_ticks`) is on: 'time', 'log' or
   * 'value', and the tick values it draws now, for a form picked to start from. */
  ticks?(key: string, values: VizValues): VizTicksAxis;
}

interface VizTicksAxis { type: string; drawn: number[] }

interface VizHostSlot {
  arg: VizArg;
  value: unknown;
  label: string;
  write(value: unknown): VizValues;
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
  /** The label the gear gives any argument (for the `@` menu): a script control's label
   * when the prepare script declares `key`. */
  labelOf(key: string): string;
  /** What `{@key}` names (VizWordOf); absent on the script band's own rows. */
  wordOf?(key: string): VizWordOf;
  /** The line under a titles field naming the script values that take a setting's word. */
  shadowNote?(tpl: string): string;
  /** A block's own gear, when it has one. */
  profile?: VizProfile | null;
  /** Write several settings at once (a package's own row writes `script_values`). */
  setValues?(changes: VizValues): void;
  set(value: unknown): void;
}

/** A block's own gear as R sends it (`derived.gear`, R's table_gear()). */
interface VizGearWire {
  tabs: { title: string; sections: { title: string; keys: string[] }[] }[];
  columns?: { name: string; label?: string; summary: Record<string, unknown> }[];
  clauses?: Record<string, string>;
}

/** A row of a block's own gear that the spec does not have. */
interface VizProfileRow {
  kind: 'script' | 'columns' | 'words' | 'editor';
  name?: string;
  sp?: VizScriptControl;
}

/** A block's own gear, built: its spec (rows first), sections, tabs and rows. */
interface VizProfile {
  spec: VizSpec;
  sections: Record<string, [string, number]>;
  groups: [string, string][];
  keys: Set<string>;
  rows: Record<string, VizProfileRow>;
  wire: VizGearWire;
  clauses: [string, string][];
  sig: string;
}

interface VizProfileKit {
  build(base: VizSpec, wire: VizGearWire, script: VizScript | null): VizProfile;
  values(prof: VizProfile, cfg: VizValues, script: VizScript | null): VizValues;
  translate(prof: VizProfile, changed: VizValues, now: VizValues): VizValues;
  setWords(tpl: string, clauses: [string, string][], on: Set<string>): string;
  scriptArg(sp: VizScriptControl): VizArg;
  scriptValue(sp: VizScriptControl, v: unknown): unknown;
  SV: string;
}

/** A package's row in a block's own gear: its label and its editor. */
interface VizGearEditor {
  label?: string;
  draw(el: HTMLElement, ctx: VizRowCtx): void;
}

/** What a title word names: a prepare script's control (`script`), with the label of the
 * block setting of the same name it takes the word from (`shadows`), or a setting. */
interface VizWordOf { label: string; script: boolean; shadows: string | null }

/** What a click on a sentence word opens: a block setting, or a prepare script's control.
 * `set` writes the value where it lives (the setting, or `script_values[name]`). */
interface VizSlotBase { label: string; set(value: unknown): void }
type VizSlotOpener = VizSlotBase & (
  | { kind: 'flag'; value: boolean }
  | { kind: 'entry'; value: unknown; toText(value: unknown): string; fromText(text: string): unknown }
  | { kind: 'number'; min: number; max: number; step: number; value: unknown }
  | { kind: 'menu'; options: VizChoice[]; selected: unknown; multi: boolean }
  /** Axis limits: two words, from and to, each typed over on its own. */
  | { kind: 'pair'; value: unknown; ends: [VizEndOpener, VizEndOpener] }
  /** Axis ticks: a word per part ("every", "7", "days"), each its own opener. */
  | { kind: 'words'; value: unknown; words: VizWordPart[] });

/** One word of a `words` opener: typed over, or a menu. */
type VizWordPart = { text: string; how: VizSlotOpener & { kind: 'entry' | 'menu' } };

/** One end of a pair: typed over like a value list. */
type VizEndOpener = VizSlotBase & { kind: 'entry'; value: unknown;
  toText(value: unknown): string; fromText(text: string): unknown };

interface VizGear {
  readonly block: string;
  update(payload: VizPayload): void;
  set(changes: VizValues): void;
  values(): VizValues;
  slot(key: string): VizSlotOpener | null;
  /** The prepare script's working declaration of `name`; it takes the word `{@name}`. */
  scriptInput(name: string): VizScriptControl | null;
  /** The prepare script's control a word names, by variable; null when none is declared. */
  scriptSlot(name: string): VizSlotOpener | null;
  wordOf(key: string, values?: VizValues): VizWordOf;
  shadowNote(tpl: string, values?: VizValues): string;
  /** The script controls the titles' words open (the band leaves them out). */
  scriptWords(): Set<string>;
  /** Take a payload's settings, columns, titles and script without drawing, for a view
   * that paints its titles before the gear's update. */
  prime(payload: VizPayload): void;
  label(key: string): string;
  render(): void;
  dispose(): void;
}

interface VizControlKit {
  /** Build the controls of one row into `el`. */
  build(el: HTMLElement, ctx: VizRowCtx): void;
  /** A text or number field that commits on Enter or blur. */
  field(el: HTMLElement, ctx: VizRowCtx, opts?: { multiline?: boolean; type?: string; commit?: (text: string) => void }): HTMLInputElement;
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
       isSet: (key: string) => boolean, own?: VizProfile | null): VizSectionPlan[];
  sectionEl(title: string): { el: HTMLElement; titleEl: HTMLElement; body: HTMLElement };
  assemble(pop: HTMLElement, built: { plan: VizSectionPlan; el: HTMLElement; titleEl: HTMLElement }[],
           current: string | null, onPick: (g: string) => void, label: string,
           tabs?: [string, string][]): void;
  applies(arg: VizArg, type: unknown): boolean;
}

interface VizTileOption { value: string; label: string; icon: string; disabled?: boolean; title?: string }

interface VizTileKit {
  build(parent: HTMLElement, options: VizTileOption[], current: string,
        onPick: (v: string) => void, scope: VizScope): HTMLElement;
}

interface VizGearNamespace {
  mount(el: HTMLElement, opts: VizGearOptions): VizGear;
  /** Controls defined outside controls.js, by name (range.js, profile.js). */
  more?: Record<string, (el: HTMLElement, ctx: VizRowCtx) => void>;
  /** Rows a package adds to a block's own gear, by key (profile.js). */
  editors: Record<string, VizGearEditor>;
  /** A block's own gear (profile.js). */
  profile: VizProfileKit;
  /** A ticks setting's words on the face (gear/ticks.js). */
  ticksSlot(host: VizGearHost, key: string, value: unknown, label: string,
            set: (v: unknown) => void, values: VizValues): VizSlotOpener;
  /** Why a limits pair cannot be set on its axis, or null (range.js). */
  limitsNote(values: VizValues, key: string, pair: unknown): string | null;
  tray(head: HTMLElement, scope: VizScope, label: string): { band: HTMLElement;
    button: HTMLButtonElement; set(on: boolean): void; isOpen(): boolean };
  sections: VizSectionKit;
  controls: VizControlKit;
  tiles: VizTileKit;
  icons: Record<string, string>;
  /** Small icons keyed `<argument>:<value>` (icons.js). */
  glyphs: Record<string, string>;
}

interface VizTitlesNamespace {
  row(el: HTMLElement, ctx: VizRowCtx): void;
  unknownTokens(tpl: string, titles: VizTitles, columns: VizColumn[]): string[];
  paintPreview(el: HTMLElement, band: VizTitleBand | null | undefined, label: (k: string) => string,
               wordOf?: (k: string) => VizWordOf): void;
  menuGroups(titles: VizTitles, columns: VizColumn[], label: (k: string) => string,
             wordOf?: (k: string) => VizWordOf):
    { head: string; note?: string; items: { label: string; tok: string; val: string; indent?: boolean }[] }[];
  slots(opts: { gear: VizGear; scope: VizScope; openGear?: () => void }): VizSlots;
  /** Every argument the titles name: their words and their offers. */
  namedArgs(titles: VizTitles | null | undefined): string[];
  /** Type a value over its word. */
  edit(scope: VizScope, anchor: HTMLElement, how: VizSlotOpener & { kind: 'number' | 'entry' }): void;
  /** A word that acts on click and on Enter or Space. */
  wire(scope: VizScope, el: HTMLElement, fn: () => void): void;
}

interface VizSlots {
  paint(el: HTMLElement, band: VizTitleBand | null | undefined): void;
  close(): void;
  dispose(): void;
}

interface VizNamespace {
  gear: VizGearNamespace;
  titles: VizTitlesNamespace;
  /** A pair setting's two words on the face (slot-pair.js). */
  slotPair(el: HTMLElement, scope: VizScope, ends: VizEndOpener[]): void;
  /** A setting's words, one per part, each opened by `open` (slot-pair.js). */
  slotWords(el: HTMLElement, scope: VizScope, words: VizWordPart[],
            open: (i: number, w: HTMLElement) => void): void;
  /** Axis ticks, read and written (core/ticks.js). */
  ticks: VizTicks;
}

type VizTicksValue = { form: 'every'; n: number; unit: string | null } |
  { form: 'at'; list: (number | string)[] };

interface VizTicks {
  UNITS: string[];
  MAX_TICKS: number;
  LOG_NOTE: string;
  LIST_NOTE: string;
  parse(v: unknown): VizTicksValue | null;
  step(n: number, unit: string | null): number | string;
  unitWord(unit: string, n: number): string;
  text(v: unknown): string;
  fromText(t: string): unknown;
  parseList(t: string): (number | string)[];
  note(v: unknown, log: boolean): string | null;
  values(v: unknown, lo: number, hi: number, type: string): number[] | null;
  roundOut(v: unknown, lo: number, hi: number): [number, number] | null;
  start(form: 'every' | 'at', drawn: number[], type: string): unknown;
  axisValue(v: number | string, type: string): number | null;
}

/** A row of blockr.ui's Blockr.menu, or a rule between groups. */
type VizMenuEntry = { label: string; icon?: string; onSelect?: () => void } | { divider: true };

/**
 * The part of blockr.ui the gear uses. blockr.ui is in Imports and every
 * block's dependency attaches its controls; `layer` and `Select.menu` are
 * newer and may be missing.
 */
interface VizUi {
  icons?: Record<string, string>;
  gearTray(band: HTMLElement, gear: HTMLButtonElement, opts?: { label?: string; open?: boolean }):
    { set(open: boolean): void; toggle(): void; isOpen(): boolean };
  trayNav(groups: { key: string; title: string;
                     sections: { el: HTMLElement; title: string; titleEl?: HTMLElement | null }[] }[],
           opts?: { current?: string; onPick?: (key: string) => void; label?: string }):
    { el: HTMLElement; show(key: string): void; current(): string };
  /** A menu under `anchor` (blockr.ui's Blockr.menu); newer, may be missing. */
  menu?(anchor: HTMLElement, config: { items: VizMenuEntry[]; onClose?: () => void },
        byKeyboard?: boolean): { el: HTMLElement; close(): void };
  segmented(options: { value: string; label: string }[], selected: string,
             onChange: (v: string) => void, opts?: { label?: string; size?: 'xs' }):
    { el: HTMLElement; set(v: string): void; get(): string };
  checkbox(label: string, checked: boolean, onChange: (on: boolean) => void):
    { el: HTMLElement; input: HTMLInputElement; set(v: boolean): void; get(): boolean };
  /** blockr.ui's amber cue on an empty required field. */
  setRequiredEmpty?(el: Element, empty: boolean): void;
  layer?(el: Element | Element[], opts?: { from?: Element; inPage?: boolean;
         escape?: (e: KeyboardEvent) => void; outside?: (e: PointerEvent) => void }):
    { remove(): void };
  Select: {
    single(el: HTMLElement, cfg: Record<string, unknown>): { destroy(): void; getValue(): string };
    multi(el: HTMLElement, cfg: Record<string, unknown>): { destroy(): void; getValue(): string[] };
    menu?(anchor: HTMLElement, cfg: Record<string, unknown>): { close(): void };
    menuMulti?: boolean;
  };
}
