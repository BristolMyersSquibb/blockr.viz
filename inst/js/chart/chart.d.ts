/**
 * Types for what R sends the chart view and what the view sends back
 * (R/chart-view.R, R/kit-server.R, R/kit-input.R, R/chart-capture.R). Dev
 * tooling only, read by tsconfig.core.json, never served to the browser.
 *
 * The payload goes through Shiny's toJSON() with null = "null", so a NULL
 * list element arrives as `null`. An empty named list arrives as `{}` and an
 * empty unnamed list as `[]`; the fields where that matters say so.
 */

/**
 * The band of a band chart (R/chart-derived.R, chart_band()). `series` is
 * null when nothing can be drawn, and then `note` says why. `refs` is null
 * when no reference column resolved to a number.
 */
interface ChartBand {
  series: ChartBandSeries | null;
  refs: ChartBandRefs | null;
  note: string | null;
}

/**
 * The band's series, keyed by facet level and then by series (else colour)
 * level. `__all__` stands for no facet and for no split.
 */
type ChartBandSeries = Record<string, Record<string, ChartBandGroup>>;

/**
 * One group's band over the grid R shares across all groups. The arrays
 * run along `x`. A grid point with too few ids is null in `center`, `lo`,
 * `hi`, `olo`, `ohi` and `hw`, so the ribbon breaks there. `olo` and `ohi`
 * come only with outer whiskers, `out_x`, `out_y` and `out_id` only with
 * points outside the local Tukey fence (`out_id` also needs an id column).
 */
interface ChartBandGroup {
  x: number[];
  center: (number | null)[];
  lo: (number | null)[];
  hi: (number | null)[];
  n: number[];
  hw: (number | null)[];
  olo?: (number | null)[];
  ohi?: (number | null)[];
  out_x?: number[];
  out_y?: number[];
  out_id?: string[];
}

/** The reference limits of a band chart; each side is absent when unset. */
interface ChartBandRefs {
  hi?: ChartBandRef;
  lo?: ChartBandRef;
}

/** A reference limit column reduced to its median, with its range. */
interface ChartBandRef {
  col: string;
  value: number;
  lo: number;
  hi: number;
  n_distinct: number;
}

/**
 * The trend lines of a scatter (R/chart-derived.R, chart_smoother()). Keyed
 * by series (else colour) level, `__all__` without one; with a facet the
 * facet level comes first.
 */
type ChartSmoother = Record<string, ChartSmootherLine | Record<string, ChartSmootherLine>>;

/** One trend line over 100 points; `y` is null where the fit has no value. */
interface ChartSmootherLine {
  x: number[];
  y: (number | null)[];
}

/**
 * A histogram's bins (R/chart-derived.R, chart_bins()), a data frame sent as
 * columns: one entry per bin in every array. `last` is missing when the data
 * has no finite value.
 */
interface ChartHist {
  label: string[];
  lo: number[];
  hi: number[];
  last?: boolean[];
}

/**
 * The board scale of the coloured column (R/scale-map-resolve.R,
 * dd_scales_config()). `var` is the chart's column name. `color`, `shape`
 * and `linetype` map a level to its value, each present only when the
 * binding sets that channel; `order` lists the levels.
 */
interface ChartScale {
  var: string;
  color?: Record<string, string>;
  shape?: Record<string, string>;
  linetype?: Record<string, string>;
  order: string[];
}

/**
 * The ECharts theme name from the board option. R sends the name only; the
 * view also accepts `{name, theme}` but R never sends that.
 */
type ChartTheme = string;

/**
 * What R computes for the chart and never takes back (R/chart-view.R,
 * chart_view() payload(); `downloads` is added in R/kit-server.R). `band` is
 * null off a band chart, `smoother` null unless a scatter asks for one,
 * `hist` null off a histogram, `scales` null without a board scale for the
 * coloured column. `palette` is the series colours, never empty.
 * `ctrl_choices` is `[]` when no board target exists. `downloads` is absent
 * when the block gives no exhibit, `[]` when downloads are off. A type alias,
 * not an interface, so it fits VizPayload's `Record<string, unknown>`.
 */
type ChartDerived = {
  titles: VizTitles;
  band: ChartBand | null;
  smoother: ChartSmoother | null;
  hist: ChartHist | null;
  scales: ChartScale | null;
  palette: string[];
  theme: ChartTheme;
  ctrl_choices: VizChoice[];
  script: VizScript;
  /** Device pixels per CSS pixel the picture is composed with, 1 to 6. */
  capture_ratio: number;
  downloads?: VizDownload[];
};

/** What R sends the chart view: the shared payload with the chart's `derived`. */
interface ChartPayload extends VizPayload {
  derived: ChartDerived;
}

/**
 * The custom message `blockr-viz:capture` (R/chart-view.R,
 * chart_capture_service()): the payload, always with its rows, plus the
 * request token and the box to draw into in CSS px.
 */
interface ChartCaptureMessage extends ChartPayload {
  req: string;
  width: number;
  height: number;
  /** The view reads this as the pixel ratio; R does not send it, so the
   *  payload's `capture_ratio` applies. */
  ratio?: number;
}

/** The browser's reply when the picture drew (input `blockr_viz_capture_result`). */
interface ChartCapturePicture extends VizPicture {
  req: string;
}

/** The browser's reply when the picture failed to draw. */
interface ChartCaptureFailure {
  req: string;
  error: string;
}

type ChartCaptureReply = ChartCapturePicture | ChartCaptureFailure;

/**
 * The values a selection holds per column. A missing value is `null`, as
 * R/kit-input.R, read_filters() reads it (it becomes NA).
 */
type ChartSelectionFilters = Record<string, any[]>;

/**
 * A click on marks: the columns and values they stand for. R needs at least
 * one entry in `filters` or `columns`.
 */
interface ChartCategoricalSelection {
  type: 'categorical';
  filters: ChartSelectionFilters;
  columns?: ChartSelectionFilters;
}

/**
 * A brush: `x` and `y` name the columns, the ranges are `[lo, hi]`. `y` and
 * `y_range` go together or are both missing. `filters` pins the facet panel.
 */
interface ChartRangeSelection {
  type: 'range';
  x: string;
  y?: string | null;
  x_range: unknown[];
  y_range?: unknown[] | null;
  filters?: ChartSelectionFilters;
}

/** A selection as the chart sends it in `<id>_select` (R/kit-input.R, viz_read_selection()). */
type ChartSelection = ChartCategoricalSelection | ChartRangeSelection;

/** The design tokens the canvas draws with (colors.js readInk()): ECharts
 * cannot resolve var(), so the view reads them per draw. */
interface Ink {
  muted: string;
  secondary: string;
  strong: string;
  border: string;
  surface: string;
  danger: string;
  raised: string;
  text: string;
  shadow: string;
  fontSize: number;
  face: string;
}

/** An aggregated chart's axes (model-common.js axes()): the category column
 * (a histogram's bin column) and the column it aggregates (".count" for a row
 * count). */
interface ChartAxes {
  category: string | null;
  measure: string;
}

/** What the models and options read (model-common.js settings()): the
 * payload's config with the spec's defaults, the chart's own defaults, and
 * an aggregated chart's axes. R's `derived` travels beside it. */
type ChartSettings = ChartConfig & Partial<ChartAxes> & Record<string, any>;

/** What an option builder draws with beside the model (view.js _option()):
 * the settings and R's derived, the columns, the ink and theme, a text
 * measure in the axis font, and each panel's width. */
interface ChartLook {
  cfg: ChartSettings;
  derived: ChartDerived;
  columns: VizColumn[];
  ink: Ink;
  theme: string | null;
  measure: (s: string) => number;
  widths?: number[];
  [extra: string]: unknown;
}

/** A light test of a mark under the selection: its keys, and its rows when
 * the caller has them. */
type ChartLit = (keys: Record<string, unknown>, rows?: unknown[]) => boolean;

/** What a family's highlight patch reads (view.js _applyHighlight). */
interface ChartPatchInput {
  m: any;
  panel: any;
  cfg: ChartSettings;
  derived: ChartDerived;
  /** The option ECharts holds now. */
  current: any;
  /** The option this view drew for the panel, if any. */
  drawn: any;
  lit: ChartLit | null;
  filters: ChartSelectionFilters | null;
  range: ChartRangeSelection | null;
  dimmed: boolean;
  /** The view's ink: a selected mark's outline is its text colour. */
  ink: Ink;
}

/**
 * A chart family as the view uses it (family-*.js): its model and option
 * (memoised on the config fields each reads, memo.js), the legend band's chips, the
 * handlers of a new ECharts instance, and the highlight patch. `widths`:
 * the option reads each panel's width. `alwaysHighlight`: every draw ends
 * with a highlight patch; the others patch only while a selection dims.
 * `prepare` and `after` run on a panel's slot before and after its option
 * is set; `actions` says whether `after` dispatches an action.
 */
interface ChartFamily {
  name: 'aggregated' | 'timeline' | 'individual';
  model(input: { rows: any[]; ix: any; columns: VizColumn[]; cfg: ChartSettings; derived: ChartDerived }): any;
  option(m: any, look: ChartLook): any;
  legend(m: any, cfg: ChartSettings, derived: ChartDerived): { col: string; items: Array<{ name: string; color: string }> } | null;
  widths: boolean;
  alwaysHighlight: boolean;
  attach(view: ChartView, slot: ChartSlot): void;
  patch(c: ChartPatchInput): { series: any[]; dimmed: boolean };
  prepare?(slot: ChartSlot, po: any): void;
  actions?(slot: ChartSlot, po: any): boolean;
  after?(slot: ChartSlot, po: any, view: ChartView): void;
}

/** One facet panel: its boxes, its ECharts instance and its hover, brush
 * and zoom state. */
interface ChartSlot {
  container: HTMLElement;
  labelEl: HTMLElement | null;
  chartDiv: HTMLElement;
  resetBtn: HTMLButtonElement;
  chart: any;
  scope: VizScope | null;
  facetVal: any;
  dimmed: boolean;
  hover: { si: number | null };
  brushable: boolean;
  /** The last brush's rectangle, to draw again after a redraw. */
  brushAreas: any[] | null;
  /** The selection that brush made, as JSON. */
  brushSel: string | null;
  zoomArmed: boolean;
  zoom: Record<string, unknown> | null;
  focus: any;
  focusSi: number | null;
  band: any;
  bandFocus: string | null;
  /** Cancels a line click waiting for a second click. */
  lineClick: (() => void) | null;
}

/** The chart view (view.js) as its parts see it: the selection, the legend,
 * the families, the picture and the gear host. */
interface ChartView extends VizView {
  readonly el: HTMLElement;
  readonly scope: VizScope;
  readonly frame: VizFrame;
  readonly gear: VizGear;
  readonly selection: ChartSelectionState;
  readonly legend: { el: HTMLElement; mark(sel: ChartSelection | null): void };
  readonly chartGrid: HTMLElement;
  settings: ChartSettings;
  derived: ChartDerived;
  columns: VizColumn[];
  data: any[];
  _ix: any;
  _memo: { models: any; options: any; modelGen: number; model: any; option: any };
  _slots: ChartSlot[];
  charts: any[];
  theme: string | null;
  _ink: Ink;
  drawn: boolean;
  family(): ChartFamily;
  highlight(lazy?: boolean): void;
  status(): void;
  _measureFont(font: string): (s: string) => number;
}

type ChartViewClass = new (el: HTMLElement, scope: VizScope, opts?: { still?: boolean }) => ChartView;

/** The view's selection (selection.js). */
interface ChartSelectionState {
  current: ChartSelection | null;
  byBrush: boolean;
  brushDrawn: boolean;
  suppressBrushClear: boolean;
  restore(saved: unknown): void;
  click(slot: ChartSlot | null, params: any, filters: ChartSelectionFilters): void;
  latch(sel: ChartSelection, brush: boolean): void;
  brushCleared(): void;
  clear(): void;
  words(): string | null;
}

/**
 * The chart's namespace, Blockr.viz.chart: the families, models, options,
 * axes and helpers every chart file hangs off it. Its members are typed
 * where they are defined, not here.
 */
type ChartNamespace = Record<string, any>;

interface VizNamespace {
  chart: ChartNamespace;
}
