# Blockr.Select

Hand-rolled vanilla-JS select with search, multi tags, drag-to-reorder, and keyboard navigation. Canonical source: `blockr.dplyr/inst/js/blockr-select.js` (~580 LOC) + `blockr.dplyr/inst/css/blockr-select.css`.

See [decisions/0001-hand-rolled-vs-libraries.md](../../decisions/0001-hand-rolled-vs-libraries.md) for why this component is hand-rolled rather than built on Tom Select.

## API

```js
// Single-select
var sel = Blockr.Select.single(container, {
  options: ["a", "b"],              // string[] or {value, label}[]
  selected: "a",                    // string | null (null auto-selects first)
  placeholder: "Column...",
  onChange: function (value) {}
});

// Multi-select with tag pills + drag reorder
var multi = Blockr.Select.multi(container, {
  options: ["a", "b", "c"],
  selected: ["a"],
  placeholder: "Select values...",
  reorderable: true,                // default true
  onChange: function (values) {}
});

// Instance methods (both modes)
sel.el                              // root DOM node
sel.getValue()                      // current value(s)
sel.setOptions(opts, selected)      // replace options, preserve valid selections
sel.destroy()                       // remove DOM + listeners
```

Options are either bare strings or `{ value, label }` pairs. Labels render next to the value in dropdown rows and single-mode value display; multi-mode tag pills stay value-only.

## DOM structure

Single mode:

```html
<div class="blockr-select blockr-select--single" tabindex="0"
     role="combobox" aria-expanded="false" aria-haspopup="listbox">
  <div class="blockr-select__control">
    <span class="blockr-select__value">col_a</span>
    <input class="blockr-select__search" type="text" aria-autocomplete="list" />
    <span class="blockr-select__arrow"></span>
  </div>
  <div class="blockr-select__dropdown" role="listbox">
    <div class="blockr-select__option" role="option" data-value="col_a">
      col_a
      <span class="blockr-select__opt-label">Analysis Value</span>
    </div>
    <div class="blockr-select__empty">No matches</div>
  </div>
</div>
```

Multi mode: the `__control` contains `.blockr-select__tags` (a flex container of `.blockr-select__tag` pills each with a `.blockr-select__tag-remove` button) plus the search input; already-selected values are excluded from the dropdown.

## Variants

- `.blockr-select--single` / `.blockr-select--multi` — mode.
- `.blockr-select--open` — dropdown visible.
- `.blockr-select--bordered` — **standalone 42px variant.** Default presentation is transparent (no border/background) so the component drops cleanly into row containers like `.fb-row`, `.ju-row`, `.su-row`. Add `--bordered` when the select stands alone (e.g. summarize group-by picker, blockr.dm table pickers).
- `.blockr-select--above` — added automatically when the dropdown would overflow the viewport bottom; flips open direction.

## Column labels

Option shape `{ value, label }` renders the label in `.blockr-select__opt-label` — grey-500, `--blockr-font-size-sm`, 8px left margin. The parent `.blockr-select__value` handles truncation via `white-space: nowrap; overflow: hidden; text-overflow: ellipsis;`, so the label ellipsizes first and the value is always preserved. Native browser tooltip via `title`; no custom mouseover handling.

Used by `blockr.dplyr` column pickers (ADaM-style `attr(col, "label")`) and reused by `blockr.dm` table pickers for dm table labels.

## Dropdown positioning

Dropdowns use the portal pattern — appended to `<body>` with `position: fixed` to escape ancestor stacking contexts (Dockview panels, `contain: paint`, `overflow: hidden` wrappers). Driven by `blockr.design/open/blockr-select-portal/3-design.md` after the Dockview stacking saga (see `blockr.design/done/blockr.dplyr-picker-modernization/dockview-stacking-analysis.md`).

## Keyboard navigation

| Key | Closed | Open |
|---|---|---|
| Enter / Space | open | select highlighted |
| ArrowDown / ArrowUp | open | move highlight |
| Escape | — | close |
| Backspace (multi, empty search) | — | remove last tag |
| Typing | open + filter | filter |

## Styling contract

- No `!important` in `blockr-select.css`. The component owns its DOM.
- Fallback values: every CSS variable reference in `blockr-select.css` has a literal fallback so the component renders correctly standalone.
- No `.selectize-*` class names — pure BEM under `.blockr-select`.

## Rich variant

`blockr.seasonal/inst/js/blockr-select-rich.js` (~680 LOC) is a two-line option variant with category badges and icons. Comment on the file says "extract to blockr.core when needed." Unification — giving `Blockr.Select` an optional `render(option)` callback — is the cheaper next step before any Tom Select swap; see the ADR.

## Bundle cost

Current stack (`blockr-core` + `blockr-select` JS/CSS + `blockr-blocks.css`): ~9.2 KB gzipped. For comparison, Tom Select base is ~18 KB, complete build ~30 KB.
