# Spacing and sizing

## Heights

| Height | Use |
|---|---|
| **42px** | standalone inputs, bordered selects — the universal "input" height |
| **30px** | nested inputs inside rows, number inputs, add-row bar |
| **26px** | icon buttons (gear, remove) |
| **24px** | pill toggles, confirm buttons |

`Blockr.Select` is transparent and unsized by default so it inherits the parent row's 30px slot; the `--bordered` modifier promotes it to the 42px standalone size.

## Border radius

Three tiers (the 6px tier is used by several block-internal sub-sections but is not yet formally documented in the master spec — see TODOs in `blockr.dplyr/dev/design-system.md`):

| Radius | Use |
|---|---|
| **8px** | inputs, rows, dropdowns, popovers |
| **6px** | internal card sub-sections (separate, bind-rows, pivot-longer, unite) |
| **4px** | pills, small buttons |

## Gaps and padding

- Row padding: 5px vertical, flex gap 6–12px between fields.
- Dropdown shadow: `0 4px 12px rgba(0, 0, 0, 0.1)`.
- Focus ring: `box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1)` (see [colors.md](./colors.md)).
- Hover transitions: `0.15s ease`.

## Responsive layout (flex-wrap, not media queries)

Block width is unknown at author time — stacks range from ~300px (narrow sidebar) to the full workspace width, and resize at runtime. **Do not use `@media` or `@container` queries** for block internals. The idiom is a wrapping flex row where each field claims an equal share above a soft minimum, then wraps:

```css
.xxb-grid  { display: flex; flex-wrap: wrap; align-items: flex-end; gap: 6px 12px; }
.xxb-field { flex: 1 1 160px; min-width: 0; display: flex; flex-direction: column; }
```

Rules:

- `flex: 1 1 160px` — fields grow equally, shrink to the ~160px soft minimum, then wrap. Tune the basis per block (short labels can go ~120px; long inputs need 200px+).
- `min-width: 0` is **mandatory** on flex children that contain `Blockr.Select` or other overflowing content — otherwise the intrinsic content width prevents shrinking.
- `align-items: flex-end` keeps inputs bottom-aligned when labels span two lines on one field but not another.
- For a field that should always occupy its own row (e.g. a multi-select with many tags), add a `--full` modifier: `.xxb-field--full { flex: 1 1 100%; }`.

Canonical examples: `blockr.dplyr/inst/css/pivot-longer-block.css` (`.plb-input-row` / `.plb-field`), `blockr.bi/inst/css/summary-table-block.css` (`.stb-grid` / `.stb-field` / `.stb-field--full`).
