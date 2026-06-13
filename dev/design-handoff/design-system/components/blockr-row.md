# Row primitives

Shared layout pieces for row-based blocks (filter, mutate, arrange, …). Source: `blockr.dplyr/inst/css/blockr-blocks.css`.

Every row-based block composes the same primitives: a repeating `.blockr-row` with a fixed-width "key" on the left, a separator, flex content on the right, and a remove button revealed on hover. Below the list sits a `.blockr-add-row` bar.

## Classes

```
.blockr-row          — flex container, 42px min-height, grey bg, 8px radius
.blockr-row-content  — flex: 1, holds the dynamic content
.blockr-row-remove   — 26px button, margin-left: auto, hidden until row hover
.blockr-add-row      — footer bar with "+ Add" link
.blockr-add-link     — the clickable "+ Add" control inside `.blockr-add-row`
.blockr-add-icon     — leading "+" glyph for `.blockr-add-link`
.blockr-label        — small label inside a row (e.g. column name)
```

Rows typically have a fixed-width left column (150–160px) for the "key" (column name), a separator (`=`, `→`), and flex content for the "value".

## Interaction

- Row remove button: `opacity: 0` at rest, `opacity: 1` on `.blockr-row:hover`; turns `--blockr-color-danger` on button hover. Never show the remove button in resting state.
- `.blockr-row:focus-within` — subtle focus treatment so keyboard users see the active row.
- Hover transitions: 0.15s ease (see [spacing-and-sizing.md](../spacing-and-sizing.md)).

## Pills

`.blockr-pill` is the 24px toggle/button used for e.g. op selectors (AND/OR, ascending/descending). 4px radius, small padding, hover lightens the background; the `.blockr-popover-toggle-active` modifier marks the current selection.

## Gear / popover

```
.blockr-gear-header  — flex container, justify-content: flex-end
.blockr-gear-btn     — 26px square icon button
.blockr-gear-active  — state when the popover is open (primary color + tinted bg)
.blockr-popover      — absolute-positioned panel, 8px radius, shadow
.blockr-popover-row, .blockr-popover-label, .blockr-popover-input,
.blockr-popover-select-wrap, .blockr-popover-toggle — popover internals
```

The popover wraps nested `Blockr.Select` instances in `.blockr-popover-select-wrap`, which overrides the transparent control to a bordered 38px control height inside the popover context. Focus/open states on the wrapped control still use the standard focus ring.

For **what belongs in the popover and how its rows are laid out** (label on top, full-width control, muted help below), see [blockr-popover.md](./blockr-popover.md). That doc is the spec; this section is just the class list.

## Confirm button

`.blockr-expr-confirm` — 24px confirm action for expression rows. `.confirmed` modifier when the expression matches the last-submitted value (turns green, `--blockr-color-primary` on hover off).

## Number and text inputs

`.blockr-num-input` / `.blockr-text-input` are the bare input primitives used inside rows when a full `Blockr.Input` is overkill. Transparent background, no border, focus adopts the shared focus ring. Placeholders use `--blockr-grey-400`.

## Composition example

```html
<div class="blockr-row">
  <span class="blockr-label">col_a</span>
  <div class="blockr-row-content">
    <!-- Blockr.Input or Blockr.Select here -->
  </div>
  <button class="blockr-row-remove" aria-label="Remove"></button>
</div>
<!-- … more rows … -->
<div class="blockr-add-row">
  <a class="blockr-add-link" href="#">
    <span class="blockr-add-icon">+</span> Add
  </a>
</div>
```

For responsive multi-column content inside `.blockr-row-content`, use the flex-wrap pattern in [spacing-and-sizing.md](../spacing-and-sizing.md) — not media or container queries.
