# Design system

Visual primitives and shared component specs for blockr's hand-rolled UI chrome.

The canonical source lives in `blockr.dplyr/inst/` (`blockr-core.js`, `blockr-blocks.css`, `blockr-select.*`, `blockr-input.*`). Other packages copy these assets until they are extracted to `blockr.core`. Values below are snapshotted against `blockr.dplyr/dev/design-system.md` and the CSS files as of 2026-04-19.

## Primitives

- [colors.md](./colors.md) — greyscale, semantic colors, CSS variable names.
- [typography.md](./typography.md) — font families, sizes, weights.
- [spacing-and-sizing.md](./spacing-and-sizing.md) — heights, padding, gaps, border radius.
- [naming-conventions.md](./naming-conventions.md) — BEM and block-prefix rules.

## Components

- [components/blockr-select.md](./components/blockr-select.md) — single + multi select (`Blockr.Select`).
- [components/blockr-input.md](./components/blockr-input.md) — code input with autocomplete (`Blockr.Input`).
- [components/blockr-row.md](./components/blockr-row.md) — shared row/pill/add-row layout primitives.
- [components/blockr-popover.md](./components/blockr-popover.md) — gear "advanced settings" popover: what goes in it and how rows are laid out.

## Related

- [decisions/0001-hand-rolled-vs-libraries.md](../decisions/0001-hand-rolled-vs-libraries.md) — why these components are hand-rolled rather than built on Tom Select et al.
- `blockr.dplyr/dev/design-system.md` — master reference (also includes known inconsistencies as TODOs).
- `blockr.dplyr/dev/blockr-select-spec.md`, `blockr.dplyr/dev/blockr-input-spec.md` — component specs with migration notes.
- `blockr.seasonal/inst/js/blockr-select-rich.js` — rich two-line variant (680 LOC) earmarked for unification with `Blockr.Select`.
