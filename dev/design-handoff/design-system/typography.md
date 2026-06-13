# Typography

Block UI inherits the page font; components never hardcode `font-family` on containers. Use `font: inherit` — the ambient font is set by Bootstrap (or `blockr.dock` when Bootstrap is off).

## Font families

| Context | Stack |
|---|---|
| All UI text | inherited from page |
| Code input fields, code completion items | `'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace` |

The mono stack is currently hardcoded in four places in `blockr-input.css`. Extracting to a `--blockr-font-mono` variable is tracked as a TODO in `blockr.dplyr/dev/design-system.md`.

## Sizes

| Variable | Size | Use |
|---|---|---|
| `--blockr-font-size-base` | `0.875rem` (14px) | body text, inputs, select values |
| `--blockr-font-size-sm` | `0.8125rem` (13px) | labels, helper text, option labels |
| `--blockr-font-size-xs` | `0.75rem` (12px) | meta tags, subtitles, category badges |

## Weights

| Variable | Weight | Use |
|---|---|---|
| `--blockr-font-weight-medium` | 500 | labels |
| `--blockr-font-weight-semibold` | 600 | emphasised text, confirmed states |

## Column labels in pickers

Option rows in `Blockr.Select` may carry a human-readable label next to the value (e.g. `AVAL` + "Analysis Value" from ADaM `attr(col, "label")`). The label sits in `.blockr-select__opt-label` at `--blockr-grey-500` / `--blockr-font-size-sm` with an 8px left margin, and ellipsizes via CSS when the parent value overflows — no character counting in R. See [components/blockr-select.md](./components/blockr-select.md).
