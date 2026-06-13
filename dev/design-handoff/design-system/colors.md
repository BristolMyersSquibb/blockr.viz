# Colors

Tailwind-aligned greyscale plus a small semantic palette. All values are CSS variables defined by `blockr.dock`; component CSS references them with literal fallbacks so blocks still render standalone.

## Greyscale

| Variable | Hex | Use |
|---|---|---|
| `--blockr-grey-50` | `#f9fafb` | input backgrounds, subtle fills |
| `--blockr-grey-100` | `#f3f4f6` | hover surfaces |
| `--blockr-grey-200` | `#e5e7eb` | borders |
| `--blockr-grey-300` | `#d1d5db` | dividers |
| `--blockr-grey-400` | `#9ca3af` | placeholders, muted icons, meta text |
| `--blockr-grey-500` | `#6b7280` | labels, descriptions, secondary text |
| `--blockr-grey-600` | `#4b5563` | emphasised body text |
| `--blockr-grey-700` | `#374151` | headings in muted context |

## Semantic

| Variable | Hex | Use |
|---|---|---|
| `--blockr-color-text-primary` | `#111827` | main body text |
| `--blockr-color-text-secondary` | `#6b7280` | de-emphasised text |
| `--blockr-color-text-muted` | `#9ca3af` | meta / helper |
| `--blockr-color-border` | `#e5e7eb` | all borders |
| `--blockr-color-bg-input` | `#f9fafb` | input / row backgrounds |
| `--blockr-color-bg-hover` | `#f3f4f6` | hover background |
| `--blockr-color-primary` | `#2563eb` | focus rings, active states |
| `--blockr-color-danger` | `#dc3545` | destructive hover (row remove) |
| `--blockr-color-error` | `#dc3545` | validation error text |
| `--blockr-blue-50` | `#eff6ff` | tinted primary background |
| `--blockr-blue-600` | `#2563eb` | primary accent (same as `--blockr-color-primary`) |

## Focus ring

All focusable elements use the same ring:

```css
border-color: var(--blockr-color-primary);
box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
```

## Destructive interaction

Row remove buttons are hidden (`opacity: 0`) and revealed on row hover, then turn `--blockr-color-danger` on button hover — never shown in resting state.

## References

- Token authority: `blockr.dock` (variables); consumers may declare fallback literals.
- Canonical reference: `blockr.dplyr/dev/design-system.md`.
