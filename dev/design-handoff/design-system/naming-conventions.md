# Naming conventions

## Shared components — BEM

Shared component classes use BEM: component name is the block, `__` for elements, `--` for modifiers.

```
.blockr-select                       — component
.blockr-select__control              — element
.blockr-select--open                 — modifier
.blockr-select--bordered             — modifier (standalone 42px variant)
.blockr-select__option--highlighted  — element + modifier
```

The `blockr-` prefix is reserved for shared primitives: `.blockr-select`, `.blockr-input`, `.blockr-row`, `.blockr-pill`, `.blockr-popover`, `.blockr-gear-btn`, `.blockr-add-row`, etc.

## Block-specific — short prefix

Every dplyr block uses a short letter prefix so block-scoped classes cannot collide with the shared primitives or with each other. Convention: initials of the block name + `b` for "block" + `-`.

| Prefix | Block |
|---|---|
| `ab-`  | arrange |
| `bcb-` | bind-cols |
| `brb-` | bind-rows |
| `fb-`  | filter |
| `jb-`  | join |
| `mb-`  | mutate |
| `plb-` | pivot-longer |
| `pwb-` | pivot-wider |
| `rb-`  | rename |
| `sb-`  | select **and** summarize (known collision) |
| `slb-` | slice |
| `spb-` | separate |
| `ub-`  | unite |
| `seas-`| seasonal (blockr.seasonal) |

### Known collision: `sb-`

Both `select-block.css` and `summarize-block.css` use `sb-`. Renaming one (e.g. `selb-` for select or `smb-` for summarize) is a pending cleanup — tracked in `blockr.dplyr/dev/design-system.md`.

## Where to put new classes

- A shared layout primitive used by three or more blocks → `blockr-*` in `blockr-blocks.css`.
- One-off structure inside a single block → `xxb-*` in that block's `.css`.
- A variant of a shared component → BEM modifier on the shared class, not a new namespace.

## R bindings

Shiny input binding classes match the component. For `Blockr.Select` consumers, the binding looks for `.blockr-select` on the input element; the binding lives in the consuming package, not in `blockr.dplyr`.
