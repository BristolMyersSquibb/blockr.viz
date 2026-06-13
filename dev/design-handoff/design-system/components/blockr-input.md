# Blockr.Input

Lightweight code input with token-based autocomplete. Replaces the ACE editor across unified blocks. Canonical source: `blockr.dplyr/inst/js/blockr-input.js` (~360 LOC) + `blockr.dplyr/inst/css/blockr-input.css`.

See [decisions/0001-hand-rolled-vs-libraries.md](../../decisions/0001-hand-rolled-vs-libraries.md) for the hand-rolled decision.

## API

```js
var input = Blockr.Input.create(container, {
  value: "mean(col_a)",
  placeholder: "R expression...",
  columns: ["col_a", "col_b"],
  categories: {                     // block-specific subset
    aggregate: ["mean", "sum"],
    arithmetic: ["abs", "round"]
  },
  multiline: false,                 // false = <input>, true = <textarea>
  maxLines: 10,                     // multiline only
  onChange: function () {},          // any text change
  onConfirm: function (value) {}    // Enter (when popup closed)
});

input.el                            // root DOM node
input.getValue()                    // trimmed string
input.setValue(v)
input.setColumns(cols)              // dynamic column updates
input.destroy()
```

## DOM structure

```html
<div class="blockr-input">
  <input class="blockr-input__field" type="text"
         placeholder="R expression..."
         autocomplete="off" spellcheck="false" />
  <div class="blockr-input__popup" role="listbox">
    <div class="blockr-input__group-label">column</div>
    <div class="blockr-input__item blockr-input__item--highlighted"
         role="option" data-value="col_a" data-is-fn="false">
      <span class="blockr-input__item-text">col_a</span>
      <span class="blockr-input__item-meta">column</span>
    </div>
    <div class="blockr-input__group-label">aggregate</div>
    <div class="blockr-input__item" role="option"
         data-value="mean" data-is-fn="true">
      <span class="blockr-input__item-text">mean</span>
      <span class="blockr-input__item-meta">aggregate</span>
    </div>
    <div class="blockr-input__empty">No matches</div>
  </div>
</div>
```

## Autocomplete behaviour

- **Trigger:** every keystroke, extract the current token at cursor position. Word boundary = start of input, or after `(`, `,`, space, `+`, `-`, `*`, `/`, `>`, `<`, `=`, `!`, `&`, `|`, `~`.
- **Matching:** case-insensitive prefix match against all completions (columns + functions). Popup opens when there are matches and the token is non-empty.
- **Ordering:** columns first (score 1001), then functions (score 1000); alphabetical within each group; category rendered as right-aligned muted meta text.
- **Insertion:**
  - Column → replace current token with the column name, backtick-quoted if needed.
  - Function → replace with `funcname()`, place cursor between parens.

## Keyboard

| Key | Popup closed | Popup open |
|---|---|---|
| Character | type (maybe open popup) | type + filter |
| ArrowUp / ArrowDown | — | move highlight |
| Enter | fire `onConfirm(getValue())` | accept highlighted completion |
| Tab | — | accept highlighted completion |
| Escape | — | close popup |

Enter-to-confirm replaces ACE's custom `confirmExpr` command; newlines are never inserted in single-line mode.

## Styling

The input itself has no border or background — it sits inside a row or row-content slot. The `.blockr-input__popup` uses the same dropdown chrome as `Blockr.Select`: 8px radius, `0 4px 12px rgba(0, 0, 0, 0.1)` shadow, `z-index: 9999`, max-height 200px with scroll.

Font: `'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace` at 14px. Popup font currently hardcodes a sans stack — tracked as a TODO to switch to `font-family: inherit` like `blockr-select.css`.

## Syntax highlighting (V2, opt-in)

Transparent-text mirror technique: the `<input>` has `color: transparent` with a visible `caret-color`, and a `<div class="blockr-input__mirror">` behind it renders the same text as coloured `<span>` tokens. Enable via `highlight: true` in the config. Tokenizer covers `string`, `number`, `operator`, `paren`, `known-func`, `column`, `keyword`, `text`. See `blockr.dplyr/dev/blockr-input-spec.md` for tokenizer details and the mirror CSS contract (font metrics must match exactly, or tokens drift from the caret).

## Migration from ACE

Mechanical — new API mirrors the old `createAceEditor` wrapper signature. The per-file helpers `withAce`, `makeCompleter`, `createAceEditor` are deleted; each call site becomes a `Blockr.Input.create` call with `onConfirm` wiring the autosubmit. Shiny dependency swaps from `shinyAce` (~300 KB of ACE) to `blockr_input_dep()`.
