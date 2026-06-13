# Design handoff — "tile" KPI renderer

**Paste this whole file into Claude Design as your prompt, and attach `tile.html` as the reference code.**
Claude Design will read the attachment for the exact current structure; this brief tells you what to keep,
what to elevate, and the aesthetic bar.

---

## Opening prompt (the one-liner to lead with)

> Polish the visual design of this "tile" KPI component for an analytics dashboard. Keep the component
> model, the slots, the layouts, and the exact data — elevate only the craft: typography, spacing, color,
> hierarchy, dark mode, and subtle motion. Benchmark against Tremor, Vercel Analytics, Linear, and Stripe.
> Return one self-contained `index.html` (inline CSS, no external dependencies beyond an optional single
> webfont link) that shows every variant below, in both light and dark, on one page.

---

## 1. What this is

A **tile**: the renderer that shows a *few important numbers boldly* from a data frame — the KPI-card /
scorecard family. It is **not** a chart (no trends/axes) and **not** a dense data table (no sort/search/
scroll). Think: the metric cards at the top of a dashboard.

It has **two layouts of the identical content** — *cards* (stacked) and *table* (aligned rows). A user
flips a toggle; nothing is gained or lost between them.

## 2. The component model — DO NOT change the semantics

A tile is built from **slots**. The renderer maps data columns to slots; unmapped slots simply don't render.

| slot | what it is |
|---|---|
| **overline** | small uppercase supertext above the value — a period/category/source (free text or a column) |
| **value** | the main number, with a **format** (compact `$1.24M`, `%`, plain) + optional **unit** |
| **secondary** ×N | a *reference* number (last month, a target, a plan), each drawn with a **display style** (below) |
| **color** | a tint or sign-coloring derived from a column (green up / red down, or value heat) |
| **caption** | small subtext below (free text or a column) |

**Secondary display styles** (this is the key idea — `delta`/`target`/`max` are NOT separate elements,
they're a secondary shown a particular way):

- `plain` — just shows the reference ("$1.10M last month")
- `Δ%` — percent change of value vs the reference, with ▲/▼ and green/red ("▲ 12.7%")
- `fill` — a **progress bar (or ring — cosmetic)** drawn as `value ÷ reference`; the reference is the
  "max"/target. A constant (`1,000`, `100%`) works as well as a column.
- `pill` — a small colored status chip ("above plan")

Keep the cards and table **rigorously consistent**: every element that appears in one must be expressible
in the other. The table is a *real table* (aligned columns, hairline row rules) — **not a grid of little
floating cards**.

## 3. Variants to render (put all of these on the page, each clearly labeled)

**A · Single value — two layouts side by side**
- cards (a stacked column) vs table (rows). Metrics:
  - Revenue · `$1.24M` · ▲ 12.7%
  - Active users · `38,400` · ▼ 3.1%
  - Avg order · `$64.20` · ▲ 7.0%

**B · One secondary, four display styles** (four small tiles in a row)
- `plain` — Revenue `$1.24M`, caption "$1.10M last month"
- `Δ%` — Revenue `$1.24M` ▲ 12.7%, caption "vs $1.10M last month"
- `fill` — Enrollment `847`, bar at 84.7%, caption "of 1,000 target"  *(this is the little progress plot)*
- `pill` — Revenue `$1.24M`, green pill "above plan"

**C · Multiple values — two layouts, WITH extras**
- cards: one region's three metrics as cards
- table (matrix): **measures across, groups down**, unit + title in the column header. Each cell = value +
  its secondary (Δ% for Revenue/Conversion; a `fill` bar for Budget):

| Region | Revenue ($M) | Conversion (%) | Budget (% of plan) |
|---|---|---|---|
| North | 1.24 ▲12.7% | 3.8 ▼0.2pp | 62% (bar) |
| South | 0.98 ▲4.0% | 4.1 ▲0.3pp | 88% (bar) |
| East | 1.51 ▲9.0% | 2.9 ▼0.5pp | 45% (bar) |
| West | 0.76 ▼3.0% | 3.3 ▲0.1pp | 95% (bar) |

**D · Multiple values — NO extras (bare scorecard)**
- Same matrix and a matching card grid, but values only (no Δ%, no bars). Just formatted, aligned numbers.

**E · Edge states** (add these — they're where polish shows)
- empty / "no data" tile (muted placeholder, same shell)
- a long label that must truncate or wrap gracefully
- a negative value, and a "down is good" case (e.g. Churn ▼ shown green) — see color note below

**F · Dark mode** — render A–D again in dark, or provide a working light/dark toggle.

## 4. Aesthetic bar (this is the actual ask)

Benchmark: **Tremor (tremor.so), Vercel Analytics, Linear Insights, Stripe dashboards, shadcn/ui**. If ours
looks boxier, louder, or more colorful than those, it's not there yet. Hard rules:

- **Typography:** weights **400 / 500 / 600 only** — never 700+/black. Value = 600, label = 500. Value:label
  size ratio ≈ 3:1. Tight letter-spacing on big numbers (≈ −0.02em).
- **`font-variant-numeric: tabular-nums` on every rendered number** (cards and tables). Numbers right-align
  in tables and line up.
- **Palette:** muted neutral surfaces + **one** saturated accent. Saturated color is reserved for
  deltas / status / fills — never decorative. Green = up/good, red = down/bad, by sign.
- **Spacing:** 20–24px card padding; 12–16px gap between label and number; generous, calm.
- **Borders:** hairline (1px) or none. **No drop shadows in light mode.** Rounded corners ~12–14px.
- **Dark mode:** built from **surface-elevation tokens**, NOT inverted colors. Desaturate accents slightly.
- **Alignment:** labels, numbers, deltas **left-aligned** in cards; numbers right-aligned in tables.
  Never center big numbers.
- **Motion (optional, subtle):** count-up on mount (~600ms ease-out), bar grow-in (~400ms). No bounce, no
  spring. A gentle hover tint on cards is welcome.
- **Polarity:** a delta's color follows "good when up/down," not the raw sign — show one example where a
  *decrease* is green (churn/cost). Don't hardcode ▲=green.

## 5. Current tokens (a starting point — feel free to refine the palette toward the benchmark)

```css
--surface-0:#f7f8fa; --surface-1:#ffffff; --surface-2:#f1f3f6;   /* page / card / subtle fill */
--ink-1:#1c2024; --ink-2:#5b6470; --ink-3:#8b95a1;               /* primary / secondary / tertiary text */
--hair:#e6e9ee; --hair-strong:#d8dde4;                           /* hairline borders */
--accent:#3a6df0;                                                /* the one accent (refine if you like) */
--green:#1f9d63; --green-soft:#e6f5ec;                           /* up / good */
--red:#d6453d;  --red-soft:#fbeceb;                              /* down / bad */
/* numbers in a mono/tabular face; UI in a clean grotesk (system stack, or add Inter via one <link>) */
```

## 6. Deliverables & constraints

- **One self-contained `index.html`** — inline `<style>`, no build step, no framework. JS only for the
  optional count-up and the dark-mode toggle. (This maps to a server-rendered HTML widget, so plain
  HTML/CSS is essential — no React/Tailwind-runtime.)
- All variants (A–F) on the page, each under a small label.
- Responsive: cards wrap; the table scrolls or stacks gracefully on narrow screens.
- **Out of scope:** sparklines / trend lines (a separate "chart" component owns those) and any data
  fetching. Keep it presentational.

## 7. One-line summary of the soul of it

A calm, Tremor-grade surface for a *handful of important numbers* — the same content rendered either as
breathing cards or as a tight aligned table, with one accent color doing the talking through deltas, fills,
and status. Make it look like it belongs next to Linear and Vercel.
