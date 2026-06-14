# Rename plan: `blockr.bi` → `blockr.viz`

**Status:** APPROVED in principle (Christoph, 2026-06-13), not yet executed.
**Owner of execution:** TBD (this doc is written so a fresh agent can pick it up).

This resolves the open question at the bottom of
[`table-and-chart-architecture.md`](./table-and-chart-architecture.md) ("Rename
`blockr.bi` → `blockr.dashboard`?"). We are **not** using `blockr.dashboard`.

## Decision & rationale

- **New name: `blockr.viz`.**
- **Why not `blockr.dashboard`:** `blockr.dock` already *builds* the dashboard
  (it owns `dock_layout()` / `dock_view()` — the frame/shell). Calling this
  package `blockr.dashboard` makes two packages claim "dashboard" while doing
  opposite jobs (dock = the frame, this = the contents that fill it). That
  collision is a recurring "which one?" tax on docs/demos/new users.
- **Why `blockr.viz`:** after the comprehensive rewrite the package's identity
  *is* the **render layer** — the noun blocks that draw (`chart`, `table`,
  `tile`) plus a couple of shapers. `blockr.dock` + `blockr.viz` reads as a clean
  pair: dock arranges, viz draws. Short, memorable, collision-free.
- **Caveat noted, accepted:** "viz" leans chart-ish while we also ship
  table/tile; in context next to dock ("the visual blocks") it reads fine, and no
  short name covers shape+render cleanly anyway — which is part of why we're
  moving off `blockr.bi`. (If `describe`/`correlate` shapers later migrate to
  `blockr.stats` per the architecture doc, the package becomes *purely* the render
  layer and `viz` fits even better.)

## Framing: do this as the capstone of the cleanup, not a standalone sed pass

Per Christoph: **the rename should ride along with the comprehensive cleanup,
documentation, and name-alignment work** already queued in
`table-and-chart-architecture.md` (Migration/TODO + Open decisions). A package
rename is a once-per-lifetime chance to also fix the *internal* names that still
say "bi". So sequence it as:

1. **Land the architecture-doc cleanup first** (or in the same push) so we rename
   a *settled* surface, not a moving one. The relevant items there:
   - `summary_table` emit tidy numbers + `.fmt` (in progress).
   - Fold `waterfall` into `chart` (bar baseline mode); then deprecate.
   - Fold `html_table` into `table`; retire `html_table`.
   - Add `correlate`; drop `transform`/`cor_method` from `table`.
   - Resolve `gt` ownership (bi adapter vs `blockr.gt`).
   - Resolve `summary_table` name (keep, or → verb `describe`).
   - The concrete bug/file-cleanup list at the bottom of that doc.
   Doing these first means fewer blocks/files exist to rename, and the deprecated
   ones (`kpi`, `pivot_table`, `waterfall`, maybe `html_table`, `gt`) may simply
   be **deleted** rather than renamed.
2. **Then the rename** (this doc).
3. **Then documentation** — README rewrite around the five-layer model + the
   dock/viz pairing, blockr.docs cross-refs, blockr-site.

If the cleanup can't all land first, the rename is still safe to do
independently — just expect to rename a few files that the cleanup would have
deleted.

---

## ⚠️ The one hard problem: serialized boards carry the package name

Saved board JSONs embed the providing package per block:

```json
"name":["new_summary_table_block"],"package":["blockr.bi"],"version":["0.0.1"]
```

So renaming the package can break **board restore** for every saved `.json` that
references a bi block — in this repo (csr/insurance/sandbox/deploy/clinical-explorer
fixtures) **and** any board saved elsewhere.

**DECIDED (Christoph, 2026-06-13): migrate the boards, no compatibility shim.**
Christoph owns the boards (including deployed/external ones) and will fix them —
"better now than later." So:

- **No transitional `blockr.bi` shim.** We do a clean break.
- **Migrate every JSON** in-repo: scripted `"package":["blockr.bi"]` →
  `"package":["blockr.viz"]` rewrite across all tracked `*.json` boards (list in
  category 6 below). Constructor names are unchanged by the rename, so only the
  `package` field moves; `version` strings inside boards are advisory and can be
  left.
- **External/deployed boards** (anything not in this repo) are Christoph's to
  migrate — flag them in the handoff, don't assume they're covered by the
  in-repo sweep.

The executing agent should still **read `blockr.core` deserialization once**
(`blockr_deser.block` / `restore_board`, where `get0()` resolves the ctor) to
confirm the `package` field is the only board-side reference that needs
rewriting — then do the sweep with confidence. This is no longer a *blocking*
unknown, just a sanity check.

---

## Blast-radius inventory

Line numbers drift — **regenerate the live list** before editing:

```bash
cd /workspace
# every reference, grouped by package
grep -rln "blockr\.bi" . --include=*.R --include=*.Rmd --include=*.md \
  --include=*.json --include=*.yml --include=*.yaml --include=DESCRIPTION \
  --include=NAMESPACE --include=*.Rproj --include=*.css --include=*.js \
  | grep -v "/.devcontainer/.library/"   # installed mirrors — regenerated, skip
# runtime call sites (these BREAK if not updated)
grep -rn "blockr\.bi:::\?" . --include=*.R | grep -v "/.devcontainer/.library/"
```

Categorized (snapshot 2026-06-13; `.devcontainer/.library/**` excluded
throughout — those are installed copies that regenerate on reinstall):

### 1. The package itself — `/workspace/blockr.bi/`
- `git mv blockr.bi blockr.viz` (rename the directory).
- `DESCRIPTION`: `Package:`, `Title:`, `Description:`, `URL:`, `BugReports:`
  (→ `…/blockr.viz`).
- Rename `blockr.bi.Rproj` → `blockr.viz.Rproj` if present.
- `tests/testthat.R`: `library(blockr.bi)` + `test_check("blockr.bi")`.
- `README.md`: title, install line, prose. (Good moment for the five-layer
  rewrite — see Documentation phase.)
- Internal self-`::` references (drop the prefix or switch to `blockr.viz::`):
  `R/summary-table-block.R` (`blockr.bi::summary_table`, x2),
  `R/tile-block.R` (`blockr.bi::tile_shape`),
  `R/html-table-block.R` (`blockr.bi::html_table`),
  `R/gt-table-block.R` (`blockr.bi::gt_table`).
- Test internals: `tests/testthat/test-pivot-table-block.R`
  (`blockr.bi:::build_pivot_expr`, ~20x),
  `tests/testthat/test-waterfall-block.R` (`blockr.bi:::build_waterfall_data`,
  ~4x). NOTE: pivot+waterfall are slated for deprecation/folding — these test
  files may be **deleted** by the cleanup, not renamed.

### 2. Internal name alignment (the "while we're in here" cleanup)
The package still wears the old name internally. Align it (each is a separate,
greppable rename — update NAMESPACE/roxygen `@export` + all call sites):
- `register_bi_blocks()` → `register_viz_blocks()` (called in `zzz.R`;
  external caller: `blockr.ai/dev/summary-table-eval-live.R`).
- `bi_demo_data` → `viz_demo_data` — **but** the architecture doc flags
  `bi_demo_data` as stale; prefer **deleting** it (migrate demos to
  `tile_demo_data` / `safetyData::adam_*`) over renaming.
- `bi_filter` / `new_bi_filter_block` — already half-migrated: `blockr.unibas`
  calls `blockr.bi::new_visual_filter_block`, and the architecture doc calls
  `bi_filter` a defunct stub → `blockr.dm::new_value_filter_block`. **Reconcile
  the filter-block names** as part of this (don't just `s/bi/viz/`).
- `echart_theme_blockr_bi()` → `echart_theme_blockr_viz()` (called in `zzz.R`).
- Shiny resource paths + htmlDependency names in `zzz.R`
  (`"blockr-bi-js"`, `"blockr-bi-css"`), `R/viz-block-dep.R`,
  `R/drilldown-chart-dep.R` → `"blockr-viz-js"` / `"blockr-viz-css"`. If you
  rename these, grep `inst/js/**` and `inst/css/**` for any hardcoded
  `blockr-bi-` path strings and update together. **Bump DESCRIPTION Version** so
  the htmlDependency cache busts (see memory `infra_htmlwidget_js_cache`).
- `utils::packageVersion("blockr.bi")` calls in `R/viz-block-dep.R`,
  `R/drilldown-chart-dep.R`.
- `system.file(..., package = "blockr.bi")` (the ones using a literal, not
  `pkgname`).
  > Note: `R/viz-block-dep.R` is already named "viz" — the rename leans into an
  > already-started direction.

### 3. Hard dependency edges — DESCRIPTION (MUST change or installs break)
- `blockr.cdex/DESCRIPTION`
- `blockr.insurance/DESCRIPTION`
- `blockr.sandbox/DESCRIPTION`
- `blockr.unibas/DESCRIPTION`
Update `Imports`/`Suggests`/`Remotes` entries. If any pin a `@branch` Remote,
see memory `infra_sandbox_deploy_transitive_remote_pin` — fix the ref at source.

### 4. Runtime cross-package call sites (break at runtime if missed)
- `blockr.unibas/R/dashboard.R`: `blockr.bi::new_visual_filter_block`,
  `::new_kpi_block`, `::new_pivot_table_block` (note: kpi+pivot are deprecated —
  this dashboard should migrate to `tile`/dplyr-pivot anyway).
- `blockr.ai/dev/*.R` (eval scripts): `summary-table-eval-live.R`
  (`register_bi_blocks()`), `drilldown-eval-live.R` /
  `drilldown-table-eval-live.R` (`register_drilldown_ai_effect()`).
- `blockr.csr/dev/*.R` (dev apps, `new_gt_table_block`/`html_table`/
  `summary_table` refs) — `blockr.csr` is an archive candidate (unused), so
  lowest priority; update or let it die with the package.

### 5. MCP block universe (drives what the AI sees)
- `blockr.mcp/R/block-universe.R` — the `package` field / comments.
- `blockr.mcp/tests/testthat/test-block-universe.R`.
- `blockr.mcp/dev/STATUS.md`.
Listed explicitly in the architecture doc's blast-radius note. Verify the AI
picker shows `blockr.viz` blocks after the change.

### 6. Serialized board JSONs (`"package":["blockr.bi"]`) — see compat section
- `blockr.csr/inst/extdata/*.json` (~15 files)
- `blockr.sandbox/dev/json/*.json` (csr-*, cedx-poc-v2, …)
- `blockr.insurance/dev/json/*.json` + `inst/examples/**` json
- `blockr.pharma/inst/examples/clinical-explorer.json`
- `blockr.deploy/shinyproxy-hetzner/apps/**/*.json`
- `blockr.ideas/07-research/pharma/cedx-explorer-demo/*.json`
Handle per the **compatibility strategy** decided above (shim and/or scripted
rewrite). Do NOT blindly sed these until the blockr.core behavior is confirmed.

### 7. Documentation
- `blockr.docs/`: `patterns/scale-map.md`,
  `design-system/spacing-and-sizing.md`,
  `design-system/components/blockr-popover.md`,
  `decisions/0001-hand-rolled-vs-libraries.md`. (blockr.docs is the **source of
  truth** for block API — memory `project_blockr_docs_source_of_truth`; update
  here first, then site.)
- `blockr-site/`: `install.md`, `packages/index.md`, `examples/index.md` (source
  `.md`). The `.vitepress/dist/assets/*.js` are **built artifacts** — regenerate
  via the site build, don't hand-edit.
- `blockr.ideas/`: `02-product/roadmap.md`, `blockr-fit-ideas.md`,
  `01-strategy/competitive-landscape.md`, `misc-ideas/…`, research demos. Prose
  references — update or annotate; low priority.
- `_blockr.design/`: `open/bi-dm-filter/**`, `open/drilldown-table-block/**`,
  `open/html-table-preview/**`, `open/ai-pipeline-builder/1-motivation.md`.
  Update **open** specs; leave `done/**` (e.g. `done/blockr.theme/**`) as
  historical record (optionally add a "renamed to blockr.viz" footnote).
- `_team-ops/.claude/rules/repo-locations.md` — update the repo entry.

### 8. Deployment (coordinate before redeploy)
- `blockr.topline/`: `manifest.json`, `app.R`, `deploy-blue.R` (+ `renv.*.lock`
  if blockr.bi is pinned there) — blue/green deploy pins the package; bump in
  lockstep with a redeploy.
- `blockr.deploy/shinyproxy-hetzner/`: `config/shinyproxy/application.yml`,
  the per-app `app.R` (library calls) + board JSONs.
- `blockr.marketing/talks/**` demo apps — refresh before any talk.
Treat these as a **post-merge deploy pass**, not part of the package PR.

### 9. Auto-memory (low priority, machine-local)
- `MEMORY.md` + topic files mentioning blockr.bi (and the `.devcontainer/.library`
  mirror of memory). Update the index line + relevant topic files after the
  rename lands. Not blocking.

### 10. External / GitHub (manual — needs Christoph)
- Rename the GitHub repo `BristolMyersSquibb/blockr.bi` → `blockr.viz` (GitHub
  auto-redirects old URLs, but update anyway). Repos are in the **private BMS
  org** (memory `project_blockr_sandbox_deploy`) — access is via scoped PAT.
- Update the working clone's `git remote set-url`.
- Update install instructions everywhere to `pak::pak("BristolMyersSquibb/blockr.viz")`.

---

## Execution sequence

> Use `/usr/lib/git-core/git` for all git ops in this container (the bare `git`
> is a stub — see root `CLAUDE.md`). Do the rename on a branch, e.g.
> `refactor/rename-blockr-viz`.

0. **Sanity-check blockr.core restore** (confirm the `package` field is the only
   board-side reference) — quick, no longer blocking. Board strategy is decided:
   clean break + migrate JSONs (category 7-bis), no shim.
1. **(Recommended) land the architecture-doc cleanup** so deprecated blocks are
   gone and won't need renaming.
2. **Rename the package dir + DESCRIPTION + .Rproj + tests/testthat.R + README
   title/install.** `git mv` to preserve history.
3. **Internal name alignment** (category 2): `register_bi_blocks`,
   `echart_theme_blockr_bi`, resource paths/htmlDependency names, `packageVersion`/
   `system.file` literals, filter-block reconciliation, `bi_demo_data`
   delete-or-rename. **Bump DESCRIPTION Version** (cache-bust JS/CSS).
4. **Regenerate NAMESPACE**: `cd /workspace/blockr.viz && Rscript -e
   'roxygen2::roxygenise()'`.
5. **Update reverse deps** (category 3 DESCRIPTIONs + category 4 runtime call
   sites). Reinstall affected packages from local source (root `CLAUDE.md`
   install recipe).
6. **MCP universe** (category 5) + verify the AI picker.
7. **Board migration** (category 6): sweep all in-repo JSONs
   (`"package":["blockr.bi"]` → `"blockr.viz"`). Hand off external/deployed
   boards to Christoph.
8. **Documentation** (category 7): blockr.docs first, then build blockr-site,
   then ideas/design/team-ops.
9. **Deploy pass** (category 8) — separate, coordinated with a redeploy.
10. **Memory + external/GitHub** (categories 9–10).

## Verification checklist

- [ ] `R CMD check` (or `devtools::check()`) on `blockr.viz` is clean.
- [ ] Reinstall blockr.viz + all reverse deps from local source; each loads
      without "could not find function" / missing-package errors.
- [ ] `grep -rn "blockr\.bi:::\?" --include=*.R /workspace | grep -v /.library/`
      returns **only** intentional shim/back-compat references (ideally none).
- [ ] A representative saved board (e.g.
      `blockr.csr/inst/extdata/csr-ae-summary-v3.json`) **restores** in a running
      app — proves the board-compat strategy works. (Use the blockr-playwright
      skill; remember dock background tabs suspend output — memory
      `reference_drilldown_verify_recipe`.)
- [ ] MCP `block_universe` lists the blocks under `blockr.viz`; AI assistant can
      add a chart/table block end-to-end.
- [ ] blockr-site builds; install + package + examples pages say `blockr.viz`.
- [ ] htmlwidget assets actually reload in the browser (version bumped).

## Rollback

- Pre-merge: discard the branch.
- Post-merge but pre-deploy: `git revert` the rename commit(s); the directory
  `git mv` reverts cleanly. Reinstall. (Migrated boards revert with the same
  commit — the JSON `package`-field sweep is in the same change set.)
- Clean break, no shim: a board missed by the sweep simply won't restore until
  its `package` field is fixed — there is no silent fallback, by design.

## Open sub-decisions to settle while executing

1. **`summary_table` name** — if the architecture doc resolves it to `describe`,
   fold that rename into this pass rather than doing it twice.
2. **`gt`/`html_table` ownership** — if they move to `blockr.gt`, they leave
   `blockr.viz` entirely and drop off this list.
