# Registry <-> constructor <-> external_ctrl drift guard.
#
# The `*_arguments()` builders are the ONLY surface the AI assistant / MCP see;
# `external_ctrl` is the set of state fields a host (or the AI) can drive on a
# live block. When a constructor arg is renamed (metric -> value, values ->
# value; see dev/unified-arg-naming.md) or a new external_ctrl field is added,
# the registry and the config_effect readers silently drift: the AI either
# invents names (rejected add_block) or never learns a control exists. These
# tests turn that bug class into a test failure.

drift_blocks <- function() {
  list(
    chart         = list(args = chart_arguments,         ctor = new_chart_block),
    table         = list(args = table_arguments,         ctor = new_table_block),
    tile          = list(args = tile_arguments,          ctor = new_tile_block),
    summary_table = list(args = summary_table_arguments, ctor = new_summary_table_block),
    gt_table      = list(args = gt_table_arguments,      ctor = new_gt_table_block)
  )
}

ctor_formals <- function(ctor) {
  setdiff(names(formals(ctor)), "...")
}

# Fields deliberately absent from the AI registry, per block. Every entry
# needs a reason; anything not listed here and not in the registry FAILS.
registry_allowlist <- list(
  chart = character(),
  table = c(
    # Runtime filter transport: written by clicks, round-trips through
    # save/restore only -- never AI-set at creation.
    "filter_type", "filter_column", "filter_values", "filter_range",
    "filter_group_cols", "filter_group_vals",
    # Display-only gear toggles / sizing, deliberately off the AI surface.
    "max_height", "sortable", "collapsible", "search", "excel_download"
  ),
  tile = c(
    # Runtime filter transport (see the table's filter_* above).
    "filter_col", "filter_value",
    # LEGACY, ctor-only for saved-board restore: `overline` lost its gear
    # control (duplicated the Name slot); `good_when` is ignored (polarity
    # is forced to "up"). Neither may be advertised to the AI.
    "overline", "good_when"
  ),
  summary_table = character(),
  gt_table = character()
)

test_that("every registry arg names a real constructor formal", {
  for (nm in names(drift_blocks())) {
    b <- drift_blocks()[[nm]]
    extra <- setdiff(names(b$args()), ctor_formals(b$ctor))
    expect_length(extra, 0)
    if (length(extra)) {
      fail(paste0(nm, "_arguments() advertises nonexistent ctor args: ",
                  paste(extra, collapse = ", ")))
    }
  }
})

test_that("every external_ctrl field is in the registry or allowlisted", {
  for (nm in names(drift_blocks())) {
    b <- drift_blocks()[[nm]]
    ctrl <- blockr.core::external_ctrl_vars(b$ctor())
    covered <- c(
      names(b$args()),
      registry_allowlist[[nm]],
      "block_name" # external_ctrl_vars() appends it for every block
    )
    missing <- setdiff(ctrl, covered)
    expect_length(missing, 0)
    if (length(missing)) {
      fail(paste0(nm, ": external_ctrl fields invisible to the AI and not ",
                  "allowlisted: ", paste(missing, collapse = ", ")))
    }
  }
})

test_that("allowlists carry no stale (non-external_ctrl) entries", {
  for (nm in names(drift_blocks())) {
    b <- drift_blocks()[[nm]]
    ctrl <- blockr.core::external_ctrl_vars(b$ctor())
    stale <- setdiff(registry_allowlist[[nm]], ctrl)
    expect_length(stale, 0)
    if (length(stale)) {
      fail(paste0(nm, ": allowlist entries no longer in external_ctrl: ",
                  paste(stale, collapse = ", ")))
    }
  }
})

# config_effect.* report the configured args back to the AI. A reader of a
# renamed / removed arg (`args$values` after the values -> value rename) is
# silent at runtime -- it just reports nothing -- so guard it statically:
# every `args$<name>` access must name a constructor formal.
config_effect_reads <- function(fn) {
  txt <- paste(deparse(body(fn)), collapse = "\n")
  m <- regmatches(txt, gregexpr("args\\$([A-Za-z0-9._]+)", txt))
  unique(sub("^args\\$", "", unlist(m)))
}

test_that("config_effect implementations only read real ctor args", {
  chart_reads <- union(config_effect_reads(config_effect.chart_block),
                       dd_chart_roles)
  expect_length(setdiff(chart_reads, ctor_formals(new_chart_block)), 0)

  table_reads <- config_effect_reads(config_effect.table_block)
  expect_length(setdiff(table_reads, ctor_formals(new_table_block)), 0)
})

test_that("gear-emptied chart fields are wedge-safe (allow_empty_state)", {
  # The JS gear legitimately empties `value` when the aggregation changes
  # (reconcileValue in drilldown-agg.js, _ensureBoxplotMetric in chart.js);
  # a field emptied by the gear but missing from allow_empty_state silently
  # freezes the block mid-configuration (the allow_empty_state wedge).
  aes <- attr(new_chart_block(), "allow_empty_state")
  expect_true("value" %in% aes)
})
