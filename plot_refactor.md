# Placeholder-aware plotting for unbalanced coins

This note documents how plotting helpers (and the sensitivity visualisations that feed into them) should behave when the input coin inherits from `unbalanced_coin`. It assumes no prior exposure to the ongoing placeholder work.

## Recap: what are placeholders?

When a hierarchy contains “holes” (e.g. an aggregate supplied directly at Level 1, or an indicator that jumps from Level 1 straight to the top), `new_unbalanced_coin()` fabricates *placeholder* nodes to balance the structure internally:

- **Placeholder indicators** clone their parent’s data so that a Level‑1 column exists wherever the aggregate needs a child.
- **Placeholder aggregates** bridge missing intermediate levels by inserting synthetic parents that simply replicate their child.

These nodes are tagged in `coin$Meta$Unbalanced` and are removed again before outputs are shown to the user. They exist purely to keep downstream verbs (Aggregate, PCA, etc.) from breaking when they expect a dense tree.

## Philosophy for unbalanced helpers

Every unbalanced S3 method follows the same pattern:

1. **Sanitise metadata**: limit `Meta$Ind` to genuine indicators/aggregates and drop rows whose `iCode` is a placeholder. Adjust lineage (`Meta$Lineage`) with `.sanitize_lineage()` so level lookups still work.
2. **Clean datasets**: when the helper calls `get_dset()` (or accesses `coin$Data[[dset]]` directly), remove placeholder columns from the working copy. This prevents duplicate child columns from entering any calculations.
3. **Delegate to the balanced method**: temporarily set `options(COINr.keep_placeholders = TRUE)` so that helper functions invoked by the base method can regenerate scaffolding if they rely on it (e.g. `Regen()` inside PCA). Call the underlying `plot_*` or analytical function.
4. **Strip placeholder artefacts**: after the delegate returns, scrub any placeholder codes from the outgoing results (legend entries, tables, weight matrices) before returning them or drawing the plot.

The goal is that public outputs look exactly like the balanced case, while the internal placeholder machinery never leaks into user-facing data.

## Implementation plan for plotting helpers

The same recipe applies to each plotting function listed below:

### `plot_bar.unbalanced_coin`
- Before calling `plot_bar.coin()`, wrap the coin as above (filter `Meta$Ind`, sanitise lineage, drop placeholder columns from `coin$Data[[dset]]`).
- Delegate to the base method with `COINr.keep_placeholders = TRUE` temporarily.
- If the plotting function returns data (e.g. for facetting), strip any placeholder codes from the frame before returning the ggplot object.

### `plot_corr.unbalanced_coin`
- Sanitise metadata and dataset the same way (`Levels` may fetch columns from aggregated sets—ensure placeholders are removed from the data used to compute correlations).
- Delegate and strip placeholder entries from the correlation tibble (rows/columns) before rendering.

### `plot_dist.unbalanced_coin`
- Remove placeholders from the dataset columns specified by `iCodes` so the density plot uses only genuine indicators.
- After delegation, ensure the data frame stored in the `ggplot` object (if accessible) has no placeholder codes.

### `plot_dot.unbalanced_coin`
- Drop placeholder columns from both the dataset and any `Meta$Ind` lookups (`Levels` is often used to label the axis).
- Strip placeholder rows from the summary table passed to the plot (if the base helper returns it).

### `plot_framework.unbalanced_coin`
- This plot is lineage heavy. Make sure `.sanitize_lineage()` runs before calling the base helper so that parent/child relationships exclude placeholders.
- Remove placeholder nodes from the graph object after delegation (if the plot builds a network) before returning.

### `plot_scatter.unbalanced_coin`
- For each dataset in `dsets`, trim placeholder columns from the working copy.
- Strip placeholder codes from axis labels and tooltips after the base call.

## Sensitivity and uncertainty plots

Sensitivity helpers operate on a simulation of coin pipelines. The same metadata sanitation should happen inside `get_sensitivity.unbalanced_coin` before the stochastic steps run. When plotting sensitivity results:

### `get_sensitivity.unbalanced_coin`
- Filter `Meta$Ind` to real nodes and clean the lineage before cloning the coin for each simulation.
- Drop placeholder columns from the datasets used to recompute scores.
- When `diagnostic_mode` is on, ensure any stored coin objects reuse `.ensure_unbalanced_class()` so callers still see the unbalanced type.

### `plot_uncertainty.unbalanced_coin`
- Remove placeholder rows/columns from the summary table before drawing the uncertainty bars.
- If the helper combines placeholder columns during the aggregation stage, ensure they’re excluded ahead of time so the final plot matches the balanced output.

### `plot_sensitivity.unbalanced_coin`
- Sanitize the sensitivity results (e.g. `RankStats`, `ParamSummary`) by dropping placeholder codes before handing them to the plotting routine.
- Keep the same post-processing used elsewhere (`.strip_placeholder_results`) to guard against nested list artefacts.

## Demo coverage and tests

1. **Demo script**: extend `tmp_script/unbalanced_proto/unbalanced_placeholders_demo.R` with the new `plot_*` calls (and `plot_uncertainty` / `plot_sensitivity`) so the behaviour is visible end-to-end.
2. **Regression tests**: mirror the PCA test strategy—create a reference balanced coin with placeholders removed, call the balanced plot helper to obtain the data/frame it uses, drop placeholder codes, and compare to the unbalanced result. Visual plots themselves can be tricky to compare directly, but their underlying data (e.g. `ggplot_build(p)$data`) can be compared.

By following this blueprint, every plot and sensitivity visualisation will produce the same data and graphics as the balanced pipeline, while still accepting unbalanced coins without manual intervention. EOF
