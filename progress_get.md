# Notes on adapting `get_*` helpers for `unbalanced_coin`

## Current status
- Wrappers landed: `get_corr`, `get_corr_flags`, `get_cronbach`, `get_data`, `get_data_avail`, `get_denom_corr`, `get_eff_weights`, `get_opt_weights`, `get_noisy_weights`, and `get_PCA`.
- Each has targeted regression coverage that leans on `unbal_iData`/`unbal_iMeta`. Tests live beside the original suites (`tests/testthat/test-correlations.R`, `test-data_sel.R`, `test-weights.R`, `test-sensitivity.R`, `test-PCA.R`).
- `tmp_script/unbalanced_proto/run_proto.R` exercises the new surface area but must remain uncommitted unless explicitly requested.
- Default posture: leave the tree dirty until instructed to commit; never commit scratch helpers, ignored files, or regenerated man pages outside a roxygen pass that is part of the task.

## Repeatable pattern
1. **Promote to S3**: hoist the legacy body into `{method}.coin` and add a tiny generic (`{method} <- function(x, ...) UseMethod("{method}")`). Keep signatures aligned so `match.call()` still works.
2. **Prepare the delegate**: inside `{method}.unbalanced_coin`, drop the `unbalanced_coin` class, collect `PlaceholderCodes`, and sanitise any metadata the base method reads (`Meta$Ind`, `Meta$Lineage`, etc.).
3. **Patch dependent data**: if the base method expects columns that only exist in the balanced view (e.g. helper aggregates in an aggregated data set), synthesise them via `PlaceholderMap` before calling the coin method.
4. **Delegate deliberately**: call the coin implementation directly (avoid `NextMethod` if the coin variant recurses back into the generic). Forward all user arguments plus `...`.
5. **Strip placeholders on the way out**: prune placeholder codes from every supported return type (data frames, matrices, lists, coin objects). If a coin comes back, reapply the unbalanced class with `.ensure_unbalanced_class()`.
6. **Register and document**: add both S3 methods to `NAMESPACE`, and mention the unbalanced behaviour in the shared roxygen block so pkgdown co-locates the docs.
7. **Test with intent**: assert (a) no placeholder codes leak into the outward results, and (b) equality with the balanced output after filtering placeholders away. Seed RNG (`set.seed`) in tests that rely on random noise.
8. **Update proto script (optional)**: if it helps manual QA, mirror the new helper in `tmp_script/unbalanced_proto/run_proto.R` with a focused call.

## Function-specific reminders
- `get_corr` / `get_corr_flags`: filter placeholder rows for both long and wide forms; direct-call `get_corr.coin` inside recursive branches.
- `get_cronbach`: scalar output—just sanitise metadata pre-call.
- `get_data`: sanitise lineage and strip placeholder columns from every data frame variant.
- `get_data_avail`: call `.sanitize_lineage()`, then dedupe both `Summary` and `ByGroup` tables.
- `get_denom_corr`: drop placeholders pre-call and filter both indicator and denominator columns afterwards.
- `get_eff_weights`: delegate, then prune placeholder rows before returning either a data frame or a coin (ensuring the class tag sticks).
- `get_opt_weights`: mind the `out2` branches (`list` vs `coin`); filter both `WeightsOpt` and `CorrResultsNorm`, and tidy coin-side analysis entries.
- `get_noisy_weights`: for unbalanced coins, fetch the balanced weight frame, generate replications, then drop placeholder rows from each replicate; seed tests.
- `get_PCA`: duplicate placeholder columns in the targeted data set via `PlaceholderMap` before delegating; afterwards strip placeholders from weights, PCA result lists, and coin analysis slots.
- Always ensure newly generated columns or weight entries retain deterministic ordering so equality checks survive across environments.

## Safety nudge
- Never commit `tmp_script/` assets or other ignored files.
- Rerun `devtools::document()` only when the task requires doc updates; `man/` files should come exclusively from roxygen.
- When in doubt, inspect `coin$Meta$Unbalanced` for `PlaceholderCodes`, `PlaceholderMap`, and the balanced lineage to avoid accidental leakage.
