# Notes on adapting get_* functions for unbalanced coins

## Current pattern from `get_corr`
- Promote the helper to an S3 generic by wrapping the original body in `get_corr.coin()` and adding `get_corr <- function(coin, ...) UseMethod("get_corr")`.
- Register both `get_corr.coin` and `get_corr.unbalanced_coin` in `NAMESPACE` so pkgdown documents both methods on the same page.
- In the unbalanced method, temporarily drop the `unbalanced_coin` class before delegating to the base method, then strip any placeholder codes from:
  * the copied metadata (`$Meta$Ind`, `$Meta$Aggregated` etc.) used for level selection, and
  * the returned data (long and wide outputs).
- Reapply the `unbalanced_coin` class if a coin object is returned, keeping the public lineage aligned with the unbalanced hierarchy.
- Update docs to mention the unbalanced behaviour in the shared `@details` block so pkgdown renders the difference once.
- Extend tests to cover placeholder suppression, both for long-form data frames and wide matrices, using the `unbal_iData`/`unbal_iMeta` fixtures.

## Reuse checklist for other `get_*`
1. Wrap the existing implementation in a `{method}.coin` and add the generic.
2. Add `{method}.unbalanced_coin` that:
   - copies the input with the `unbalanced_coin` class removed,
   - removes placeholder codes from any metadata the base method relies on,
   - calls the coin method (`NextMethod` is ok if there is no internal recursion),
   - prunes placeholder artefacts from all possible return types (coin, data frame, matrix, list), and
   - restores the `unbalanced_coin` class if the result is a coin.
3. Update `NAMESPACE` registrations.
4. Document the unbalanced behaviour in the shared roxygen block so pkgdown keeps both methods on one reference page.
5. Add regression tests that assert the placeholder codes never surface in the outward results and that the unbalanced outputs match the balanced ones after placeholders are removed.
