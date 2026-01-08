# NA

\[x\] Update aggregation input schema to accept per-parent function
overrides alongside existing scalar/per-level specs. \[x\] Introduce
aggregation spec parser that normalises inputs into a tibble keyed by
parent aggregates with validated defaults. \[x\] Refactor
[`Aggregate.coin()`](https://bluefoxr.github.io/COINr/reference/Aggregate.coin.md)
to iterate over parent aggregates, pulling
function/parameter/weight/threshold info from the parsed spec. \[x\]
Ensure
[`Aggregate.unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/Aggregate.coin.md)
continues to manage placeholder scaffolding transparently after the
refactor. \[x\] Record parsed aggregation specs in metadata/logs without
altering public structures consumed by downstream helpers. \[x\] Extend
regression coverage to include mixed-function aggregation scenarios
(balanced and unbalanced cases). \[x\] Surface the mixed-function
aggregation example in
`tmp_script/unbalanced_proto/unbalanced_placeholders_demo.R`. \[ \]
Draft documentation updates for the new API.
