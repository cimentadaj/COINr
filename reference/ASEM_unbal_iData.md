# ASEM indicator data with an unbalanced hierarchy

A copy of
[ASEM_iData](https://bluefoxr.github.io/COINr/reference/ASEM_iData.md)
paired with a modified hierarchy that introduces uneven depth across the
connectivity pillars. Several aggregates such as "Connectivity" and
"Physical" are split into additional intermediate nodes (see
[ASEM_unbal_iMeta](https://bluefoxr.github.io/COINr/reference/ASEM_unbal_iMeta.md)),
which causes some indicators to roll up through two steps while others
feed directly into the higher levels. The data are unchanged from the
published ASEM release; only the hierarchy is altered to illustrate the
behaviour of
[`new_unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md).

## Usage

``` r
ASEM_unbal_iData
```

## Format

A data frame with 51 rows. Columns include the unit name (`uName`), unit
code (`uCode`), four denominator columns (`Area`, `Energy`, `GDP`,
`Population`), grouping variables, a `Time` stamp, and 55 indicator
columns referenced by `iCode` values in
[ASEM_unbal_iMeta](https://bluefoxr.github.io/COINr/reference/ASEM_unbal_iMeta.md).

## See also

[ASEM_unbal_iMeta](https://bluefoxr.github.io/COINr/reference/ASEM_unbal_iMeta.md),
[ASEM_iData](https://bluefoxr.github.io/COINr/reference/ASEM_iData.md),
[`new_unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md)
