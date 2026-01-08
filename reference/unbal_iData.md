# Prototype unbalanced indicator data

A minimal indicator table used in tests and examples for
[`new_unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md).
Each row is a unit (`uCode`) and the remaining columns are raw
indicators (`IndA1`, `IndA2`, `IndB`). The hierarchy is intentionally
unbalanced: `IndA1`/`IndA2` are aggregated into `SubA`, while `IndB`
feeds directly into the top-level `Index`.

## Usage

``` r
unbal_iData
```

## Format

A data frame with 3 rows and 4 variables:

- uCode:

  Character unit identifier.

- IndA1:

  First component indicator contributing to `SubA`.

- IndA2:

  Second component indicator contributing to `SubA`.

- IndB:

  Indicator that links directly to the top-level `Index`.

## See also

[unbal_iMeta](https://bluefoxr.github.io/COINr/reference/unbal_iMeta.md)
