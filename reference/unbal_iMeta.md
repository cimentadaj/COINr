# Prototype unbalanced metadata

Companion metadata for
[unbal_iData](https://bluefoxr.github.io/COINr/reference/unbal_iData.md)
defining the unbalanced hierarchy. `IndA1` and `IndA2` roll up into
`SubA`, which together with `IndB` aggregates into the top-level
`Index`. Weights are set to 0.5 so that each child contributes equally
within its parent group.

## Usage

``` r
unbal_iMeta
```

## Format

A data frame with 5 rows and 6 variables:

- iCode:

  Indicator or aggregate code.

- Level:

  Intended hierarchy level (1 = indicator).

- Parent:

  Parent code (blank/`NA` at the top level).

- Direction:

  Indicator direction (`1` = higher is better).

- Weight:

  Nominal weight within the parent group.

- Type:

  `"Indicator"` or `"Aggregate"`.

## See also

[unbal_iData](https://bluefoxr.github.io/COINr/reference/unbal_iData.md)
