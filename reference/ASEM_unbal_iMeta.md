# ASEM metadata defining an unbalanced hierarchy

Companion metadata for
[ASEM_unbal_iData](https://bluefoxr.github.io/COINr/reference/ASEM_unbal_iData.md)
that adds intermediate aggregates beneath selected pillars. The helper
nodes (`PhysTrans`, `ConFinance`, `P2PCulture`, and others) create
branches of different depths beneath the `Connectivity` top level,
resulting in a hierarchy that contains both four-level and five-level
paths. When constructing an unbalanced coin the metadata should be
supplied without the `iName` column so that automatically generated
placeholder nodes remain valid.

## Usage

``` r
ASEM_unbal_iMeta
```

## Format

A data frame with 81 rows and 10 variables:

- Level:

  Integer depth inferred from the modified hierarchy.

- iCode:

  Indicator or aggregate code.

- iName:

  Human-readable name (remove before calling
  [`new_unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md)
  so placeholder nodes can be inserted).

- Direction:

  Indicator direction (`1` = higher is better, `-1` = lower is better).

- Weight:

  Nominal weights carried over from the published ASEM metadata.

- Unit:

  Original indicator measurement units.

- Target:

  Target values, where available.

- Denominator:

  Optional denominators used for normalisation.

- Parent:

  Immediate parent code in the unbalanced hierarchy.

- Type:

  Entry type (`"Indicator"`, `"Aggregate"`, `"Group"`, or
  `"Denominator"`).

## Details

Intermediate aggregates are introduced for the physical, economic
connectivity, people-to-people, environmental, social, and sustainable
finance pillars. These additions yield paths of different depths (e.g.
`Flights → PhysTrans → Physical → Conn → Index` versus
`CostImpEx → Instit → Conn → Index`). The dataset provides a convenient
starting point for testing unbalanced workflows without modifying the
raw ASEM indicators.

## See also

[ASEM_unbal_iData](https://bluefoxr.github.io/COINr/reference/ASEM_unbal_iData.md),
[ASEM_iMeta](https://bluefoxr.github.io/COINr/reference/ASEM_iMeta.md),
[`new_unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md)
