# Unbalanced Scenario Indicator Data

Small indicator datasets and metadata tables used in the unbalanced
placeholder scenarios included with the package. Each scenario
demonstrates a different hierarchy irregularity (missing levels,
supplied aggregates, or intentionally imbalanced branches) for testing
unbalanced coin handling.

## Usage

``` r
unbal_scen1_iData

unbal_scen1_iMeta

unbal_scen2_iData

unbal_scen2_iMeta

unbal_scen3_iData

unbal_scen3_iMeta

unbal_scen4_iMeta
```

## Format

Data frames with three units (rows) and scenario-specific indicator
columns. Metadata tables include columns `iCode`, `Parent`, `Type`,
`Level`, `Weight`, `Direction`, and optional denominator references.

An object of class `data.frame` with 3 rows and 6 columns.

An object of class `data.frame` with 7 rows and 7 columns.

An object of class `data.frame` with 3 rows and 6 columns.

An object of class `data.frame` with 8 rows and 7 columns.

An object of class `data.frame` with 3 rows and 9 columns.

An object of class `data.frame` with 10 rows and 7 columns.

An object of class `data.frame` with 10 rows and 7 columns.

## Details

- `unbal_scen1_iData`/`unbal_scen1_iMeta`: Scenario 1 where an aggregate
  is supplied alongside its child indicators. Includes denominator
  column `DenomA`.

- `unbal_scen2_iData`/`unbal_scen2_iMeta`: Scenario 2 featuring an
  indicator linked directly to the top level, bypassing intermediate
  nodes. Includes denominators for all indicators including the
  bypassing one.

- `unbal_scen3_iData`/`unbal_scen3_iMeta`: Scenario 3 with additional
  branches requiring multiple placeholder aggregates and indicator
  denominators.

- `unbal_scen4_iMeta`: Scenario 4 metadata where an aggregate remains at
  Level 1, used to trigger validation failures when balancing.
