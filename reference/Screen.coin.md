# Screen units based on data availability

Screens units based on a data availability threshold and presence of
zeros. Units can be optionally "forced" to be included or excluded,
making exceptions for the data availability threshold.

## Usage

``` r
# S3 method for class 'coin'
Screen(
  x,
  dset,
  unit_screen,
  dat_thresh = NULL,
  nonzero_thresh = NULL,
  Force = NULL,
  out2 = "coin",
  write_to = NULL,
  ...
)

# S3 method for class 'unbalanced_coin'
Screen(
  x,
  dset,
  unit_screen,
  dat_thresh = NULL,
  nonzero_thresh = NULL,
  Force = NULL,
  out2 = "unbalanced_coin",
  write_to = NULL,
  ...
)
```

## Arguments

- x:

  A `coin` object for `Screen.coin()` or an `unbalanced_coin` created
  with
  [`new_unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md)
  for `Screen.unbalanced_coin()`.

- dset:

  The data set to be checked/screened

- unit_screen:

  Specifies whether and how to screen units based on data availability
  or zero values.

  - If set to `"byNA"`, screens units with data availability below
    `dat_thresh`

  - If set to `"byzeros"`, screens units with non-zero values below
    `nonzero_thresh`

  - If set to `"byNAandzeros"`, screens units based on either of the
    previous two criteria being true.

- dat_thresh:

  A data availability threshold (`>= 1` and `<= 0`) used for flagging
  low data and screening units if `unit_screen != "none"`. Default 0.66.

- nonzero_thresh:

  As `dat_thresh` but for non-zero values. Defaults to 0.05, i.e. it
  will flag any units with less than 5% non-zero values (equivalently
  more than 95% zero values).

- Force:

  A data frame with any additional countries to force inclusion or
  exclusion. Required columns `uCode` (unit code(s)) and `Include`
  (logical: `TRUE` to include and `FALSE` to exclude). Specifications
  here override exclusion/inclusion based on data rules.

- out2:

  Where to output the results. For `Screen.coin()`, `"coin"` (default)
  appends results to the coin and `"list"` returns data objects. For
  `Screen.unbalanced_coin()`, `"unbalanced_coin"` (default) appends
  results while preserving the unbalanced structure and `"list"` returns
  data objects; using `"coin"` is not supported.

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Screened"`.

- ...:

  arguments passed to or from other methods.

## Value

For `Screen.coin()`, an updated `coin` with data frames showing missing
data in `.$Analysis` and a new data set `.$Data$Screened` when
`out2 = "coin"`, otherwise a list of screening outputs when
`out2 = "list"`. For `Screen.unbalanced_coin()`, an updated
`unbalanced_coin` when `out2 = "unbalanced_coin"` or a list of screening
outputs when `out2 = "list"`.

## Details

The two main criteria of interest are `NA` values, and zeros. The
summary table gives percentages of `NA` values for each unit, across
indicators, and percentage zero values (*as a percentage of non-`NA`
values*). Each unit is flagged as having low data or too many zeros
based on thresholds.

See also
[`vignette("screening")`](https://bluefoxr.github.io/COINr/articles/screening.md).

This method mirrors `Screen.coin()` but restores lineage/max-level for
unbalanced hierarchies and rejects `out2 = "coin"`. Placeholder nodes
are removed from all outward-facing outputs.

## Functions

- `Screen(unbalanced_coin)`: Wrapper that retains the unbalanced
  structure.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# screen units from raw dset
coin <- Screen(coin, dset = "Raw", unit_screen = "byNA",
               dat_thresh = 0.85, write_to = "Filtered_85pc")
#> Written data set to .$Data$Filtered_85pc

# some details about the coin by calling its print method
coin
#> --------------
#> A coin with...
#> --------------
#> Input:
#>   Units: 51 (AUS, AUT, BEL, ...)
#>   Indicators: 49 (Goods, Services, FDI, ...)
#>   Denominators: 4 (Area, Energy, GDP, ...)
#>   Groups: 4 (GDP_group, GDPpc_group, Pop_group, ...)
#> 
#> Structure:
#>   Level 1 Indicator: 49 indicators (FDI, ForPort, Goods, ...) 
#>   Level 2 Pillar: 8 groups (ConEcFin, Instit, P2P, ...) 
#>   Level 3 Sub-index: 2 groups (Conn, Sust) 
#>   Level 4 Index: 1 groups (Index) 
#> 
#> Data sets:
#>   Raw (51 units)
#>   Filtered_85pc (48 units)

data("unbal_iData", package = "COINr")
data("unbal_iMeta", package = "COINr")
unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
#> iData column 'IndA1' converted from integer to numeric.
#> iData column 'IndA2' converted from integer to numeric.
#> iData column 'IndB' converted from integer to numeric.
Screen(unbal, dset = "Raw", unit_screen = "byNA", dat_thresh = 0.9)
#> Written data set to .$Data$Screened
#> --------------
#> An unbalanced coin with...
#> --------------
#> Input:
#>   Units: 3 (U1, U2, U3)
#>   Indicators: 3 (IndB, IndA1, IndA2)
#>   Denominators: 0 ()
#>   Groups: 0 (none)
#> 
#> Structure:
#>   Level 1 : 3 indicators (IndA1, IndA2, IndB) 
#>   Level 2 : 2 groups (SubA, Index) 
#>   Level 3 : 1 groups (Index) 
#> 
#> Data sets:
#>   Raw (3 units)
#>   Screened (3 units)
#> 
#> Unbalanced hierarchy summary:
#>   Depth 2: 1 nodes (IndB)
#>   Depth 3: 2 nodes (IndA1, IndA2)
#>   Depth range: 2-3 levels
```
