# Denominate data set in a coin

"Denominates" or "scales" indicators by other variables. Typically this
is done by dividing extensive variables such as GDP by a scaling
variable such as population, to give an intensive variable (GDP per
capita).

## Usage

``` r
# S3 method for class 'coin'
Denominate(
  x,
  dset,
  denoms = NULL,
  denomby = NULL,
  denoms_ID = NULL,
  f_denom = NULL,
  write_to = NULL,
  out2 = "coin",
  ...
)

# S3 method for class 'unbalanced_coin'
Denominate(
  x,
  dset,
  denoms = NULL,
  denomby = NULL,
  denoms_ID = NULL,
  f_denom = NULL,
  write_to = NULL,
  out2 = "unbalanced_coin",
  ...
)
```

## Arguments

- x:

  A `coin` object for `Denominate.coin()` or an `unbalanced_coin`
  returned by
  [`new_unbalanced_coin()`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md)
  for `Denominate.unbalanced_coin()`.

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- denoms:

  An optional data frame of denominator data. Columns should be
  denominator data, with column names corresponding to entries in
  `denomby`. This must also include an ID column identified by
  `denoms_ID` to match rows. If `denoms` is not specified, will extract
  any potential denominator columns that were attached to `iData` when
  calling
  [`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).

- denomby:

  Optional data frame which specifies which denominators to use for each
  indicator, and any scaling factors to apply. Should have columns
  `iCode`, `Denominator`, `ScaleFactor`. `iCode` specifies an indicator
  code found in `dset`, `Denominator` specifies a column name from
  `denoms` to use to denominate the corresponding column from `x`.
  `ScaleFactor` allows the possibility to scale denominators if needed,
  and specifies a factor to multiply the resulting values by. For
  example, if GDP is a denominator and is measured in dollars, dividing
  will create very small numbers (order 1e-10 and smaller) which could
  cause problems with numerical precision. If `denomby` is not
  specified, specifications will be taken from the "Denominator" column
  in `iMeta`, if it exists.

- denoms_ID:

  An ID column for matching `denoms` with the data to be denominated.
  This column should contain `uMeta` codes to match with the data set
  extracted from the coin.

- f_denom:

  A function which takes two numeric vector arguments and is used to
  perform the denomination for each column. By default, this is
  division, i.e. `x[[col]]/denoms[[col]]` for given columns, but any
  function can be passed that takes two numeric vectors as inputs and
  returns a single numeric vector. See details.

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Denominated"`.

- out2:

  For `Denominate.coin()`, either `"coin"` (default) to return the
  updated coin or `"df"` to output the denominated data set. For
  `Denominate.unbalanced_coin()`, either `"unbalanced_coin"` (default)
  to retain the class or `"df"`; specifying `"coin"` is not supported
  because it would drop the unbalanced hierarchy.

- ...:

  arguments passed to or from other methods

## Value

For `Denominate.coin()`, an updated `coin` when `out2 = "coin"` or a
data frame when `out2 = "df"`. For `Denominate.unbalanced_coin()`, an
updated `unbalanced_coin` when `out2 = "unbalanced_coin"` or a data
frame when `out2 = "df"`.

## Details

This function denominates a data set `dset` inside the coin. By default,
denominating variables are taken from the coin, specifically as
variables in `iData` with `Type = "Denominator"` in `iMeta` (input to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)).
Specifications to map denominators to indicators are also taken by
default from `iMeta$Denominator`, if it exists.

These specifications can be overridden using the `denoms` and `denomby`
arguments. The operator for denomination can also be changed using the
`f_denom` argument.

See also documentation for
[`Denominate.data.frame()`](https://bluefoxr.github.io/COINr/reference/Denominate.data.frame.md)
which is called by this method.

Compared with `Denominate.coin()`, the unbalanced method blocks
`out2 = "coin"` and keeps the restored lineage/max-level of the original
unbalanced hierarchy while stripping internal placeholder nodes.

## Functions

- `Denominate(unbalanced_coin)`: Wrapper that retains the unbalanced
  structure.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# denominate (here, we only need to say which dset to use, takes
# specs and denominators from within the coin)
coin <- Denominate(coin, dset = "Raw")
#> Written data set to .$Data$Denominated

data("unbal_iData", package = "COINr")
data("unbal_iMeta", package = "COINr")
unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
#> iData column 'IndA1' converted from integer to numeric.
#> iData column 'IndA2' converted from integer to numeric.
#> iData column 'IndB' converted from integer to numeric.
denoms <- data.frame(uCode = unbal_iData$uCode, DenSub = c(2, 4, 5), DenB = c(10, 12, 15))
specs <- data.frame(
  iCode = c("IndA1", "IndA2", "IndB"),
  Denominator = c("DenSub", "DenSub", "DenB"),
  ScaleFactor = 1
)
Denominate(unbal, dset = "Raw", denoms = denoms, denomby = specs)
#> Written data set to .$Data$Denominated
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
#>   Denominated (3 units)
#> 
#> Unbalanced hierarchy summary:
#>   Depth 2: 1 nodes (IndB)
#>   Depth 3: 2 nodes (IndA1, IndA2)
#>   Depth range: 2-3 levels
```
