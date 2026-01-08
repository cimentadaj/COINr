# P-values for correlations

This is a stripped down version of the "cor.mtest()" function from the
"corrplot" package. It uses the
[`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) function to
calculate pairwise p-values. Unlike the corrplot version, this only
calculates p-values, and not confidence intervals. Credit to corrplot
for this code, replicated here to avoid depending on their package for a
single function.

## Usage

``` r
get_pvals(x, ...)

# Default S3 method
get_pvals(x, ...)

# S3 method for class 'coin'
get_pvals(
  x,
  dset,
  iCodes = NULL,
  Level = NULL,
  uCodes = NULL,
  use_group = NULL,
  also_get = "none",
  ...
)

# S3 method for class 'unbalanced_coin'
get_pvals(
  x,
  dset,
  iCodes = NULL,
  Level = NULL,
  uCodes = NULL,
  use_group = NULL,
  also_get = "none",
  ...
)
```

## Arguments

- x:

  Object to analyse. For the default method this should be a numeric
  data frame or matrix.

- ...:

  Additional arguments passed to
  [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html), e.g.
  `conf.level = 0.95`.

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- iCodes:

  Optional indicator codes to retrieve. If `NULL` (default), returns all
  iCodes found in the selected data set. Can also refer to indicator
  groups.

- Level:

  Optionally, the level in the hierarchy to extract data from.

- uCodes:

  Optional unit codes to filter rows of the resulting data set. Can also
  be used in conjunction with groups.

- use_group:

  Optional grouping to filter rows of the data set. Specified as
  `list(Group_Var = Group)`.

- also_get:

  Character vector specifying any additional columns to attach to the
  data set that are not indicators or aggregates. Set
  `also_get = "none"` to return only numeric columns.

## Value

Matrix of p-values

## Details

The generic can operate on a numeric matrix/data frame directly, or on
the indicator data stored in a
[`coin`](https://bluefoxr.github.io/COINr/reference/new_coin.md) object.
See the method documentation for details on additional selection
arguments. When supplied with an
[`unbalanced_coin`](https://bluefoxr.github.io/COINr/reference/new_unbalanced_coin.md),
helper nodes inserted during construction are removed from both the
delegated data and the returned matrix so that only genuine indicators
and aggregates are represented.

## Examples

``` r
# a matrix of random numbers, 3 cols
x <- matrix(runif(30), 10, 3)

# get correlations between cols
cor(x)
#>             [,1]      [,2]        [,3]
#> [1,] 1.000000000 0.6951459 0.003308194
#> [2,] 0.695145917 1.0000000 0.284636766
#> [3,] 0.003308194 0.2846368 1.000000000

# get p values of correlations between cols
get_pvals(x)
#>            [,1]       [,2]      [,3]
#> [1,] 0.00000000 0.02564321 0.9927634
#> [2,] 0.02564321 0.00000000 0.4253975
#> [3,] 0.99276340 0.42539753 0.0000000
```
