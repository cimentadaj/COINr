# Round down a data frame

Tiny function just to round down a data frame for display in a table,
ignoring non-numeric columns.

## Usage

``` r
round_df(df, decimals = 2)
```

## Arguments

- df:

  A data frame to input

- decimals:

  The number of decimal places to round to (default 2)

## Value

A data frame, with any numeric columns rounded to the specified amount.

## Details

This function replaces the now-defunct `roundDF()` from COINr \< v1.0.

## Examples

``` r
round_df( as.data.frame(matrix(runif(20),10,2)), decimals = 3)
#>       V1    V2
#> 1  0.989 0.716
#> 2  0.853 0.236
#> 3  0.832 0.651
#> 4  0.402 0.248
#> 5  0.818 0.554
#> 6  0.703 0.146
#> 7  0.033 0.827
#> 8  0.673 0.112
#> 9  0.624 0.010
#> 10 0.574 0.191
```
