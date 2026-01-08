# Round a data frame to specified significant figures

Tiny function just to round down a data frame by significant figures for
display in a table, ignoring non-numeric columns.

## Usage

``` r
signif_df(df, digits = 3)
```

## Arguments

- df:

  A data frame to input

- digits:

  The number of decimal places to round to (default 3)

## Value

A data frame, with any numeric columns rounded to the specified amount.

## Examples

``` r
signif_df( as.data.frame(matrix(runif(20),10,2)), digits = 3)
#>       V1    V2
#> 1  0.806 0.851
#> 2  0.648 0.578
#> 3  0.819 0.272
#> 4  0.862 0.441
#> 5  0.240 0.652
#> 6  0.259 0.463
#> 7  0.339 0.895
#> 8  0.264 0.834
#> 9  0.989 0.200
#> 10 0.336 0.917

```
