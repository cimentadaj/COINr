# Calculate Cronbach's Alpha

Calculates Cronbach's Alpha, given a data frame with all numeric columns

## Usage

``` r
cronbach_alpha(x)
```

## Arguments

- x:

  A data frame of indicator data, optionally with `uCode` column.

## Value

Cronbach alpha value

## Examples

``` r
# Calculate Cronbach's alpha for mtcars
cronbach_alpha(mtcars[, 1:4])
#> [1] 0.5015205
```
