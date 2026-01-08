# Normalise using Borda scores

Calculates Borda scores as `rank(x) - 1`.

## Usage

``` r
n_borda(x, ties.method = "min")
```

## Arguments

- x:

  A numeric vector

- ties.method:

  This argument is passed to
  [`base::rank()`](https://rdrr.io/r/base/rank.html) - see there for
  details.

## Value

Numeric vector

## Examples

``` r
x <- runif(20)
n_borda(x)
#>  [1]  6 12 15 14 19  3 10 13  7  5  8  9 17  4 16 18  0 11  2  1
```
