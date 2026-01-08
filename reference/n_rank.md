# Normalise using ranks

This is simply a wrapper for
[`base::rank()`](https://rdrr.io/r/base/rank.html). Higher scores will
give higher ranks.

## Usage

``` r
n_rank(x, ties.method = "min")
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
n_rank(x)
#>  [1]  9 12  5 13 15 20  4 16 19  7  3 14 10 11  2  6  1 17 18  8
```
