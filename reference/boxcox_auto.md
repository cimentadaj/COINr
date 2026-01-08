# Box-Cox transformation with automatic lambda selection

Selects the Box-Cox parameter that minimises the absolute skewness of
the transformed data over a supplied search range. The transform is then
applied to the full vector, with diagnostics on the pre- and
post-treatment skewness returned for logging.

## Usage

``` r
boxcox_auto(
  x,
  na.rm = FALSE,
  lambda_range = c(-3, 3),
  n_lambda = 61,
  makepos = TRUE
)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Logical: if `TRUE`, `NA`s are removed before estimating the lambda
  parameter. Default `FALSE`.

- lambda_range:

  Numeric vector of length 2 giving the lower and upper bounds for the
  search. Default `c(-3, 3)`.

- n_lambda:

  Integer giving the number of lambda values to evaluate between the
  bounds. Default `61`.

- makepos:

  Logical: passed to
  [`boxcox()`](https://bluefoxr.github.io/COINr/reference/boxcox.md) to
  ensure the data are positive prior to transformation. Default `TRUE`.

## Value

A list with the transformed vector (`x`), a character vector describing
the treatment applied (`treated`), and diagnostic entries for the
selected `lambda`, and skewness before and after transformation.

## Examples

``` r
set.seed(123)
x <- rexp(20, rate = 2)
boxcox_auto(x)
#> $x
#>  [1] 0.213699011 0.172028182 0.259122752 0.001209024 0.013170799 0.110504117
#>  [7] 0.109838229 0.051915285 0.307600712 0.000000000 0.232126103 0.152238432
#> [13] 0.099802371 0.127320527 0.068402809 0.214502503 0.272917520 0.151915697
#> [19] 0.174718962 0.321060565
#> 
#> $treated
#>  [1] "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto"
#>  [6] "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto"
#> [11] "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto"
#> [16] "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto" "boxcox_auto"
#> 
#> $lambda
#> [1] -3
#> 
#> $Skew_Before
#> [1] 2.290813
#> 
#> $Skew_After
#> [1] 0.04437077
#> 
```
