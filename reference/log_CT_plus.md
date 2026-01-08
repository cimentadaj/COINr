# Log transform a vector (skew corrected)

Performs a log transform on a numeric vector, but with consideration for
the direction of the skew. The aim here is to reduce the absolute value
of skew, regardless of its direction.

## Usage

``` r
log_CT_plus(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A log-transformed vector of data, and treatment details wrapped in a
list.

## Details

Specifically:

If the skew of `x` is positive, this performs a modified "COIN Tool log"
transform: `log(x-min(x) + a)`, where `a <- 0.01*(max(x)-min(x))`.

If the skew of `x` is negative, it performs an equivalent transformation
`-log(xmax + a - x)`.

## Examples

``` r
x <- runif(20)
log_CT(x)
#> $x
#>  [1] -0.36058422 -0.28179187 -1.99766598 -0.05928368 -1.13396263 -3.51183107
#>  [7] -0.59245130 -0.37035650 -0.21003425 -2.52676442 -0.07342154 -1.23669351
#> [13] -1.17412037 -4.67440420 -1.61954941 -0.10499503 -1.78101132 -1.89676179
#> [19] -2.53713786 -1.64088518
#> 
#> $treated
#>  [1] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#>  [9] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#> [17] "log_CT" "log_CT" "log_CT" "log_CT"
#> 
```
