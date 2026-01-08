# Log-transform a vector

Performs a log transform on a numeric vector.

## Usage

``` r
log_CT(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A log-transformed vector of data, and treatment details wrapped in a
list.

## Details

Specifically, this performs a modified "COIN Tool log" transform:
`log(x-min(x) + a)`, where `a <- 0.01*(max(x)-min(x))`.

## Examples

``` r
x <- runif(20)
log_CT(x)
#> $x
#>  [1] -0.39040918 -3.26822467 -2.00268199 -0.55986201 -1.54392269 -4.66109271
#>  [7] -0.14442619 -0.04597219 -1.08986133 -1.94399488 -1.86509756 -1.97466401
#> [13] -0.48380035 -1.16667128 -0.43893621 -1.67744262 -0.21427860 -0.59280638
#> [19] -0.09223156 -1.10970161
#> 
#> $treated
#>  [1] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#>  [9] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#> [17] "log_CT" "log_CT" "log_CT" "log_CT"
#> 
```
