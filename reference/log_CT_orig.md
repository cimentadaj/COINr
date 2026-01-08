# Log-transform a vector

Performs a log transform on a numeric vector.

## Usage

``` r
log_CT_orig(x, na.rm = FALSE)
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

Specifically, this performs a "COIN Tool log" transform:
`log(x-min(x) + 1)`.

## Examples

``` r
x <- runif(20)
log_CT_orig(x)
#> $x
#>  [1] 0.3064986 0.6526017 0.2062683 0.5783504 0.1885428 0.0000000 0.6166365
#>  [8] 0.4390350 0.3190297 0.6319781 0.4411185 0.1509122 0.4741896 0.6728320
#> [15] 0.3160487 0.6381194 0.6277480 0.2828770 0.1055535 0.6796330
#> 
#> $treated
#>  [1] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#>  [6] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#> [11] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#> [16] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#> 
```
