# Log-transform a vector

Performs a log transform on a numeric vector. This function is currently
not recommended - see comments below.

## Usage

``` r
log_GII(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A log-transformed vector of data.

## Details

Specifically, this performs a "GII log" transform, which is what was
encoded in the GII2020 spreadsheet.

Note that this transformation is currently NOT recommended because it
seems quite volatile and can flip the direction of the indicator. If the
maximum value of the indicator is less than one, this reverses the
direction.

## Examples

``` r
x <- runif(20)
log_GII(x)
#> $x
#>  [1] 0.452157147 0.649661256 0.162128859 0.157761326 0.972208260 0.951731103
#>  [7] 0.968805769 0.885208836 0.686521530 0.182236390 0.652949228 0.322341057
#> [13] 0.957047813 0.400664274 0.919013803 0.622355981 0.007870568 0.838253056
#> [19] 0.885658464 0.057809380
#> 
#> $treated
#>  [1] "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII"
#>  [8] "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII"
#> [15] "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII"
#> 
```
