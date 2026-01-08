# Normalise as fraction of max value

The ratio of each value of `x` to `max(x)`.

## Usage

``` r
n_fracmax(x)
```

## Arguments

- x:

  A numeric vector

## Value

Numeric vector

## Details

\$\$ x / x\_{max} \$\$

## Examples

``` r
x <- runif(20)
n_fracmax(x)
#>  [1] 0.06462962 0.81228323 0.80936083 0.24417130 0.46331480 0.36528360
#>  [7] 0.73437656 0.28279428 0.99766300 0.81597462 0.12348512 0.39223982
#> [13] 0.62109050 0.35605129 1.00000000 0.52573674 0.31374998 0.38986099
#> [19] 0.62335579 0.55293641
```
