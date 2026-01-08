# Normalise as distance to maximum value

A measure of the distance to the maximum value, where the maximum value
is the highest-scoring value. The formula used is:

## Usage

``` r
n_dist2max(x)
```

## Arguments

- x:

  A numeric vector

## Value

Numeric vector

## Details

\$\$ 1 - (x\_{max} - x)/(x\_{max} - x\_{min}) \$\$

This means that the closer a value is to the maximum, the higher its
score will be. Scores will be in the range of 0 to 1.

## Examples

``` r
x <- runif(20)
n_dist2max(x)
#>  [1] 0.094510425 0.617933419 0.824414019 0.096717454 0.000000000 0.557075330
#>  [7] 0.057017848 0.376518211 0.763938051 0.087660584 1.000000000 0.341112645
#> [13] 0.002907064 0.234430888 0.917434246 0.736796286 0.125338796 0.625833476
#> [19] 0.996893264 0.965379241
```
