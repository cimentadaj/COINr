# Normalise as distance to reference value

A measure of the distance to a specific value found in `x`, specified by
`iref`. The formula is:

## Usage

``` r
n_dist2ref(x, iref, cap_max = FALSE)
```

## Arguments

- x:

  A numeric vector

- iref:

  An integer which indexes `x` to specify the reference value. The
  reference value will be `x[iref]`.

- cap_max:

  If `TRUE`, any value of `x` that exceeds `x[iref]` will be assigned a
  score of 1, otherwise will have a score greater than 1.

## Value

Numeric vector

## Details

\$\$ 1 - (x\_{ref} - x)/(x\_{ref} - x\_{min}) \$\$

Values exceeding `x_ref` can be optionally capped at 1 if
`cap_max = TRUE`.

## Examples

``` r
x <- runif(20)
n_dist2ref(x, 5)
#>  [1] 0.25466444 0.44573892 0.90114425 0.54892468 1.00000000 0.66592951
#>  [7] 1.21524009 0.53013982 0.83822955 0.27781970 0.22930768 0.17137098
#> [13] 0.00000000 1.22472689 0.79118773 0.55920931 0.52528715 0.34460709
#> [19] 0.06459408 0.74351868
```
