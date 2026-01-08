# Minmax a vector

Scales a vector using min-max method.

## Usage

``` r
n_minmax(x, l_u = c(0, 100))
```

## Arguments

- x:

  A numeric vector

- l_u:

  A vector `c(l, u)`, where `l` is the lower bound and `u` is the upper
  bound. `x` will be scaled exactly onto this interval.

## Value

Normalised vector

## Details

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns `minmax_lower`, and `minmax_upper` to
the `iMeta` table, which specify the lower and upper bounds to scale
each indicator to. Then set `f_n_para = "use_iMeta"` within the
`global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
x <- runif(20)
n_minmax(x)
#>  [1]  98.1460999   0.8810694  81.9536087 100.0000000  48.0626828  94.6371675
#>  [7]  44.6693781  81.6032029  13.0381163  90.0099198  99.6920681  21.4226650
#> [13]   0.0000000  87.2268184  42.8117375  17.3478731   2.5418467   3.4885061
#> [19]  82.4419342  28.5387613
```
