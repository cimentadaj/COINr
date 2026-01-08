# Scale a vector

Scales a vector for normalisation using the method applied in the
GII2020 for some indicators. This does
`x_scaled <- (x-l)/(u-l) * scale_factor`. Note this is *not* the minmax
transformation (see
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)).
This is a linear transformation with shift `u` and scaling factor `u-l`.

## Usage

``` r
n_scaled(x, npara = c(0, 100), scale_factor = 100)
```

## Arguments

- x:

  A numeric vector

- npara:

  Parameters as a vector `c(l, u)`. See description.

- scale_factor:

  Optional scaling factor to apply to the result. Default 100.

## Value

Scaled vector

## Details

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns `scaled_lower`, `scaled_upper` and
`scale_factor` to the `iMeta` table, which specify the first and second
elements of `npara`, respectively. Then set `f_n_para = "use_iMeta"`
within the `global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
x <- runif(20)
n_scaled(x, npara = c(1,10))
#>  [1]  -4.3101529  -8.5677561  -1.6583907 -10.0306747  -2.0818316  -3.6184695
#>  [7]  -6.2301507  -6.5456580  -2.7528974  -3.1872977  -8.4769380  -8.5932521
#> [13] -10.8629583  -3.9449695  -4.3684706  -0.1673328  -2.0081993  -4.7004185
#> [19]  -1.8662041  -5.7118004
```
