# Z-score a vector

Standardises a vector `x` by scaling it to have a mean and standard
deviation specified by `m_sd`.

## Usage

``` r
n_zscore(x, m_sd = c(0, 1))
```

## Arguments

- x:

  A numeric vector

- m_sd:

  A vector `c(m, sd)`, where `m` is desired mean and `sd` is the target
  standard deviation.

## Value

Numeric vector

## Details

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns `zscore_mean`, and `zscore_sd` to the
`iMeta` table, which specify the mean and standard deviation to scale
each indicator to, respectively. Then set `f_n_para = "use_iMeta"`
within the `global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
x <- runif(20)
n_zscore(x)
#>  [1]  1.04523622 -0.81344113 -1.41142724  0.97536512 -1.06511141  1.03877755
#>  [7] -0.07558681  0.19520290 -0.28127694 -0.06863074  0.11150150 -1.27783706
#> [13] -0.54614152 -0.82553794  1.55644895 -1.38231312  1.41446253  1.66747560
#> [19] -0.43559490  0.17842844
```
