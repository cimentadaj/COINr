# Quick normalisation of a coin

This is a wrapper function for
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md),
which offers a simpler syntax but less flexibility. It normalises a data
set within a coin using a specified function `f_n` which is used to
normalise each indicator, with additional function arguments passed by
`f_n_para`. By default, `f_n = "n_minmax"` and `f_n_para` is set so that
the indicators are normalised using the min-max method, between 0 and
100.

## Usage

``` r
# S3 method for class 'coin'
qNormalise(
  x,
  dset,
  f_n = "n_minmax",
  f_n_para = list(l_u = c(0, 100)),
  directions = NULL,
  out2 = "coin",
  ...
)

# S3 method for class 'unbalanced_coin'
qNormalise(
  x,
  dset,
  f_n = "n_minmax",
  f_n_para = list(l_u = c(0, 100)),
  directions = NULL,
  out2 = "unbalanced_coin",
  ...
)
```

## Arguments

- x:

  For `qNormalise.coin()`, a coin object; for
  `qNormalise.unbalanced_coin()` an `unbalanced_coin` object.

- dset:

  Name of data set to normalise

- f_n:

  Name of a normalisation function (as a string) to apply to each
  indicator. Default `"n_minmax"`.

- f_n_para:

  Any further arguments to pass to `f_n`, as a named list.

- directions:

  An optional data frame containing the following columns:

  - `iCode` The indicator code, corresponding to the column names of the
    data frame

  - `Direction` numeric vector with entries either `-1` or `1` If
    `directions` is not specified, the directions will be taken from the
    `iMeta` table in the coin, if available.

- out2:

  For `qNormalise.coin()`, either `"coin"` (default) to return the
  updated coin or `"df"` to return a data frame. For
  `qNormalise.unbalanced_coin()`, either `"unbalanced_coin"` (default)
  to retain the class or `"df"` for a data frame output; the value
  `"coin"` is not permitted because it would drop the unbalanced
  metadata.

- ...:

  arguments passed to or from other methods.

## Value

A coin (or `unbalanced_coin`) with normalised data set.

## Details

Essentially, this function is similar to
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
but brings parameters into the function arguments rather than being
wrapped in a list. It also does not allow individual normalisation.

See
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
documentation for more details, and
[`vignette("normalise")`](https://bluefoxr.github.io/COINr/articles/normalise.md).

Compared with `qNormalise.coin()`, this method defaults `out2` to
`"unbalanced_coin"` and rejects `"coin"`, ensuring placeholder columns
are stripped from any outward-facing data.

## Functions

- `qNormalise(unbalanced_coin)`: Wrapper that keeps the unbalanced
  hierarchy while applying the quick normalisation defaults.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# normalise raw data set using min max, but change to scale 1-10
coin <- qNormalise(coin, dset = "Raw", f_n = "n_minmax",
                   f_n_para = list(l_u = c(1,10)))
#> Written data set to .$Data$Normalised
```
