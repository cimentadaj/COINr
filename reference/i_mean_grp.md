# Impute by group mean

Replaces `NA`s in a numeric vector with the grouped arithmetic means of
the non-`NA` values. Groups are defined by the `f` argument.

## Usage

``` r
i_mean_grp(x, f, skip_f_na = TRUE)
```

## Arguments

- x:

  A numeric vector

- f:

  A grouping variable, of the same length of `x`, that specifies the
  group that each value of `x` belongs to. This will be coerced to a
  factor.

- skip_f_na:

  If `TRUE`, will work around any `NA`s in `f` (the corresponding values
  of `x` will be excluded from the imputation and returned unaltered).
  Else if `FALSE`, will cause an error.

## Value

A numeric vector

## Examples

``` r
x <- c(NA, runif(10), NA)
f <- c(rep("a", 6), rep("b", 6))
i_mean_grp(x, f)
#>  [1] 0.6962037 0.9607643 0.5003032 0.8443124 0.5897470 0.5858916 0.3645047
#>  [8] 0.2926380 0.7873800 0.7203784 0.7549835 0.5839769
```
