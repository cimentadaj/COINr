# Interpolate time-indexed data frame

Given a numeric data frame `Y` with rows indexed by a time vector `tt`,
interpolates at time values specified by the vector `tt_est`. If
`tt_est` is not in `tt`, will create new rows in the data frame
corresponding to these interpolated points.

## Usage

``` r
approx_df(Y, tt, tt_est = NULL, ...)
```

## Arguments

- Y:

  A data frame with all numeric columns

- tt:

  A time vector with length equal to `nrow(Y)`, indexing the rows in
  `Y`.

- tt_est:

  A time vector of points to interpolate in `Y`. If `NULL`, will attempt
  to interpolate all points in `Y` (you may need to adjust the `rule`
  argument of
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) here).
  Note that points not specified in `tt_est` will not be interpolated.
  `tt_est` does not need to be a subset of `tt`.

- ...:

  Further arguments to pass to
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) other than
  `x`, `y` and `xout`.

## Value

A list with:

- `.$tt` the vector of time points, including time values of
  interpolated points

- `.$Y` the corresponding interpolated data frame

Both outputs are sorted by `tt`.

## Details

This is a wrapper for
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html), with some
differences. In the first place,
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) is applied
to each column of `Y`, using `tt` each time as the corresponding time
vector indexing `Y`. Interpolated values are generated at points
specified in `tt_est` but these are appended to the existing data
(whereas [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)
will only return the interpolated points and nothing else). Further
arguments to [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)
can be passed using the `...` argument.

## Examples

``` r
# a time vector
tt <- 2011:2020

# two random vectors with some missing values
y1 <- runif(10)
y2 <- runif(10)
y1[2] <- y1[5] <- NA
y2[3] <- y2[5] <- NA
# make into df
Y <- data.frame(y1, y2)

# interpolate for time = 2012
Y_int <- approx_df(Y, tt, 2012)
Y_int$Y
#>            y1        y2
#> 1  0.10550700 0.6364719
#> 2  0.24287895 0.6112205
#> 3  0.38025089        NA
#> 4  0.23077194 0.2587537
#> 5          NA        NA
#> 6  0.11956036 0.0209384
#> 7  0.98528207 0.7948596
#> 8  0.04807715 0.7414841
#> 9  0.04710344 0.7480149
#> 10 0.80947861 0.5289729

# notice Y_int$y2 is unchanged since at 2012 it did not have NA value
stopifnot(identical(Y_int$Y$y2, y2))

# interpolate at value not in tt
approx_df(Y, tt, 2015.5)
#> $tt
#>  [1] 2011.0 2012.0 2013.0 2014.0 2015.0 2015.5 2016.0 2017.0 2018.0 2019.0
#> [11] 2020.0
#> 
#> $Y
#>            y1         y2
#> 1  0.10550700 0.63647187
#> 2          NA 0.61122047
#> 3  0.38025089         NA
#> 4  0.23077194 0.25875375
#> 5          NA         NA
#> 6  0.14736325 0.08039224
#> 7  0.11956036 0.02093840
#> 8  0.98528207 0.79485958
#> 9  0.04807715 0.74148408
#> 10 0.04710344 0.74801486
#> 11 0.80947861 0.52897290
#> 
```
