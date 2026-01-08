# Estimate sensitivity indices

Post process a sample to obtain sensitivity indices. This function takes
a univariate output which is generated as a result of running a Monte
Carlo sample from
[`SA_sample()`](https://bluefoxr.github.io/COINr/reference/SA_sample.md)
through a system. Then it estimates sensitivity indices using this
sample.

## Usage

``` r
SA_estimate(yy, N, d, Nboot = NULL)
```

## Arguments

- yy:

  A vector of model output values, as a result of a \\N(d+2)\\ Monte
  Carlo design.

- N:

  The number of sample points per dimension.

- d:

  The dimensionality of the sample

- Nboot:

  Number of bootstrap draws for estimates of confidence intervals on
  sensitivity indices. If this is not specified, bootstrapping is not
  applied.

## Value

A list with the output variance, plus a data frame of first order and
total order sensitivity indices for each variable, as well as
bootstrapped confidence intervals if `!is.null(Nboot)`.

## Details

This function is built to be used inside
[`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md).

## See also

- [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
  Perform global sensitivity or uncertainty analysis on a COIN

- [`SA_sample()`](https://bluefoxr.github.io/COINr/reference/SA_sample.md)
  Input design for estimating sensitivity indices

## Examples

``` r
# This is a generic example rather than applied to a COIN (for reasons of speed)

# A simple test function
testfunc <- function(x){
x[1] + 2*x[2] + 3*x[3]
}

# First, generate a sample
X <- SA_sample(500, 3)

# Run sample through test function to get corresponding output for each row
y <- apply(X, 1, testfunc)

# Estimate sensitivity indices using sample
SAinds <- SA_estimate(y, N = 500, d = 3, Nboot = 1000)
SAinds$SensInd
#>   Variable          Si        STi       Si_q5     Si_q95     STi_q5    STi_q95
#> 1       V1 -0.01032695 0.08003633 -0.09978119 0.08616881 0.07285386 0.08687806
#> 2       V2  0.12742073 0.30607576 -0.04163174 0.31081223 0.28102163 0.33455907
#> 3       V3  0.78195327 0.65318772  0.52818851 1.04511170 0.59853210 0.70725144
# Notice that total order indices have narrower confidence intervals than first order.
```
