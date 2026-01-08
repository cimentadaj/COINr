# Noisy replications of weights

Given a data frame of weights, this function returns multiple replicates
of the weights, with added noise. This is intended for use in
uncertainty and sensitivity analysis.

## Usage

``` r
get_noisy_weights(w, ...)

# S3 method for class 'data.frame'
get_noisy_weights(w, noise_specs, Nrep, ...)

# S3 method for class 'unbalanced_coin'
get_noisy_weights(w, noise_specs, Nrep, ...)
```

## Arguments

- w:

  A data frame of weights, in the format found in `.$Meta$Weights`.

- ...:

  Additional arguments passed to class-specific implementations.

- noise_specs:

  a data frame with columns:

  - `Level`: The aggregation level to apply noise to

  - `NoiseFactor`: The size of the perturbation: setting e.g. 0.2
    perturbs by +/- 20% of nominal values.

- Nrep:

  The number of weight replications to generate.

## Value

A list of `Nrep` sets of weights (data frames).

## Details

Weights are expected to be in a data frame format with columns `Level`,
`iCode` and `Weight`, as used in `iMeta`. Note that no `NA`s are allowed
anywhere in the data frame.

Noise is added using the `noise_specs` argument, which is specified by a
data frame with columns `Level` and `NoiseFactor`. The aggregation level
refers to number of the aggregation level to target while the
`NoiseFactor` refers to the size of the perturbation. If e.g. a row is
`Level = 1` and `NoiseFactor = 0.2`, this will allow the weights in
aggregation level 1 to deviate by +/- 20% of their nominal values (the
values in `w`).

This function replaces the now-defunct `noisyWeights()` from COINr \<
v1.0.

When the nominal weights come from an object inheriting from
`unbalanced_coin`, `get_noisy_weights()` delegates to the balanced
representation and removes any placeholder helper nodes from each
generated weight set so that only genuine indicators and aggregates are
present in the output.

## See also

- [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
  Perform global sensitivity or uncertainty analysis on a COIN

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# get nominal weights
w_nom <- coin$Meta$Weights$Original

# build data frame specifying the levels to apply the noise at
# here we vary at levels 2 and 3
noise_specs = data.frame(Level = c(2,3),
                         NoiseFactor = c(0.25, 0.25))

# get 100 replications
noisy_wts <- get_noisy_weights(w = w_nom, noise_specs = noise_specs, Nrep = 100)

# examine one of the noisy weight sets, last few rows
tail(noisy_wts[[1]])
#>       iCode Level    Weight
#> 55  Environ     2 1.0496991
#> 56   Social     2 1.2098610
#> 57 SusEcFin     2 1.2414120
#> 58     Conn     3 0.7689013
#> 59     Sust     3 1.0389687
#> 60    Index     4 1.0000000
```
