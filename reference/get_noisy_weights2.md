# Noisy replications of weights

Given a data frame of weights, this function returns multiple replicates
of the weights, with added noise. This is intended for use in
uncertainty and sensitivity analysis. *NOTE:* this function is a
modified version of
[`get_noisy_weights()`](https://bluefoxr.github.io/COINr/reference/get_noisy_weights.md).

## Usage

``` r
get_noisy_weights2(
  w,
  noise_specs = NULL,
  individual_specs = NULL,
  Nrep,
  correct_uniform_dist = FALSE,
  uniform_tol = 0.01
)
```

## Arguments

- w:

  A data frame of weights, in the format found in `.$Meta$Weights`.

- noise_specs:

  a data frame with columns:

  - `Level`: The aggregation level to apply noise to

  - `NoiseFactor`: The size of the perturbation: setting e.g. 0.2
    perturbs by +/- 20% of nominal values.

- individual_specs:

  Optional list for specifying specific noise factors on individual
  indicators. A named list where names are the iCodes of
  indicators/aggregates, and the entries are the noise factors. This
  overrides the `noise_specs` specifications for those indicators (if
  there were any).

- Nrep:

  The number of weight replications to generate.

- correct_uniform_dist:

  Logical: if `TRUE`, calls `get_perturbed_weight_sample()` to generate
  more-uniform weight samples via rejection sampling.

- uniform_tol:

  Tolerance parameter passed to `get_perturbed_weight_sample()`.

## Value

A list of `Nrep` sets of weights (data frames).

## Details

Weights are expected to be in a data frame format with columns `Level`,
`iCode`, `Weight` and `Parent` as used in `iMeta`.

Noise is added using two arguments. The first is the `noise_specs`
argument, which is specified by a data frame with columns `Level` and
`NoiseFactor`. The aggregation level refers to number of the aggregation
level to target while the `NoiseFactor` refers to the size of the
perturbation. If e.g. a row is `Level = 1` and `NoiseFactor = 0.2`, this
will allow the weights in aggregation level 1 to deviate by +/- 20% of
their nominal values (the values in `w`). Note that due to rescaling to
sum to 1, this tends to result in truncated normal distributions for
each weight, unless `correct_uniform_dist = TRUE` (see below).

If you need more control over the noise applied to individual
indicators, use the `individual_specs` argument. Any noise
specifications here will override those in `noise_specs` if they are
specified in both. Keep in mind that if `correct_uniform_dist = TRUE`,
it is not possible to perturb only one weight within a group. This is
because it is impossible to vary a single value while keeping the sum
near 1.

Finally, the `correct_uniform_dist` argument allows to switch to use the
`get_perturbed_weight_sample()` function which returns weight samples
with more uniform distributions (as opposed to the truncated normal
distribution as mentioned previously).

See the examples vignette for some discussion and demos on the
distributions of weights.

## See also

- [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
  Perform global sensitivity or uncertainty analysis on a COIN

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# get nominal weights
w_nom <- coin$Meta$Ind[coin$Meta$Ind$Type %in% c("Indicator", "Aggregate"),
                       c("iCode", "Weight", "Level", "Parent")]

# build data frame specifying the levels to apply the noise at
# here we vary at levels 2 and 3
noise_specs = data.frame(Level = c(2,3),
                         NoiseFactor = c(0.25, 0.25))

# get 100 replications
noisy_wts <- get_noisy_weights2(w = w_nom, noise_specs = noise_specs, Nrep = 100)

# examine one of the noisy weight sets, last few rows
tail(noisy_wts[[1]])
#>       iCode    Weight Level Parent pert_by
#> 55  Environ 0.2857691     2   Sust    0.25
#> 56   Social 0.3378470     2   Sust    0.25
#> 57 SusEcFin 0.3763839     2   Sust    0.25
#> 58     Conn 0.4430384     3  Index    0.25
#> 59     Sust 0.5569616     3  Index    0.25
#> 60    Index 1.0000000     4   <NA>    0.00

## Example with individual specs for individual components

# specify for two components
individual_specs <- list(Physical = 1, P2P = 0.75)
# run
noisy_wts <- get_noisy_weights2(w = w_nom, noise_specs = noise_specs,
                                individual_specs = individual_specs, Nrep = 100)
# Note that the individual specs override the general specs.

## Example specifying on whole groups (with helper function)
# We want 25% noise on pillars in connectivity group,
# and 50% noise on pillars in sustainability group.

# First find iCodes in those groups
p_conn <- get_iCodes_in_group(coin, "Conn", 2)
p_sust <- get_iCodes_in_group(coin, "Sust", 2)

# make list: the values first
individual_specs <- c(rep(0.25, length(p_conn)), rep(0.5, length(p_sust))) |>
  as.list()
# add the names
names(individual_specs) <- c(p_conn, p_sust)

# now run...
noisy_wts <- get_noisy_weights2(w = w_nom, noise_specs = noise_specs,
                                individual_specs = individual_specs, Nrep = 100)
```
