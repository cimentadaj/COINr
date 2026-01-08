# Sensitivity and uncertainty analysis of a coin (version 2)

This function performs global sensitivity and uncertainty analysis of a
coin. You must specify which parameters of the coin to vary, and the
alternatives/distributions for those parameters. This function is a
modified version of
[`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
(branched Nov 2023). See there for the general documentation. In the
Details below we focus on the modifications that are different from the
COINr version.

## Usage

``` r
get_sensitivity2(
  coin,
  SA_specs,
  N,
  SA_type = "UA",
  dset,
  iCode,
  Nboot = NULL,
  quietly = FALSE,
  check_addresses = TRUE,
  bump_geometric_mean = FALSE,
  report_progress = "bar",
  monitor_convergence = TRUE,
  converge_on = NULL
)
```

## Arguments

- coin:

  A coin

- SA_specs:

  Specifications of the input uncertainties

- N:

  The number of regenerations

- SA_type:

  The type of analysis to run. `"UA"` runs an uncertainty analysis.
  `"SA"` runs a sensitivity analysis (which anyway includes an
  uncertainty analysis).

- dset:

  The data set to extract the target variable from (passed to
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)).

- iCode:

  The variable within `dset` to use as the target variable (passed to
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)).

- Nboot:

  Number of bootstrap samples to take when estimating confidence
  intervals on sensitivity indices.

- quietly:

  Set to `TRUE` to suppress progress messages.

- check_addresses:

  Logical: if `FALSE` skips the check of the validity of the parameter
  addresses. Default `TRUE`, but useful to set to `FALSE` if running
  this e.g. in a Rmd document (because may require user input).

- bump_geometric_mean:

  Logical: if `TRUE` will automatically deal with zero and negative
  values entering the geometric mean by ensuring that the minimum value
  of each indicator is set to 1% of its range. NOTE: this argument is
  currently inactive until we decide if and how to implement this.

- report_progress:

  Controls the console output if `quietly = FALSE`. Either set to
  `"bar"` to see a progress bar, or `"text"` to see text with a little
  more detail (although the latter will fill up the console).

- monitor_convergence:

  Logical: if `TRUE`, at every fifth iteration an estimation of the
  estimation error is calculated via bootstrapping. This is recorded in
  the output list.

- converge_on:

  Optional parameter which specifies convergence. If specified, when the
  bootstrapped error is less than `converge_on`, further iterations will
  be cancelled. Set to e.g. 0.01 which roughly translates as requiring
  1% error or less in rank estimations.

## Value

Sensitivity analysis results as a list, containing:

- `.$Scores` a data frame with a row for each unit, and columns are the
  scores for each replication.

- `.$Ranks` as `.$Scores` but for unit ranks

- `.$RankStats` summary statistics for ranks of each unit

- `.$Para` a list containing parameter values for each run

- `.$Nominal` the nominal scores and ranks of each unit (i.e. from the
  original COIN)

- `.$Sensitivity` (only if `SA_type = "SA"`) sensitivity indices for
  each parameter. Also confidence intervals if `Nboot` was specified.

- `.$est_err` vector of estimated error at every fifth iteration (only
  if `monitor_convergence = TRUE`)

- Some information on the time elapsed, average time, and the parameters
  perturbed.

- Depending on the setting of `store_results`, may also contain a list
  of Methods or a list of COINs for each replication.

## Details

This version also includes the possibility to switch between text or
progress bar output to the console using the `report_progress` argument.

## Examples

``` r
# for examples, see `vignette("sensitivity")`
# (this is because package examples are run automatically and this function can
# take a few minutes to run at realistic settings)
```
