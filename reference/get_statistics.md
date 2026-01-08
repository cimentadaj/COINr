# Get statistics for multivariate analysis

Retrieve main statistics used to evaluate the consistency of an
aggregate through multivariate analysis including PCA, rotated PCA, and
Cronbach's alpha.

## Usage

``` r
get_statistics(
  coin,
  dset = "Aggregated",
  level,
  exclude_corr = 0.3,
  warnings = TRUE,
  verbose = TRUE
)
```

## Arguments

- coin:

  A coin-class object.

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- level:

  The aggregation levels where the statistics are computed.

- exclude_corr:

  Reference value used to exclude potential combinations of pairs of
  indicators that have a lower pearson correlation coefficient than the
  one provided. The default value is equal to 0.3.

- warnings:

  A logical value. If `TRUE` information about the inconsistencies is
  printed.

- verbose:

  A logical value. If `TRUE` information on the number of iterations is
  printed.

## Value

A list with two components:

- `Statistics`: A data frame containing the main statistics for each
  dimension.

- `Correlations`: A list containing the correlation matrices computed by
  dimension using the Pearson's correlation coefficient.

## Details

Principle Component Analysis (PCA) is performed using the function
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html), note that rows
with missing values will be removed before running the PCA.

Afterwards a rotated PCA is computed using the function
[`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html). A
"varimax" rotation is used on as many components as selected latent
dimensions, based on the number of eigenvalues above 1. The output of
the analysis is summarised in the following way:

- `rotated_loadings_threshold`: A logical value. If `TRUE` all
  indicators are loaded with more than 0.5 in a single component.

- `rotated_loadings_direction`: A logical value. If `TRUE` all loadings
  in the same component have the same sign.

- `rotated_items`: A character where the name of the indicators with
  loadings above 0.5 are pasted separating each rotated component by ' /
  '. It is useful when having more than two latent dimensions to
  identify in which component each indicator is loaded.

## Examples

``` r
## Build example up to aggregate data set
coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

## Run function:
statistics <- get_statistics(coin, "Aggregated", level = c(2, 3))
```
