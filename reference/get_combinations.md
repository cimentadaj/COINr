# Test indicator combinations for composite indicators

Performs exhaustive analysis of indicator combinations within specified
dimensions to identify optimal subsets based on multiple statistical
criteria including PCA variance, Cronbach's alpha, and correlation
patterns. This function systematically tests all possible combinations
within specified size ranges and evaluates their statistical properties.

## Usage

``` r
get_combinations(coin, ...)

# S3 method for class 'coin'
get_combinations(
  coin,
  dset,
  dimension = NULL,
  f_ag = "a_amean",
  exclude_corr = 0.3,
  add_elements = NULL,
  drop_elements = NULL,
  global_min_max = NULL,
  indiv_min_max = NULL,
  PCA_ref = 0.65,
  cronbach_alpha_ref = 0.7,
  warnings = TRUE,
  verbose = TRUE,
  ...
)

# S3 method for class 'unbalanced_coin'
get_combinations(
  coin,
  dset,
  dimension = NULL,
  f_ag = "a_amean",
  exclude_corr = 0.3,
  add_elements = NULL,
  drop_elements = NULL,
  global_min_max = NULL,
  indiv_min_max = NULL,
  PCA_ref = 0.65,
  cronbach_alpha_ref = 0.7,
  warnings = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- coin:

  A coin or unbalanced_coin object

- ...:

  Additional arguments (currently unused, for S3 method compatibility)

- dset:

  Name of the data set to use within the coin (e.g., "Raw",
  "Normalised")

- dimension:

  Character vector of level 2 aggregate names (parents) to analyze.
  These should correspond to dimension/pillar names in your framework.

- f_ag:

  Aggregation function to use. Default is `'a_amean'` (arithmetic mean).
  Any aggregation function available in COINr can be used.

- exclude_corr:

  Numeric threshold (between -1 and 1) for excluding combinations.
  Combinations containing indicator pairs with correlation below this
  threshold are excluded. Default is 0.3. Set to -1 to include all
  combinations regardless of correlation.

- add_elements:

  Optional named list specifying indicators to add to specific
  dimensions. List names should match dimension names, and values should
  be character vectors of indicator codes. Example:
  `list('Physical' = c("Ind1", "Ind2"))`.

- drop_elements:

  Optional named list specifying indicators to remove from specific
  dimensions. Structure is identical to `add_elements`.

- global_min_max:

  Numeric vector of length 2: `c(min, max)` specifying the minimum and
  maximum number of indicators to include in combinations across all
  dimensions. Overridden by `indiv_min_max` if specified for a
  dimension. If NULL, defaults to `c(2, n)` where n is the number of
  available indicators.

- indiv_min_max:

  Optional named list with dimension-specific min/max ranges. Example:
  `list('Physical' = c(3, 6), 'Institutional' = c(2, 5))`. Overrides
  `global_min_max` for specified dimensions.

- PCA_ref:

  Numeric threshold (0-1) for minimum variance explained by first
  principal component. Combinations must meet this threshold to be
  classified as successful. Default is 0.65 (65%).

- cronbach_alpha_ref:

  Numeric threshold (0-1) for minimum Cronbach's alpha value.
  Combinations must meet this threshold to be classified as successful.
  Default is 0.7.

- warnings:

  Logical: if TRUE (default), displays warning messages for invalid
  arguments or missing indicators.

- verbose:

  Logical: if TRUE (default), displays progress information during
  computation including dimension names and combination counts.

## Value

A list with four components:

- Info:

  Data frame summarizing results for each dimension: number of
  combinations tested, number of successful combinations, min/max sizes,
  original and current indicator counts, and numbers of added/dropped
  indicators.

- Combinations:

  Named list (by dimension) of data frames containing detailed
  statistics for ALL tested combinations. Each data frame includes:
  combination ID, number of indicators, indicator codes, correlation
  statistics (mean, sd, min, max), PCA variance, eigenvalue count,
  Cronbach's alpha, rotated PCA results, and suggested additional
  indicators.

- Successful:

  Named list (by dimension) of data frames containing only combinations
  that meet all success criteria (subset of Combinations).

- Correlations:

  Named list (by dimension) containing three correlation matrices: 'All'
  (full correlation matrix), 'Low' (correlations below `exclude_corr`),
  and 'High' (correlations above 0.92).

## Details

The function generates and evaluates all possible combinations of
indicators within each specified dimension. For each combination, it:

- Aggregates indicators using the specified aggregation function

- Computes correlations between indicators and the aggregate

- Performs non-rotated PCA to assess dimensionality

- Performs rotated PCA (varimax) to examine loading patterns

- Calculates Cronbach's alpha for internal consistency

- Identifies suggested additional indicators that could strengthen the
  combination

Combinations are classified as "successful" if they meet all three
criteria:

- First principal component explains ≥ `PCA_ref` of variance (default
  65%)

- Fewer than 2 eigenvalues \> 1 (suggesting unidimensionality)

- Cronbach's alpha ≥ `cronbach_alpha_ref` (default 0.7)

**Computational Complexity Warning:** The number of combinations grows
exponentially with the number of indicators. For n indicators tested
across all sizes from 2 to n:

- 5 indicators: 26 combinations

- 10 indicators: 1,013 combinations

- 15 indicators: 32,752 combinations

- 20 indicators: 1,048,555 combinations

Each combination requires aggregation, correlation computation, two PCA
analyses, and Cronbach's alpha calculation. For large indicator sets
(\>12), consider using `global_min_max` or `indiv_min_max` to restrict
the search space.

**Interpretation Guide:**

- **PCA variance:** Higher is better. Values ≥0.65 suggest indicators
  share a common underlying dimension.

- **Eigenvalues:** Fewer eigenvalues \>1 indicates better
  unidimensionality. Ideally only 1 eigenvalue \>1.

- **Cronbach's alpha:** 0.7-0.8 = acceptable, 0.8-0.9 = good, \>0.9 =
  excellent (but may indicate redundancy).

- **Suggested indicators:** Indicators not in the combination that
  correlate more strongly with the aggregate than the weakest indicator
  in the combination.

## Methods (by class)

- `get_combinations(coin)`: Get indicator combinations for coin class

- `get_combinations(unbalanced_coin)`: Get indicator combinations for
  unbalanced_coin class

## See also

[`get_cronbach`](https://bluefoxr.github.io/COINr/reference/get_cronbach.md),
[`get_PCA`](https://bluefoxr.github.io/COINr/reference/get_PCA.md),
[`Aggregate`](https://bluefoxr.github.io/COINr/reference/Aggregate.md)

## Examples

``` r
# Build example coin
coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

# Test combinations for two dimensions
results <- get_combinations(
  coin = coin,
  dset = "Normalised",
  dimension = c("Physical", "Political"),
  PCA_ref = 0.65,
  cronbach_alpha_ref = 0.7,
  exclude_corr = 0.3,
  verbose = FALSE
)
#> Warning: The argument "global_min_max" is not well defined. Minimum and maximum values will be adjusted based on the number of indicators
#> Warning: The arguments of "indiv_min_max" for Physical is not well defined
#> Warning: The arguments of "indiv_min_max" for Political is not well defined

# View summary information
print(results$Info)
#>   dimension combinations successful min max original_length current_length
#> 1  Physical           18          5   2   8               8              8
#> 2 Political            2          2   2   3               3              3
#>   added_indic dropped_indic
#> 1           0             0
#> 2           0             0

# View successful combinations for Physical dimension
if (!is.null(results$Successful$Physical)) {
  head(results$Successful$Physical)
}
#>    ID dimension num_Indcs original           indicators         corr_values
#> 5   5  Physical         2        0           Cov4G; LPI        0.932; 0.885
#> 8   8  Physical         2        0           Bord; Elec         0.92; 0.847
#> 13 13  Physical         3        0  Cov4G; LPI; Flights 0.889; 0.836; 0.724
#> 14 14  Physical         3        0     Cov4G; LPI; Bord 0.847; 0.821; 0.746
#> 15 15  Physical         3        0 Cov4G; LPI; ConSpeed 0.894; 0.853; 0.708
#>         mean         sd   min   max negative_corr       PCA PCA_NAs eigen_dim
#> 5  0.9085000 0.03323402 0.885 0.932             0 0.8283619       0         1
#> 8  0.8835000 0.05161880 0.847 0.920             0 0.7853343       0         1
#> 13 0.8163333 0.08423974 0.724 0.889             0 0.6718072       0         1
#> 14 0.8046667 0.05244362 0.746 0.847             0 0.6530301       0         1
#> 15 0.8183333 0.09772581 0.708 0.894             0 0.6777811       0         1
#>     cronbach rotated_loadings_threshold rotated_loadings_direction
#> 5  0.7776198                       TRUE                       TRUE
#> 8  0.7054998                       TRUE                       TRUE
#> 13 0.7496434                       TRUE                       TRUE
#> 14 0.7164560                       TRUE                       TRUE
#> 15 0.7518345                       TRUE                       TRUE
#>           rotated_indic           suggested_indc        corr_suggested_indc
#> 5            LPI; Cov4G                                                    
#> 8            Bord; Elec                                                    
#> 13  LPI; Flights; Cov4G ForPort; Lang; NGOs; CPI 0.753; 0.742; 0.746; 0.864
#> 14     LPI; Bord; Cov4G                      CPI                      0.786
#> 15 LPI; ConSpeed; Cov4G               CPI; RDExp                0.843; 0.77

# Restrict combination size globally
results2 <- get_combinations(
  coin = coin,
  dset = "Normalised",
  dimension = "Physical",
  global_min_max = c(3, 5),
  verbose = FALSE
)
#> Warning: The arguments of "indiv_min_max" for Physical is not well defined
```
