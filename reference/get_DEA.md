# Re-aggregate with DEA

Performs the aggregation in the last aggregation Level (Pillars to
Index) using the non-parametric flexible-weighting Benefit-if-the-Doubt
(BoD) model. This is an alternative aggregation technique used in
Auditing Composite Indices by CC-COIN. The function must take as a
minimum an input `coin` which is a coin-class object developed through
COINr package that has been aggregated and so includes a dataset called
"Aggregated". If no other parameters are specified, this will estimate
the conventional form of the BoD model, in which no additional
restrictions on the aggregation weights are placed. Such restrictions
can be placed through parameters inherited from
[`a_dea()`](https://bluefoxr.github.io/COINr/reference/a_dea.md), see
Details below.

## Usage

``` r
get_DEA(coin, level = NULL, wr_type = 0, wr_bounds = NULL, wr_rhs = NULL)

# S3 method for class 'coin'
get_DEA(coin, level = NULL, wr_type = 0, wr_bounds = NULL, wr_rhs = NULL)

# S3 method for class 'unbalanced_coin'
get_DEA(coin, level = NULL, wr_type = 0, wr_bounds = NULL, wr_rhs = NULL)
```

## Arguments

- coin:

  A coin-class object.

- level:

  The Level in which we wish to re-aggregate the coin using DEA.
  Defaults to the final aggregation Level. If specified as a lower
  level, then it will aggregate all indicators at that Level in a
  composite. See Details.

- wr_type:

  A single numeric value indicating the type of weight restrictions to
  include to the model. Can take the values: 0, 1, 2, 3, and 4. Defaults
  to 0 (no additional restrictions). See
  [`a_dea()`](https://bluefoxr.github.io/COINr/reference/a_dea.md) for
  further details.

- wr_bounds:

  A numeric vector or a matrix containing information on the weight
  restrictions. See
  [`a_dea()`](https://bluefoxr.github.io/COINr/reference/a_dea.md) for
  further details.

- wr_rhs:

  A numeric vector containing the values of the right-hand side of
  different forms of weight restrictions. To be used when `wr_type` is
  set to 1. Defaults to `NULL` which indicates a zero right hand side.
  See [`a_dea()`](https://bluefoxr.github.io/COINr/reference/a_dea.md)
  for further details.

## Value

A list containing the following:

- a data frame named `DEA_CI` containing the unit names, the Pillars to
  be aggregated, the nominal aggregated Index, and the DEA re-aggregated
  Index in a column named "Dea".

- a data frame `DEA_CI_ranks` containing the ranks of the units based on
  the DEA re-aggregated Index.

- a data frame named `dea_weights` containing the aggregation weights
  selected by each unit.

- a data frame named `normalised_weights`containing the average
  normalised weights. These are obtained by averaging across rows of the
  `dea_weights` data frame and then dividing each average term by the
  sum of the average terms.

## Examples

``` r
# example #1 (no additional weight restrictions)
# build example coin up to Aggregated dataset.
coin <- build_example_coin(up_to = "Aggregate")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Imputed
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Treated
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Aggregated
# re-aggregate Index
dea_reagg <- get_DEA(coin)

# example #2 (with additional weight restrictions, type 2 restrictions in the
# contribution of each Pillar to the index, max 75%, min 5%)
dea_reagg <- get_DEA(coin, wr_type = 2, wr_bounds = c(0.05, 0.75))

# example #3 (aggregation at a lower level)
# In the ASEM data, Level 3 is the final aggregation Level where the two Pillars
# get aggregated to ASEM. Specifying `level = 2` will aggregate the
# eight indicators (five under Connectivity Pillar and three under Syst Pillar)
# to an overall composite.
dea_reagg <- get_DEA(coin, level = 2, wr_type = 2, wr_bounds = c(0.05, 0.75))

```
