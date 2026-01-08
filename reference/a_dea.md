# Aggregate a data frame using DEA

Aggregates a data frame of indicator values into a single column using
the Benefit of the Doubt (BoD) model. Uses the data frame of indicator
values for all units an input, so `by_df` need to be set to `TRUE` in
the parameters of
[`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md).
The function does not accept weights from iMeta but generates a
unit-specific a vector of weights, so `w` needs to be set to `"none"` in
the parameters of
[`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md).

## Usage

``` r
a_dea(
  x,
  w = NA,
  cross = FALSE,
  wr_type = 0,
  wr_bounds = NULL,
  wr_rhs = NULL,
  with_details = FALSE
)
```

## Arguments

- x:

  A numeric data frame or matrix of indicator data, with observations as
  rows and indicators as columns. No other columns should be present
  (e.g. label columns).

- w:

  Set to NA by default to avoid passing weights from iMeta file. Ignore.

- cross:

  Logical. If set to `TRUE`, then average cross efficiency scores will
  be returned. See Details and the dedicated vingette.

- wr_type:

  A single numeric value indicating the type of weight restrictions to
  include to the model. Can take the values: 0, 1, 2, 3, and 4, each
  representing a different type of weight restrictions. Defaults to 0
  (no additional restrictions). See Details.

- wr_bounds:

  A numeric vector or a matrix containing information on the weight
  restriction parameters. A different type of object must be passed on
  to `wr_bounds` for different values of the `wr_type` parameter. See
  details for examples on how to customize the object 'wr_bounds' to the
  particular restrictions that you wish to include in the model.

- wr_rhs:

  A numeric vector containing the values of the right-hand side of
  different forms of weight restrictions. To be used when `wr_type` is
  set to 1. Defaults to `NULL` which indicates a zero right hand side.
  See details for examples on how to customize the object `wr_rhs`.

- with_details:

  Logical. Set to `FALSE` by default when the function is called within
  [`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md).
  If `TRUE` it also outputs weights and intensities, all wrapped as a
  list.

## Value

A numeric vector, or if `with_details = TRUE` , a list also containing
weights and intensities.

## Details

Aggregates a data frame of indicator values into a single column using
the Benefit of the Doubt (BoD) model, a special form of the
non-parametric Data Envelopment Analysis (DEA) approach. To obtain the
composite indicator value, a linear program is estimated for each unit
(see equations (4-5a-5b) in Cherchye et al. (2007), p.120) which
endogenously selects the aggregation weights so that the composite
indicator of the evaluated unit is maximized. Results in a different set
of weights for each unit, which are 'best-possible' in the sense that
the unit can assign larger (smaller) weights to indicators in which it
performs relatively better (worse).

The model cannot properly accommodate missing values, so the data frame
`x` must not include any `NA` values. Missing values can be imputed at
the imputation step of building the composite indicator via the
'Impute()' function. Note that this function does not necessarily impute
all missing values. In case some missing values remain in `x`, the
respective units will be removed from the analysis and a `NA` value will
be assigned to their aggregate indicator.

**Appending additional restrictions in the weights**

The conventional version restricts the aggregation weights selected by
the units to be non-negative. In this case, it is possible that units
assign zero weights to some indicators. The function offers the
possibility to append various types of additional restrictions on the
aggregation weights. In general, a set of such a restriction can be
represented as

\\a\_{1}w\_{1} + a\_{2}w\_{2} + \ldots + a\_{N}w\_{N} \leq b\\

where\\a\_{1}, \ldots, a\_{N}\\ and b are parameters specified by the
analyst and \\w\_{1}, \ldots, w\_{N}\\ are the aggregation weights. The
information of the weight restrictions is supplied through the following
parameters:

- `wr_type` Specifies the type of additional restrictions, takes values
  0 (default, no restrictions), 1, 2, 3, 4.

- `wr_bounds` Includes the parameters \\a\_{1}, \ldots, a\_{N}\\

- `wr_rhs` Includes the parameters \\b\\

Four different types of restrictions are covered:

`wr_type = 1` includes the following types of restrictions:

- weak ordering of aggregation weights (\\w\_{j} \geq w\_{l}\\, \\j \neq
  l\\)

- strict ordering of aggregation weights (\\w\_{j} - w\_{l} \geq a\\,
  \\j \neq l\\)

- general form of relative weight restrictions (\\w\_{j} / w\_{l} \geq
  b\\)

- absolute (lower or upper) bounds on indicator weights (\\w\_{j} \geq
  b\\ , \\w\_{j} \leq b\\)

- equal weights between pairs of indicators (\\w\_{j} = w\_{l}\\, needs
  to be expressed as two equivalent inequalities \\w\_{j} \geq w\_{l}\\
  and \\w\_{j} \leq w\_{l}\\)

In all these cases the restrictions are included into the model by
adequately specifying the values of the parameters `wr_bounds` and
`wr_rhs`. To do so, all the different restrictions should be re-written
in the general form specified above. The values \\a\_{1}, \ldots,
a\_{N}\\ populate an \\R x N\\ matrix (where \\R\\ is the number of
restrictions specified and \\N\\ is the number of indicators to be
aggregated) which is passed on to `wr_bounds`. The parameters of the
first restriction form the first row of this matrix,and so on. The
values \\b\\ of the right-hand-sides are passed on to `wr_rhs` as a
numeric vector of length equal to the number of weight restrictions. If
`wr_rhs` is not defined it will default to a vector of zeros. For
example, assuming a setting where three indicators need to be
aggregated, the restriction \\w\_{2} \geq w\_{1}\\ needs to be written
in the general form as \\1 \cdot w\_{1} - 1 \cdot w\_{2} + 0 \cdot
w\_{3} \leq 0\\. Then `wr_bounds` is a matrix with a single row given
as: ` matrix(c(-1, 1, 0), byrow=TRUE, ncol=3) ` and `wr_rhs` is a single
numeric value equal to zero, which does not need to be specified since
it will automatically take assume this value if `wr_rhs` is not
specified.

- Note 1: The parameter byrow=TRUE must be specified so the matrix
  passed on to `wr_bounds` is filled by row.

- Note 2: The weights follow the order in which indicators appear in the
  `iMeta` file (so \\w\_{1}\\ will correspond to the first indicator
  appearing in `iMeta` in the respective aggregation level, and so on)

`wr_type = 2` Upper or lower bounds on the contribution of each
individual indicator to the aggregated composite (pie-share
restrictions). These take the form: \\\frac{w\_{j} x\_{j}}{\sum w\_{j}
x\_{j}} \geq l\\, \\j=1,\ldots,N\\, \\\frac{w\_{j} x\_{j}}{\sum w\_{j}
x\_{j}} \leq u\\, \\j=1,\ldots,N\\ In this case, the chosen values for u
and l need to be passed on to `wr_bounds` as a numeric vector of length
2 defined as `c(l, u)`, where u and l take values within the range of
\\\[0,1\]\\. wr_rhs does not need to be specified and if specified by
mistake it will not be taken into account.

- Warning 1: Both bounds should always be specified, so if you wish to
  include only a lower (upper) bound, you should also specify the upper
  (lower) bound as 1 (0).

- Warning 2: The percentage contributions by definition sum up to 1, so
  the upper bound of the shares must be specified so that \\u \cdot N
  \geq 1\\ and the lower bound of the shares must be specified so that
  \\l \cdot N \leq 1\\.

- Warning 3: This type of restrictions fails if for some unit there is a
  zero value for an indicator. In this case a 'NA' value will be
  assigned to the unit's aggregated composite. See the dedicated
  vignette for more details.

`wr_type = 3` Implements the Value Efficiency Analysis BoD (VEA-BoD)
model of Ravanos and Karagiannis (2021) for aggregating composite
indicators. This form of weight restrictions forces all the units to use
the best-practice aggregation weights of a specially chosen unit, which
reflects the views of policymakers about the most preferred mix of
individual indicators. Information on the chosen unit is passed on to
the parameter `wr_bounds` as a single numeric value specifying the row
of the data frame where the data for the chosen unit are stored. If for
example the chosen unit's uCode name is "ZZZ", we manually check the row
position of "ZZZ" in the dataset If "ZZZ" is in row 10, we specify
`wr_bounds` as `c(10)`. wr_rhs does not need to be specified and if
specified by mistake it will not be taken into account.

`wr_type = 4` includes upper or lower bounds on the share of each
individual aggregation weight to the sum of aggregation weights
(weight-share restrictions). These take the form: \\\frac{w\_{j}}{\sum
w\_{j}} \geq l\\, \\j=1,\ldots,N\\, \\\frac{w\_{j}}{\sum w\_{j}} \leq
u\\, \\j=1,\ldots,N\\ In this case, the chosen values for u and l need
to be passed on to `wr_bounds` as a numeric vector defined as `c(l, u)`
of length two, where u and l take values within the range of
\\\[0,1\]\\. wr_rhs does not need to be specified and if specified by
mistake it will not be taken into account.

- Warning 1: Both bounds should always be specified, so if you wish to
  include only a lower (upper) bound, you should also specify the upper
  (lower) bound as 1 (0).

- Warning 2: The percentage contributions by definition sum up to 1, so
  the upper bound of the shares must be specified so that \\u \cdot N
  \geq 1\\ and the lower bound of the shares must be specified so that
  \\l \cdot N \leq 1\\. This type of restrictions does not fail if for
  some unit there is a zero value for an indicator.

**Important notice (use of weight restrictions in aggregation levels
other than the last)**

The DEA aggregation function without weight restrictions can be readily
used for aggregating indicators in hierarchy levels other than the last
one, and can also be used in more than one hierarchy levels at the same
time. However, The addition of weight restrictions in these cases should
be done with caution. Currently in COINr aggregation at the same
hierarchy level uses by default the same aggregation across all
aggregations within this level. This means that any weight restrictions
put by the analysis will be the same across the different aggregations
to be performed within the same hierarchy level. If within the same
hierarchy level the different aggregations involve a different number of
indicators each time (e.g., three indicators make up Pillar A and four
indicators make up Pillar B) then weight restrictions of the type 1
above cannot be used. Also, restrictions of type 2 and 4 (share
restrictions) will put the same bounds to all different aggregations and
their upper and lower bounds need to be cautiously selected so that they
secure feasible solutions in all the different aggregations (see above
for more details). Lastly, restrictions on type 3 (VEA) will use the
same MPS across the different aggregations within the same hierarchy
level, and in this case the MPS should be selected so that it does not
have missing data in any of the different indicators to be aggregated,
otherwise it cannot be selected as MPS. Given a cautious selection of
weight restriction types and parameters, the analyst can place different
types of weight restrictions and bounds for each hierarchy level in
which DEA is used by specifying accordingly the `f_ag_para` argument in
[`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md)
as a list of lists, each of which will include the parameters
`wr_bounds` and `wr_rhs` defined appropriately for each hierarchy level.

More information on types of weight restrictions can be found in Allen
et al. (1997) and in Cherchye et al. (2007) for the case of the BoD
model.

**Cross efficiency estimation**

If parameter `cross` is set to `"TRUE"`, then the model will use the
cross efficiency approach to obtain aggregate composite scores. In this
approach, each unit is assessed \\K\\ different times (where \\K\\
equals the number of units), each time using the set of aggregation
weights that a different sample unit has selected. This results into N
aggregated composite scores for each of the units. The final composite
indicator score of each unit is obtained as the arithmetic average of
these \\K\\ scores. This is the value returned by the function.

## Examples

``` r
# Example 1: a_dea() as an aggregation function in the third aggregation level
# build example coin
library(COINr)
coin <- build_example_coin(up_to = "Normalise")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Imputed
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Treated
#> Written data set to .$Data$Normalised
coin <- Aggregate(coin, dset = "Normalised",
                f_ag = c("a_amean", "a_amean", "a_dea"),
                by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#> Written data set to .$Data$Aggregated

# Example 2: a_dea() as an aggregation functon in the second and third aggregation levels
library(COINr)
coin <- Aggregate(coin, dset = "Normalised",
                  f_ag = c("a_amean", "a_dea", "a_dea"),
                  by_df = c(FALSE, TRUE, TRUE))
#> Written data set to .$Data$Aggregated
#> (overwritten existing data set)

# Example 3: Restricting the weight of the first indicator (Connectivity) to
# be larger than or equal to that of the second (Sustainability) (type = 1)
library(COINr)
coin <- Aggregate(coin, dset = "Normalised",
                 f_ag = c("a_amean", "a_amean","a_dea"),
                 f_ag_para = list(NULL, NULL,
                                   list(wr_type = 1,
                                        wr_bounds=matrix(c(-1, 1),byrow=TRUE, ncol=2)
                                        )),
                by_df = c(FALSE, FALSE, TRUE),
                w =list(NULL, NULL, "none"))
#> wr_rhs is not defined, defaulting to a vector of zeros
#> Written data set to .$Data$Aggregated
#> (overwritten existing data set)

# Example 4: Including upper (75%) and lower (10%) contribution restrictions
library(COINr)
coin <- Aggregate(coin, dset = "Normalised",
                  f_ag = c("a_amean", "a_amean", "a_dea"),
                  f_ag_para = list(NULL, NULL, list(wr_type = 2, wr_bounds =c(0.1, 0.75))),
                  by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#> Written data set to .$Data$Aggregated
#> (overwritten existing data set)

# Example 5: Including a VEA-type of restriction with unit 10 as the chosen unit
library(COINr)
coin <- Aggregate(coin, dset = "Normalised",
                  f_ag = c("a_amean", "a_amean", "a_dea"),
                  f_ag_para = list(NULL, NULL, list(wr_type = 3, wr_bounds  = 10)),
                  by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#> Written data set to .$Data$Aggregated
#> (overwritten existing data set)

# Example 6: Including upper (75%) and lower (10%) restrictions in weight shares
library(COINr)
coin <- Aggregate(coin, dset = "Normalised",
                  f_ag = c("a_amean", "a_amean", "a_dea"),
                  f_ag_para = list(NULL, NULL, list(wr_type = 4, wr_bounds =c(0.1, 0.75))),
                  by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#> Written data set to .$Data$Aggregated
#> (overwritten existing data set)

# Example 7: Computing 'average cross efficiency' composite scores
library(COINr)
coin <- Aggregate(coin, dset = "Normalised",
                  f_ag = c("a_amean", "a_amean", "a_dea"),
                  f_ag_para = list(NULL, NULL, list(cross = TRUE)),
                  by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#> Written data set to .$Data$Aggregated
#> (overwritten existing data set)

```
