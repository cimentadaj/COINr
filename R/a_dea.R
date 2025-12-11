#' Re-aggregate with DEA
#'
#' Performs the aggregation in the last aggregation Level (Pillars to Index) using the non-parametric flexible-weighting
#' Benefit-if-the-Doubt (BoD) model. This is an alternative aggregation technique used in Auditing Composite Indices by
#' CC-COIN. The function must take as a minimum an input `coin` which is a coin-class object developed through COINr package
#' that has been aggregated and so includes a dataset called "Aggregated". If no other parameters are specified, this will
#' estimate the conventional form of the BoD model, in which no additional restrictions on the aggregation weights are
#' placed. Such restrictions can be placed through parameters inherited from [a_dea()], see Details below.
#'
#' @param coin A coin-class object.
#' @param level The Level in which we wish to re-aggregate the coin using DEA. Defaults to the final aggregation Level. If specified as a lower level,
#' then it will aggregate all indicators at that Level in a composite. See Details.
#' @param wr_type A single numeric value indicating the type of weight restrictions to include to the model. Can take the values: 0, 1, 2, 3, and 4.
#' Defaults to 0 (no additional restrictions). See [a_dea()] for further details.
#' @param wr_bounds A numeric vector or a matrix containing information on the weight restrictions. See [a_dea()] for further details.
#' @param wr_rhs A numeric vector containing the values of the right-hand side of different forms of weight restrictions. To be used when `wr_type` is set to 1.
#' Defaults to `NULL` which indicates a zero right hand side. See [a_dea()] for further details.
#'
#' @return A list containing the following:
#'
#' * a data frame named `DEA_CI` containing the unit names, the Pillars to be aggregated, the nominal aggregated Index, and the DEA re-aggregated Index
#' in a column named "Dea".
#' * a data frame `DEA_CI_ranks` containing the ranks of the units based on the DEA re-aggregated Index.
#' * a data frame named `dea_weights` containing the aggregation weights selected by each unit.
#' * a data frame named `normalised_weights`containing the average normalised weights. These are obtained by averaging
#' across rows of the `dea_weights` data frame and then dividing each average term by the sum of the average terms.
#'
#' @export
#'
#' @examples
#'
#' # example #1 (no additional weight restrictions)
#' # build example coin up to Aggregated dataset.
#' coin <- build_example_coin(up_to = "Aggregate")
#' # re-aggregate Index
#' dea_reagg <- get_DEA(coin)
#'
#' # example #2 (with additional weight restrictions, type 2 restrictions in the
#' # contribution of each Pillar to the index, max 75%, min 5%)
#' dea_reagg <- get_DEA(coin, wr_type = 2, wr_bounds = c(0.05, 0.75))
#'
#' # example #3 (aggregation at a lower level)
#' # In the ASEM data, Level 3 is the final aggregation Level where the two Pillars
#' # get aggregated to ASEM. Specifying `level = 2` will aggregate the
#' # eight indicators (five under Connectivity Pillar and three under Syst Pillar)
#' # to an overall composite.
#' dea_reagg <- get_DEA(coin, level = 2, wr_type = 2, wr_bounds = c(0.05, 0.75))
#'
#'
get_DEA <- function(coin, level = NULL, wr_type = 0, wr_bounds = NULL, wr_rhs = NULL){
  UseMethod("get_DEA")
}

#' @rdname get_DEA
#' @export
get_DEA.coin <- function(coin, level = NULL, wr_type = 0, wr_bounds = NULL, wr_rhs = NULL){

  # get dataset and checks --------------------------------------------------

  indat <- get_dset(coin, dset = "Aggregated")

  # get metadata
  imeta <- coin$Meta$Ind
  if (is.null(level)) {
    level <- max(imeta$Level, na.rm = TRUE)-1
  }
  imeta_p <- imeta[!is.na(imeta$Level) & imeta$Level == level, ]
  Pillars <- indat[, which(colnames(indat) %in% imeta_p$iCode), drop = FALSE]
  uCode <- data.frame('uCode' =  indat$uCode)

  # Check for sufficient pillars - DEA needs at least 2 inputs
 if(ncol(Pillars) < 2){
    stop("DEA requires at least 2 pillars at the specified level. Found only ", ncol(Pillars),
         " pillar(s): ", paste(names(Pillars), collapse = ", "), ". ",
         "For unbalanced coins, placeholders are excluded which may leave insufficient pillars.")
  }

  # estimate results
  results <- a_dea(Pillars, wr_type = wr_type, wr_bounds = wr_bounds,
                   wr_rhs = wr_rhs, with_details = TRUE)

  # get outputs

  # DEA CI
  Dea <- as.data.frame(results$Agg)
  names(Dea) <- "Dea"
  uCode_e <- cbind(uCode, Dea)
  DEA_CI <- merge(indat, uCode_e, by = "uCode", all = TRUE)

  index_code <- imeta$iCode[which(imeta$Level == coin$Meta$maxlev)]
  DEA_CI <- DEA_CI[c("uCode", "Dea", index_code, imeta_p$iCode)]

  # weights
  Weights <- as.data.frame(results$dea_weights)
  averages <- sapply(Weights, mean, na.rm = TRUE)
  sum_averages <- sum(averages)

  # normalised weights
  normalized_averages <- averages / sum_averages
  norm.weights <- as.data.frame(t(normalized_averages))
  colnames(norm.weights) <- colnames(Weights)

  # Issue a message for all the units excluded from the analysis due to NA values
  na_names <- uCode_e$uCode[is.na(uCode_e$Dea)]

  if(length(na_names) > 0){
    message("The following units have NA values in some Pilar(s) and the DEA score was not calculated for them: ", toString(na_names))
  }

  # Output
  list(
    DEA_CI = DEA_CI,
    DEA_CI_ranks = rank_df(DEA_CI), # WB: also added ranks
    dea_weights = Weights,
    norm.weights = norm.weights)
}

#' @rdname get_DEA
#' @export
get_DEA.unbalanced_coin <- function(coin, level = NULL, wr_type = 0, wr_bounds = NULL, wr_rhs = NULL){
  # Get placeholder codes
  placeholders <- coin$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]
  keep_placeholders <- isTRUE(getOption("COINr.keep_placeholders"))

  # Get base classes (remove unbalanced_coin class)
  base_classes <- setdiff(class(coin), "unbalanced_coin")
  if(length(base_classes) == 0){
    base_classes <- "coin"
  }

  # Create base coin
  base_coin <- structure(coin, class = base_classes)

  # Remove placeholders from metadata if present
  if(length(placeholders) > 0){
    if(!is.null(base_coin$Meta$Ind)){
      base_coin$Meta$Ind <- base_coin$Meta$Ind[base_coin$Meta$Ind$iCode %nin% placeholders, , drop = FALSE]
    }
  }

  # Call the coin method
  res <- get_DEA.coin(base_coin, level = level, wr_type = wr_type,
                      wr_bounds = wr_bounds, wr_rhs = wr_rhs)

  # Filter out placeholders from results if needed
  if(length(placeholders) > 0 && !keep_placeholders){
    # Remove placeholder columns from DEA_CI
    if(!is.null(res$DEA_CI)){
      placeholder_cols <- names(res$DEA_CI)[names(res$DEA_CI) %in% placeholders]
      if(length(placeholder_cols) > 0){
        res$DEA_CI <- res$DEA_CI[, !(names(res$DEA_CI) %in% placeholder_cols), drop = FALSE]
      }
    }
    # Remove placeholder columns from dea_weights
    if(!is.null(res$dea_weights)){
      placeholder_cols <- names(res$dea_weights)[names(res$dea_weights) %in% placeholders]
      if(length(placeholder_cols) > 0){
        res$dea_weights <- res$dea_weights[, !(names(res$dea_weights) %in% placeholder_cols), drop = FALSE]
      }
    }
    # Remove placeholder columns from norm.weights
    if(!is.null(res$norm.weights)){
      placeholder_cols <- names(res$norm.weights)[names(res$norm.weights) %in% placeholders]
      if(length(placeholder_cols) > 0){
        res$norm.weights <- res$norm.weights[, !(names(res$norm.weights) %in% placeholder_cols), drop = FALSE]
      }
    }
  }

  res
}


#' Aggregate a data frame using DEA
#'
#' Aggregates a data frame of indicator values into a single column using the Benefit of the Doubt (BoD) model.
#' Uses the data frame of indicator values for all units an input, so `by_df` need to be set to `TRUE` in the parameters of [Aggregate()].
#' The function does not accept weights from iMeta but generates a unit-specific a vector of weights, so `w` needs to be set to `"none"` in the parameters of [Aggregate()].
#'
#' @param x A numeric data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w Set to NA by default to avoid passing weights from iMeta file. Ignore.
#' @param cross Logical. If set  to `TRUE`, then average cross efficiency scores will be returned. See Details and the dedicated vingette.
#' @param wr_type A single numeric value indicating the type of weight restrictions to include to the model.
#' Can take the values: 0, 1, 2, 3, and 4, each representing a different type of weight restrictions. Defaults to 0 (no additional restrictions). See Details.
#' @param wr_bounds A numeric vector or a matrix containing information on the weight restriction parameters.
#' A different type of object must be passed on to `wr_bounds` for different values of the `wr_type` parameter.
#' See details for examples on how to customize the object 'wr_bounds' to the particular restrictions that you wish to include in the model.
#' @param wr_rhs A numeric vector containing the values of the right-hand side of different forms of weight restrictions. To be used when `wr_type` is set to 1.
#' Defaults to `NULL` which indicates a zero right hand side. See details for examples on how to customize the object `wr_rhs`.
#' @param with_details Logical. Set to `FALSE` by default when the function is called within [Aggregate()]. If `TRUE` it also outputs weights and intensities, all wrapped as a list.
#'
#' @return A numeric vector, or if `with_details = TRUE` , a list also containing weights and intensities.
#' @export
#'
#' @details
#'
#' Aggregates a data frame of indicator values into a single column using the Benefit of the Doubt (BoD) model, a special form of the non-parametric Data Envelopment Analysis (DEA) approach.
#' To obtain the composite indicator value, a linear program is estimated for each unit (see equations (4-5a-5b) in Cherchye et al. (2007), p.120) which endogenously selects the aggregation weights
#' so that the composite indicator of the evaluated unit is maximized. Results in a different set of weights for each unit, which are 'best-possible' in the sense that the unit can assign
#' larger (smaller) weights to indicators in which it performs relatively better (worse).
#'
#' The model cannot properly accommodate missing values, so the data frame `x` must not include any `NA` values. Missing values can be imputed at the imputation step of building the composite indicator via the 'Impute()' function.
#' Note that this function does not necessarily impute all missing values. In case some missing values remain in `x`, the respective units will be removed from the analysis and a `NA` value will be assigned to their aggregate indicator.
#'
#' **Appending additional restrictions in the weights**
#'
#' The conventional version restricts the aggregation weights selected by the units to be non-negative. In this case, it is possible that units assign zero weights to some indicators.
#' The function offers the possibility to append various types of additional restrictions on the aggregation weights. In general, a set of such a restriction can be represented as
#'
#' \eqn{a_{1}w_{1} + a_{2}w_{2} + \ldots + a_{N}w_{N} \leq b}
#'
#' where\eqn{a_{1}, \ldots, a_{N}} and b are parameters specified by the analyst and \eqn{w_{1}, \ldots, w_{N}} are the aggregation weights. The information of the weight restrictions is supplied through the following parameters:
#'
#' * `wr_type`   Specifies the type of additional restrictions, takes values 0 (default, no restrictions), 1, 2, 3, 4.
#' * `wr_bounds` Includes the parameters \eqn{a_{1}, \ldots, a_{N}}
#' * `wr_rhs`    Includes the parameters \eqn{b}
#'
#' Four different types of restrictions are covered:
#'
#' `wr_type = 1` includes the following types of restrictions:
#' * weak ordering of aggregation weights (\eqn{w_{j} \geq w_{l}}, \eqn{j \neq l})
#' * strict ordering of aggregation weights (\eqn{w_{j} - w_{l} \geq a}, \eqn{j \neq l})
#' * general form of relative weight restrictions (\eqn{w_{j} / w_{l} \geq b})
#' * absolute (lower or upper) bounds on indicator weights (\eqn{w_{j} \geq b} , \eqn{w_{j} \leq b})
#' * equal weights between pairs of indicators (\eqn{w_{j} = w_{l}},
#' needs to be expressed as two equivalent inequalities \eqn{w_{j} \geq w_{l}} and \eqn{w_{j} \leq w_{l}})
#'
#' In all these cases the restrictions are included into the model by adequately specifying the values of the parameters `wr_bounds` and `wr_rhs`. To do so, all the different restrictions
#' should be re-written in the general form specified above. The values \eqn{a_{1}, \ldots, a_{N}} populate an \eqn{R x N} matrix (where \eqn{R} is the number of restrictions specified
#' and \eqn{N} is the number of indicators to be aggregated) which is passed on to `wr_bounds`.
#' The parameters of the first restriction form the first row of this matrix,and so on. The values \eqn{b} of the right-hand-sides are passed on to `wr_rhs`
#' as a numeric vector of length equal to the number of weight restrictions. If `wr_rhs` is not defined it will default to a vector of zeros. For example, assuming a setting where three indicators need to
#' be aggregated, the restriction \eqn{w_{2} \geq w_{1}} needs to be written in the general form as \eqn{1 \cdot w_{1} - 1 \cdot w_{2} + 0 \cdot w_{3} \leq 0}. Then `wr_bounds` is a matrix with a single row given as:
#' \code{ matrix(c(-1, 1, 0), byrow=TRUE, ncol=3) }
#' and `wr_rhs` is a single numeric value equal to zero, which does not need to be specified since it will automatically take assume this value if `wr_rhs` is not specified.
#' * Note 1: The parameter byrow=TRUE must be specified so the matrix passed on to `wr_bounds` is filled by row.
#' * Note 2: The weights follow the order in which indicators appear in the `iMeta` file (so \eqn{w_{1}} will correspond to the first indicator
#' appearing in `iMeta` in the respective aggregation level, and so on)
#'
#' `wr_type = 2` Upper or lower bounds on the contribution of each individual indicator to the aggregated composite (pie-share restrictions). These take the form:
#' \eqn{\frac{w_{j} x_{j}}{\sum w_{j} x_{j}} \geq l}, \eqn{j=1,\ldots,N},
#' \eqn{\frac{w_{j} x_{j}}{\sum w_{j} x_{j}} \leq u}, \eqn{j=1,\ldots,N}
#' In this case, the chosen values for u and l need to be passed on to `wr_bounds` as a numeric vector of length 2 defined as \code{c(l, u)}, where u and l take values within the range of \eqn{[0,1]}.
#' wr_rhs does not need to be specified and if specified by mistake it will not be taken into account.
#' * Warning 1: Both bounds should always be specified, so if you wish to include only a lower (upper) bound, you should also specify the upper (lower) bound as 1 (0).
#' * Warning 2: The percentage contributions by definition sum up to 1, so the upper bound of the shares must be specified so that \eqn{u \cdot N \geq 1} and the lower bound of the shares must be specified
#' so that \eqn{l \cdot N \leq 1}.
#' * Warning 3: This type of restrictions fails if for some unit there is a zero value for an indicator. In this case a 'NA' value will be assigned to the unit's aggregated composite.
#' See the dedicated vignette for more details.
#'
#' `wr_type = 3` Implements the Value Efficiency Analysis BoD (VEA-BoD) model of Ravanos and Karagiannis (2021) for aggregating composite indicators. This form of weight restrictions forces all the units to use the best-practice aggregation weights
#' of a specially chosen unit, which reflects the views of policymakers about the most preferred mix of individual indicators.  Information on the chosen unit is passed on to the parameter
#' `wr_bounds` as a single numeric value specifying the row of the data frame where the data for the chosen unit are stored. If for example the chosen unit's uCode name is "ZZZ", we manually check the row position of "ZZZ" in the dataset
#' If "ZZZ" is in row 10, we specify `wr_bounds` as \code{c(10)}.
#' wr_rhs does not need to be specified and if specified by mistake it will not be taken into account.
#'
#' `wr_type = 4` includes upper or lower bounds on the share of each individual aggregation weight to the sum of aggregation weights (weight-share restrictions). These take the form:
#' \eqn{\frac{w_{j}}{\sum w_{j}} \geq l}, \eqn{j=1,\ldots,N},
#' \eqn{\frac{w_{j}}{\sum w_{j}} \leq u}, \eqn{j=1,\ldots,N}
#' In this case, the chosen values for u and l need to be passed on to `wr_bounds` as a numeric vector defined as \code{c(l, u)} of length two, where u and l take values within the range of \eqn{[0,1]}.
#' wr_rhs does not need to be specified and if specified by mistake it will not be taken into account.
#' * Warning 1: Both bounds should always be specified, so if you wish to include only a lower (upper) bound, you should also specify the upper (lower) bound as 1 (0).
#' * Warning 2: The percentage contributions by definition sum up to 1, so the upper bound of the shares must be specified so that \eqn{u \cdot N \geq 1} and the lower bound of the shares must be specified
#' so that \eqn{l \cdot N \leq 1}.
#' This type of restrictions does not fail if for some unit there is a zero value for an indicator.
#'
#' **Important notice (use of weight restrictions in aggregation levels other than the last)**
#'
#' The DEA aggregation function without weight restrictions can be readily used for aggregating indicators in hierarchy levels
#' other than the last one, and can also be used in more than one hierarchy levels at the same time. However, The addition of weight restrictions in these cases should be done with caution.
#' Currently in COINr aggregation at the same hierarchy level uses by default the same aggregation across all aggregations within this level. This means that any weight restrictions
#' put by the analysis will be the same across the different aggregations to be performed within the same hierarchy level. If within the same hierarchy level the different aggregations involve
#' a different number of indicators each time (e.g., three indicators make up Pillar A and four indicators make up Pillar B) then weight restrictions of the type 1 above cannot be used.
#' Also, restrictions of type 2 and 4 (share restrictions) will put the same bounds to all different aggregations and their upper and lower bounds need to be cautiously selected so that they secure feasible solutions
#' in all the different aggregations (see above for more details). Lastly, restrictions on type 3 (VEA) will use the same MPS across the different aggregations within the same hierarchy level, and in this case the MPS
#' should be selected so that it does not have missing data in any of the different indicators to be aggregated, otherwise it cannot be selected as MPS.
#' Given a cautious selection of weight restriction types and parameters, the analyst can place different types of weight restrictions and bounds for each hierarchy level in which DEA is used
#' by specifying accordingly the `f_ag_para` argument in `Aggregate()` as a list of lists, each of which will include the parameters `wr_bounds` and `wr_rhs` defined appropriately for each hierarchy level.
#'
#' More information on types of weight restrictions can be found in Allen et al. (1997) and in Cherchye et al. (2007) for the case of the BoD model.
#'
#' **Cross efficiency estimation**
#'
#' If parameter `cross` is set  to `"TRUE"`, then the model will use the cross efficiency approach to obtain aggregate composite scores. In this approach, each unit is assessed \eqn{K} different times (where \eqn{K} equals the
#' number of units), each time using the set of aggregation weights that a different sample unit has selected. This results into N aggregated composite scores for each of the units. The final composite indicator score
#' of each unit is obtained as the arithmetic average of these \eqn{K} scores. This is the value returned by the function.
#'
#' @examples
#'
#' # Example 1: a_dea() as an aggregation function in the third aggregation level
#' # build example coin
#' library(COINr)
#' coin <- build_example_coin(up_to = "Normalise")
#' coin <- Aggregate(coin, dset = "Normalised",
#'                 f_ag = c("a_amean", "a_amean", "a_dea"),
#'                 by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#'
#' # Example 2: a_dea() as an aggregation functon in the second and third aggregation levels
#' library(COINr)
#' coin <- Aggregate(coin, dset = "Normalised",
#'                   f_ag = c("a_amean", "a_dea", "a_dea"),
#'                   by_df = c(FALSE, TRUE, TRUE))
#'
#' # Example 3: Restricting the weight of the first indicator (Connectivity) to
#' # be larger than or equal to that of the second (Sustainability) (type = 1)
#' library(COINr)
#' coin <- Aggregate(coin, dset = "Normalised",
#'                  f_ag = c("a_amean", "a_amean","a_dea"),
#'                  f_ag_para = list(NULL, NULL,
#'                                    list(wr_type = 1,
#'                                         wr_bounds=matrix(c(-1, 1),byrow=TRUE, ncol=2)
#'                                         )),
#'                 by_df = c(FALSE, FALSE, TRUE),
#'                 w =list(NULL, NULL, "none"))
#'
#' # Example 4: Including upper (75%) and lower (10%) contribution restrictions
#' library(COINr)
#' coin <- Aggregate(coin, dset = "Normalised",
#'                   f_ag = c("a_amean", "a_amean", "a_dea"),
#'                   f_ag_para = list(NULL, NULL, list(wr_type = 2, wr_bounds =c(0.1, 0.75))),
#'                   by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#'
#' # Example 5: Including a VEA-type of restriction with unit 10 as the chosen unit
#' library(COINr)
#' coin <- Aggregate(coin, dset = "Normalised",
#'                   f_ag = c("a_amean", "a_amean", "a_dea"),
#'                   f_ag_para = list(NULL, NULL, list(wr_type = 3, wr_bounds  = 10)),
#'                   by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#'
#' # Example 6: Including upper (75%) and lower (10%) restrictions in weight shares
#' library(COINr)
#' coin <- Aggregate(coin, dset = "Normalised",
#'                   f_ag = c("a_amean", "a_amean", "a_dea"),
#'                   f_ag_para = list(NULL, NULL, list(wr_type = 4, wr_bounds =c(0.1, 0.75))),
#'                   by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#'
#' # Example 7: Computing 'average cross efficiency' composite scores
#' library(COINr)
#' coin <- Aggregate(coin, dset = "Normalised",
#'                   f_ag = c("a_amean", "a_amean", "a_dea"),
#'                   f_ag_para = list(NULL, NULL, list(cross = TRUE)),
#'                   by_df = c(FALSE, FALSE, TRUE), w = list(NULL, NULL, "none"))
#'
#'
a_dea <- function(x, w=NA, cross=FALSE, wr_type = 0, wr_bounds = NULL,
                  wr_rhs = NULL, with_details = FALSE){

  # CHECKS ---------------------------------------------------------------------

  # Checks for type of x -------------------------------------------------------
  N <- ncol(x) # set the number of indicators
  K <- nrow(x) # set the number of units
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0 || ncol(x) == 0) {
    stop("the input to a_DEA_df must be a non-empty data frame.")
  }
  all_numeric <- all(sapply(x, function(col) all(is.numeric(col) | is.na(col))))
  stopifnot(all_numeric == TRUE)

  # check for zero values and issue a warning
  zero_exists <- apply(x, 1, function(x) any(x == 0, na.rm = TRUE))
  if(any(zero_exists)){
    warning("The dataset contains at least one zero value for an indicator and a unit. The use of contribution restrictions (wr_type =2) is not recommended.")
  }

  # Checks for wr_type and wr_bounds -------------------------------------------

  # Check if wr_type is numeric and within the range of 0 to 3
  if (is.null(wr_type) || wr_type == 0) {
    wr_type <- 0
    wr_bounds <- NULL
    wr_rhs <- NULL }
  else if (!is.numeric(wr_type) || !wr_type %in% 0:4) {
    stop("wr_type must be a numeric value between 0 and 4.")
  }
  # Check if wr_bounds is NULL or a numeric matrix with N columns when wr_type is 1
  if (!is.null(wr_type)) {
    if (wr_type == 1) {
      if (!is.matrix(wr_bounds) || ncol(wr_bounds) != N || !all(sapply(wr_bounds, is.numeric))) {
        stop("wr_bounds must be a numeric matrix with N columns when wr_type is 1 (ordering of weights, relative restrictions or non homogeneous linear restrictions).")
      } else {
        if (!is.null(wr_rhs)) {
          if (!is.numeric(wr_rhs)|| length(wr_rhs) != nrow(wr_bounds)) {
            stop("wr_rhs must be a numeric vector of length equal to the number of rows in wr_bounds (i.e, equal to the number of weight restrictions)")
          } else {
            wr_lhs <-c(rep(0, nrow(wr_bounds)))
            message(" wr_rhs is not defined, defaulting to a vector of zeros")
          }
        }
      }
    }
    # Check if wr_bounds is a column of length=2 when wr_type is 2 or 4
    else if (wr_type == 2 || wr_type == 4) {
      if (!is.null(wr_rhs)) {
        wr_rhs <- NULL
        message("Type of weight restrictions selected: restrictions in contribution or weight shares. Specified value for wr_rhs will be overriden.")
      }
      if (!is.vector(wr_bounds) || length(wr_bounds) != 2) {
        stop("wr_bounds must be a column of length=2 when wr_type is 2 or 4.")
      }
      if (any(is.na(wr_bounds)) || any(wr_bounds < 0)) {
        stop("wr_bounds must be a column of length=2 containing positive numeric values when wr_type is 2 or 4.")
      }
      if ((wr_bounds[1])*N > 1 || (wr_bounds[2])*N < 1) {
        stop("the specified value for the upper bound of the shares must be such that u*N >= 1 and the specified value for the lower bound of the shares must be such that l*N <= 1.")
      }
    }

    # Check if wr_bounds is a numeric value between 1 and K when wr_type is 3
    else if (wr_type == 3) {
      if (!is.null(wr_rhs)) {
        wr_rhs <- NULL
        message("Type of weight restrictions selected: VEA. Specified value for wr_rhs will be overriden.")
      }
      if (!is.numeric(wr_bounds) || length(wr_bounds) > 1 || wr_bounds < 1 || wr_bounds > K) {
        stop("when wr_type = 3 (VEA), wr_bounds must be a numeric value between 1 and K (the number of sample units), indicating the unit chosen as the Most Preferred Solution.")
      }
      aq <- x[wr_bounds,]
      if(any(is.na(aq))){
        stop("The sample unit chosen as the MPS contains missing values for some indicators. It will not be used in the evaluation. Please choose another sample unit as the MPS.")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # If wr_type = 3, check a priori if the chosen unit is efficient and if not obtain its radial projection on the frontier
  if (wr_type == 3) {
    # estimate a simple DEA model only for the chosen mps unit
    # set the evaluated unit in vector z
    z <- x[wr_bounds,]
    lprec <- lpSolveAPI::make.lp(0, N)
    lpSolveAPI::lp.control(lprec,sense='max')
    lpSolveAPI::set.objfn(lprec, z[, ])
    # loop for technology defining constraints. Any units with NA values are excluded from the analysis
    for (j in 1:K) {
      if (!any(is.na(x[j,]))) {
        lpSolveAPI::add.constraint(lprec, x[j, ], "<=", 1)
      }
    }
    solve(lprec)
    effmps <- lpSolveAPI::get.objective(lprec)
    if (effmps == 1) {
      mps <- x[wr_bounds,] # if the chosen unit is efficient then use it as the MPS
    } else {
      mps <- (1/effmps)*x[wr_bounds,] # if the chosen unit is not efficient then obtain its radial projection to use as the MPS
    }
  }
  #-----------------------------------------------------------------------------

  # Check if cross is a logical variable
  if (!is.null(cross) & !is.logical(cross)) {
    stop("cross must be a logical variable defined as TRUE or FALSE.")
  }
  # ----------------------------------------------------------------------------
  # define a vector to store the efficiency scores, a data frame to store the weights, and a data frame to store the
  # lambdas from the dual solution (the benchmark for each unit).

  eff <- c(rep(NA, K))
  Weights <- as.data.frame(matrix(rep(NA, (N*K)), nrow=K, ncol=N))
  colnames(Weights) <- colnames(x)
  Lambdas <- as.data.frame(matrix(rep(NA, (K*K)), nrow=K, ncol=K))

  # run the DEA model for each row of data frame x (i.e., each unit) and gather the results into the specified vector eff
  # Checks performed for units having NA values. These units automatically get an aggregate index of NA and are also excluded from the
  # technology defining constraints

  for (i in 1:K){

    #set the evaluated unit in vector z
    z <- x[i,]

    # Exclusion case 1: if vector z contains NA values then stop and assign a NA value to the aggregated index for that unit.
    # Exclusion case 2: if vector z contains at least one zero value and wr_type =2 then stop and keep the NA value to the aggregated index,
    # the Weights and the Intensities for that unit.
    # If the vector z does not contain missing values then run the DEA linear program for z

    if (any(is.na(z)) || any(z == 0) & wr_type == 2) {
      next
    } else {

      lprec <- lpSolveAPI::make.lp(0, N)
      lpSolveAPI::lp.control(lprec,sense='max')
      lpSolveAPI::set.objfn(lprec, z[, ])

      # loop for technology defining constraints. Any units with NA values are excluded from the analysis
      for (j in 1:K) {
        if (!any(is.na(x[j,]))) {
          lpSolveAPI::add.constraint(lprec, x[j, ], "<=", 1)
        }
      }
      # ------------------------------------------------------------------------
      # loop for including different types of weight restrictions
      if (wr_type == 1){
        bounds <- t(as.data.frame(wr_bounds))
        P <- ncol(bounds)
        for (k in 1:P){
          lpSolveAPI::add.constraint(lprec, bounds[, k], "<=", wr_rhs[k])
        }
      }
      if (wr_type == 2|| wr_type == 4){
        L <- wr_bounds[1]
        U <- wr_bounds[2]
        #create matrix for the share restrictions
        upper <- matrix(-U, nrow = N, ncol = N)
        diag(upper) <- (1-U)
        lower <- matrix(L, nrow = N, ncol = N)
        diag(lower) <- (L-1)
        merged<- cbind(upper, lower)
        shares <- as.data.frame(merged)
        shares
        P <- dim(shares)[2] # number of extra restrictions
        for (k in 1:P){
          if (wr_type == 2){
            lpSolveAPI::add.constraint(lprec, x[i, ] * shares[, k], "<=", 0)
          } else {
            lpSolveAPI::add.constraint(lprec, shares[, k], "<=", 0)
          }
        }
      }
      if (wr_type == 3) {
        lpSolveAPI::add.constraint(lprec, mps[, ], "=", 1)
      }
    }
    # ------------------------------------------------------------------------
    solve(lprec)
    W <- lpSolveAPI::get.variables(lprec)
    E <- lpSolveAPI::get.objective(lprec)
    L <- lpSolveAPI::get.dual.solution(lprec)[-1][seq(1,nrow(x))]
    # store the calculated efficiency to the dedicated column and the calculated weights to the dedicated matrix
    eff[i] <- E
    Weights[i,] <- W
    Lambdas[i,] <- L
  }

  # If cross = TRUE, create a cross efficiency matrix and pass on eff the average cross efficiency
  if (cross == TRUE) {
    cross <- as.matrix(x) %*% as.matrix(t(Weights))
    Cross <- data.frame(cross)
    avecross <- apply(Cross, 1, mean, na.rm=TRUE)
    for (i in 1:K){
      if (!is.na(eff[i])){
        eff[i] <- avecross[i]
      }
    }
  }

  #Issue a message for all the units excluded from the analysis due to NA values
  na_indices <- which(is.na(eff))
  if (!is.null(na_indices)) {
    message <- paste("A composite index has not been estimated for The following units (pasting unit rows): ", paste(na_indices, collapse = ", "))
  }

  # Clip scores to [0, 1] to avoid numerical precision issues
  Agg <- pmin(pmax(as.numeric(eff), 0), 1)

  if(with_details){
    list(Agg = Agg, dea_weights = Weights, dea_intensities = Lambdas)
  } else {
    Agg
  }

}
