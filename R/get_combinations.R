#' Test indicator combinations for composite indicators
#'
#' Performs exhaustive analysis of indicator combinations within specified dimensions to identify
#' optimal subsets based on multiple statistical criteria including PCA variance, Cronbach's alpha,
#' and correlation patterns. This function systematically tests all possible combinations within
#' specified size ranges and evaluates their statistical properties.
#'
#' The function generates and evaluates all possible combinations of indicators within each specified
#' dimension. For each combination, it:
#' \itemize{
#'   \item Aggregates indicators using the specified aggregation function
#'   \item Computes correlations between indicators and the aggregate
#'   \item Performs non-rotated PCA to assess dimensionality
#'   \item Performs rotated PCA (varimax) to examine loading patterns
#'   \item Calculates Cronbach's alpha for internal consistency
#'   \item Identifies suggested additional indicators that could strengthen the combination
#' }
#'
#' Combinations are classified as "successful" if they meet all three criteria:
#' \itemize{
#'   \item First principal component explains ≥ \code{PCA_ref} of variance (default 65%)
#'   \item Fewer than 2 eigenvalues > 1 (suggesting unidimensionality)
#'   \item Cronbach's alpha ≥ \code{cronbach_alpha_ref} (default 0.7)
#' }
#'
#' @param coin A coin or unbalanced_coin object
#' @param dset Name of the data set to use within the coin (e.g., "Raw", "Normalised")
#' @param dimension Character vector of level 2 aggregate names (parents) to analyze.
#'   These should correspond to dimension/pillar names in your framework.
#' @param f_ag Aggregation function to use. Default is \code{'a_amean'} (arithmetic mean).
#'   Any aggregation function available in COINr can be used.
#' @param exclude_corr Numeric threshold (between -1 and 1) for excluding combinations.
#'   Combinations containing indicator pairs with correlation below this threshold are excluded.
#'   Default is 0.3. Set to -1 to include all combinations regardless of correlation.
#' @param add_elements Optional named list specifying indicators to add to specific dimensions.
#'   List names should match dimension names, and values should be character vectors of indicator codes.
#'   Example: \code{list('Physical' = c("Ind1", "Ind2"))}.
#' @param drop_elements Optional named list specifying indicators to remove from specific dimensions.
#'   Structure is identical to \code{add_elements}.
#' @param global_min_max Numeric vector of length 2: \code{c(min, max)} specifying the minimum and
#'   maximum number of indicators to include in combinations across all dimensions. Overridden by
#'   \code{indiv_min_max} if specified for a dimension. If NULL, defaults to \code{c(2, n)} where
#'   n is the number of available indicators.
#' @param indiv_min_max Optional named list with dimension-specific min/max ranges.
#'   Example: \code{list('Physical' = c(3, 6), 'Institutional' = c(2, 5))}.
#'   Overrides \code{global_min_max} for specified dimensions.
#' @param PCA_ref Numeric threshold (0-1) for minimum variance explained by first principal component.
#'   Combinations must meet this threshold to be classified as successful. Default is 0.65 (65%).
#' @param cronbach_alpha_ref Numeric threshold (0-1) for minimum Cronbach's alpha value.
#'   Combinations must meet this threshold to be classified as successful. Default is 0.7.
#' @param warnings Logical: if TRUE (default), displays warning messages for invalid arguments
#'   or missing indicators.
#' @param verbose Logical: if TRUE (default), displays progress information during computation
#'   including dimension names and combination counts.
#' @param ... Additional arguments (currently unused, for S3 method compatibility)
#'
#' @return A list with four components:
#' \describe{
#'   \item{Info}{Data frame summarizing results for each dimension: number of combinations tested,
#'     number of successful combinations, min/max sizes, original and current indicator counts,
#'     and numbers of added/dropped indicators.}
#'   \item{Combinations}{Named list (by dimension) of data frames containing detailed statistics
#'     for ALL tested combinations. Each data frame includes: combination ID, number of indicators,
#'     indicator codes, correlation statistics (mean, sd, min, max), PCA variance, eigenvalue count,
#'     Cronbach's alpha, rotated PCA results, and suggested additional indicators.}
#'   \item{Successful}{Named list (by dimension) of data frames containing only combinations that
#'     meet all success criteria (subset of Combinations).}
#'   \item{Correlations}{Named list (by dimension) containing three correlation matrices:
#'     'All' (full correlation matrix), 'Low' (correlations below \code{exclude_corr}),
#'     and 'High' (correlations above 0.92).}
#' }
#'
#' @details
#' \strong{Computational Complexity Warning:}
#' The number of combinations grows exponentially with the number of indicators. For n indicators
#' tested across all sizes from 2 to n:
#' \itemize{
#'   \item 5 indicators: 26 combinations
#'   \item 10 indicators: 1,013 combinations
#'   \item 15 indicators: 32,752 combinations
#'   \item 20 indicators: 1,048,555 combinations
#' }
#' Each combination requires aggregation, correlation computation, two PCA analyses, and Cronbach's
#' alpha calculation. For large indicator sets (>12), consider using \code{global_min_max} or
#' \code{indiv_min_max} to restrict the search space.
#'
#' \strong{Interpretation Guide:}
#' \itemize{
#'   \item \strong{PCA variance:} Higher is better. Values ≥0.65 suggest indicators share
#'     a common underlying dimension.
#'   \item \strong{Eigenvalues:} Fewer eigenvalues >1 indicates better unidimensionality.
#'     Ideally only 1 eigenvalue >1.
#'   \item \strong{Cronbach's alpha:} 0.7-0.8 = acceptable, 0.8-0.9 = good, >0.9 = excellent
#'     (but may indicate redundancy).
#'   \item \strong{Suggested indicators:} Indicators not in the combination that correlate more
#'     strongly with the aggregate than the weakest indicator in the combination.
#' }
#'
#' @examples
#' # Build example coin
#' coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)
#'
#' # Test combinations for two dimensions
#' results <- get_combinations(
#'   coin = coin,
#'   dset = "Normalised",
#'   dimension = c("Physical", "Political"),
#'   PCA_ref = 0.65,
#'   cronbach_alpha_ref = 0.7,
#'   exclude_corr = 0.3,
#'   verbose = FALSE
#' )
#'
#' # View summary information
#' print(results$Info)
#'
#' # View successful combinations for Physical dimension
#' if (!is.null(results$Successful$Physical)) {
#'   head(results$Successful$Physical)
#' }
#'
#' # Restrict combination size globally
#' results2 <- get_combinations(
#'   coin = coin,
#'   dset = "Normalised",
#'   dimension = "Physical",
#'   global_min_max = c(3, 5),
#'   verbose = FALSE
#' )
#'
#' @seealso
#' \code{\link{get_cronbach}}, \code{\link{get_PCA}}, \code{\link{Aggregate}}
#'
#' @importFrom dplyr %>%
#' @export
get_combinations <- function(coin, ...) {
  UseMethod("get_combinations")
}

#' @describeIn get_combinations Get indicator combinations for coin class
#' @export
get_combinations.coin <- function(coin, dset, dimension = NULL, f_ag = 'a_amean',
                                   exclude_corr = 0.3, add_elements = NULL,
                                   drop_elements = NULL, global_min_max = NULL,
                                   indiv_min_max = NULL, PCA_ref = 0.65,
                                   cronbach_alpha_ref = 0.7,
                                   warnings = TRUE, verbose = TRUE, ...) {

  # Define internal helper functions -----------------------------------------------------

  possible_combinations <- function(items, size){

    sample <- length(unique(items))

    combinations <- factorial(sample) / (factorial(size) * factorial(sample - size))

    results <- utils::combn(items, size, simplify = FALSE)

    return(list('size' = combinations, 'combinations' = results))

  }

  check_number <- function(x) is.numeric(x) && length(x) >= 1 && !anyNA(x)

  # Internal cronbach_alpha calculation (kept for consistency with COINauditr)

  cronbach_alpha <- function(x){

    k = ncol(x[, which(colnames(x) != 'uCode')])

    # get covariance matrix
    cvtrix <- stats::cov(x, use = "pairwise.complete.obs")

    # sum of all elements of cov matrix
    sigall <- sum(cvtrix, na.rm = TRUE)

    # mean of all elements except diagonal
    sigav <- (sigall - sum(diag(cvtrix), na.rm = TRUE))/(k * (k - 1))

    # calculate Cronbach alpha
    cronbach <- (k^2 * sigav) / sigall

    return(cronbach)

  }

  # Validation checks -----------------------------------------------------

  if(!isTRUE(is.coin(coin))){

    stop('The argument "coin" is not a coin object', call. = FALSE)

  }

  iData <- COINr::get_dset(coin, dset = dset)
  iMeta <- coin[['Meta']]$Ind

  # Check if the argument dimension is character or factor

  if(is.null(dimension) || (!is.character(dimension) & !is.factor(dimension))){

    stop('The argument "dimension" is not a character or factor', call. = FALSE)

  }

  # Check if the argument global_min_max is a number

  if(is.null(global_min_max) || anyNA(global_min_max) || !check_number(global_min_max)){

    if(isTRUE(warnings)){

      message('Warning: The argument "global_min_max" is not well defined. Minimum and maximum values will be adjusted based on the number of indicators')

    }
  }

  # Check if the argument indiv_min_max is a number

  if(!is.null(indiv_min_max)){

    missing_arg <- names(indiv_min_max)[!(names(indiv_min_max) %in% iMeta$Parent)]

    if(length(missing_arg) > 0){

      if(isTRUE(warnings)){

        message('Warning: The following elements in the argument "indiv_min_max" were not found in the iMeta file provided: ', paste0(missing_arg, collapse = ', '), ' -> using global_min_max')

      }
    }
  }

  # Check if the argument add_elements has undefined indicators

  if(!is.null(add_elements)){

    missing_arg <- names(add_elements)[!(names(add_elements) %in% iMeta$Parent)]

    if(length(missing_arg) > 0){

      if(isTRUE(warnings)){

        message('Warning: The following elements in the argument "add_elements" were not found in the iMeta file provided: ', paste0(missing_arg, collapse = ', '))

      }
    }
  }

  # Check if the argument drop_elements has undefined indicators

  if(!is.null(drop_elements)){

    missing_arg <- names(drop_elements)[!(names(drop_elements) %in% iMeta$Parent)]

    if(length(missing_arg) > 0){

      if(isTRUE(warnings)){

        message('Warning: The following elements in the argument "drop_elements" were not found in the iMeta file provided: ', paste0(missing_arg, collapse = ', '))

      }
    }
  }

  # Main analysis -----------------------------------------------------

  # Initialize result containers

  info <- data.frame()

  all_dimn <- list()
  successful_dim <- list()
  corr_dim <- list()

  # Loop over the different dimensions

  for(n in 1:length(dimension)){

    parent <- dimension[[n]]

    if(isTRUE(verbose)){

      print(parent)

    }

    # Find indicators in the iMeta file

    elements <- iMeta[iMeta$Parent == parent & iMeta$Level == 1, ]$iCode[!is.na(iMeta[iMeta$Parent == parent & iMeta$Level == 1, ]$iCode)]

    if(length(elements) < 1){

      stop(paste0('No level 1 indicators have been found for parent: ', parent), call. = FALSE)

    }

    # Save original indicators

    original_elements <- elements

    # Add elements

    if(!is.null(add_elements[[parent]]) && length(add_elements[[parent]]) > 0){

      missing_arg <- add_elements[[parent]][!(add_elements[[parent]] %in% iMeta$iCode)]

      if(length(missing_arg) > 0){

        if(isTRUE(warnings)){

          message(paste0('Warning: The following indicators for ' , parent, ' in the argument "add_elements" were not found in the iMeta file provided: ', paste0(missing_arg, collapse = ', ')))

        }
      }

      elements <- unique(c(elements, add_elements[[parent]][add_elements[[parent]] %in% iMeta$iCode]))

    }

    # Drop elements

    if(!is.null(drop_elements[[parent]]) && length(drop_elements[[parent]]) > 0){

      missing_arg <- drop_elements[[parent]][!(drop_elements[[parent]] %in% iMeta$iCode)]

      if(length(missing_arg) > 0){

        if(isTRUE(warnings)){

          message(paste0('Warning: The following indicators for ' , parent, ' in the argument "drop_elements" were not found in the iMeta file provided: ', paste0(missing_arg, collapse = ', ')))

        }
      }

      elements <- elements[!(elements %in% drop_elements[[parent]][drop_elements[[parent]] %in% iMeta$iCode])]

    }

    # Stop if we have less than 2 indicators since we cannot aggregate

    if(length(elements) < 2){

      stop('No possible combinations can be established with less than 2 indicators', call. = FALSE)

    }

    # Check if the argument "indiv_min_max" is valid, otherwise use the global min max or the number of indicators

    if(!is.null(indiv_min_max[[parent]]) && !anyNA(indiv_min_max[[parent]]) && check_number(indiv_min_max[[parent]])){

      min_indc <- min(indiv_min_max[[parent]])
      max_indc <- max(indiv_min_max[[parent]])

    }else{

      if(anyNA(indiv_min_max[[parent]]) || !check_number(indiv_min_max[[parent]])){

        if(isTRUE(warnings)){

          message(paste0('Warning: The arguments of "indiv_min_max" for ', parent, ' is not well defined'))

        }
      }

      if(!is.null(global_min_max) && !anyNA(global_min_max) && check_number(global_min_max)){

        min_indc <- min(global_min_max)
        max_indc <- max(global_min_max)

      }else{

        min_indc <- 2
        max_indc <- length(elements)

      }
    }

    # Change the minimum number of indicators if it is below 2

    if(min_indc < 2){

      min_indc <- 2

      if(isTRUE(warnings)){

        message(paste0('Warning: The minimum number of indicators considered is lower than the possible establish, ', 2, ' will be use'))

      }
    }

    # Check if the maximum number of indicators is not plausible

    if(max_indc > length(elements) || max_indc < min_indc){

      max_indc <- length(elements)

      if(isTRUE(warnings)){

        message(paste0('The maximum number of indicators considered is greater than the possible establish, ', max_indc, ' will be use'))

      }
    }

    size <- seq(min_indc, max_indc, by = 1)

    combinations <- list()

    # Compute all possible combinations

    for(i in size){

      test <- possible_combinations(items = elements, size = i)

      combinations <- c(combinations, test$combinations)

    }

    # Compute correlations between indicators

    correlations_all <- as.data.frame(stats::cor(iData[, which(colnames(iData) %in% elements)], use = 'pairwise.complete.obs', method = 'pearson'))

    # placate CMD check
    Var1 <- Var2 <- a <- b <- corr <- . <- NULL

    correlations_high <- correlations_all %>%
      dplyr::mutate(Var1 = row.names(.)) %>%
      tidyr::pivot_longer(cols = colnames(.)[!(colnames(.) == 'Var1')], names_to = 'Var2', values_to = 'corr') %>%
      dplyr::mutate(a = pmin(Var1, Var2), b = pmax(Var1, Var2)) %>%
      dplyr::filter(Var1 != Var2, corr > 0.92) %>%
      dplyr::distinct(a, b, .keep_all = TRUE) %>%
      dplyr::select(-a, -b) %>%
      as.data.frame()

    correlations_low <- correlations_all %>%
      dplyr::mutate(Var1 = row.names(.)) %>%
      tidyr::pivot_longer(cols = colnames(.)[!(colnames(.) == 'Var1')], names_to = 'Var2', values_to = 'corr') %>%
      dplyr::mutate(a = pmin(Var1, Var2), b = pmax(Var1, Var2)) %>%
      dplyr::filter(Var1 != Var2, corr < exclude_corr) %>%
      dplyr::distinct(a, b, .keep_all = TRUE) %>%
      dplyr::select(-a, -b) %>%
      as.data.frame()

    # Only positively correlated indicators

    if(check_number(exclude_corr) && exclude_corr >= -1 && exclude_corr <= 1){

      if(nrow(correlations_low) > 0){

        for(j in 1:nrow(correlations_low)){

          if(length(combinations) > 0){

            combinations <- combinations[!unlist(sapply(combinations, function(x) (correlations_low[j, 'Var1'] %in% unlist(x) & correlations_low[j, 'Var2'] %in% unlist(x)) == TRUE))]

          }
        }
      }
    }

    # Total possible combinations

    total_comb <- length(combinations)

    if(isTRUE(verbose)){

      cat(paste0('Possible combinations: ', total_comb), sep = "\n")

    }

    # If combinations are available

    if(total_comb > 0){

      all_cases <- data.frame()

      # Evaluate each combination

      for(i in 1:length(combinations)){

        if(isTRUE(verbose)){

          cat(paste0(i, '/', length(combinations)), '\r')
          utils::flush.console()

        }

        indicators <- combinations[i][[1]]

        # Check if the combination is the same as the one defined in the iMeta file

        original <- identical(sort(original_elements), sort(indicators))

        imeta_test <- iMeta[iMeta$iCode %in% indicators, ]

        data <- iData[, which(colnames(iData) %in% c('uCode', indicators))]

        list_weights <- imeta_test[order(match(imeta_test$iCode, colnames(data)[-which(colnames(data) == 'uCode')])), ]$Weight

        data$Output <- COINr::Aggregate(data[, -which(colnames(data) == 'uCode')], f_ag = f_ag, list(w = as.numeric(list_weights)))

        correlation <- as.data.frame(stats::cor(data[, -which(colnames(data) == 'uCode')], use = 'pairwise.complete.obs', method = 'pearson'))
        correlation <- correlation[which(rownames(correlation) != 'Output'), ]

        correlation$Output <- round(correlation$Output, digits = 3)
        correlation <- correlation[order(-correlation$Output), ]

        # Compute correlations and find suggested indicators

        check_top_ind <- dplyr::left_join(
          data[, -which(colnames(data) %in% elements)], iData[, -which(colnames(iData) %in% elements)],
          by = 'uCode'
        )
        check_top_ind <- as.data.frame(stats::cor(check_top_ind[, -1], use = 'pairwise.complete.obs', method = 'pearson'))

        check_top_ind$Output <- round(check_top_ind$Output, digits = 3)
        check_top_ind <- check_top_ind[order(-check_top_ind$Output) & check_top_ind$Output > min(correlation[which(rownames(correlation) != 'Output'), 'Output']) & !is.na(check_top_ind$Output), ]

        # Compute non rotated PCA

        NAs <- nrow(data) - nrow(stats::na.omit(data[, -which(colnames(data) %in% c('uCode', 'Output'))]))

        PCAres <- stats::prcomp(stats::na.omit(data[, -which(colnames(data) %in% c('uCode', 'Output'))]), center = TRUE, scale = TRUE)

        var_pc <-  PCAres$sdev[1]^2 / sum(PCAres$sdev^2)

        # Find number of eigen values above 1

        eigens <- length(PCAres$sdev[PCAres$sdev^2 > 1])

        # Compute rotated PCA

        RPC <- psych::principal(stats::na.omit(data[, -which(colnames(data) %in% c('uCode', 'Output'))]), rotate = "varimax", nfactors = eigens,  residuals = FALSE, scores = FALSE, use = 'pairwise.complete.obs')

        all_rc_sign <- all(apply(RPC$loadings, 2, function(x) length(unique(x > 0)) == 1) == TRUE) # Find if all indicators in one component have the same direction

        all_rc_0.5 <- all(unique(apply(RPC$loadings, 1, function(x) max(x) > 0.5)) == TRUE) # Find if the maximum loading of an indicator is above 0.5 in one of the components

        rc_indc <- paste(apply(RPC$loadings, 2, function(x) paste(names(x)[x > 0.5], collapse = '; ')), collapse = ' / ')

        # Cronbach's alpha

        cronbach_alph <- cronbach_alpha(data[, -which(colnames(data) %in% c('uCode', 'Output'))])

        # Summarise information and concatenate the results

        Results <- data.frame(
          'ID' = i,
          'dimension' = parent,
          'num_Indcs' = length(rownames(correlation)[which(rownames(correlation) != 'Output')]),
          'original' = ifelse(isTRUE(original), 1, 0),
          'indicators' = paste(rownames(correlation)[which(rownames(correlation) != 'Output')], collapse = '; '),
          'corr_values' = paste(correlation[which(rownames(correlation) != 'Output'), 'Output'], collapse = '; '),
          'mean' =  mean(correlation[which(rownames(correlation) != 'Output'), 'Output']),
          'sd' = stats::sd(correlation[which(rownames(correlation) != 'Output'), 'Output']),
          'min' = min(correlation[which(rownames(correlation) != 'Output'), 'Output']),
          'max' = max(correlation[which(rownames(correlation) != 'Output'), 'Output']),
          'negative_corr' = sum(ifelse(correlation[which(rownames(correlation) != 'Output'), 'Output'] >= 0, 0, 1)),
          'PCA' = var_pc, 'PCA_NAs' = NAs,
          'eigen_dim' = eigens, 'cronbach' = cronbach_alph,
          'rotated_loadings_threshold' = all_rc_0.5,
          'rotated_loadings_direction' = all_rc_sign,
          'rotated_indic' = rc_indc,
          'suggested_indc' =  paste(rownames(check_top_ind)[which(rownames(check_top_ind) != 'Output')], collapse = '; '),
          'corr_suggested_indc' = paste(check_top_ind[which(rownames(check_top_ind) != 'Output'), 'Output'], collapse = '; ')
        )

        all_cases <- rbind(all_cases, Results)

      }

      # Successful combinations based on the PCA, eigenvalues and cronbach alpha

      successful <- all_cases[all_cases$PCA >= PCA_ref & all_cases$eigen_dim < 2 & all_cases$cronbach >= cronbach_alpha_ref, ]

    }else{

      successful <- NULL
      all_cases <- NULL

    }

    # Save information at the parent level

    all_dimn[[parent]] <- all_cases
    successful_dim[[parent]] <- successful

    corr_dim[[parent]] <- list('All' = correlations_all, 'Low' = correlations_low, 'High' = correlations_high)

    info <- rbind(info, data.frame('dimension' = parent, 'combinations' = total_comb, 'successful' = ifelse(!is.null(successful), nrow(successful), 0), 'min' = min_indc, 'max' = max_indc, 'original_length' = length(original_elements), 'current_length' = length(elements),
                                   'added_indic' = length(unique(add_elements[[parent]][add_elements[[parent]] %in% iMeta$iCode])), 'dropped_indic' = length(unique(drop_elements[[parent]][drop_elements[[parent]] %in% iMeta$iCode]))))

  }

  return(list('Info' = info, 'Combinations' = all_dimn, 'Successful' = successful_dim, 'Correlations' = corr_dim))

}

#' @describeIn get_combinations Get indicator combinations for unbalanced_coin class
#' @export
get_combinations.unbalanced_coin <- function(coin, dset, dimension = NULL, f_ag = 'a_amean',
                                              exclude_corr = 0.3, add_elements = NULL,
                                              drop_elements = NULL, global_min_max = NULL,
                                              indiv_min_max = NULL, PCA_ref = 0.65,
                                              cronbach_alpha_ref = 0.7,
                                              warnings = TRUE, verbose = TRUE, ...) {

  # For unbalanced coins, use the same implementation as coin class
  # The function handles placeholders through the metadata structure
  get_combinations.coin(coin = coin, dset = dset, dimension = dimension, f_ag = f_ag,
                        exclude_corr = exclude_corr, add_elements = add_elements,
                        drop_elements = drop_elements, global_min_max = global_min_max,
                        indiv_min_max = indiv_min_max, PCA_ref = PCA_ref,
                        cronbach_alpha_ref = cronbach_alpha_ref,
                        warnings = warnings, verbose = verbose)

}
