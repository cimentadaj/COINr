#' Statistics of indicators
#'
#' Given a coin and a specified data set (`dset`), returns a table of statistics with entries for each column.
#'
#' The statistics (columns in the output table) are as follows (entries correspond to each column):
#'
#' * `Min`: the minimum
#' * `Max`: the maximum
#' * `Mean`: the (arirthmetic) mean
#' * `Median`: the median
#' * `Std`: the standard deviation
#' * `Skew`: the skew
#' * `Kurt`: the kurtosis
#' * `N.Avail`: the number of non-`NA` values
#' * `N.NonZero`: the number of non-zero values
#' * `N.Unique`: the number of unique values
#' * `Frc.Avail`: the fraction of non-`NA` values
#' * `Frc.NonZero`: the fraction of non-zero values
#' * `Frc.Unique`: the fraction of unique values
#' * `Flag.Avail`: a data availability flag - columns with `Frc.Avail < t_avail` will be flagged as `"LOW"`, else `"ok"`.
#' * `Flag.NonZero`: a flag for columns with a high proportion of zeros. Any columns with `Frc.NonZero < t_zero` are
#' flagged as `"LOW"`, otherwise `"ok"`.
#' * `Flag.Unique`: a unique value flag - any columns with `Frc.Unique < t_unq` are flagged as `"LOW"`, otherwise `"ok"`.
#' * `Flag.SkewKurt`: a skew and kurtosis flag which is an indication of possible outliers. Any columns with
#' `abs(Skew) > t_skew` AND `Kurt > t_kurt` are flagged as `"OUT"`, otherwise `"ok"`.
#'
#' The aim of this table, among other things, is to check the basic statistics of each column/indicator, and identify
#' any possible issues for each indicator. For example, low data availability, having a high proportion of zeros and/or
#' a low proportion of unique values. Further, the combination of skew and kurtosis (i.e. the `Flag.SkewKurt` column)
#' is a simple test for possible outliers, which may require treatment using [Treat()].
#'
#' The table can be returned either to the coin or as a standalone data frame - see `out2`.
#'
#' See also `vignette("analysis")`.
#'
#' @details
#' For objects inheriting from `unbalanced_coin`, the method delegates to the balanced
#' representation created by [new_unbalanced_coin()] and removes placeholder helper nodes
#' from the returned statistics. When `out2 = "coin"`, the resulting analysis table is
#' stored on the unbalanced object with the class tag preserved.
#'
#' @param t_skew Absolute skewness threshold. See details.
#' @param t_kurt Kurtosis threshold. See details.
#' @param t_avail Data availability threshold. See details.
#' @param x A coin
#' @param dset A data set present in `.$Data`
#' @param nsignif Number of significant figures to round the output table to.
#' @param out2 Either `"df"` (default) to output a data frame of indicator statistics, or "`coin`" to output an
#' updated coin with the data frame attached under `.$Analysis`.
#' @param ... arguments passed to or from other methods.
#' @param t_zero A threshold between 0 and 1 for flagging indicators with high proportion of zeroes. See details.
#' @param t_unq A threshold between 0 and 1 for flagging indicators with low proportion of unique values. See details.plot
#'
#' @examples
#' # build example coin
#' coin <-  build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get table of indicator statistics for raw data set
#' get_stats(coin, dset = "Raw", out2 = "df")
#'
#' @return Either a data frame or updated coin - see `out2`.
#'
#' @export
get_stats.coin <- function(x, dset, t_skew = 2, t_kurt = 3.5, t_avail = 0.65, t_zero = 0.5,
                                 t_unq = 0.5, nsignif = 3, out2 = "df", ...){

  stopifnot(out2 %in% c("df", "coin"))

  # get data
  iData <- get_data(x, dset = dset, ...)

  # get iData_ (only numeric indicator cols)
  iData_ <- extract_iData(x, iData, GET = "iData_")

  # get stats table
  stat_tab <- get_stats(iData_, t_skew = t_skew, t_kurt = t_kurt, t_avail = t_avail, t_zero = t_zero,
                        t_unq = t_unq, nsignif = nsignif)

  # write to coin or output as df
  if(out2 == "df"){
    stat_tab
  } else {
    x$Analysis[[dset]][["Stats"]] <- stat_tab
    x
  }

}



#' Statistics of columns
#'
#' Takes a data frame and returns a table of statistics with entries for each column.
#'
#' The statistics (columns in the
#' output table) are as follows (entries correspond to each column):
#'
#' * `Min`: the minimum
#' * `Max`: the maximum
#' * `Mean`: the (arirthmetic) mean
#' * `Median`: the median
#' * `Std`: the standard deviation
#' * `Skew`: the skew
#' * `Kurt`: the kurtosis
#' * `N.Avail`: the number of non-`NA` values
#' * `N.NonZero`: the number of non-zero values
#' * `N.Unique`: the number of unique values
#' * `Frc.Avail`: the fraction of non-`NA` values
#' * `Frc.NonZero`: the fraction of non-zero values
#' * `Frc.Unique`: the fraction of unique values
#' * `Flag.Avail`: a data availability flag - columns with `Frc.Avail < t_avail` will be flagged as `"LOW"`, else `"ok"`.
#' * `Flag.NonZero`: a flag for columns with a high proportion of zeros. Any columns with `Frc.NonZero < t_zero` are
#' flagged as `"LOW"`, otherwise `"ok"`.
#' * `Flag.Unique`: a unique value flag - any columns with `Frc.Unique < t_unq` are flagged as `"LOW"`, otherwise `"ok"`.
#' * `Flag.SkewKurt`: a skew and kurtosis flag which is an indication of possible outliers. Any columns with
#' `abs(Skew) > t_skew` AND `Kurt > t_kurt` are flagged as `"OUT"`, otherwise `"ok"`.
#'
#' The aim of this table, among other things, is to check the basic statistics of each column/indicator, and identify
#' any possible issues for each indicator. For example, low data availability, having a high proportion of zeros and/or
#' a low proportion of unique values. Further, the combination of skew and kurtosis (i.e. the `Flag.SkewKurt` column)
#' is a simple test for possible outliers, which may require treatment using [Treat()].
#'
#' See also `vignette("analysis")`.
#'
#' @param t_skew Absolute skewness threshold. See details.
#' @param t_kurt Kurtosis threshold. See details.
#' @param t_avail Data availability threshold. See details.
#' @param x A data frame with only numeric columns.
#' @param nsignif Number of significant figures to round the output table to.
#' @param ... arguments passed to or from other methods.
#' @param t_zero A threshold between 0 and 1 for flagging indicators with high proportion of zeroes. See details.
#' @param t_unq A threshold between 0 and 1 for flagging indicators with low proportion of unique values. See details.
#'
#' @importFrom stats median sd
#'
#' @examples
#' # stats of mtcars
#' get_stats(mtcars)
#'
#' @return A data frame of statistics for each column
#'
#' @export
get_stats.data.frame <- function(x, t_skew = 2, t_kurt = 3.5, t_avail = 0.65, t_zero = 0.5,
                                 t_unq = 0.5, nsignif = 3, ...){


  # CHECKS ------------------------------------------------------------------

  not_numeric <- !(sapply(x, is.numeric))
  if(any(not_numeric)){
    stop("Non-numeric cols detected in data frame. Input must be a data frame with only numeric columns.")
  }

  # STATS -------------------------------------------------------------------

  n <- nrow(x)

  # this function gets all stats for one column of data
  stats_i <- function(xi){

    n_avail <- sum(!is.na(xi))
    prc_avail <- n_avail/n
    sk <- skew(xi, na.rm = TRUE)
    kt <- kurt(xi, na.rm = TRUE)
    nzero <- sum(xi != 0, na.rm = TRUE)
    nunq <- length(unique(xi[!is.na(xi)]))
    nsame <- max(table(xi)) # the largest number of elements with the same value

    data.frame(
      Min = min(xi, na.rm = TRUE),
      Max = max(xi, na.rm = TRUE),
      Mean = mean(xi, na.rm = TRUE),
      Median = stats::median(xi, na.rm = TRUE),
      Std = stats::sd(xi, na.rm = TRUE),
      Skew = sk,
      Kurt = kt,
      N.Avail = n_avail,
      N.NonZero = nzero,
      N.Unique = nunq,
      N.Same = nsame,
      Frc.Avail = prc_avail,
      Frc.NonZero = nzero/n_avail,
      Frc.Unique = nunq/n_avail,
      Frc.Same = nsame/n_avail,
      Flag.Avail = ifelse(prc_avail >= t_avail, "ok", "LOW"),
      Flag.NonZero = ifelse(nzero/n >= t_zero, "ok", "LOW"),
      Flag.Unique = ifelse(nunq/n >= t_unq, "ok", "LOW"),
      Flag.SkewKurt = ifelse((abs(sk) > t_skew) & (kt > t_kurt), "OUT", "ok")
    )
  }

  # now apply function to all cols and add iCode column
  stats_tab <- lapply(x, stats_i)
  stats_tab <- Reduce(rbind, stats_tab)
  stats_tab <- cbind(iCode = names(x), stats_tab)


  # OUTPUT ------------------------------------------------------------------

  # sfs
  if(!is.null(nsignif)){
    stats_tab <- signif_df(stats_tab, nsignif)
  }

  stats_tab

}


#' Statistics of columns/indicators
#'
#' Generic function for reports various statistics from a data frame or coin. See method documentation:
#'
#' * [get_stats.data.frame()]
#' * [get_stats.coin()]
#'
#' See also `vignette("analysis")`.
#'
#' This function replaces the now-defunct `getStats()` from COINr < v1.0.
#'
#' @param x Object (data frame or coin)
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' # see individual method documentation
#'
#' @return A data frame of statistics for each column
#'
#' @export
get_stats <- function(x, ...){
  UseMethod("get_stats")
}


#' Get statistics for multivariate analysis
#'
#' Retrieve main statistics used to evaluate the consistency of an aggregate through
#' multivariate analysis including PCA, rotated PCA, and Cronbach's alpha.
#'
#' @param coin A coin-class object.
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param level The aggregation levels where the statistics are computed.
#' @param exclude_corr Reference value used to exclude potential combinations of pairs of indicators that have a lower pearson correlation coefficient than the one provided. The default value is equal to  0.3.
#' @param warnings A logical value. If `TRUE` information about the inconsistencies is printed.
#' @param verbose A logical value. If `TRUE` information on the number of iterations is printed.
#'
#' @details
#' Principle Component Analysis (PCA) is performed using the function [stats::prcomp()], note that rows with missing values will be removed before running the PCA.
#'
#' Afterwards a rotated PCA is computed using the function [psych::principal()]. A "varimax" rotation is used on as many components as selected latent dimensions, based on the number of eigenvalues above 1.
#' The output of the analysis is summarised in the following way:
#' * `rotated_loadings_threshold`: A logical value. If `TRUE` all indicators are loaded with more than 0.5 in a single component.
#' * `rotated_loadings_direction`: A logical value. If `TRUE` all loadings in the same component have the same sign.
#' * `rotated_items`: A character where the name of the indicators with loadings above 0.5 are pasted separating each rotated component by ' / '. It is useful when having more than two latent dimensions to identify in which component each indicator is loaded.
#'
#' @return A list with two components:
#' * `Statistics`: A data frame containing the main statistics for each dimension.
#' * `Correlations`: A list containing the correlation matrices computed by dimension using the Pearson's correlation coefficient.
#'
#' @examples
#' ## Build example up to aggregate data set
#' coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)
#'
#' ## Run function:
#' statistics <- get_statistics(coin, "Aggregated", level = c(2, 3))
#'
#' @export
#'
get_statistics <- function(coin, dset = "Aggregated", level, exclude_corr = 0.3, warnings = TRUE, verbose = TRUE){


  #check_number <- function(x) all(sapply(x, function(x) is.numeric(x) && x%%1 == 0 && x > 1))

  # Checks -----------------------------------------------------

  if(!is.coin(coin)){
    stop('The argument "coin" is not a coin object', call. = FALSE)
  }

  iData <- get_dset(coin, dset = dset)
  iMeta <- coin[['Meta']]$Ind

  valid_levels <- 2:coin$Meta$maxlev

  if(!all(level %in% valid_levels)){
    stop('The levels provided are not a list of positive integers greater than 1', call. = FALSE)
  }

  if(!is.numeric(exclude_corr) || exclude_corr < -1 || exclude_corr > 1){
    message('The correlation threshold is not a number between -1 and 1, 0.3 will be used')
    exclude_corr <- 0.3
  }

  # Run analysis -----------------------------------------------------

  all_dimn <- data.frame()
  corr_dim <- list()

  dimension <- iMeta[iMeta$Level %in% level, ]$iCode

  for(n in 1:length(dimension)){

    parent <- dimension[[n]]

    level <- iMeta[iMeta$iCode == parent, ]$Level

    if(level > 1){

      indicators <- iMeta[iMeta$Parent == parent & iMeta$Level == level - 1, ]$iCode[!is.na(iMeta[iMeta$Parent == parent & iMeta$Level == level - 1, ]$iCode)]

      # For unbalanced coins, exclude placeholder indicators
      if(!is.null(iMeta$IsPlaceholder)){
        is_placeholder <- iMeta[iMeta$iCode %in% indicators, ]$IsPlaceholder
        is_placeholder[is.na(is_placeholder)] <- FALSE
        indicators <- indicators[!is_placeholder]
      }

      if(length(indicators) < 2){
        # Skip this dimension if fewer than 2 real indicators (need at least 2 for correlation/PCA)
        if(isTRUE(verbose)){
          if(length(indicators) == 0){
            message(paste0('Skipping dimension "', parent, '" - no real indicators found (all placeholders)'))
          } else {
            message(paste0('Skipping dimension "', parent, '" - only ', length(indicators), ' indicator (need at least 2 for multivariate analysis)'))
          }
        }
        next
      }

      imeta_test <- iMeta[iMeta$iCode %in% indicators, ]

      data <- iData[, which(colnames(iData) %in% c('uCode', indicators))]

      if(is.null(nrow(data)) || nrow(data) < 0){
        stop(paste0('No data provided in the dset "', dset, '" has been found for parent: ', parent), call. = FALSE)
      }

    } else {

      if(isTRUE(warnings)){
        message(paste0('The dimension: "', parent, '" has level 1, use the argument "indicators" to compute statistics between indicators'))
      }
    }

    correlations_all <- as.data.frame(stats::cor(data[, which(colnames(data) %in% indicators)], use = 'pairwise.complete.obs', method = 'pearson'))

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

    # Compute non rotated PCA

    NAs <- nrow(data) - nrow(stats::na.omit(data[, -which(colnames(data) == 'uCode')]))

    PCAres <- stats::prcomp(stats::na.omit(data[, -which(colnames(data) == 'uCode')]), center = TRUE, scale = TRUE)

    var_pc <-  PCAres$sdev[1]^2 / sum(PCAres$sdev^2)

    # Find number of eigen values above 1

    eigens <- length(PCAres$sdev[PCAres$sdev^2 > 1])

    # Compute rotated PCA

    RPC <- psych::principal(stats::na.omit(data[, -which(colnames(data) == 'uCode')]), rotate = "varimax", nfactors = eigens,  residuals = FALSE, scores = FALSE, use = 'pairwise.complete.obs')

    all_rc_sign <- all(apply(RPC$loadings, 2, function(x) length(unique(x > 0)) == 1) == TRUE) # Find if all indicators in one component have the same direction

    all_rc_0.5 <- all(unique(apply(RPC$loadings, 1, function(x) max(x) > 0.5)) == TRUE) # Find if the maximum loading of an indicator is above 0.5 in one of the components

    rc_indc <- paste(apply(RPC$loadings, 2, function(x) paste(names(x)[x > 0.5], collapse = '; ')), collapse = ' / ')

    # Cronbach

    cronbach_alph <- cronbach_alpha(data[, -which(colnames(data) == 'uCode')])

    all_dimn <- rbind(all_dimn, data.frame('dimension' = parent, 'level' = level, 'num_items' = length(indicators), 'items' = paste0(indicators, collapse = '; '), 'low_corr_pairs' = nrow(correlations_low), 'PCA_variance' = var_pc, 'PCA_NAs' = NAs,
                                           'eigen_dim' = eigens, 'cronbach' = cronbach_alph, 'rotated_loadings_thrs' = all_rc_0.5, 'rotated_loadings_direction' = all_rc_sign, 'rotated_items' = rc_indc))

    corr_dim[[parent]] <- list('All' = correlations_all, 'Low' = correlations_low, 'High' = correlations_high)

  }

  return(list('Statistics' = all_dimn, 'Correlations' = corr_dim))

}


#' Calculate Cronbach's Alpha
#'
#' Calculates Cronbach's Alpha, given a data frame with all numeric columns
#'
#' @param x A data frame of indicator data, optionally with `uCode` column.
#'
#' @return Cronbach alpha value
#'
#' @export
#'
#' @examples
#' # Calculate Cronbach's alpha for mtcars
#' cronbach_alpha(mtcars[, 1:4])
#'
cronbach_alpha <- function(x){

  stopifnot(all(sapply(x, is.numeric)))

  k = ncol(x)

  # get covariance matrix
  cvtrix <- stats::cov(x, use = "pairwise.complete.obs")

  # sum of all elements of cov matrix
  sigall <- sum(cvtrix, na.rm = TRUE)

  # mean of all elements except diagonal
  sigav <- (sigall - sum(diag(cvtrix), na.rm = TRUE))/(k * (k - 1))

  # calculate Cronbach alpha
  (k^2 * sigav) / sigall

}
