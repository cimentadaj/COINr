#' Aggregate indicators
#'
#' Aggregates indicators following the structure specified in `iMeta`, for each coin inside the purse.
#' See [Aggregate.coin()], which is applied to each coin, for more information
#'
#' @param x A purse-class object
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param f_ag The name of an aggregation function, a string. This can either be a single string naming
#' a function to use for all aggregation levels, or else a character vector of function names of length `n-1`, where `n` is
#' the number of levels in the index structure. In this latter case, a different aggregation function may be used for each level
#' in the index: the first in the vector will be used to aggregate from Level 1 to Level 2, the second from Level 2 to Level 3, and
#' so on.
#' @param w An optional data frame of weights. If `f_ag` does not require accept weights, set to `"none"`. Alternatively, can be the
#' name of a weight set found in `.$Meta$Weights`. This can also be specified as a list specifying the aggregation weights for each
#' level, in the same way as the previous parameters.
#' @param f_ag_para Optional parameters to pass to `f_ag`, other than `x` and `w`. As with `f_ag`, this can specified to have different
#' parameters for each aggregation level by specifying as a nested list of length `n-1`. See details.
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' within an aggregation group has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values. Can also be specified as a vector of length `n-1`, where `n` is
#' the number of levels in the index structure, to specify different data availability thresholds by level.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details. Can also be specified as a logical vector of length `n-1`, where `n` is
#' the number of levels in the index structure.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Aggregated"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse with new treated data sets added at `.$Data[[write_to]]` in each coin.
#' @export
#'
#' @examples
#' # build example purse up to normalised data set
#' purse <- build_example_purse(up_to = "Normalise", quietly = TRUE)
#'
#' # aggregate using defaults
#' purse <- Aggregate(purse, dset = "Normalised")
#'
Aggregate.purse <- function(x, dset, f_ag = NULL, w = NULL, f_ag_para = NULL, dat_thresh = NULL,
                             write_to = NULL, by_df = FALSE, ...){

  # input check
  check_purse(x)

  # apply unit screening to each coin
  x$coin <- lapply(x$coin, function(coin){
    Aggregate.coin(coin, dset, f_ag = f_ag, w = w, f_ag_para = f_ag_para, dat_thresh = dat_thresh,
                    out2 = "coin", write_to = write_to, by_df = by_df)
  })
  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}

.parse_level_identifier <- function(name, nlev){
  if(is.null(name) || !nzchar(name)){
    return(NA_integer_)
  }
  if(grepl("^\\d+$", name)){
    lev <- as.integer(name)
  } else if(grepl("^(Level|level|LEVEL)\\d+$", name)){
    lev <- as.integer(sub("^(Level|level|LEVEL)", "", name))
  } else if(grepl("^(L|l)\\d+$", name)){
    lev <- as.integer(sub("^(L|l)", "", name))
  } else {
    return(NA_integer_)
  }
  if(is.na(lev) || lev < 2 || lev > nlev){
    return(NA_integer_)
  }
  lev
}

.initialise_level_spec <- function(levels, default_value){
  out <- vector("list", length(levels))
  names(out) <- as.character(levels)
  for(ii in seq_along(levels)){
    out[[ii]] <- list(
      default = default_value,
      overrides = stats::setNames(vector("list", 0), character(0))
    )
  }
  out
}

.build_parent_lookup <- function(imeta){
  nlev <- max(imeta$Level, na.rm = TRUE)
  parents_by_level <- lapply(2:nlev, function(lev){
    parents <- unique(imeta$Parent[imeta$Level == (lev - 1)])
    parents[!is.na(parents)]
  })
  names(parents_by_level) <- as.character(2:nlev)
  parent_lookup <- unlist(
    lapply(names(parents_by_level), function(level_key){
      parents <- parents_by_level[[level_key]]
      if(length(parents) == 0){
        return(NULL)
      }
      stats::setNames(rep(as.integer(level_key), length(parents)), parents)
    }),
    use.names = TRUE
  )
  list(
    nlev = nlev,
    parents_by_level = parents_by_level,
    parent_lookup = parent_lookup
  )
}

.resolve_function_spec <- function(f_ag, nlev, parents_by_level, parent_lookup){
  level_keys <- as.integer(names(parents_by_level))
  resolved <- .initialise_level_spec(level_keys, "a_amean")

  if(is.null(f_ag)){
    return(resolved)
  }

  assign_default <- function(level, value){
    if(!is.character(value) || length(value) != 1){
      stop("Default aggregation functions must be supplied as single character strings.")
    }
    resolved[[as.character(level)]]$default <<- value
  }
  add_override <- function(level, parent, value){
    if(!is.character(value) || length(value) != 1){
      stop("Parent-specific aggregation functions must be supplied as single character strings.")
    }
    resolved[[as.character(level)]]$overrides[[parent]] <<- value
  }

  if(is.character(f_ag)){
    if(length(f_ag) == 1){
      for(lev in level_keys){
        assign_default(lev, f_ag)
      }
      return(resolved)
    }
    if(length(f_ag) == length(level_keys)){
      for(ii in seq_along(level_keys)){
        assign_default(level_keys[ii], f_ag[ii])
      }
      return(resolved)
    }
    stop("If f_ag is a character vector it must be length 1 or number of aggregation levels minus one.")
  }

  if(is.list(f_ag) &&
     length(f_ag) == length(level_keys) &&
     all(vapply(f_ag, function(x) is.character(x) && length(x) == 1, logical(1))) &&
     (is.null(names(f_ag)) || all(names(f_ag) == ""))){
    for(ii in seq_along(level_keys)){
      assign_default(level_keys[ii], f_ag[[ii]])
    }
    return(resolved)
  }

  if(!is.list(f_ag)){
    stop("Unrecognised input for f_ag. Supply a character vector, or a list describing level and parent overrides.")
  }

  entries <- f_ag
  entry_names <- names(entries)
  if(is.null(entry_names)){
    stop("When supplying a list for f_ag with overrides, each element must be named.")
  }

  default_idx <- which(tolower(entry_names) %in% c("default", "defaults"))
  if(length(default_idx) > 1){
    stop("Only one 'default' entry may be supplied in f_ag.")
  }
  if(length(default_idx) == 1){
    default_fun <- entries[[default_idx]]
    for(lev in level_keys){
      assign_default(lev, default_fun)
    }
    entries[[default_idx]] <- NULL
    entry_names <- names(entries)
  }

  for(ii in seq_along(entries)){
    name_i <- entry_names[ii]
    value_i <- entries[[ii]]
    lev <- .parse_level_identifier(name_i, nlev)

    if(!is.na(lev)){
      if(is.character(value_i)){
        assign_default(lev, value_i)
        next
      }
      if(!is.list(value_i)){
        stop("Level-specific entries in f_ag must be character strings or lists.")
      }

      level_parents <- parents_by_level[[as.character(lev)]]
      level_names <- names(value_i)
      working_list <- value_i

      if(length(working_list) > 0 && (is.null(level_names) || level_names[1] == "")){
        assign_default(lev, working_list[[1]])
        working_list <- working_list[-1]
        level_names <- names(working_list)
      }

      if(length(working_list) > 0){
        default_idx_lvl <- which(tolower(level_names) %in% c("default", "defaults"))
        if(length(default_idx_lvl) > 1){
          stop("Only one 'default' entry may be supplied inside each level entry of f_ag.")
        }
        if(length(default_idx_lvl) == 1){
          assign_default(lev, working_list[[default_idx_lvl]])
          working_list <- working_list[-default_idx_lvl]
          level_names <- names(working_list)
        }

        if(length(working_list) > 0){
          if(is.null(level_names) || any(level_names == "")){
            stop("Parent overrides inside each level of f_ag must be named.")
          }
          unknown_parents <- setdiff(level_names, level_parents)
          if(length(unknown_parents) > 0){
            stop("Unrecognised parent codes in f_ag for level ", lev, ": ", toString(unknown_parents))
          }
          for(jj in seq_along(working_list)){
            add_override(lev, level_names[jj], working_list[[jj]])
          }
        }
      }
    } else {
      parent <- name_i
      lev_for_parent <- parent_lookup[[parent]]
      if(is.na(lev_for_parent)){
        stop("Entry '", parent, "' in f_ag does not match any parent code in the hierarchy.")
      }
      add_override(lev_for_parent, parent, value_i)
    }
  }

  resolved
}

.resolve_parameter_spec <- function(f_ag_para, nlev, parents_by_level, parent_lookup){
  level_keys <- as.integer(names(parents_by_level))
  resolved <- .initialise_level_spec(level_keys, NULL)

  if(is.null(f_ag_para)){
    return(resolved)
  }
  if(!is.list(f_ag_para)){
    stop("f_ag_para must be NULL or a list.")
  }

  assign_default <- function(level, value){
    if(!is.null(value) && !is.list(value)){
      stop("Default parameter entries must be NULL or lists.")
    }
    resolved[[as.character(level)]]$default <<- value
  }
  add_override <- function(level, parent, value){
    if(!is.null(value) && !is.list(value)){
      stop("Parameter overrides must be NULL or lists.")
    }
    resolved[[as.character(level)]]$overrides[[parent]] <<- value
  }

  if(length(f_ag_para) == 1){
    entry_name <- names(f_ag_para)
    is_level_name <- !is.null(entry_name) && !is.na(.parse_level_identifier(entry_name, nlev))
    is_parent_name <- !is.null(entry_name) && entry_name %in% names(parent_lookup)
    if(is.null(entry_name) || (!is_level_name && !is_parent_name)){
      for(lev in level_keys){
        assign_default(lev, f_ag_para)
      }
      return(resolved)
    }
  }

  if(length(f_ag_para) == length(level_keys) &&
     (is.null(names(f_ag_para)) || all(names(f_ag_para) == ""))){
    for(ii in seq_along(level_keys)){
      assign_default(level_keys[ii], f_ag_para[[ii]])
    }
    return(resolved)
  }

  entries <- f_ag_para
  entry_names <- names(entries)
  if(is.null(entry_names)){
    stop("When supplying a list for f_ag_para with overrides, each element must be named.")
  }

  default_idx <- which(tolower(entry_names) %in% c("default", "defaults"))
  if(length(default_idx) > 1){
    stop("Only one 'default' entry may be supplied in f_ag_para.")
  }
  if(length(default_idx) == 1){
    for(lev in level_keys){
      assign_default(lev, entries[[default_idx]])
    }
    entries[[default_idx]] <- NULL
    entry_names <- names(entries)
  }

  for(ii in seq_along(entries)){
    name_i <- entry_names[ii]
    value_i <- entries[[ii]]
    lev <- .parse_level_identifier(name_i, nlev)

    if(!is.na(lev)){
      if(is.null(value_i)){
        assign_default(lev, NULL)
        next
      }
      if(!is.list(value_i)){
        stop("Level entries in f_ag_para must be NULL or lists.")
      }

      level_parents <- parents_by_level[[as.character(lev)]]
      level_names <- names(value_i)
      working_list <- value_i

      if(length(working_list) > 0 && (is.null(level_names) || level_names[1] == "")){
        assign_default(lev, working_list[[1]])
        working_list <- working_list[-1]
        level_names <- names(working_list)
      }

      if(length(working_list) > 0){
        default_idx_lvl <- which(tolower(level_names) %in% c("default", "defaults"))
        if(length(default_idx_lvl) > 1){
          stop("Only one 'default' entry may be supplied inside each level entry of f_ag_para.")
        }
        if(length(default_idx_lvl) == 1){
          assign_default(lev, working_list[[default_idx_lvl]])
          working_list <- working_list[-default_idx_lvl]
          level_names <- names(working_list)
        }
      }

      if(length(working_list) == 0){
        next
      }

      if(is.null(level_names) || any(level_names == "")){
        assign_default(lev, working_list)
        next
      }

      unknown_parents <- setdiff(level_names, level_parents)
      if(length(unknown_parents) == length(level_names)){
        assign_default(lev, working_list)
        next
      }
      if(length(unknown_parents) > 0){
        stop("Unrecognised parent codes in f_ag_para for level ", lev, ": ", toString(unknown_parents))
      }
      for(jj in seq_along(working_list)){
        add_override(lev, level_names[jj], working_list[[jj]])
      }
    } else {
      parent <- name_i
      lev_for_parent <- parent_lookup[[parent]]
      if(is.na(lev_for_parent)){
        stop("Entry '", parent, "' in f_ag_para does not match any parent code in the hierarchy.")
      }
      add_override(lev_for_parent, parent, value_i)
    }
  }

  resolved
}


#' Aggregate indicators in a coin
#'
#' Aggregates a named data set specified by `dset` using aggregation function(s) `f_ag`, weights `w`, and optional
#' function parameters `f_ag_para`. Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends.
#'
#' When `by_df = FALSE`, aggregation is performed row-wise using the function `f_ag`, such that for each row `x_row`, the output is
#' `f_ag(x_row, f_ag_para)`, and for the whole data frame, it outputs a numeric vector. Otherwise if `by_df = TRUE`,
#' the entire data frame of each indicator group is passed to `f_ag`.
#'
#' The function `f_ag` must be supplied as a string, e.g. `"a_amean"`, and it must take as a minimum an input
#' `x` which is either a numeric vector (if `by_df = FALSE`), or a data frame (if `by_df = TRUE`). In the former
#' case `f_ag` should return a single numeric value (i.e. the result of aggregating `x`), or in the latter case
#' a numeric vector (the result of aggregating the whole data frame in one go).
#'
#' Weights are passed to the function `f_ag` as an argument named `w`. This means that the function should have
#' arguments that look like `f_ag(x, w, ...)`, where `...` are possibly other input arguments to the function. If the
#' aggregation function doesn't use weights, you can set `w = "none"`, and no weights will be passed to it.
#'
#' `f_ag` can optionally have other parameters, apart from `x` and `w`, specified as a list in `f_ag_para`.
#'
#' The aggregation specifications can be set to be different for each level of aggregation: the arguments `f_ag`,
#' `f_ag_para`, `dat_thresh`, `w` and `by_df` can all be optionally specified as vectors or lists of length n-1, where
#' n is the number of levels in the index. In this case, the first value in each vector/list will be used for the first
#' round of aggregation, i.e. from indicators to the aggregates at level 2. The next will be used to aggregate from
#' level 2 to level 3, and so on.
#'
#' When different functions are used for different levels, it is important to get the list syntax correct. For example, in a case with
#' three aggregations using different functions, say we want to use `a_amean()` for the first two levels, then a custom
#' function `f_cust()` for the last. `f_cust()` has some additional parameters `a` and `b`. In this case, we would specify e.g.
#' `f_ag_para = list(NULL, NULL, list(a = 2, b = 3))` - this is becauase `a_amean()` requires no additional parameters, so
#' we pass `NULL`.
#'
#' Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends. To see a list browse COINr functions alphabetically or
#' type `a_` in the R Studio console and press the tab key (after loading COINr), or see the [online documentation](https://bluefoxr.github.io/COINr/articles/aggregate.html#coinr-aggregation-functions).
#'
#' Optionally, a data availability threshold can be assigned below which the aggregated value will return
#' `NA` (see `dat_thresh` argument). If `by_df = TRUE`, this will however be ignored because aggregation is not
#' done on individual rows. Note that more complex constraints could be built into `f_ag` if needed.
#'
#' @param x A `coin` object for `Aggregate.coin()` or an `unbalanced_coin` built with [new_unbalanced_coin()] for
#' `Aggregate.unbalanced_coin()`.
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param f_ag The name of an aggregation function, a string. This can either be a single string naming
#' a function to use for all aggregation levels, or else a character vector of function names of length `n-1`, where `n` is
#' the number of levels in the index structure. In this latter case, a different aggregation function may be used for each level
#' in the index: the first in the vector will be used to aggregate from Level 1 to Level 2, the second from Level 2 to Level 3, and
#' so on.
#' @param w An optional data frame of weights. If `f_ag` does not require accept weights, set to `"none"`. Alternatively, can be the
#' name of a weight set found in `.$Meta$Weights`. This can also be specified as a list specifying the aggregation weights for each
#' level, in the same way as the previous parameters.
#' @param f_ag_para Optional parameters to pass to `f_ag`, other than `x` and `w`. As with `f_ag`, this can specified to have different
#' parameters for each aggregation level by specifying as a nested list of length `n-1`. See details.
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' within an aggregation group has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values. Can also be specified as a vector of length `n-1`, where `n` is
#' the number of levels in the index structure, to specify different data availability thresholds by level.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details. Can also be specified as a logical vector of length `n-1`, where `n` is
#' the number of levels in the index structure.
#' @param out2 For `Aggregate.coin()`, either `"coin"` (default) to return the updated coin or `"df"` to output the
#' aggregated data set. For `Aggregate.unbalanced_coin()`, either `"unbalanced_coin"` (default) to retain the class or
#' `"df"`; using `"coin"` is not allowed because placeholders would be lost.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Aggregated"`.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # build example up to normalised data set
#' coin <- build_example_coin(up_to = "Normalise")
#'
#' # aggregate normalised data set
#' coin <- Aggregate(coin, dset = "Normalised")
#'
#' @return For `Aggregate.coin()`, an updated `coin` with aggregated data at `.$Data[[write_to]]` when `out2 = "coin"`,
#' or a data frame when `out2 = "df"`. For `Aggregate.unbalanced_coin()`, an updated `unbalanced_coin` when
#' `out2 = "unbalanced_coin"` or a data frame when `out2 = "df"`.
#'
#' @export
Aggregate.coin <- function(x, dset, f_ag = NULL, w = NULL, f_ag_para = NULL, dat_thresh = NULL,
                            by_df = FALSE, out2 = "coin", write_to = NULL, ...){

  # Write to Log ------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")


  # Check and set by_df -----------------------------------------------------

  nlev <- max(coin$Meta$Ind$Level, na.rm = TRUE)

  stopifnot(is.logical(by_df))

  if(length(by_df) == 1){
    by_dfs <- rep(by_df, nlev-1)
  } else if (length(by_df) != (nlev -1)) {
    stop("by_df must have either length 1 (same for all levels) or length equal to (number of levels - 1), in your case: ", nlev-1)
  } else {
    by_dfs <- by_df
  }

  # CHECK AND SET w ---------------------------------------------------------

  # If weights are supplied we have to see what kind of thing it is. This gets
  # a bit messy due to the number of different input types that w can take, including
  # varying by level.

  # NULL indicates that we should use metadata weights
  if(!is.null(w)){

    # case where weights is invariant at all levels
    if(length(w) == 1 || is.data.frame(w)){

      if(is.data.frame(w)){

        stopifnot(exists("iCode", w),
                  exists("Weight", w))
        w1 <- rep(list(w), nlev - 1)

      } else if(is.character(w)){

        if(w != "none"){

          # we look for a named weight set
          w1 <- coin$Meta$Weights[[w]]
          if(is.null(w1)){
            stop("Weight set with name '", w, "' not found in .$Meta$Weights.")
          }
          stopifnot(is.data.frame(w1),
                    exists("iCode", w1),
                    exists("Weight", w1))
          # copy for all levels
          w1 <- rep(list(w1), nlev - 1)

        } else {
          # convert w1 to NULL - means no weights will be passed to function
          w1 <- rep(list(NULL), nlev - 1)
          #w1 <- NULL
        }
      } else {
        stop("w must be either a string indicating a name of a weight set, or a data frame of weights, or 'none', or NULL (to use weights from metadata).")
      }

    } else {

      # w has been specified as a list of length > 1
      if(!is.list(w)){
        stop("If length(w) > 1, it must be specified as a list.")
      }
      if(length(w) != nlev - 1){
        stop("If w is specified as a list with length > 1, it must have length equal to the number of aggregation levels minus one.")
      }

      # Process each entry in the list separately
      w1 <- lapply(w, function(wi){

        if(is.data.frame(wi)){

          stopifnot(exists("iCode", wi),
                    exists("Weight", wi))

        } else if (is.character(wi)){

          if(wi != "none"){

            # we look for a named weight set
            wi <- coin$Meta$Weights[[wi]]
            if(is.null(wi)){
              stop("Weight set with name '", wi, "' not found in .$Meta$Weights.")
            }
            stopifnot(is.data.frame(wi),
                      exists("iCode", wi),
                      exists("Weight", wi))

          } else {
            # convert w1 to NULL - means no weights will be passed to function
            wi <- NULL
          }
        } else if (is.null(wi)){

          wi <- coin$Meta$Ind[c("iCode", "Weight")]

        } else {
          stop("Entries in w must be either a data frame of weights, name of weight set, 'none' or NULL.")
        }
        wi
      })

    }

  } else{
    # if w was NULL, get from metadata
    w1 <- rep(list(coin$Meta$Ind[c("iCode", "Weight")]), nlev - 1)
  }

  # from this point, w1 should be a list of either data frames or NULLs
  stopifnot(
    all(sapply(w1, is.data.frame) | sapply(w1, is.null))
  )

  # Other Prep --------------------------------------------------------------------

  if(is.null(dat_thresh)){
    dat_threshs <- rep(list(NULL), nlev - 1)
  } else {
    if(!is.numeric(dat_thresh)){
      stop("dat_thresh must be a numeric value or vector of length (number of levels - 1) - in your case: ", nlev)
    }
    if(any((dat_thresh < 0) | (dat_thresh > 1))){
      stop("dat_thresh must only contain numeric values between 0 and 1.")
    }
    if(length(dat_thresh) == 1){
      dat_threshs <- rep(dat_thresh, nlev - 1)
    } else {
      if(length(dat_thresh) != (nlev - 1)){
        stop("dat_thresh must have either length 1 (same for all levels) or length equal to number of levels - in your case: ", nlev)
      }
      dat_threshs <- dat_thresh
    }
  }

  # Aggregate ---------------------------------------------------------------
  # Here we apply the aggregation by level

  # get data (also performing checks)
  indat <- get_dset(coin, dset)
  # get metadata
  imeta <- coin$Meta$Ind[
    !is.na(coin$Meta$Ind$Level) &
      !is.na(coin$Meta$Ind$iCode) &
      coin$Meta$Ind$Type %in% c("Indicator", "Aggregate"),
    ]

  hierarchy <- .build_parent_lookup(imeta)
  stopifnot(identical(hierarchy$nlev, nlev))

  function_specs <- .resolve_function_spec(f_ag, hierarchy$nlev, hierarchy$parents_by_level, hierarchy$parent_lookup)
  parameter_specs <- .resolve_parameter_spec(f_ag_para, hierarchy$nlev, hierarchy$parents_by_level, hierarchy$parent_lookup)

  resolved_fun_list <- lapply(names(hierarchy$parents_by_level), function(level_key){
    parents <- hierarchy$parents_by_level[[level_key]]
    if(length(parents) == 0){
      return(NULL)
    }
    spec <- function_specs[[level_key]]
    data.frame(
      Level = rep(as.integer(level_key), length(parents)),
      Parent = parents,
      Function = vapply(parents, function(parent){
        override <- spec$overrides[[parent]]
        if(is.null(override)){
          spec$default
        } else {
          override
        }
      }, character(1)),
      stringsAsFactors = FALSE
    )
  })
  resolved_fun_list <- Filter(Negate(is.null), resolved_fun_list)
  resolved_fun_df <- if(length(resolved_fun_list) > 0){
    do.call(rbind, resolved_fun_list)
  } else {
    data.frame(Level = integer(0), Parent = character(0), Function = character(0), stringsAsFactors = FALSE)
  }

  resolved_param_list <- lapply(names(hierarchy$parents_by_level), function(level_key){
    parents <- hierarchy$parents_by_level[[level_key]]
    spec <- parameter_specs[[level_key]]
    if(length(parents) == 0){
      return(stats::setNames(vector("list", 0), character(0)))
    }
    out <- lapply(parents, function(parent){
      override <- spec$overrides[[parent]]
      if(is.null(override)){
        spec$default
      } else {
        override
      }
    })
    names(out) <- parents
    out
  })
  names(resolved_param_list) <- names(hierarchy$parents_by_level)

  if(!is.null(coin$Log$Aggregate)){
    coin$Log$Aggregate$resolved_f_ag <- resolved_fun_df
    coin$Log$Aggregate$resolved_f_ag_para <- resolved_param_list
  }

  indat_ag <- indat

  for(level_key in names(hierarchy$parents_by_level)){
    lev <- as.integer(level_key)
    parents <- hierarchy$parents_by_level[[level_key]]
    if(length(parents) == 0){
      next
    }

    imeta_l <- imeta[imeta$Level == (lev - 1), ]
    level_fun_spec <- function_specs[[level_key]]
    level_param_spec <- parameter_specs[[level_key]]
    weight_set <- w1[[lev - 1]]
    dat_thresh_val <- if(is.list(dat_threshs)) dat_threshs[[lev - 1]] else dat_threshs[lev - 1]
    if(length(dat_thresh_val) == 0){
      dat_thresh_val <- NULL
    }
    by_df_val <- by_dfs[lev - 1]

    level_results <- lapply(parents, function(parent_code){
      codes <- imeta_l$iCode[imeta_l$Parent == parent_code]
      fun_name <- level_fun_spec$overrides[[parent_code]]
      if(is.null(fun_name)){
        fun_name <- level_fun_spec$default
      }
      param_override <- level_param_spec$overrides[[parent_code]]
      params_to_use <- if(is.null(param_override)){
        level_param_spec$default
      } else {
        param_override
      }
      if(!is.null(weight_set)){
        wts <- weight_set$Weight[match(codes, weight_set$iCode)]
        params_to_use <- c(list(w = wts), params_to_use)
      }
      if(length(params_to_use) == 0){
        params_to_use <- NULL
      }
      do.call(
        "Aggregate",
        list(
          x = indat_ag[codes],
          f_ag = fun_name,
          f_ag_para = params_to_use,
          dat_thresh = dat_thresh_val,
          by_df = by_df_val
        )
      )
    })
    names(level_results) <- parents
    level_df <- as.data.frame(level_results, check.names = FALSE)
    indat_ag <- cbind(indat_ag, level_df)
  }

  # Output ------------------------------------------------------------------

  # output list
  if(out2 == "df"){
    indat_ag
  } else {
    if(is.null(write_to)){
      write_to <- "Aggregated"
    }
    write_dset(coin, indat_ag, dset = write_to)
  }


}


#' Aggregate data frame
#'
#' Aggregates a data frame into a single column using a specified function. Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends.
#'
#' Aggregation is performed row-wise using the function `f_ag`, such that for each row `x_row`, the output is
#' `f_ag(x_row, f_ag_para)`, and for the whole data frame, it outputs a numeric vector. The data frame `x` must
#' only contain numeric columns.
#'
#' The function `f_ag` must be supplied as a string, e.g. `"a_amean"`, and it must take as a minimum an input
#' `x` which is either a numeric vector (if `by_df = FALSE`), or a data frame (if `by_df = TRUE`). In the former
#' case `f_ag` should return a single numeric value (i.e. the result of aggregating `x`), or in the latter case
#' a numeric vector (the result of aggregating the whole data frame in one go).
#'
#' `f_ag` can optionally have other parameters, e.g. weights, specified as a list in `f_ag_para`.
#'
#' Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends. To see a list browse COINr functions alphabetically or
#' type `a_` in the R Studio console and press the tab key (after loading COINr), or see the [online documentation](https://bluefoxr.github.io/COINr/articles/aggregate.html#coinr-aggregation-functions).
#'
#' Optionally, a data availability threshold can be assigned below which the aggregated value will return
#' `NA` (see `dat_thresh` argument). If `by_df = TRUE`, this will however be ignored because aggregation is not
#' done on individual rows. Note that more complex constraints could be built into `f_ag` if needed.
#'
#' @param x Data frame to be aggregated
#' @param f_ag The name of an aggregation function, as a string.
#' @param f_ag_para Any additional parameters to pass to `f_ag`, as a named list.
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' of `x` has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # get some indicator data - take a few columns from built in data set
#' X <- ASEM_iData[12:15]
#'
#' # normalise to avoid zeros - min max between 1 and 100
#' X <- Normalise(X,
#'                global_specs = list(f_n = "n_minmax",
#'                                     f_n_para = list(l_u = c(1,100))))
#'
#' # aggregate using harmonic mean, with some weights
#' y <- Aggregate(X, f_ag = "a_hmean", f_ag_para = list(w = c(1, 1, 2, 1)))
#'
#' @return A numeric vector
#'
#' @export
Aggregate.data.frame <- function(x, f_ag = NULL, f_ag_para = NULL, dat_thresh = NULL,
                                  by_df = FALSE, ...){

  # CHECKS ------------------------------------------------------------------
  # x must be a df but check all numeric
  not_numeric <- !sapply(x, is.numeric)
  if(any(not_numeric)){
    stop("Non-numeric column(s) in x.")
  }

  if(!is.null(f_ag_para)){
    if(!is.list(f_ag_para)){
      stop("f_ag_para must be a list")
    }
  }

  # DEFAULTS ----------------------------------------------------------------

  # default mean of cols
  if(is.null(f_ag)){
    f_ag <- "a_amean"
    f_ag_para = list(w = rep(1, ncol(x)))
  }

  if(is.null(dat_thresh)){
    dat_thresh <- -1 # effectively no limit
  }

  # Resolve aggregation function and its arguments
  agg_fun <- match.fun(f_ag)
  fun_formals <- names(formals(agg_fun))
  if(is.null(fun_formals)){
    fun_formals <- character(0)
  }
  fun_formals_no_dots <- fun_formals[fun_formals != "..."]
  data_arg_name <- if(length(fun_formals_no_dots) > 0){
    fun_formals_no_dots[[1]]
  } else {
    ""
  }
  accepts_w <- "w" %in% fun_formals

  set_data_arg <- function(val){
    if(is.null(data_arg_name) || data_arg_name == ""){
      list(val)
    } else {
      stats::setNames(list(val), data_arg_name)
    }
  }

  filter_params <- function(params){
    if(is.null(params)){
      return(NULL)
    }
    out <- params
    if(!accepts_w && is.list(out) && length(out) > 0){
      nms <- names(out)
      if(!is.null(nms)){
        out <- out[nms != "w"]
      }
    }
    out
  }

  filtered_para <- filter_params(f_ag_para)

  # AGGREGATE ---------------------------------------------------------------

  lx <- ncol(x)

  # call aggregation function

  if(by_df){

    # DATA FRAME AGGRGATION
    call_args <- set_data_arg(x)
    y <- do.call(f_ag, c(call_args, filtered_para))

  } else {

    # BY-ROW AGGREGATION
    y <- apply(x, 1, function(x_row){
      if(sum(!is.na(x_row))/lx < dat_thresh){
        NA
      } else {
        row_args <- set_data_arg(x_row)
        do.call(f_ag, c(row_args, filtered_para))
      }
    })

  }


  if(!is.numeric(y)){
    if(all(is.na(y))){
      # if we get all NAs, this comes back as a logical vector, so convert
      y <- as.numeric(y)
    } else {
      stop("The output of f_ag has not successfully created a numeric vector.")
    }
  }
  if(length(y) != nrow(x)){
    stop("The ouput of f_ag is not the same length as nrow(x).")
  }

  y
}

#' Aggregate data
#'
#' Methods for aggregating numeric vectors, data frames, coins and purses. See individual method documentation
#' for more details:
#'
#' * [Aggregate.data.frame()]
#' * [Aggregate.coin()]
#' * [Aggregate.unbalanced_coin()]
#' * [Aggregate.purse()]
#'
#' @param x Object to be aggregated
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' # see individual method documentation
#'
#' @return An object similar to the input
#'
#' @export
Aggregate <- function(x, ...){
  UseMethod("Aggregate")
}


#' Weighted arithmetic mean
#'
#' The vector of weights `w` is relative since the formula is:
#'
#' \deqn{ y = \frac{1}{\sum w_i} \sum w_i x_i }
#'
#' If `x` contains `NA`s, these `x` values and the corresponding `w` values are removed before applying the
#' formula above.
#'
#' @param x A numeric vector.
#' @param w A vector of numeric weights of the same length as `x`.
#'
#' @examples
#' x <- c(1:10)
#' w <- c(10:1)
#' a_amean(x,w)
#'
#' @return The weighted mean as a scalar value
#'
#' @export
a_amean <- function(x, w){

  # Checks
  stopifnot(is.numeric(x),
            is.numeric(w),
            length(w) == length(x))

  if(any(is.na(w))){
    stop("w cannot contain NAs")
  }

  # remove w entries corresponding to NAs in x
  w <- w[!is.na(x)]
  # also x
  x <- x[!is.na(x)]

  if(length(x)==0){
    return(NA)
  }

  # w to sum to 1
  w <- w/sum(w)

  sum(w*x)

}

#' Weighted geometric mean
#'
#' Weighted geometric mean of a vector. `NA` are skipped by default.
#'
#' This function replaces the now-defunct `geoMean()` from COINr < v1.0.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # weighted geometric mean
#' a_gmean(x,w)
#'
#' @return The geometric mean, as a numeric value.
#'
#' @export

a_gmean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified for geometric mean, using equal weights.")
  }

  if(any(!is.na(x))){

    if(any((x <= 0), na.rm = TRUE)){
      stop("Negative or zero values found when applying geometric mean. This doesn't work because geometric
         mean uses log. Normalise to remove negative/zero values first or use another aggregation method.")}

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA
    # calculate geom mean
    gm <- exp( sum(w * log(x), na.rm = TRUE)/sum(w, na.rm = TRUE) )

  } else {
    gm <- NA
  }

  gm

}


#' Weighted harmonic mean
#'
#' Weighted harmonic mean of a vector. `NA` are skipped by default.
#'
#' This function replaces the now-defunct `harMean()` from COINr < v1.0.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # weighted harmonic mean
#' a_hmean(x,w)
#'
#' @return Weighted harmonic mean, as a numeric value.
#'
#' @export
a_hmean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified harmonic mean, using equal weights.")
  }

  if(any(!is.na(x))){

    if(any(x == 0, na.rm = TRUE)){
      stop("Zero values found when applying harmonic mean. This doesn't work because harmonic
         mean uses 1/x. Normalise to remove zero values first or use another aggregation method.")}

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA

    hm <- sum(w, na.rm = TRUE)/sum(w/x, na.rm = TRUE)

  } else {
    hm <- NA
  }

  hm

}


#' Weighted generalised mean
#'
#' Weighted generalised mean of a vector. `NA` are skipped by default.
#'
#' The generalised mean is as follows:
#'
#' \deqn{ y = \left( \frac{1}{\sum w_i} \sum w_i x_i^p  \right)^{1/p} }
#'
#' where `p` is a coefficient specified in the function argument here. Note that:
#'
#' - For negative `p`, all `x` values must be positive
#' - Setting `p = 0` will result in an error due to the negative exponent. This case
#' is equivalent to the geometric mean in the limit, so use [a_gmean()] instead.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#' @param p Coefficient - see details.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # cubic mean
#' a_genmean(x,w, p = 2)
#'
#' @return Weighted harmonic mean, as a numeric value.
#'
#' @export
a_genmean <- function(x, w = NULL, p){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified, using equal weights.")
  }

  if(p==0){
    stop("Setting p = 0 results in an infinite exponent. In the limit, this case is equal to the geometric mean: use a_gmean() instead.")
  }

  if(any(!is.na(x))){

    if(any(x <= 0, na.rm = TRUE) && p < 0){
      stop("Zero or negative values found when applying generalised mean with negative p: cannot be calculated.")
    }

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA

    gm <- (sum(w*x^p)/sum(w))^(1/p)

  } else {
    gm <- NA
  }

  gm

}



#' Outranking matrix
#'
#' Constructs an outranking matrix based on a data frame of indicator data and corresponding weights.
#'
#' @param X A data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A vector of weights, which should have length equal to `ncol(X)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # get a sample of a few indicators
#' ind_data <- COINr::ASEM_iData[12:16]
#' # calculate outranking matrix
#' outlist <- outrankMatrix(ind_data)
#' # see fraction of dominant pairs (robustness)
#' outlist$fracDominant
#'
#' @return A list with:
#' * `.$OutRankMatrix` the outranking matrix with `nrow(X)` rows and columns (matrix class).
#' * `.$nDominant` the number of dominance/robust pairs
#' * `.$fracDominant` the percentage of dominance/robust pairs
#'
#' @export

outrankMatrix <- function(X, w = NULL){

  stopifnot(is.data.frame(X) | is.matrix(X))

  if (!all(apply(X, 2, is.numeric))){
    stop("Non-numeric columns in input data frame or matrix not allowed.")
  }

  nInd <- ncol(X)
  nUnit <- nrow(X)

  if(is.null(w)){
    # default equal weights
    w <- rep(1,nInd)
    message("No weights specified for outranking matrix, using equal weights.")
  }

  # make w sum to 1
  w = w/sum(w, na.rm = TRUE)

  # prep outranking matrix
  orm <- matrix(NA, nrow = nUnit, ncol = nUnit)

  for (ii in 1:nUnit){

    # get iith row, i.e. the indicator values of unit ii
    rowii <- X[ii,]

    for (jj in 1:nUnit){

      if (ii==jj){
        # diag vals are zero
        orm[ii, jj] <- 0
      } else if (ii>jj){
        # to save time, only calc upper triangle of matrix. If lower triangle, do 1-upper
        orm[ii, jj] <- 1 - orm[jj, ii]
      } else {

        # get jjth row, i.e. the indicator values of unit jj
        rowjj <- X[jj,]

        # get score. Sum of weights where ii scores higher than jj, and half sum of weights where they are equal
        orm[ii, jj] <- sum(
          sum(w[rowii > rowjj], na.rm = TRUE),
          sum(w[rowii == rowjj], na.rm = TRUE)/2,
          na.rm = TRUE)

      }
    }
  }

  # find number of dominance pairs
  ndom <- sum(orm==1, na.rm = TRUE)
  npairs <- (nUnit^2 - nUnit)/2
  prcdom <- ndom/npairs

  list(
    OutRankMatrix = orm,
    nDominant = ndom,
    fracDominant = prcdom)

}

#' Copeland scores
#'
#' Aggregates a data frame of indicator values into a single column using the Copeland method.
#' This function calls `outrankMatrix()`.
#'
#' The outranking matrix is transformed as follows:
#'
#' * values > 0.5 are replaced by 1
#' * values < 0.5 are replaced by -1
#' * values == 0.5 are replaced by 0
#' * the diagonal of the matrix is all zeros
#'
#' The Copeland scores are calculated as the row sums of this transformed matrix.
#'
#' This function replaces the now-defunct `copeland()` from COINr < v1.0.
#'
#' @param X A numeric data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A numeric vector of weights, which should have length equal to `ncol(X)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # some example data
#' ind_data <- COINr::ASEM_iData[12:16]
#'
#' # aggregate with vector of weights
#' outlist <- outrankMatrix(ind_data)
#'
#' @return Numeric vector of Copeland scores.
#'
#' @export
a_copeland <- function(X, w = NULL){

  # get outranking matrix
  orm <- outrankMatrix(X, w)$OutRankMatrix

  orm[orm > 0.5] <- 1
  orm[orm < 0.5] <- -1
  orm[orm == 0.5] <- 0
  diag(orm) <- 0

  # get scores by summing across rows
  rowSums(orm, na.rm = TRUE)

  # outlist <- list(Scores = scores, OutRankMat = orm)
  # outlist
}
