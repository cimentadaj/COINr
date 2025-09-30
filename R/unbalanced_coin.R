# Unbalanced coin constructor and aggregation helpers

.ensure_unbalanced_class <- function(res){
  if(!inherits(res, "coin")){
    stop("Expected a coin object when returning from unbalanced method.")
  }
  if(!is.null(res$Meta$Lineage_unbalanced)){
    res$Meta$Lineage <- res$Meta$Lineage_unbalanced
  }
  if(!is.null(res$Meta$Unbalanced$OriginalMeta$Level)){
    res$Meta$maxlev <- max(res$Meta$Unbalanced$OriginalMeta$Level, na.rm = TRUE)
  }
  class(res) <- unique(c("unbalanced_coin", class(res)))
  res
}


# internal helper: compute hierarchical levels (distance from leaves)
.compute_levels <- function(meta){
  codes <- meta$iCode
  types <- meta$Type
  # map parents to children
  child_map <- split(meta$iCode, meta$Parent)
  get_children <- function(code){
    children <- child_map[[code]]
    if(is.null(children)) character(0) else children
  }

  levels <- stats::setNames(rep(NA_real_, length(codes)), codes)
  # initialise leaves (no children)
  leaves <- codes[vapply(codes, function(code){ length(get_children(code)) == 0 }, logical(1))]
  levels[leaves] <- 1

  unresolved <- codes[is.na(levels) & types %in% c("Indicator", "Aggregate")]
  itr <- 0
  max_itr <- length(codes) * 5
  while(length(unresolved) > 0){
    progress <- FALSE
    for(code in unresolved){
      children <- get_children(code)
      if(length(children) == 0){
        levels[[code]] <- 1
        progress <- TRUE
      } else if(all(!is.na(levels[children]))){
        levels[[code]] <- max(levels[children]) + 1
        progress <- TRUE
      }
    }
    if(!progress){
      stop("Unable to resolve levels for unbalanced coin. Check for cycles or missing parents.")
    }
    unresolved <- codes[is.na(levels) & types %in% c("Indicator", "Aggregate")]
    itr <- itr + 1
    if(itr > max_itr){
      stop("Exceeded iteration limit while computing levels for unbalanced coin.")
    }
  }

  # ensure any remaining NA (non-structural types) stay NA
  levels
}

# helper to create placeholder codes uniquely
.make_placeholder_code <- function(child, parent, existing_codes, idx){
  base <- paste(child, parent, "ph", idx, sep = "_")
  code <- base
  counter <- 1
  while(code %in% existing_codes){
    counter <- counter + 1
    code <- paste(base, counter, sep = "_")
  }
  code
}

# helper to balance metadata by inserting placeholders
.balance_metadata <- function(meta){
  meta$Parent[meta$Parent == ""] <- NA_character_
  meta$Parent <- ifelse(is.na(meta$Parent), NA_character_, meta$Parent)
  levels_initial <- .compute_levels(meta)
  meta$Level <- ifelse(meta$Type %in% c("Indicator", "Aggregate"),
                       levels_initial[meta$iCode], meta$Level)

  placeholder_rows <- list()
  placeholder_codes <- character(0)
  placeholder_map <- list()
  existing_codes <- meta$iCode

  # ensure IsPlaceholder column exists for later flagging
  if(!"IsPlaceholder" %in% names(meta)){
    meta$IsPlaceholder <- FALSE
  }

  for(idx in seq_len(nrow(meta))){
    row <- meta[idx, , drop = FALSE]
    child <- row$iCode
    parent <- row$Parent

    if(is.na(parent) || !(row$Type %in% c("Indicator", "Aggregate"))){
      next
    }
    if(parent %nin% existing_codes){
      stop(sprintf("Parent '%s' referenced by '%s' not found in metadata.", parent, child))
    }
    lvl_child <- levels_initial[[child]]
    lvl_parent <- levels_initial[[parent]]
    if(is.na(lvl_parent) || is.na(lvl_child)){
      next
    }
    gap <- lvl_parent - lvl_child - 1
    if(gap <= 0){
      next
    }
    # build placeholder chain from parent down to child
    prev_parent <- parent
    holder_codes <- character(gap)
    for(k in seq_len(gap)){
      ph_code <- .make_placeholder_code(child, parent, existing_codes, k)
      holder_codes[k] <- ph_code
      ph_row <- row
      for(col in names(ph_row)){ ph_row[[col]] <- NA }
      ph_row$iCode <- ph_code
      ph_row$Parent <- prev_parent
      ph_row$Type <- "Aggregate"
      ph_row$Weight <- ifelse(k == 1, row$Weight, 1)
      ph_row$Direction <- 1
      ph_row$Level <- NA
      ph_row$IsPlaceholder <- TRUE
      placeholder_rows[[length(placeholder_rows) + 1]] <- ph_row
      existing_codes <- c(existing_codes, ph_code)
      prev_parent <- ph_code
    }
    # reassign child to final placeholder in chain
    meta$Parent[idx] <- prev_parent
    meta$IsPlaceholder[idx] <- ifelse(meta$IsPlaceholder[idx], TRUE, FALSE)
    placeholder_codes <- c(placeholder_codes, holder_codes)
    placeholder_map[[child]] <- holder_codes
  }

  if(length(placeholder_rows) > 0){
    placeholder_df <- do.call(rbind, placeholder_rows)
    meta <- rbind(meta, placeholder_df)
  }

  # recompute levels now balanced
  levels_balanced <- .compute_levels(meta)
  meta$Level <- ifelse(meta$Type %in% c("Indicator", "Aggregate"),
                       levels_balanced[meta$iCode], meta$Level)
  meta$IsPlaceholder[is.na(meta$IsPlaceholder)] <- FALSE

  list(meta = meta,
       placeholders = unique(placeholder_codes),
       map = placeholder_map,
       levels_unbalanced = levels_initial,
       levels_balanced = levels_balanced)
}

#' Create a new unbalanced coin
#'
#' @inheritParams new_coin
#' @return An object of class `unbalanced_coin` that delegates to the standard coin pipeline
#'   while keeping track of placeholder nodes used to balance the hierarchy internally.
#' @export
new_unbalanced_coin <- function(iData, iMeta, exclude = NULL, split_to = NULL,
                                level_names = NULL, retain_all_uCodes_on_split = FALSE,
                                quietly = FALSE){

  stopifnot(is.data.frame(iData), is.data.frame(iMeta))
  required_cols <- c("iCode", "Parent", "Type")
  missing_cols <- setdiff(required_cols, names(iMeta))
  if(length(missing_cols) > 0){
    stop("iMeta is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  iMeta$iCode <- as.character(iMeta$iCode)
  iMeta$Parent <- as.character(iMeta$Parent)
  iMeta$Parent[iMeta$Parent == ""] <- NA_character_
  iMeta$Type <- as.character(iMeta$Type)

  balanced <- .balance_metadata(iMeta)
  meta_balanced <- balanced$meta
  placeholder_codes <- balanced$placeholders
  placeholder_map <- balanced$map
  levels_unbalanced <- balanced$levels_unbalanced

  meta_unbalanced <- iMeta
  meta_unbalanced$Level <- ifelse(meta_unbalanced$Type %in% c("Indicator", "Aggregate"),
                                  levels_unbalanced[meta_unbalanced$iCode],
                                  meta_unbalanced$Level)

  lineage_unbalanced <- get_lineage(meta_unbalanced, level_names = level_names)

  coin <- new_coin(iData, meta_balanced, exclude = exclude, split_to = split_to,
                   level_names = NULL, retain_all_uCodes_on_split = retain_all_uCodes_on_split,
                   quietly = quietly)

  coin$Meta$Unbalanced <- list(
    OriginalMeta = meta_unbalanced,
    BalancedMeta = meta_balanced,
    PlaceholderCodes = placeholder_codes,
    PlaceholderMap = placeholder_map
  )
  coin$Meta$Lineage_balanced <- coin$Meta$Lineage
  coin$Meta$Lineage_unbalanced <- lineage_unbalanced
  coin$Meta$Lineage <- lineage_unbalanced
  coin$Meta$maxlev_balanced <- max(meta_balanced$Level, na.rm = TRUE)
  coin$Meta$maxlev <- max(meta_unbalanced$Level, na.rm = TRUE)

  class(coin) <- unique(c("unbalanced_coin", class(coin)))
  coin
}

#' @export
print.unbalanced_coin <- function(x, ...){
  NextMethod()

  meta <- x$Meta$Unbalanced$OriginalMeta
  if(!is.null(meta)){
    lineage <- x$Meta$Lineage
    depth_counts <- apply(lineage, 1, function(row) sum(!is.na(row)))
    depth_summary <- table(depth_counts)
    cat("\nUnbalanced hierarchy summary:\n")
    for(ii in seq_along(depth_summary)){
      d <- names(depth_summary)[ii]
      codes <- lineage$Level_1[depth_counts == as.numeric(d)]
      cat(sprintf("  Depth %s: %s nodes (%s)\n",
                  d,
                  as.integer(depth_summary[[ii]]),
                  paste(codes, collapse = ", ")))
    }
    if(length(unique(depth_counts)) > 1){
      cat(sprintf("  Depth range: %s-%s levels\n",
                  min(depth_counts), max(depth_counts)))
    }
  }

  invisible(x)
}

#' Aggregate method for unbalanced coins
#'
#' @param x,dset,f_ag,w,f_ag_para,dat_thresh,by_df,out2,write_to,... See [Aggregate.coin()].
#' @describeIn Aggregate.coin Wrapper that preserves unbalanced hierarchies.
#' @details Compared with [Aggregate.coin()], this method keeps the public lineage and maximum level of the unbalanced hierarchy while delegating the computation to the balanced implementation. Requests for `out2 = "coin"` are disallowed because that would discard the unbalanced class tagging.
#' @examplesIf requireNamespace("COINr", quietly = TRUE)
#' data("unbal_iData", package = "COINr")
#' data("unbal_iMeta", package = "COINr")
#' unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
#' Aggregate(unbal, dset = "Raw")
#' @export
Aggregate.unbalanced_coin <- function(x, dset, f_ag = NULL, w = NULL, f_ag_para = NULL, dat_thresh = NULL,
                                      by_df = FALSE, out2 = "unbalanced_coin", write_to = NULL, ...) {
  placeholders <- x$Meta$Unbalanced$PlaceholderCodes
  call <- match.call()
  out2 <- if("out2" %in% names(call)) eval(call$out2, parent.frame()) else "unbalanced_coin"
  if(identical(out2, "coin"))
    stop("Set out2 = 'unbalanced_coin' to retain the unbalanced object or use 'df' for a data frame output.")
  next_out2 <- if(identical(out2, "unbalanced_coin")) "coin" else out2
  write_to_name <- if("write_to" %in% names(call)) eval(call$write_to, parent.frame()) else NULL
  if(is.null(write_to_name)){
    write_to_name <- "Aggregated"
  }

  res <- NextMethod(out2 = next_out2)

  if(is.data.frame(res)){
    if(length(placeholders) > 0){
      res <- res[setdiff(names(res), placeholders)]
    }
    return(res)
  }

  if(length(placeholders) > 0 && !is.null(res$Data[[write_to_name]])){
    keep <- setdiff(names(res$Data[[write_to_name]]), placeholders)
    res$Data[[write_to_name]] <- res$Data[[write_to_name]][keep]
  }

  if(identical(out2, "unbalanced_coin")){
    res <- .ensure_unbalanced_class(res)
  }
  res
}



#' @param x,dset,f_i,f_i_para,impute_by,use_group,group_level,normalise_first,out2,write_to,disable,warn_on_NAs,... See [Impute.coin()].
#' @describeIn Impute.coin Wrapper that retains the unbalanced structure.
#' @details In addition to the behaviour of [Impute.coin()], this method keeps the unbalanced metadata view and rejects `out2 = "coin"` to avoid dropping the `unbalanced_coin` class.
#' @examplesIf requireNamespace("COINr", quietly = TRUE)
#' data("unbal_iData", package = "COINr")
#' data("unbal_iMeta", package = "COINr")
#' unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
#' unbal$Data$Raw$IndA1[2] <- NA
#' Impute(unbal, dset = "Raw")
#' @export
Impute.unbalanced_coin <- function(x, dset, f_i = NULL, f_i_para = NULL, impute_by = "column",
                                 use_group = NULL, group_level = NULL, normalise_first = NULL,
                                 out2 = "unbalanced_coin", write_to = NULL, disable = FALSE,
                                 warn_on_NAs = TRUE, ...) {
  placeholders <- x$Meta$Unbalanced$PlaceholderCodes
  call <- match.call()
  out2 <- if("out2" %in% names(call)) eval(call$out2, parent.frame()) else "unbalanced_coin"
  if(identical(out2, "coin"))
    stop("Set out2 = 'unbalanced_coin' to retain the unbalanced object or use 'df' for a data frame output.")
  next_out2 <- if(identical(out2, "unbalanced_coin")) "coin" else out2
  write_to_name <- if("write_to" %in% names(call)) eval(call$write_to, parent.frame()) else NULL
  if(is.null(write_to_name)){
    write_to_name <- "Imputed"
  }

  res <- NextMethod(out2 = next_out2)

  if(length(placeholders) == 0){
    if(identical(out2, "unbalanced_coin")) res <- .ensure_unbalanced_class(res)
    return(res)
  }

  if(is.data.frame(res)){
    res <- res[setdiff(names(res), placeholders)]
    return(res)
  }

  if(!is.null(res$Data[[write_to_name]])){
    keep <- setdiff(names(res$Data[[write_to_name]]), placeholders)
    res$Data[[write_to_name]] <- res$Data[[write_to_name]][keep]
  }

  if(identical(out2, "unbalanced_coin")){
    res <- .ensure_unbalanced_class(res)
  }
  res
}

#' @param x,dset,denoms,denomby,denoms_ID,f_denom,write_to,out2,... See [Denominate.coin()].
#' @describeIn Denominate.coin Wrapper that retains the unbalanced structure.
#' @details Compared with [Denominate.coin()], the unbalanced method blocks `out2 = "coin"` and keeps the restored lineage/max-level of the original unbalanced hierarchy while stripping internal placeholder nodes.
#' @examplesIf requireNamespace("COINr", quietly = TRUE)
#' data("unbal_iData", package = "COINr")
#' data("unbal_iMeta", package = "COINr")
#' unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
#' denoms <- data.frame(uCode = unbal_iData$uCode, DenSub = c(2, 4, 5), DenB = c(10, 12, 15))
#' specs <- data.frame(
#'   iCode = c("IndA1", "IndA2", "IndB"),
#'   Denominator = c("DenSub", "DenSub", "DenB"),
#'   ScaleFactor = 1
#' )
#' Denominate(unbal, dset = "Raw", denoms = denoms, denomby = specs)
#' @export
Denominate.unbalanced_coin <- function(x, dset, denoms = NULL, denomby = NULL, denoms_ID = NULL,
                                        f_denom = NULL, write_to = NULL, out2 = "unbalanced_coin", ...) {
  placeholders <- x$Meta$Unbalanced$PlaceholderCodes
  call <- match.call()
  out2 <- if("out2" %in% names(call)) eval(call$out2, parent.frame()) else "unbalanced_coin"
  if(identical(out2, "coin"))
    stop("Set out2 = 'unbalanced_coin' to retain the unbalanced object or use 'df' for a data frame output.")
  next_out2 <- if(identical(out2, "unbalanced_coin")) "coin" else out2
  write_to_name <- if("write_to" %in% names(call)) eval(call$write_to, parent.frame()) else NULL
  if(is.null(write_to_name)){
    write_to_name <- "Denominated"
  }

  res <- NextMethod(out2 = next_out2)

  if(length(placeholders) == 0){
    if(identical(out2, "unbalanced_coin")) res <- .ensure_unbalanced_class(res)
    return(res)
  }

  if(is.data.frame(res)){
    res <- res[setdiff(names(res), placeholders)]
    return(res)
  }

  if(!is.null(res$Data[[write_to_name]])){
    keep <- setdiff(names(res$Data[[write_to_name]]), placeholders)
    res$Data[[write_to_name]] <- res$Data[[write_to_name]][keep]
  }

  if(identical(out2, "unbalanced_coin")){
    res <- .ensure_unbalanced_class(res)
  }
  res
}

#' @param x,dset,global_specs,indiv_specs,combine_treat,out2,write_to,write2log,disable,... See [Treat.coin()].
#' @describeIn Treat.coin Wrapper that retains the unbalanced structure.
#' @details Compared with [Treat.coin()], this method keeps the unbalanced metadata perspective, strips placeholder nodes from returned data, and forbids `out2 = "coin"` to preserve the `unbalanced_coin` class.
#' @examplesIf requireNamespace("COINr", quietly = TRUE)
#' data("unbal_iData", package = "COINr")
#' data("unbal_iMeta", package = "COINr")
#' unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
#' Treat(unbal, dset = "Raw")
#' @export
Treat.unbalanced_coin <- function(x, dset, global_specs = NULL, indiv_specs = NULL,
                                  combine_treat = FALSE, out2 = "unbalanced_coin", write_to = NULL,
                                  write2log = TRUE, disable = FALSE, ...) {
  placeholders <- x$Meta$Unbalanced$PlaceholderCodes
  call <- match.call()
  out2 <- if("out2" %in% names(call)) eval(call$out2, parent.frame()) else "unbalanced_coin"
  if(identical(out2, "coin"))
    stop("Set out2 = 'unbalanced_coin' to retain the unbalanced object or use 'list' for analysis outputs.")
  next_out2 <- if(identical(out2, "unbalanced_coin")) "coin" else out2
  write_to_name <- if("write_to" %in% names(call)) eval(call$write_to, parent.frame()) else NULL
  if(is.null(write_to_name)){
    write_to_name <- "Treated"
  }

  res <- NextMethod(out2 = next_out2)

  if(length(placeholders) == 0){
    if(identical(out2, "unbalanced_coin") && inherits(res, "coin")) res <- .ensure_unbalanced_class(res)
    return(res)
  }

  if(is.list(res) && !inherits(res, "coin")){
    if(!is.null(res$x_treat)){
      keep <- setdiff(names(res$x_treat), placeholders)
      res$x_treat <- res$x_treat[keep]
    }
    return(res)
  }

  if(is.data.frame(res)){
    res <- res[setdiff(names(res), placeholders)]
    return(res)
  }

  if(!is.null(res$Data[[write_to_name]])){
    keep <- setdiff(names(res$Data[[write_to_name]]), placeholders)
    res$Data[[write_to_name]] <- res$Data[[write_to_name]][keep]
  }

  if(identical(out2, "unbalanced_coin")){
    res <- .ensure_unbalanced_class(res)
  }
  res
}

#' #' @param x,dset,unit_screen,dat_thresh,nonzero_thresh,Force,out2,write_to,... See [Screen.coin()].
#' @describeIn Screen.coin Wrapper that retains the unbalanced structure.
#' @details This method mirrors [Screen.coin()] but restores lineage/max-level for unbalanced hierarchies and rejects `out2 = "coin"`. Placeholder nodes are removed from all outward-facing outputs.
#' @examplesIf requireNamespace("COINr", quietly = TRUE)
#' data("unbal_iData", package = "COINr")
#' data("unbal_iMeta", package = "COINr")
#' unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
#' Screen(unbal, dset = "Raw", unit_screen = "byNA", dat_thresh = 0.9)
#' @export
Screen.unbalanced_coin <- function(x, dset, unit_screen, dat_thresh = NULL, nonzero_thresh = NULL,
                                   Force = NULL, out2 = "unbalanced_coin", write_to = NULL, ...) {
  placeholders <- x$Meta$Unbalanced$PlaceholderCodes
  call <- match.call()
  out2 <- if("out2" %in% names(call)) eval(call$out2, parent.frame()) else "unbalanced_coin"
  if(identical(out2, "coin"))
    stop("Set out2 = 'unbalanced_coin' to retain the unbalanced object or use 'list'/'df' for data outputs.")
  next_out2 <- if(identical(out2, "unbalanced_coin")) "coin" else out2
  write_to_name <- if("write_to" %in% names(call)) eval(call$write_to, parent.frame()) else NULL
  if(is.null(write_to_name)){
    write_to_name <- "Screened"
  }

  res <- NextMethod(out2 = next_out2)

  if(length(placeholders) == 0){
    if(identical(out2, "unbalanced_coin") && inherits(res, "coin")) res <- .ensure_unbalanced_class(res)
    return(res)
  }

  if(is.list(res) && !inherits(res, "coin")){
    if(!is.null(res$ScreenedData)){
      res$ScreenedData <- res$ScreenedData[setdiff(names(res$ScreenedData), placeholders)]
    }
    return(res)
  }

  if(is.data.frame(res)){
    res <- res[setdiff(names(res), placeholders)]
    return(res)
  }

  if(!is.null(res$Data[[write_to_name]])){
    keep <- setdiff(names(res$Data[[write_to_name]]), placeholders)
    res$Data[[write_to_name]] <- res$Data[[write_to_name]][keep]
  }

  if(identical(out2, "unbalanced_coin")){
    res <- .ensure_unbalanced_class(res)
  }
  res
}
