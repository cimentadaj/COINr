# Unbalanced coin constructor and aggregation helpers

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
#' @inheritParams Aggregate.coin
#' @export
Aggregate.unbalanced_coin <- function(x, dset, ...){
  placeholders <- x$Meta$Unbalanced$PlaceholderCodes
  call <- match.call()
  out2 <- if("out2" %in% names(call)) eval(call$out2, parent.frame()) else "coin"
  write_to_name <- if("write_to" %in% names(call)) eval(call$write_to, parent.frame()) else NULL
  if(is.null(write_to_name)){
    write_to_name <- "Aggregated"
  }

  res <- NextMethod()

  if(is.data.frame(res)){
    if(length(placeholders) > 0){
      keep <- setdiff(names(res), placeholders)
      res <- res[keep]
    }
    return(res)
  }

  if(length(placeholders) > 0){
    if(!is.null(res$Data[[write_to_name]])){
      keep <- setdiff(names(res$Data[[write_to_name]]), placeholders)
      res$Data[[write_to_name]] <- res$Data[[write_to_name]][keep]
    }
  }
  res
}
