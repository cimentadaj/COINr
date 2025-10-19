
#' Sensitivity and uncertainty analysis of a coin
#'
#' This function performs global sensitivity and uncertainty analysis of a coin. You must specify which
#' parameters of the coin to vary, and the alternatives/distributions for those parameters.
#'
#' COINr implements a flexible variance-based global sensitivity analysis approach, which allows almost any assumption
#' to be varied, as long as the distribution of alternative values can be described. Variance-based "sensitivity indices"
#' are estimated using a Monte Carlo design (running the composite indicator many times with a particular combination of
#' input values). This follows the methodology described in \doi{10.1111/j.1467-985X.2005.00350.x}.
#'
#' To understand how this function works, please see `vignette("sensitivity")`. Here, we briefly recap the main input
#' arguments.
#'
#' First, you can select whether to run an uncertainty analysis `SA_type = "UA"` or sensitivity analysis `SA_type = "SA"`.
#' The number of replications (regenerations of the coin) is specified by `N`. Keep in mind that the *total* number of
#' replications is `N` for an uncertainty analysis but is `N*(d + 2)` for a sensitivity analysis due to the experimental
#' design used.
#'
#' To run either types of analysis, you must specify *which* parts of the coin to vary and *what the distributions/alternatives are*
#' This is done using `SA_specs`, a structured list. See `vignette("sensitivity")` for details and examples.
#'
#' You also need to specify the target of the sensitivity analysis. This should be an indicator or aggregate that can be
#' found in one of the data sets of the coin, and is specified using the `dset` and `iCode` arguments.
#'
#' If `SA_type = "SA"`, it is advisable to set `Nboot` to e.g. 100 or more, which is the number of bootstrap samples
#' to take when estimating confidence intervals on sensitivity indices. This does *not* perform extra regenerations of the
#' coin, so setting this to a higher number shouldn't have much impact on computational time.
#'
#' If you want to understand what is going on more deeply in the regenerated coins in the sensitivity analysis, set
#' `diagnostic_mode = TRUE` in `get_sensitivity()`. This will additionally output a list containing every coin that was generated
#' as part of the sensitivity analysis, and allows you to check in detail whether the coins are generated as you expect.
#' Clearly it is better to run this on a low sample size as the output can potentially become quite large.
#'
#' This function replaces the now-defunct `sensitivity()` from COINr < v1.0.
#'
#' @details
#' When `coin` inherits from `unbalanced_coin`, the method delegates to the balanced representation
#' used internally by [new_unbalanced_coin()] and removes placeholder helper nodes from all tabular
#' outputs. Any diagnostic coins stored in the result (see `diagnostic_mode`) are re-tagged with the
#' unbalanced class before returning so that downstream tooling continues to operate on the original
#' hierarchy.
#'
#' @param coin A coin
#' @param SA_specs Specifications of the input uncertainties
#' @param N The number of regenerations
#' @param SA_type The type of analysis to run. `"UA"` runs an uncertainty analysis. `"SA"` runs a sensitivity
#' analysis (which anyway includes an uncertainty analysis).
#' @param dset The data set to extract the target variable from (passed to [get_data()]).
#' @param iCode The variable within `dset` to use as the target variable (passed to [get_data()]).
#' @param quietly Set to `TRUE` to suppress progress messages.
#' @param Nboot Number of bootstrap samples to take when estimating confidence intervals on sensitivity
#' indices.
#' @param check_addresses Logical: if `FALSE` skips the check of the validity of the parameter addresses. Default `TRUE`,
#' but useful to set to `FALSE` if running this e.g. in a Rmd document (because may require user input).
#' @param diagnostic_mode Logical: if `TRUE`, this will additionally attach all coins generated as part of the sensitivity
#' analysis to the output list. This is intended to be used to check what is going on within the sensitivity analysis.
#'
#' @importFrom stats runif
#'
#' @return Sensitivity analysis results as a list, containing:
#' * `.$Scores` a data frame with a row for each unit, and columns are the scores for each replication.
#' * `.$Ranks` as `.$Scores` but for unit ranks
#' * `.$RankStats` summary statistics for ranks of each unit
#' * `.$Para` a list containing parameter values for each run
#' * `.$Nominal` the nominal scores and ranks of each unit (i.e. from the original COIN)
#' * `.$Sensitivity` (only if `SA_type = "SA"`) sensitivity indices for each parameter. Also confidence intervals if `Nboot`
#' * `.$coins` (only if `diagnostic_mode = TRUE`) a list of all coins generated during the sensitivity analysis
#' * Some information on the time elapsed, average time, and the parameters perturbed.
#' * Depending on the setting of `store_results`, may also contain a list of Methods or a list of COINs for each replication.
#'
#' @export
#'
#' @examples
#' # for examples, see `vignette("sensitivity")`
#' # (this is because package examples are run automatically and this function can
#' # take a few minutes to run at realistic settings)
#'
get_sensitivity <- function(coin, SA_specs, N, SA_type = "UA", dset, iCode, Nboot = NULL, quietly = FALSE,
                            check_addresses = TRUE, diagnostic_mode = FALSE){
  UseMethod("get_sensitivity")
}

if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("x", "xend", "y", "yend", "modified", "step_id", "label_wrapped"))
}

.get_sensitivity_impl <- function(coin, SA_specs, N, SA_type = "UA", dset, iCode, Nboot = NULL,
                                  quietly = FALSE, check_addresses = TRUE, diagnostic_mode = FALSE){

  t0 <- proc.time()

  check_coin_input(coin)
  stopifnot(is.list(SA_specs),
            is.numeric(N),
            length(N) == 1,
            N > 2,
            SA_type %in% c("SA", "UA"))

  check_specs <- sapply(SA_specs, function(li){
    !is.null(li$Address) & !is.null(li$Distribution) & !is.null(li$Type)
  })

  if(any(!check_specs)){
    stop("One or more entries in SA_specs is missing either the $Name, $Address or $Distribution entries.")
  }

  d <- length(SA_specs)

  if(SA_type == "UA"){
    XX <- matrix(stats::runif(d * N), nrow = N, ncol = d)
  } else {
    if(d == 1){
      stop("Only one uncertain input defined. It is not meaningful to run a sensitivity analysis\n      with only one input variable. Consider changing SA_type to \"UA\".")
    }
    XX <- SA_sample(N, d)
  }

  XX <- as.data.frame(XX)
  NT <- nrow(XX)

  XX_p <- mapply(function(x, spec){
    sample_2_para(x, distribution = spec$Distribution, dist_type = spec$Type)
  }, XX, SA_specs, SIMPLIFY = FALSE)
  names(XX_p) <- names(SA_specs)

  addresses <- sapply(SA_specs, `[[`, "Address")

  # determine earliest log stage affected by the specs so regeneration can restart later in the pipeline
  log_entries <- .get_log_entries(coin)
  log_sequence <- character()
  if(!is.null(log_entries)){
    log_sequence <- names(log_entries)
    log_sequence <- log_sequence[log_sequence != "can_regen"]
  }

  address_stage_index <- function(address){
    if(length(log_sequence) == 0){
      return(Inf)
    }
    match_expr <- regexpr("^\\$Log\\$([^\\$]+)", address, perl = TRUE)
    if(match_expr[1] == -1){
      return(Inf)
    }
    stage_name <- sub("^\\$Log\\$([^\\$]+).*", "\\1", address, perl = TRUE)
    idx <- match(stage_name, log_sequence, nomatch = NA_integer_)
    if(is.na(idx)){
      Inf
    } else {
      idx
    }
  }

  stage_indices <- vapply(addresses, address_stage_index, numeric(1))
  regen_from_idx <- suppressWarnings(min(stage_indices))
  normalise_idx <- match('Normalise', log_sequence)
  if(is.na(normalise_idx)){
    normalise_idx <- length(log_sequence)
  }
  if(!is.finite(regen_from_idx) || regen_from_idx <= normalise_idx){
    regen_from <- NULL
  } else {
    start_idx <- max(1, regen_from_idx - 1)
    if(start_idx <= 1 || start_idx > length(log_sequence)){
      regen_from <- NULL
    } else {
      regen_from <- log_sequence[start_idx]
      if(!quietly){
        message("Optimising regeneration: restarting from ", regen_from)
      }
    }
  }

  if(check_addresses){
    invisible(lapply(addresses, check_address, coin))
  }

  SA_scores <- get_data(coin, dset = dset, iCodes = iCode)

  v_fail <- SA_scores
  v_fail[names(v_fail) == iCode] <- NA

  names(SA_scores)[names(SA_scores) == iCode] <- "Nominal"

  if(diagnostic_mode){
    coin_list <- vector(mode = "list", length = NT)
  }

  coin_cache <- new.env(parent = emptyenv())
  cache_hits <- 0L

  for(irep in seq_len(NT)){

    l_para_rep <- lapply(XX_p, `[[`, irep)

    if(!quietly){
      message(paste0("Rep ", irep, " of ", NT, " ... ", round(irep * 100 / NT, 1), "% complete"))
    }

    cache_key <- rawToChar(serialize(l_para_rep, connection = NULL, ascii = TRUE))
    coin_success <- FALSE
    v_out <- v_fail
    coin_rep <- NULL

    if(exists(cache_key, envir = coin_cache, inherits = FALSE)){
      cache_entry <- get(cache_key, envir = coin_cache, inherits = FALSE)
      cache_hits <- cache_hits + 1L
      coin_success <- isTRUE(cache_entry$ok)
      v_out <- cache_entry$v_out
      if(diagnostic_mode){
        coin_rep <- cache_entry$coin
      }
    } else {
      coin_candidate <- regen_edit(l_para_rep, addresses, coin, regen_from = regen_from)
      coin_success <- is.coin(coin_candidate)
      if(coin_success){
        v_out <- get_data(coin_candidate, dset = dset, iCodes = iCode)
        stopifnot(setequal(colnames(v_out), c("uCode", iCode)))
      } else {
        v_out <- v_fail
      }
      stored_coin <- if(diagnostic_mode) coin_candidate else NULL
      cache_entry <- list(
        ok = coin_success,
        v_out = v_out,
        coin = stored_coin
      )
      assign(cache_key, cache_entry, envir = coin_cache)
      coin_rep <- stored_coin
      if(!diagnostic_mode){
        coin_candidate <- NULL
      }
    }

    if(diagnostic_mode){
      coin_list[[irep]] <- coin_rep
    }

    if(!coin_success){
      v_out <- v_fail
    }

    SA_scores <- merge(SA_scores, v_out, by = "uCode", all = TRUE)
    names(SA_scores)[names(SA_scores) == iCode] <- paste0("r_", irep)
  }

  if(!quietly && cache_hits > 0L){
    message("Reused ", cache_hits, " cached regenerations.")
  }

  SA_ranks <- rank_df(SA_scores)
  SA_ranks_ <- SA_ranks[names(SA_ranks) %nin% c("uCode", "Nominal")]
  if(SA_type == "SA"){
    SA_ranks_ <- SA_ranks_[, 1:(2 * N)]
  }

  RankStats <- data.frame(
    uCode = SA_ranks$uCode,
    Nominal = SA_ranks$Nominal,
    Mean = apply(SA_ranks_, MARGIN = 1, mean, na.rm = TRUE),
    Median = apply(SA_ranks_, MARGIN = 1, stats::median, na.rm = TRUE),
    Q5 = apply(SA_ranks_, MARGIN = 1, stats::quantile, probs = 0.05, na.rm = TRUE),
    Q95 = apply(SA_ranks_, MARGIN = 1, stats::quantile, probs = 0.95, na.rm = TRUE)
  )

  SA_out <- list(
    Scores = SA_scores,
    Ranks = SA_ranks,
    RankStats = RankStats,
    Para = XX_p
  )

  if(SA_type == "SA"){
    y_AvDiffs <- apply(SA_ranks[names(SA_ranks) %nin% c("uCode", "Nominal")], 2,
                       function(x) mean(abs(x - SA_ranks$Nominal), na.rm = TRUE))
    SAout <- SA_estimate(y_AvDiffs, N = N, d = d, Nboot = Nboot)
    Sinds <- SAout$SensInd
    Sinds$Variable <- names(SA_specs)
    SA_out$Sensitivity <- Sinds
  }

  SA_out$Nominal <- data.frame(
    uCode = SA_scores$uCode,
    Score = SA_scores$Nominal,
    Rank = SA_ranks$Nominal
  )

  if(diagnostic_mode){
    SA_out$coins <- coin_list
  }

  tf <- proc.time()
  tdiff <- tf - t0
  telapse <- as.numeric(tdiff[3])
  taverage <- telapse / NT

  if(!quietly){
    message(paste0("Time elapsed = ", round(telapse, 2), "s, average ", round(taverage, 2), "s/rep."))
  }

  SA_out
}

#' @rdname get_sensitivity
#' @export
get_sensitivity.coin <- function(coin, SA_specs, N, SA_type = "UA", dset, iCode, Nboot = NULL,
                                 quietly = FALSE, check_addresses = TRUE, diagnostic_mode = FALSE){
  .get_sensitivity_impl(coin = coin, SA_specs = SA_specs, N = N, SA_type = SA_type,
                        dset = dset, iCode = iCode, Nboot = Nboot, quietly = quietly,
                        check_addresses = check_addresses, diagnostic_mode = diagnostic_mode)
}


# Regenerate an edited coin
#
# This is similar to [edit_coin()] but works with a list of parameters to change, rather than one, and also outputs
# a regenerated coin.
#
# @param l_para A list of parameter values to change. Should be of the format `list(para_name = new_value)`, where
# `new_value`
# @param addresses A list or character vector of addresses. `names(addresses)` must correspond to `names(l_para)`.
# @param coin A coin, to be edited.
#
# @return A regenerated coin
# @export
#
# @examples
# #
regen_edit <- function(l_para, addresses, coin, regen_from = NULL){

  d <- length(l_para)
  p_names <- names(l_para)
  stopifnot(length(addresses) == d,
            is.coin(coin),
            setequal(names(addresses), p_names))

  # copy
  coin_i <- coin

  # modify parameters
  for(ii in 1:d){
    coin_i <- edit_coin(coin_i, address = addresses[names(addresses) == p_names[ii]],
                        new_value = l_para[[ii]])
  }

  # regenerate the results
  tryCatch(
    expr = Regen(coin_i, from = regen_from, quietly = TRUE),
    error = function(e){
      message("Regen failed. Probably a conflict between methods.")
      print(e)
      return(NA)
    }
  )

}


#' Visualise the regeneration pipeline targeted by `get_sensitivity()`
#'
#' Generates a static `ggplot2` graphic showing the order and configuration of
#' each build step that will be executed during sensitivity or uncertainty
#' analysis. Steps touched by the supplied specifications are highlighted and
#' annotated with the corresponding overrides.
#'
#' @param coin A coin object.
#' @param SA_specs A list of sensitivity specifications as supplied to
#'   [get_sensitivity()].
#' @param focus Either `"all"` (default) to show the full pipeline or
#'   `"modified"` to keep only steps affected by the specifications.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_sensitivity_pipeline <- function(coin, SA_specs,
                                      focus = c("all", "modified")){

  focus <- match.arg(focus)

  build_pipeline <- function(coin, SA_specs){
    check_coin_input(coin)
    stopifnot(is.list(SA_specs))

    log_entries <- .get_log_entries(coin)
    if(is.null(log_entries)){
      stop("No regeneration log found in the supplied coin.")
    }

    step_names <- names(log_entries)
    step_names <- step_names[step_names != "can_regen"]

    default_inputs <- c(
      Denominate = "Raw",
      Impute = "Denominated",
      Screen = "Imputed",
      Treat = "Screened",
      Normalise = "Treated",
      Aggregate = "Normalised"
    )
    default_outputs <- c(
      new_coin = "Raw",
      Denominate = "Denominated",
      Impute = "Imputed",
      Screen = "Screened",
      Treat = "Treated",
      Normalise = "Normalised",
      Aggregate = "Aggregated"
    )

    spec_table <- .collect_spec_metadata(SA_specs)
    ignored_specs <- spec_table$spec_id[is.na(spec_table$step_id)]
    if(length(ignored_specs) > 0){
      warning("Some SA_specs do not reference the coin log and are omitted: ",
              paste(ignored_specs, collapse = ", "), call. = FALSE)
    }
    spec_table <- spec_table[!is.na(spec_table$step_id), , drop = FALSE]

    rows <- vector("list", length(step_names))

    for(ii in seq_along(step_names)){
      step <- step_names[ii]
      args <- log_entries[[step]]
      if(!is.null(args$dset)) {
        input <- .format_scalar(args$dset)
      } else if(step %in% names(default_inputs)) {
        input <- default_inputs[[step]]
      } else {
        input <- NA_character_
      }
      if(!is.null(args$write_to)) {
        output <- .format_scalar(args$write_to)
      } else if(step %in% names(default_outputs)) {
        output <- default_outputs[[step]]
      } else {
        output <- NA_character_
      }

      log_parts <- character()
      if(!is.null(args$dset)){
        log_parts <- c(log_parts, paste0("dset = ", .format_scalar(args$dset)))
      }
      if(!is.null(args$write_to)){
        log_parts <- c(log_parts, paste0("write_to = ", .format_scalar(args$write_to)))
      }
      log_args <- paste(log_parts, collapse = " | ")

      step_specs <- spec_table[spec_table$step_id == step, , drop = FALSE]
      spec_desc <- character()
      if(nrow(step_specs) > 0){
        for(jj in seq_len(nrow(step_specs))){
          spec_row <- step_specs[jj, ]
          log_value <- .extract_log_value(args, spec_row$path_tokens[[1]])
          log_label <- if(is.null(log_value)) "NULL" else .format_scalar(log_value)
          spec_label <- .format_distribution(spec_row$distribution[[1]], spec_row$dist_type)
          path_label <- if(length(spec_row$path_tokens[[1]]) == 0){
            "<entry>"
          } else {
            paste(spec_row$path_tokens[[1]], collapse = "$")
          }
          spec_desc <- c(
            spec_desc,
            paste0(
              spec_row$spec_id, ": ", path_label,
              " | log = ", log_label,
              " | spec ", spec_row$dist_type, " = ", spec_label
            )
          )
        }
      }

      rows[[ii]] <- data.frame(
        step_id = step,
        order = ii,
        input_dset = ifelse(length(input), input, NA_character_),
        output_dset = ifelse(length(output), output, NA_character_),
        log_args = if(nchar(log_args) == 0) NA_character_ else log_args,
        spec_args = if(length(spec_desc) == 0) NA_character_ else paste(spec_desc, collapse = "; "),
        modified = nrow(step_specs) > 0,
        stringsAsFactors = FALSE
      )
    }

    do.call(rbind, rows)
  }

  pipeline_df <- build_pipeline(coin, SA_specs)

  if(focus == "modified"){
    pipeline_df <- pipeline_df[pipeline_df$modified, , drop = FALSE]
    if(nrow(pipeline_df) == 0){
      stop("No pipeline steps are targeted by the supplied SA_specs.")
    }
  }

  pipeline_df$x <- seq_len(nrow(pipeline_df))

  wrap_text <- function(txt, width = 28){
    if(is.na(txt) || !nzchar(txt)){
      return(txt)
    }
    paste(strwrap(txt, width = width), collapse = "\n")
  }

  wrap_lines <- function(lines, width = 28){
    if(all(is.na(lines))){
      return(NA_character_)
    }
    pieces <- vapply(lines, wrap_text, character(1L))
    paste(pieces[!is.na(pieces) & nzchar(pieces)], collapse = "\n")
  }

  defaults_df <- data.frame(
    step_id = pipeline_df$step_id,
    x = pipeline_df$x,
    y = 1,
    flow_label = paste0(pipeline_df$input_dset, " -> ", pipeline_df$output_dset),
    log_label = ifelse(is.na(pipeline_df$log_args), NA_character_,
                       paste0("Log: ", pipeline_df$log_args)),
    stringsAsFactors = FALSE
  )
  defaults_df$label_wrapped <- apply(defaults_df[, c("flow_label", "log_label")], 1, function(rows){ wrap_lines(rows, width = 26) })

  specs_df <- data.frame(
    step_id = pipeline_df$step_id,
    x = pipeline_df$x,
    y = 0,
    label = ifelse(is.na(pipeline_df$spec_args),
                   "Spec: (default)",
                   pipeline_df$spec_args),
    modified = pipeline_df$modified,
    stringsAsFactors = FALSE
  )
  specs_df$label_wrapped <- vapply(specs_df$label, function(x) wrap_text(x, width = 26), character(1L))

  if(nrow(pipeline_df) > 1){
    flow_df <- data.frame(
      x = pipeline_df$x[-nrow(pipeline_df)] + 0.1,
      xend = pipeline_df$x[-1] - 0.1,
      stringsAsFactors = FALSE
    )
  } else {
    flow_df <- NULL
  }

  diff_df <- specs_df[specs_df$modified, , drop = FALSE]

  p <- ggplot2::ggplot() +
    (if(!is.null(flow_df)) ggplot2::geom_segment(data = flow_df,
                          ggplot2::aes(x = x, xend = xend, y = 1, yend = 1),
                          linewidth = 0.55, colour = "#636363",
                          arrow = grid::arrow(length = grid::unit(0.14, "cm"), ends = "last")) else NULL) +
    (if(!is.null(flow_df)) ggplot2::geom_segment(data = flow_df,
                          ggplot2::aes(x = x, xend = xend, y = 0, yend = 0),
                          linewidth = 0.55, colour = "#bdbdbd",
                          arrow = grid::arrow(length = grid::unit(0.12, "cm"), ends = "last")) else NULL) +
    ggplot2::geom_point(data = defaults_df,
                        ggplot2::aes(x = x, y = y), size = 4.5,
                        colour = "#1f78b4", fill = "#a6cee3", shape = 21, stroke = 1) +
    ggplot2::geom_point(data = specs_df,
                        ggplot2::aes(x = x, y = y, fill = modified),
                        size = 4.5, colour = "#525252", shape = 21, stroke = 1.1) +
    ggplot2::geom_segment(data = diff_df,
                          ggplot2::aes(x = x, xend = x, y = 1, yend = 0),
                          colour = "#fb6a4a", linewidth = 0.6, linetype = "dashed") +
    ggplot2::geom_text(data = defaults_df,
                       ggplot2::aes(x = x, y = y + 0.12, label = step_id),
                       fontface = "bold", size = 3.8, vjust = 0) +
    ggplot2::geom_text(data = defaults_df,
                       ggplot2::aes(x = x, y = y - 0.12, label = label_wrapped),
                       size = 3.2, lineheight = 1.05, vjust = 1) +
    ggplot2::geom_label(data = specs_df,
                        ggplot2::aes(x = x, y = y - 0.27, label = label_wrapped, fill = modified),
                        size = 2.9, label.size = 0.12,
                        label.padding = grid::unit(0.12, "lines"),
                        colour = "#202020") +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#fb6a4a", `FALSE` = "#a6d96a"),
                               breaks = c(FALSE, TRUE),
                               labels = c("Follows log defaults", "Overrides via SA_specs"),
                               name = NULL) +
    ggplot2::scale_x_continuous(breaks = pipeline_df$x,
                                labels = pipeline_df$step_id,
                                expand = ggplot2::expansion(mult = c(0.02, 0.04))) +
    ggplot2::coord_cartesian(xlim = c(0.5, max(pipeline_df$x) + 0.5),
                             ylim = c(-0.72, 1.12)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  attr(p, "pipeline_df") <- pipeline_df

  p
}


.get_log_entries <- function(coin){
  log_entries <- NULL
  if(!is.null(coin$Log)){
    log_entries <- coin$Log[["Log"]]
    if(!is.list(log_entries) && is.list(coin$Log)){
      log_entries <- coin$Log
    }
  }
  if(is.list(log_entries)){
    log_entries
  } else {
    NULL
  }
}

.collect_spec_metadata <- function(SA_specs){

  if(length(SA_specs) == 0){
    return(data.frame(
      spec_id = character(0),
      step_id = character(0),
      path_tokens = I(list()),
      distribution = I(list()),
      dist_type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- vector("list", length(SA_specs))

  idx <- 1L
  for(spec_name in names(SA_specs)){
    spec <- SA_specs[[spec_name]]
    address <- spec$Address
    if(is.null(address)){
      warning("SA_spec '", spec_name, "' does not contain an Address entry.", call. = FALSE)
      next
    }
    tokens <- strsplit(sub("^\\$", "", address), "\\$", fixed = FALSE)[[1]]
    if(length(tokens) == 0){
      next
    }
    if(tokens[1] == "Log"){
      if(length(tokens) >= 2){
        step <- tokens[2]
        path_tokens <- if(length(tokens) > 2) tokens[3:length(tokens)] else character(0)
      } else {
        step <- NA_character_
        path_tokens <- character(0)
      }
    } else {
      step <- NA_character_
      path_tokens <- tokens[-1]
    }
    out[[idx]] <- list(
      spec_id = spec_name,
      step_id = step,
      path_tokens = list(path_tokens),
      distribution = list(if(is.null(spec$Distribution)) NULL else spec$Distribution),
      dist_type = if(is.null(spec$Type)) "unspecified" else spec$Type
    )
    idx <- idx + 1L
  }

  out <- out[!vapply(out, is.null, logical(1))]

  if(length(out) == 0){
    return(data.frame(
      spec_id = character(0),
      step_id = character(0),
      path_tokens = I(list()),
      distribution = I(list()),
      dist_type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(out, function(row){
    data.frame(
      spec_id = row$spec_id,
      step_id = row$step_id,
      path_tokens = I(row$path_tokens),
      distribution = I(row$distribution),
      dist_type = row$dist_type,
      stringsAsFactors = FALSE
    )
  }))
}

.extract_log_value <- function(x, tokens){
  if(length(tokens) == 0){
    return(x)
  }
  current <- x
  for(token in tokens){
    if(is.null(current)){
      return(NULL)
    }
    if(is.list(current)){
      if(!is.null(current[[token]])){
        current <- current[[token]]
        next
      }
      suppressWarnings(num_token <- as.integer(token))
      if(!is.na(num_token) && num_token >= 1 && num_token <= length(current)){
        current <- current[[num_token]]
        next
      }
    }
    return(NULL)
  }
  current
}

.format_distribution <- function(distribution, dist_type){

  if(is.null(distribution)){
    return("NULL")
  }

  if(identical(dist_type, "continuous") && is.atomic(distribution) && length(distribution) == 2){
    return(paste0("[", paste(.format_scalar(distribution), collapse = ", "), "]"))
  }

  if(identical(dist_type, "discrete")){

    if(is.atomic(distribution)){
      vals <- .collapse_vals(distribution)
      return(paste0("{", vals, "}"))
    }

    if(is.list(distribution)){
      alt_desc <- vapply(seq_along(distribution), function(ii){
        desc <- .format_alternative(distribution[[ii]])
        paste0(ii, ": ", desc)
      }, character(1))
      if(length(alt_desc) > 3){
        alt_desc <- c(alt_desc[1:2], paste0("...", length(alt_desc) - 2, " more"))
      }
      return(paste0("{", paste(alt_desc, collapse = "; "), "}"))
    }
  }

  .format_scalar(distribution)
}

.collapse_vals <- function(x, max_n = 6){
  if(length(x) > max_n){
    paste0(paste(x[seq_len(max_n - 1)], collapse = ", "), ", ...")
  } else {
    paste(x, collapse = ", ")
  }
}

.format_alternative <- function(alt){
  if(is.list(alt) && length(alt) == 1 && is.null(names(alt))){
    return(.format_scalar(alt[[1]]))
  }
  if(is.list(alt)){
    nm <- names(alt)
    pieces <- vapply(seq_along(alt), function(ii){
      label <- if(is.null(nm) || nm[ii] == ""){
        paste0("[[", ii, "]]")
      } else {
        nm[ii]
      }
      paste0(label, "=", .format_scalar(alt[[ii]]))
    }, character(1))
    return(paste(pieces, collapse = ", "))
  }
  .format_scalar(alt)
}

.format_scalar <- function(x){

  if(is.null(x)){
    return("NULL")
  }

  if(is.atomic(x)){
    if(length(x) == 0){
      return("NULL")
    }
    if(length(x) == 1){
      return(as.character(x))
    }
    return(.collapse_vals(as.character(x)))
  }

  if(is.list(x)){
    if(length(x) == 0){
      return("list()")
    }
    if(length(x) == 1 && is.null(names(x))){
      return(.format_scalar(x[[1]]))
    }
    nm <- names(x)
    pieces <- vapply(seq_along(x), function(ii){
      label <- if(is.null(nm) || nm[ii] == ""){
        paste0("[[", ii, "]]")
      } else {
        nm[ii]
      }
      paste0(label, "=", .format_scalar(x[[ii]]))
    }, character(1))
    return(paste(pieces, collapse = ", "))
  }

  if(is.environment(x)){
    return("<env>")
  }

  paste0("<", paste(class(x), collapse = ","), ">")
}


# Convert a numeric sample to parameter values
#
# Converts a numeric sample `x`, which should have values between 0 and 1, to a corresponding vector or list of
# parameter values, based on `distribution`.
#
# The `distribution` argument specifies how to map `x` to parameter values and can be used in two different ways,
# depending on `dist_type`. If `dist_type = "discrete"`, then `distribution` should be a vector or list of alternative
# parameter values (or objects). Each entry of `x` is mapped to an entry from `distribution` by treating `distribution`
# as a discrete uniform distribution over its entries.
#
# If `dist_type = "continuous"`, `distribution` is assumed to be a continuous uniform distribution, such that
# `distribution` is a 2-length numeric vector with the first value being the lower bound, and the second value the
# upper bound. For example, if `distribution = c(5, 10)`, then `x` will be mapped onto a continuous uniform distribution
# bounded by 5 and 10.
#
# @param distribution The distribution to sample using `x` - see details.
# @param dist_type Either `"discrete"` or `"continuous"` - see details.
# @param checks Logical: if `TRUE` runs some checks on inputs, set to `FALSE` to increase speed slightly.
# @param x A numeric vector with values between 0 and 1
#
# @return A vector or list of parameter values.
#
# @examples
# #
sample_2_para <- function(x, distribution, dist_type = "discrete", checks = TRUE){

  if(checks){
    stopifnot(is.numeric(x),
              any(x >= 0),
              any(x <= 1),
              is.character(dist_type),
              length(dist_type) == 1,
              dist_type %in% c("discrete", "continuous"))
  }

  # specs can be a set of discrete alternatives, or else a uniform distribution
  if(dist_type == "discrete"){

    # the number of discrete alternatives
    n_alt <- length(distribution)
    # convert x to indexes of the discrete parameters
    i_para <- cut(x, n_alt, 1:n_alt)
    # now get the output vector/list
    l_out <- distribution[i_para]

  } else {

    # here we assume a uniform distribution
    if(checks){
      stopifnot(is.numeric(distribution),
                length(distribution) == 2,
                distribution[2] > distribution[1])
    }

    # we simply scale x up to the interval covered by the distribution
    l_out <- x*(distribution[2] - distribution[1]) + distribution[1]

  }

  # output
  l_out


}


# Edit objects inside a coin
#
# Changes the object found at `address` to `new_value`.
#
# @param coin A coin
# @param address A string specifying the location in the coin of the object to edit. This should begin with `"$"`, omitting the coin itself
# in the address. E.g. if you target `coin$x$y$z` enter `"$x$y$z"`.
# @param new_value The new value to assign at `address`.
# @param checks Logical: if `TRUE`, runs some basic checks, otherwise omitted if `FALSE`. Setting `FALSE` may speed
# things up a bit in sensitivity analysis, for example.
#
# @return An updated coin
# @export
#
# @examples
# #
edit_coin <- function(coin, address, new_value, checks = FALSE){

  # checks
  if(checks){
    check_coin_input(coin)
    stopifnot(is.character(address),
              length(address) == 1,
              substr(address,1,1) == "$")
  }

  # this is the call to evaluate, as a string
  expr_str <- paste0("coin", address, " <- new_value")
  # evaluate the call
  eval(str2lang(expr_str))

  # output
  coin

}

# Check address in coin
check_address <- function(address, coin){

  # checks
  stopifnot(is.character(address),
            length(address) == 1)

  # check address begins with $
  if(substr(address,1,1) != "$"){
    stop("Address must begin with '$'! Your address: ", address, call. = FALSE)
  }

  # Check if points to a Log operation that doesn't exist at all
  is_log_address <- substr(address, 1, 5) == "$Log$"

  if(is_log_address){

    subaddress <- get_log_subaddress(address)
    subaddress_value <- eval(str2lang(paste0("coin", subaddress)))

    if(is.null(subaddress_value)){
      require_user_input(address, " is a coin log address of a function that doesn't yet exist in the coin.\nThis is not recommended and can cause unexpected behaviour. Continue anyway (y/n)?  ")
    }

  } else {

    # Check generally whether the address parameter exists

    # this is the call to evaluate, as a string
    expr_str <- paste0("coin", address)
    # evaluate the call
    address_value <- eval(str2lang(expr_str))

    if(is.null(address_value)){
      require_user_input(address, " is not currently present in the coin or else is NULL. Continue anyway (y/n)?  ")
    }

  }
}

require_user_input <- function(address, message_fragment){
  xx <- readline(paste0("Address ", address, message_fragment))

  if(xx %nin% c("y", "n")){
    stop("You didn't input y or n. I'm taking that as a no.", call. = FALSE)
  }

  if(xx == "n"){
    stop("Exiting sensitivity analysis. Please check the address: ", address, call. = FALSE)
  }
}


# Extracts the log part of a coin address up to the function. This is in order
# to see whether a function has been run and issue a warning otherwise.
# E.g. if address is "$Log$Denominate$dset" will return "$Log$Denominate". If
# the address is not a valid log address, returns NULL.
get_log_subaddress <- function(address){

  # Define the regex pattern
  pattern <- "^([^$]*\\$[^$]*\\$[^$]*)\\$"

  # Use `regmatches` and `gregexpr` to apply the regex
  matches <- regmatches(address, gregexpr(pattern, address))

  # Extract the match and display it
  if (length(matches[[1]]) > 0) {
    result <- regmatches(address, gregexpr(pattern, address))[[1]][1]
    sub(pattern, "\\1", result)
  } else {
    NULL
  }

}


#' Estimate sensitivity indices
#'
#' Post process a sample to obtain sensitivity indices. This function takes a univariate output
#' which is generated as a result of running a Monte Carlo sample from [SA_sample()] through a system.
#' Then it estimates sensitivity indices using this sample.
#'
#' This function is built to be used inside [get_sensitivity()].
#'
#' @param yy A vector of model output values, as a result of a \eqn{N(d+2)} Monte Carlo design.
#' @param N The number of sample points per dimension.
#' @param d The dimensionality of the sample
#' @param Nboot Number of bootstrap draws for estimates of confidence intervals on sensitivity indices.
#' If this is not specified, bootstrapping is not applied.
#'
#' @importFrom stats var
#'
#' @examples
#' # This is a generic example rather than applied to a COIN (for reasons of speed)
#'
#' # A simple test function
#' testfunc <- function(x){
#' x[1] + 2*x[2] + 3*x[3]
#' }
#'
#' # First, generate a sample
#' X <- SA_sample(500, 3)
#'
#' # Run sample through test function to get corresponding output for each row
#' y <- apply(X, 1, testfunc)
#'
#' # Estimate sensitivity indices using sample
#' SAinds <- SA_estimate(y, N = 500, d = 3, Nboot = 1000)
#' SAinds$SensInd
#' # Notice that total order indices have narrower confidence intervals than first order.
#'
#' @seealso
#' * [get_sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#' * [SA_sample()] Input design for estimating sensitivity indices
#'
#' @return A list with the output variance, plus a data frame of first order and total order sensitivity indices for
#' each variable, as well as bootstrapped confidence intervals if `!is.null(Nboot)`.
#'
#' @export
SA_estimate <- function(yy, N, d, Nboot = NULL){

  # checks
  stopifnot(is.numeric(yy))
  if(length(yy) != N*(d+2)){
    stop("The length of 'yy' does not correspond to the values of 'N' and 'd'. The vector 'yy' should be of length N(d+2).")
  }

  # put into matrix format: just the ABis
  yyABi <- matrix(yy[(2*N +1):length(yy)], nrow = N)
  # get yA and yB
  yA <- yy[1:N]
  yB <- yy[(N+1) : (2*N)]

  # calculate variance
  varY <- stats::var(c(yA,yB))

  # calculate Si
  Si <- apply(yyABi, 2, function(x){
    mean(yB*(x - yA))/varY
  })

  # calculate ST
  STi <- apply(yyABi, 2, function(x){
    sum((yA - x)^2)/(2*N*varY)
  })

  # make a df
  SensInd <- data.frame(Variable = paste0("V", 1:d),
                        Si = Si,
                        STi = STi)

  ## BOOTSTRAP ## -----

  if (!is.null(Nboot)){

    # Get the "elements" to sample from
    STdiffs <- apply(yyABi, 2, function(x){
      yA - x
    })
    Sidiffs <- apply(yyABi, 2, function(x){
      yB*(x - yA)
    })

    # prep matrices for bootstrap samples
    Si_boot <- matrix(NA, d, Nboot)
    STi_boot <- Si_boot

    # do the bootstrapping bit
    for (iboot in 1:Nboot){

      # calculate Si
      Si_boot[,iboot] <- apply(Sidiffs, 2, function(x){
        mean(sample(x, replace = TRUE))/varY
      })

      # calculate ST
      STi_boot[,iboot] <- apply(STdiffs, 2, function(x){
        sum(sample(x, replace = TRUE)^2)/(2*N*varY)
      })

    }

    # get quantiles of sensitivity indices to add to
    SensInd$Si_q5 <- apply(Si_boot, MARGIN = 1,
                           function(xx) stats::quantile(xx, probs = 0.05, na.rm = TRUE))
    SensInd$Si_q95 <- apply(Si_boot, MARGIN = 1,
                            function(xx) stats::quantile(xx, probs = 0.95, na.rm = TRUE))
    SensInd$STi_q5 <- apply(STi_boot, MARGIN = 1,
                            function(xx) stats::quantile(xx, probs = 0.05, na.rm = TRUE))
    SensInd$STi_q95 <- apply(STi_boot, MARGIN = 1,
                             function(xx) stats::quantile(xx, probs = 0.95, na.rm = TRUE))

  }

  # return outputs
  list(Variance = varY,
       SensInd = SensInd)

}


#' Generate sample for sensitivity analysis
#'
#' Generates an input sample for a Monte Carlo estimation of global sensitivity indices. Used in
#' the [get_sensitivity()] function. The total sample size will be \eqn{N(d+2)}.
#'
#' This function generates a Monte Carlo sample as described e.g. in the [Global Sensitivity Analysis: The Primer book](https://onlinelibrary.wiley.com/doi/book/10.1002/9780470725184).
#'
#' @param N The number of sample points per dimension.
#' @param d The dimensionality of the sample
#'
#' @importFrom stats runif
#'
#' @examples
#' # sensitivity analysis sample for 3 dimensions with 100 points per dimension
#' X <- SA_sample(100, 3)
#'
#' @return A matrix with \eqn{N(d+2)} rows and `d` columns.
#'
#' @seealso
#' * [get_sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN.
#' * [SA_estimate()] Estimate sensitivity indices from system output, as a result of input design from SA_sample().
#'
#' @export
SA_sample <- function(N, d){

  # a random (uniform) sample
  Xbase <- matrix(stats::runif(d*N*2), nrow = N, ncol = d*2)
  # get first half
  XA <- Xbase[, 1:d]
  # get second half
  XB <- Xbase[, (d+1):(2*d)]
  # make big matrix (copy matrix d times on the bottom)
  XX <- matrix(rep(t(XA), d), ncol = ncol(XA), byrow = TRUE )

  # now substitute in columns from B into A
  for (ii in 1:d){
    XX[(1 + (ii-1)*N):(ii*N), ii] <- XB[, ii]
  }

  # add original matrices on the beginning
  XX <-  rbind(XA, XB, XX)

  XX
}


#' Plot ranks from an uncertainty/sensitivity analysis
#'
#' Plots the ranks resulting from an uncertainty and sensitivity analysis, in particular plots
#' the median, and 5th/95th percentiles of ranks.
#'
#' To use this function you first need to run [get_sensitivity()]. Then enter the resulting list as the
#' `SAresults` argument here.
#'
#' See `vignette("sensitivity")`.
#'
#' This function replaces the now-defunct `plotSARanks()` from COINr < v1.0.
#'
#' @param SAresults A list of sensitivity/uncertainty analysis results from [get_sensitivity()].
#' @param plot_units A character vector of units to plot. Defaults to all units. You can also set
#' to `"top10"` to only plot top 10 units, and `"bottom10"` for bottom ten.
#' @param order_by If set to `"nominal"`, orders the rank plot by nominal ranks
#' (i.e. the original ranks prior to the sensitivity analysis). Otherwise if `"median"`, orders by
#' median ranks.
#' @param dot_colour Colour of dots representing median ranks.
#' @param line_colour Colour of lines connecting 5th and 95th percentiles.
#'
#' @importFrom ggplot2 geom_line geom_point scale_shape_manual scale_size_manual labs guides
#' @importFrom ggplot2 scale_color_manual theme_classic theme scale_y_discrete scale_x_reverse
#' @importFrom ggplot2 coord_flip element_text
#'
#' @examples
#' # for examples, see `vignette("sensitivity")`
#' # (this is because package examples are run automatically and sensitivity analysis
#' # can take a few minutes to run at realistic settings)
#'
#' @seealso
#' * [get_sensitivity()] Perform global sensitivity or uncertainty analysis on a coin
#' * [plot_sensitivity()] Plot sensitivity indices following a sensitivity analysis.
#'
#' @return A plot of rank confidence intervals, generated by 'ggplot2'.
#'
#' @export
plot_uncertainty <- function(SAresults, plot_units = NULL, order_by = "nominal",
                        dot_colour = NULL, line_colour = NULL){
  UseMethod("plot_uncertainty")
}

#' @rdname plot_uncertainty
#' @export
plot_uncertainty.list <- function(SAresults, plot_units = NULL, order_by = "nominal",
                        dot_colour = NULL, line_colour = NULL){

  placeholders <- attr(SAresults, "placeholder_codes")
  if(!is.null(placeholders) && length(placeholders) > 0){
    if(!is.null(SAresults$RankStats)){
      SAresults$RankStats <- .drop_placeholder_rows(SAresults$RankStats, placeholders)
    }
    if(!is.null(SAresults$Nominal)){
      SAresults$Nominal <- .drop_placeholder_rows(SAresults$Nominal, placeholders)
    }
  }

  rnks <- SAresults$RankStats

  if(!is.null(plot_units)){
    if(length(plot_units == 1)){

      if (plot_units == "top10"){
        unit_include <- SAresults$Nominal$uCode[SAresults$Nominal$Rank <= 10]
      } else if (plot_units == "bottom10"){
        unit_include <- SAresults$Nominal$uCode[
          SAresults$Nominal$Rank >= (max(SAresults$Nominal$Rank, na.rm = TRUE)-10)]
      } else {
        stop("plot_units not recognised: should be either a character vector of unit codes or else
      \"top10\" or \"bottom10\" ")
      }
    } else {
      # vector, so this should be a vector of unit codes
      unit_include <- plot_units
      if(any(unit_include %nin% rnks$uCode)){
        stop("One or more units in 'plot_units' not found in SA results.")
      }
    }
    rnks <- rnks[rnks$uCode %in% unit_include,]
    SAresults$Nominal <- SAresults$Nominal[SAresults$Nominal$uCode %in% unit_include,]
  }

  # set ordering of plot
  if (order_by == "nominal"){
    plot_order <- SAresults$Nominal$uCode[order(SAresults$Nominal$Score, decreasing = FALSE)]
  } else if (order_by == "median"){
    plot_order <- SAresults$Nominal$uCode[order(rnks$Median, decreasing = TRUE)]
  }

  # first, pivot to long
  rownames(rnks) <- rnks$uCode
  qstats <- lengthen(rnks[c("Median", "Q5", "Q95")])
  names(qstats) <- c("uCode", "Statistic", "Rank")
  stats_long <- merge(rnks[c("uCode", "Nominal", "Mean")], qstats, by = "uCode", all = TRUE)
  # stats_long <- tidyr::pivot_longer(rnks,
  #                                   cols = c("Median", "Q5", "Q95"),
  #                                   names_to = "Statistic",
  #                                   values_to = "Rank")

  # colours
  if(is.null(dot_colour)){
    dot_colour <- "#83af70"
  }
  if(is.null(line_colour)){
    line_colour <- "grey"
  }

  # generate plot
  ggplot2::ggplot(stats_long, aes(x = .data$Rank, y = .data$uCode)) +
    ggplot2::geom_line(aes(group = .data$uCode), color = line_colour) +
    ggplot2::geom_point(aes(color = .data$Statistic, shape = .data$Statistic, size= .data$Statistic)) +
    ggplot2::scale_shape_manual(values = c(16, 15, 15)) +
    ggplot2::scale_size_manual(values = c(2, 0, 0)) +
    ggplot2::labs(y = "", color = "") +
    ggplot2::guides(shape = "none", size = "none", color = "none") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="top") +
    ggplot2::scale_color_manual(values = c(dot_colour, "#ffffff", "#ffffff")) +
    ggplot2::scale_y_discrete(limits = plot_order) +
    ggplot2::scale_x_reverse() +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))  +
    ggplot2::theme(text=ggplot2::element_text(family="sans"))

}


#' Plot sensitivity indices
#'
#' Plots sensitivity indices as bar or pie charts.
#'
#' To use this function you first need to run [get_sensitivity()]. Then enter the resulting list as the
#' `SAresults` argument here.
#'
#' See `vignette("sensitivity")`.
#'
#' This function replaces the now-defunct `plotSA()` from COINr < v1.0.
#'
#' @param SAresults A list of sensitivity/uncertainty analysis results from [plot_sensitivity()].
#' @param ptype Type of plot to generate - either `"bar"`, `"pie"` or `"box"`.
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal geom_errorbar coord_polar theme_void
#' @importFrom ggplot2 facet_wrap
#' @importFrom rlang .data
#'
#' @examples
#' # for examples, see `vignette("sensitivity")`
#' # (this is because package examples are run automatically and sensitivity analysis
#' # can take a few minutes to run at realistic settings)
#'
#' @return A plot of sensitivity indices generated by ggplot2.
#'
#' @seealso
#' * [get_sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#' * [plot_uncertainty()] Plot confidence intervals on ranks following a sensitivity analysis
#'
#' @export
plot_sensitivity <- function(SAresults, ptype = "bar"){
  UseMethod("plot_sensitivity")
}

#' @rdname plot_sensitivity
#' @export
plot_sensitivity.list <- function(SAresults, ptype = "bar"){

  stopifnot(is.list(SAresults))
  placeholders <- attr(SAresults, "placeholder_codes")
  if(!is.null(placeholders) && length(placeholders) > 0 && !is.null(SAresults$Sensitivity)){
    SAresults$Sensitivity <- .drop_placeholder_rows(SAresults$Sensitivity, placeholders)
  }
  # prep data first
  Sdf <- SAresults$Sensitivity
  if(is.null(Sdf)){
    stop("Sensitivity indices not found. Did you run get_sensitivity with SA_type = 'SA'?")
  }
  numcols <- Sdf[names(Sdf) != "Variable"]

  # set any negative values to zero. By definition they can't be negative.
  numcols[numcols < 0] <- 0
  Sdf[names(Sdf) != "Variable"] <- numcols

  if(ptype == "bar"){

    # the full bar is STi. It is divided into Si and the remainder, so we need STi - Si
    Sdf$Interactions = Sdf$STi - Sdf$Si
    Sdf$Interactions[Sdf$Interactions < 0] <- 0

    # rename col to improve plot
    colnames(Sdf)[colnames(Sdf) == "Si"] <- "MainEffect"
    # now pivot to get in format for ggplot
    bardf <- lengthen(Sdf, cols = c("MainEffect", "Interactions"))
    # bardf <- tidyr::pivot_longer(Sdf,
    #                              cols = c("MainEffect", "Interactions"))

    # make stacked bar plot
    plt <- ggplot2::ggplot(bardf, ggplot2::aes(fill=.data$name, y=.data$Value, x=.data$Variable)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = NULL) +
      ggplot2::theme_minimal()


  } else if (ptype == "pie"){

    # we are plotting first order sensitivity indices. So, also estimate interactions.
    Sis <- Sdf[c("Variable", "Si")]
    intsum <- max(c(1 - sum(Sis$Si, na.rm = TRUE), 0))
    Sis <- rbind(Sis, data.frame(Variable = "Interactions", Si = intsum))

    # Basic piechart
    plt <- ggplot2::ggplot(Sis, ggplot2::aes(x = "", y = .data$Si, fill = .data$Variable)) +
      ggplot2::geom_bar(stat="identity", width=1, color="white") +
      ggplot2::coord_polar("y", start=0) +
      ggplot2::theme_void() # remove background, grid, numeric labels

  } else if (ptype == "box"){

    if(any(c("Si_q5", "Si_q95", "STi_q5", "STi_q95") %nin% names(Sdf))){
      stop("Quantiles not found for sensitivity indices (required for box plot). Did you forget to set Nboot when running get_sensitivity()?")
    }
    Sdf <- lengthen(Sdf, cols = c("Si", "STi"))
    #Sdf1 <- tidyr::pivot_longer(Sdf, cols = c("Si", "STi"))
    Sdf$q5 <- ifelse(Sdf$name == "STi", Sdf$STi_q5, Sdf$Si_q5)
    Sdf$q95 <- ifelse(Sdf$name == "STi", Sdf$STi_q95, Sdf$Si_q95)
    Sdf$q5[Sdf$q5 > 1] <- 1
    Sdf$q95[Sdf$q95 > 1] <- 1
    Sdf$Value[Sdf$Value > 1] <- 1

    plt <- ggplot2::ggplot(Sdf, ggplot2::aes(x = .data$Variable, y = .data$Value, ymax = .data$q95, ymin = .data$q5)) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::geom_errorbar(width = 0.2) +
      ggplot2::theme_bw() +
      facet_wrap(~name) +
      ggplot2::labs(
        x = NULL,
        y = NULL)

  }

  plt +
    ggplot2::theme(text=element_text(family="sans"))

}


#' Noisy replications of weights
#'
#' Given a data frame of weights, this function returns multiple replicates of the weights, with added
#' noise. This is intended for use in uncertainty and sensitivity analysis.
#'
#' Weights are expected to be in a data frame format with columns `Level`, `iCode` and `Weight`, as
#' used in `iMeta`. Note that no `NA`s are allowed anywhere in the data frame.
#'
#' Noise is added using the `noise_specs` argument, which is specified by a data frame with columns
#' `Level` and `NoiseFactor`. The aggregation level refers to number of the aggregation level to target
#' while the `NoiseFactor` refers to the size of the perturbation. If e.g. a row is `Level = 1` and
#' `NoiseFactor = 0.2`, this will allow the weights in aggregation level 1 to deviate by +/- 20% of their
#' nominal values (the values in `w`).
#'
#' This function replaces the now-defunct `noisyWeights()` from COINr < v1.0.
#'
#' @param w A data frame of weights, in the format found in `.$Meta$Weights`.
#' @param noise_specs a data frame with columns:
#'  * `Level`: The aggregation level to apply noise to
#'  * `NoiseFactor`: The size of the perturbation: setting e.g. 0.2 perturbs by +/- 20% of nominal values.
#' @param Nrep The number of weight replications to generate.
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get nominal weights
#' w_nom <- coin$Meta$Weights$Original
#'
#' # build data frame specifying the levels to apply the noise at
#' # here we vary at levels 2 and 3
#' noise_specs = data.frame(Level = c(2,3),
#'                          NoiseFactor = c(0.25, 0.25))
#'
#' # get 100 replications
#' noisy_wts <- get_noisy_weights(w = w_nom, noise_specs = noise_specs, Nrep = 100)
#'
#' # examine one of the noisy weight sets, last few rows
#' tail(noisy_wts[[1]])
#'
#' @details
#' When the nominal weights come from an object inheriting from `unbalanced_coin`,
#' `get_noisy_weights()` delegates to the balanced representation and removes any
#' placeholder helper nodes from each generated weight set so that only genuine
#' indicators and aggregates are present in the output.
#' @param ... Additional arguments passed to class-specific implementations.
#'
#' @return A list of `Nrep` sets of weights (data frames).
#'
#' @seealso
#' * [get_sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#'
#' @export
get_noisy_weights <- function(w, ...){
  UseMethod("get_noisy_weights")
}

#' @rdname get_noisy_weights
#' @export
get_noisy_weights.data.frame <- function(w, noise_specs, Nrep, ...){

  # CHECKS

  stopifnot(is.data.frame(noise_specs))

  if(any(is.na(w))){
    stop("NAs found in w: NAs are not allowed.")
  }

  if(any(c("iCode", "Level", "Weight") %nin% names(w))){
    stop("One or more required columns (iCode, Level, Weight) not found in w.")
  }

  if (length(unique(noise_specs$Level)) < nrow(noise_specs)){
    stop("Looks like you have duplicate Level values in the noise_specs df?")
  }

  # make list for weights
  wlist <- vector(mode = "list", length = Nrep)

  for (irep in 1:Nrep){

    # make fresh copy of weights
    wrep <- w

    for (ii in noise_specs$Level){
      # weights for this level
      wts <- wrep$Weight[w$Level == ii]
      # vector of noise: random number in [0,1] times 2, -1. This interprets NoiseFactor as
      # a +/-% deviation.
      wnoise <- (stats::runif(length(wts))*2 - 1)*noise_specs$NoiseFactor[noise_specs$Level == ii]*wts
      # add noise to weights and store
      wts <- wts + wnoise
      wrep$Weight[w$Level == ii] <- wts
    }
    wlist[[irep]] <- wrep
  }

  return(wlist)
}
