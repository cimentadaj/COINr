suppressPackageStartupMessages({
  if(requireNamespace("devtools", quietly = TRUE)){
    devtools::load_all(quiet = TRUE)
  }
  if(!"package:COINr" %in% search()){
    library(COINr)
  }
})

make_large_iData <- function(rep_factor = 100){
  stopifnot(rep_factor >= 1)
  data("ASEM_iData", package = "COINr")
  suffix <- sprintf("_%03d", seq_len(rep_factor))
  big_list <- lapply(seq_along(suffix), function(idx){
    df <- ASEM_iData
    df$uCode <- paste0(df$uCode, suffix[idx])
    df$uName <- paste0(df$uName, suffix[idx])
    df
  })
  do.call(rbind, big_list)
}

data("ASEM_iMeta", package = "COINr")

large_iData <- make_large_iData(rep_factor = 200)

# Build the coin through the full pipeline
coin_large <- new_coin(large_iData, ASEM_iMeta,
                       level_names = c("Indicator", "Pillar", "Sub-index", "Index"),
                       quietly = TRUE)
coin_large <- Denominate(coin_large, dset = "Raw", quietly = TRUE)
coin_large <- Impute(coin_large, dset = "Denominated", f_i = "i_mean_grp", use_group = "EurAsia_group",
                     quietly = TRUE)
coin_large <- Screen(coin_large, dset = "Imputed", dat_thresh = 0.9, unit_screen = "byNA",
                     quietly = TRUE)
coin_large <- Treat(coin_large, dset = "Screened", global_specs = list(f1_para = list(winmax = 5)),
                    quietly = TRUE)
coin_large <- Normalise(coin_large, dset = "Treated", global_specs = list(f_n = "n_minmax",
                                                                           f_n_para = list(c(0, 100))),
                        quietly = TRUE)
coin_large <- Aggregate(coin_large, dset = "Normalised", f_ag = "a_amean", quietly = TRUE)

# Define sensitivity specs similar to package tests
winmax_spec <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                    Distribution = 1:5,
                    Type = "discrete")

norm_alternatives <- list(
  list(f_n = "n_minmax", f_n_para = list(c(0, 100))),
  list(f_n = "n_minmax", f_n_para = list(c(0, 100)))
)
normalisation_spec <- list(Address = "$Log$Normalise$global_specs",
                           Distribution = norm_alternatives,
                           Type = "discrete")

SA_specs <- list(Winmax = winmax_spec, Normalisation = normalisation_spec)

set.seed(2024)

# Time get_sensitivity on the large dummy coin
baseline_time <- system.time({
  suppressMessages({
    baseline_res <- get_sensitivity(coin_large, SA_specs = SA_specs, N = 10,
                                    SA_type = "SA", dset = "Aggregated", iCode = "Index",
                                    quietly = TRUE)
  })
})

print(baseline_time)

cat("\n=== More Complete Sensitivity Test with Multiple Steps ===\n")

# Define more comprehensive sensitivity specs covering multiple pipeline steps
# 1. Imputation alternatives
imputation_spec <- list(
  Address = "$Log$Impute$f_i",
  Distribution = c("i_mean", "i_median", "i_mean_grp"),
  Type = "discrete"
)

# 2. Winsorization alternatives
winmax_spec_comprehensive <- list(
  Address = "$Log$Treat$global_specs$f1_para$winmax",
  Distribution = c(3, 5, 7),
  Type = "discrete"
)

# 3. Normalisation alternatives (different methods)
normalisation_spec_comprehensive <- list(
  Address = "$Log$Normalise$global_specs",
  Distribution = list(
    list(f_n = "n_minmax", f_n_para = list(c(0, 100))),
    list(f_n = "n_zscore"),
    list(f_n = "n_borda")
  ),
  Type = "discrete"
)

# 4. Aggregation alternatives
aggregation_spec <- list(
  Address = "$Log$Aggregate$f_ag",
  Distribution = c("a_amean", "a_gmean"),
  Type = "discrete"
)

# Combine all specifications
SA_specs_comprehensive <- list(
  Imputation = imputation_spec,
  Winmax = winmax_spec_comprehensive,
  Normalisation = normalisation_spec_comprehensive,
  Aggregation = aggregation_spec
)

set.seed(2024)

# Time the comprehensive sensitivity analysis
comprehensive_time <- system.time({
  suppressMessages({
    comprehensive_res <- get_sensitivity(
      coin_large,
      SA_specs = SA_specs_comprehensive,
      N = 50,  # More iterations to test different permutations
      SA_type = "SA",
      dset = "Aggregated",
      iCode = "Index",
      quietly = TRUE
    )
  })
})

cat("\nComprehensive Sensitivity Test Results:\n")
cat(sprintf("  Number of specifications: %d\n", length(SA_specs_comprehensive)))
cat(sprintf("  Number of iterations: %d\n", 50))
cat(sprintf("  Elapsed time: %.2f seconds\n", comprehensive_time["elapsed"]))
cat(sprintf("  User time: %.2f seconds\n", comprehensive_time["user.self"]))
cat(sprintf("  System time: %.2f seconds\n", comprehensive_time["sys.self"]))

# Summary of results
if(!is.null(comprehensive_res) && !is.null(comprehensive_res$Scores)){
  cat(sprintf("  Result dimensions: %d x %d\n", nrow(comprehensive_res$Scores), ncol(comprehensive_res$Scores)))
}

print(comprehensive_time)
