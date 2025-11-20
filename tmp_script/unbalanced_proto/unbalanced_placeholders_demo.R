#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if(requireNamespace("devtools", quietly = TRUE)){
    devtools::load_all(quiet = TRUE)
  }
  if(!"package:COINr" %in% search()){
    library(COINr)
  }
})

data("unbal_scen1_iData", package = "COINr")
data("unbal_scen1_iMeta", package = "COINr")

cat("\nSCENARIO 1 - ORIGINAL META\n")
print(unbal_scen1_iMeta)

coin <- new_unbalanced_coin(unbal_scen1_iData, unbal_scen1_iMeta, quietly = TRUE)
coin <- Impute(coin, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
coin <- Denominate(coin, dset = "Imputed", write_to = "Denominated")
coin <- Treat(coin, dset = "Denominated", write_to = "Treated")
coin <- Normalise(coin, dset = "Treated", write_to = "Normalised")
coin <- Aggregate(coin, dset = "Normalised")
coin <- Screen(coin, dset = "Normalised", unit_screen = "byNA", dat_thresh = 0.9)

cat("\nSCENARIO 1 - BALANCED META WITH PLACEHOLDERS\n")
print(coin$Meta$Unbalanced$BalancedMeta)

cat("\nSCENARIO 1 - RESULTS SUMMARY\n")
print(get_results(coin, dset = "Aggregated", tab_type = "Aggs"))

cat("\nAggregate with different functions per function/parent\n")
mixed_f_ag <- list(
  Level2 = list(default = 'a_amean', SubA = "a_genmean"),
  Level3 = list(default = 'a_genmean')
)

mixed_f_ag_para <- list(
  Level2 = list(SubA = list(p = 2)),
  Level3 = list(default = list(p = 3))
)

mixed_raw <- Aggregate(
  coin,
  dset = "Normalised",
  f_ag = mixed_f_ag,
  f_ag_para = mixed_f_ag_para
)

print(get_results(mixed_raw, dset = "Aggregated", tab_type = "Aggs"))

coin <- mixed_raw

print(get_corr(coin, Levels = c(1, 2), dset = "Aggregated", make_long = FALSE, pval = 1))
print(get_cronbach(coin, dset = "Aggregated", iCodes = c("IndA1", "IndA2"), Level = 1))
print(get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75, grouplev = 3))
print(get_denom_corr(coin, dset = "Denominated", cor_thresh = 0.5))
print(get_data(coin, dset = "Aggregated", Level = 3, also_get = "none"))

cat("\nADDITIONAL EXAMPLES\n")
print(get_data_avail(coin, dset = "Aggregated", out2 = "list"))
print(get_pvals(coin, dset = "Normalised", Level = 1))
print(get_eff_weights(coin, out2 = "df"))
print(get_PCA(coin, dset = "Aggregated", Level = 2, by_groups = TRUE, out2 = "list", nowarnings = TRUE))
print(get_stats(coin, dset = "Aggregated", out2 = "df"))
print(get_unit_summary(coin, usel = "U2", Levels = c(1, 2, 3), dset = "Aggregated", nround = NULL))

level2_codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Level == 2 &
                                     coin$Meta$Ind$Type == "Aggregate" &
                                     !coin$Meta$Ind$IsPlaceholder %in% TRUE]
opt_res <- get_opt_weights(coin, itarg = "equal", dset = "Aggregated", Level = 2,
                           iCodes = level2_codes, out2 = "list")
print(opt_res$WeightsOpt)

noise_specs <- data.frame(Level = c(1, 3), NoiseFactor = c(0.15, 0.10))
print(get_noisy_weights(coin, noise_specs = noise_specs, Nrep = 3))

res_removed <- remove_elements(coin, Level = 1, dset = "Aggregated", iCode = "IndB", quietly = TRUE)
print(res_removed$Scores)

# Correlations are 1 simply because it's rounding up, but they re are ~ 0.99 as you can see with get_corr
print(plot_corr(coin, dset = "Aggregated", Levels = c(1, 2), showvals = TRUE, pval = 1))

print(plot_bar(coin, dset = "Aggregated", iCode = "AggSolo", axes_label = "iName", stack_children = FALSE))

print(plot_dist(coin, dset = "Aggregated", iCodes = c("AggSolo", "SubA"), type = "Violindot"))

print(plot_dot(coin, dset = "Aggregated", iCode = "AggSolo", Level = 2, usel = c("U1", "U3")))

print(plot_framework(coin, colour_level = 2, transparency = TRUE, text_label = "iName"))

print(plot_scatter(coin, dsets = c("Aggregated", "Aggregated"), iCodes = c("AggSolo", "Index"), axes_label = "iName"))

sa_specs <- list(
  Winmax = list(
    Address = "$Log$Treat$global_specs$f1_para$winmax",
    Distribution = 1:3,
    Type = "discrete"
  ),
  Normalisation = list(
    Address = "$Log$Normalise$global_specs",
    Distribution = list(
      list(f_n = "n_minmax", f_n_para = list(c(1, 100))),
      list(f_n = "n_minmax", f_n_para = list(c(1, 100)))
    ),
    Type = "discrete"
  )
)

SA_res <- get_sensitivity(coin, SA_specs = sa_specs, N = 10, SA_type = "SA",
                          dset = "Aggregated", iCode = "Index", quietly = TRUE, Nboot = NULL)

print(plot_uncertainty(SA_res, order_by = "nominal"))
print(plot_sensitivity(SA_res, ptype = "bar"))

# DEA aggregation in final level
coin_dea <- Aggregate(coin, dset = "Normalised", f_ag = c("a_amean", "a_dea"),
                      by_df = c(FALSE, TRUE), w = list(NULL, "none"))

# Re-aggregate with DEA and weight restrictions
dea_result <- get_DEA(coin, wr_type = 2, wr_bounds = c(0.10, 0.75))

# Test indicator combinations within a dimension
combo_res <- get_combinations(coin, dset = "Normalised", dimension = "SubA",
                               PCA_ref = 0.65, cronbach_alpha_ref = 0.7, verbose = FALSE)

# Find all indicators within a group
get_iCodes_in_group(coin, iCode_group = "SubA", at_level = 1)

# Perturb weights by percentage
perturbed_weights <- get_perturbed_weight_samples(c(0.25, 0.25, 0.25, 0.25),
                                                   pert_by = 0.1, Nrep = 10, quietly = TRUE)

# Generate noisy weights
w_nom <- coin$Meta$Ind[coin$Meta$Ind$Type %in% c("Indicator", "Aggregate"),
                       c("iCode", "Weight", "Level", "Parent")]
noise_specs <- data.frame(Level = c(2, 3), NoiseFactor = c(0.25, 0.25))
noisy_wts <- get_noisy_weights2(w = w_nom, noise_specs = noise_specs, Nrep = 3)

# Sensitivity analysis with convergence monitoring
sa_specs_simple <- list(Winmax = list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                                       Distribution = 1:3, Type = "discrete"))
SA_res_v2 <- get_sensitivity2(coin, SA_specs = sa_specs, N = 100, SA_type = "SA",
                              dset = "Aggregated", iCode = "Index", quietly = FALSE,
                              monitor_convergence = TRUE, converge_on = NULL, Nboot = 20)
plot_convergence(SA_res_v2)

# Multivariate statistics by dimension
multivar_stats <- get_statistics(coin, dset = "Normalised", level = 2, warnings = FALSE)

cat("\n\n=== PIPELINE VALIDATION EXAMPLES ===\n\n")

# Create SA specs that modify some parameters
sa_specs_valid <- list(
  Winmax = list(
    Address = "$Log$Treat$global_specs$f1_para$winmax",
    Distribution = 1:3,
    Type = "discrete"
  ),
  Normalisation = list(
    Address = "$Log$Normalise$global_specs$f_n",
    Distribution = c("n_minmax", "n_zscore", "n_rank"),
    Type = "discrete"
  )
)

# Get pipeline table with validation
pipeline_valid <- get_sensitivity_pipeline(coin, sa_specs_valid, validate = TRUE)
print(pipeline_valid)

# Get only modified steps
cat("\nModified steps only:\n")
pipeline_modified <- get_sensitivity_pipeline(coin, sa_specs_valid, focus = "modified")
print(pipeline_modified)


plot_sensitivity_pipeline(coin, sa_specs_valid)
