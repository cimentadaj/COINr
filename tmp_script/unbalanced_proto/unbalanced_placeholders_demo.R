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
print(get_corr(coin, Levels = c(1, 3), dset = "Aggregated", make_long = FALSE))
print(get_cronbach(coin, dset = "Aggregated", iCodes = c("IndA1", "IndA2"), Level = 1))
print(get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75, grouplev = 3))
print(get_denom_corr(coin, dset = "Denominated", cor_thresh = 0.5))
print(get_data(coin, dset = "Aggregated", Level = 3, also_get = "none"))

cat("\nADDITIONAL EXAMPLES\n")
print(get_data_avail(coin, dset = "Aggregated", out2 = "list"))
print(get_pvals(coin, dset = "Normalised", Level = 1))
print(get_eff_weights(coin, out2 = "df"))
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
