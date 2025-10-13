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
