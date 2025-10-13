#!/usr/bin/env Rscript

devtools::install_github('cimentadaj/COINr@unbalanced-coin-prototype')
library(COINr)

# -----------------------------------------------------------------------------
# Load packaged unbalanced ASEM data
# -----------------------------------------------------------------------------

data("ASEM_unbal_iData", package = "COINr")
data("ASEM_unbal_iMeta", package = "COINr")

meta_coin <- ASEM_unbal_iMeta
meta_coin$iName <- NULL

coin <- new_unbalanced_coin(ASEM_unbal_iData, meta_coin, quietly = TRUE)

# -----------------------------------------------------------------------------
# Pipeline on the generated unbalanced ASEM coin
# -----------------------------------------------------------------------------

coin <- Impute(coin, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
coin <- Denominate(coin, dset = "Imputed", write_to = "Denominated")
coin <- Treat(coin, dset = "Denominated", write_to = "Treated")
coin <- Normalise(coin, dset = "Treated", write_to = "Normalised")
coin <- Aggregate(coin, dset = "Normalised")
coin <- Screen(coin, dset = "Normalised", unit_screen = "byNA", dat_thresh = 0.9)

print(coin)

get_cronbach(coin, dset = "Aggregated", iCodes = "PhysTrans", Level = 1)
get_corr(coin, dset = "Aggregated", Levels = 3, pval = 0)
get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75, grouplev = 3)
get_denom_corr(coin, dset = "Raw", cor_thresh = 0.5)
get_data(coin, dset = "Aggregated", Level = 3, also_get = "none")
get_data_avail(coin, dset = "Aggregated", out2 = "list")

get_pvals(coin, dset = "Normalised", Level = 1)

get_eff_weights(coin, out2 = "df")

level4_codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Level == 4 & coin$Meta$Ind$Type == "Aggregate"]
level4_codes <- level4_codes[!is.na(level4_codes)]
get_opt_weights(coin, itarg = "equal", dset = "Aggregated", Level = 4, iCodes = level4_codes, out2 = "list")

noise_specs <- data.frame(Level = c(1, 4), NoiseFactor = c(0.15, 0.1))
get_noisy_weights(coin, noise_specs = noise_specs, Nrep = 3)

get_PCA(coin, dset = "Aggregated", Level = 3, by_groups = TRUE, out2 = "list", nowarnings = TRUE)

get_results(coin, dset = "Aggregated", tab_type = "Aggs")
get_stats(coin, dset = "Aggregated", out2 = "df")
get_unit_summary(coin, usel = "AUT", Levels = c(1, 3, 5), dset = "Aggregated", nround = NULL)

res <- remove_elements(coin, Level = coin$Meta$maxlev - 2, dset = "Aggregated", iCode = "Conn", quietly = TRUE)
res$MeanAbsDiff

# -----------------------------------------------------------------------------
# Quick plot examples for the unbalanced ASEM coin
# -----------------------------------------------------------------------------

plot_bar(coin, dset = "Aggregated", iCode = "Conn")
plot_corr(coin, dset = "Aggregated", Levels = 3)
plot_dist(coin, dset = "Raw", iCodes = c("FDI", "Goods"))
plot_dot(coin, dset = "Aggregated", iCode = "Conn", usel = "AUT")
framework_coin <- coin
framework_coin$Meta$Ind$Parent[framework_coin$Meta$Ind$iCode == "Index"] <- "Index"
plot_framework(framework_coin, colour_level = 3)
plot_scatter(coin, dsets = c("Normalised", "Normalised"), iCodes = c("FDI", "Goods"))

SA_specs <- list(
  Winmax = list(Address = "$Log$Treat$global_specs$f1_para$winmax", Distribution = 1:5, Type = "discrete"),
  Normalisation = list(Address = "$Log$Normalise$global_specs",
                       Distribution = list(
                         list(f_n = "n_minmax", f_n_para = list(c(1, 100))),
                         list(f_n = "n_minmax", f_n_para = list(c(1, 100)))
                       ),
                       Type = "discrete")
)

SA_res <- get_sensitivity(coin, SA_specs = SA_specs, N = 5, SA_type = "SA",
                dset = "Aggregated", iCode = "Index", Nboot = 10, quietly = TRUE)

plot_uncertainty(SA_res)

head(SA_res$RankStats)

plot_sensitivity(SA_res, ptype = 'box')
