#!/usr/bin/env Rscript

#devtools::install_github("cimentadaj/COINr@unbalanced-coin-prototype")
devtools::load_all()
library(COINr)

unbal_data <- unbal_iData
unbal_data$DenSub <- c(2, 4, 5)
unbal_data$DenB <- c(10, 12, 15)

unbal_meta <- rbind(
  unbal_iMeta,
  data.frame(
    iCode = c("DenSub", "DenB"),
    Level = NA_integer_,
    Parent = NA_character_,
    Direction = NA_integer_,
    Weight = NA_real_,
    Type = "Denominator",
    stringsAsFactors = FALSE
  )
)

coin <- new_unbalanced_coin(unbal_data, unbal_meta, quietly = TRUE)

coin$Data$Raw$IndA1[2] <- NA

coin <- Impute(coin, dset = "Raw", f_i = "i_mean", write_to = "Imputed")

coin <- Denominate(coin, dset = "Raw", denoms = data.frame(
  uCode = unbal_iData$uCode,
  DenSub = c(2, 4, 5),
  DenB = c(10, 12, 15)
), denomby = data.frame(
  iCode = c("IndA1", "IndA2", "IndB"),
  Denominator = c("DenSub", "DenSub", "DenB"),
  ScaleFactor = 1
), write_to = "Denom")

coin <- Treat(coin, dset = "Imputed", write_to = "Treated")

coin <- Normalise(coin, dset = "Treated", write_to = "Normalised")

coin <- Aggregate(coin, dset = "Raw")

coin <- Screen(coin, dset = "Raw", unit_screen = "byNA", dat_thresh = 0.9)

coin

get_cronbach(coin, dset = "Aggregated", iCodes = "SubA", Level = 2)
get_corr(coin, dset = "Aggregated", Levels = 2, pval = 0)
get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75, grouplev = 2)
get_denom_corr(coin, dset = "Raw", cor_thresh = 0.5)
get_data(coin, dset = "Aggregated", Level = 2, also_get = "none")
get_data_avail(coin, dset = "Aggregated", out2 = "list")

get_pvals(coin, dset = "Normalised", Level = 1)

get_eff_weights(coin, out2 = "df")

get_opt_weights(coin, itarg = "equal", dset = "Aggregated", Level = 2, out2 = "list")

noise_specs <- data.frame(Level = c(1, 2), NoiseFactor = c(0.15, 0.1))
get_noisy_weights(coin, noise_specs = noise_specs, Nrep = 3)

get_PCA(coin, dset = "Aggregated", Level = 2, by_groups = TRUE, out2 = "list", nowarnings = TRUE)

get_results(coin, dset = "Aggregated", tab_type = "Aggs")

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
                dset = "Aggregated", iCode = "Index", quietly = TRUE)

plot_uncertainty(SA_res)

head(SA_res$RankStats)

plot_sensitivity(SA_res, ptype = 'box')

# minimal vignette-style pipeline on the unbalanced example
results_coin <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
results_coin <- Impute(results_coin, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
results_coin <- Treat(results_coin, dset = "Imputed", write_to = "Treated")
results_coin <- Normalise(results_coin, dset = "Treated", write_to = "Normalised")
results_coin <- Aggregate(results_coin, dset = "Normalised")
get_results(results_coin, dset = "Aggregated", tab_type = "Aggs")
