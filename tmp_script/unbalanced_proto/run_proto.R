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
