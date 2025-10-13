#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if(requireNamespace("devtools", quietly = TRUE)){
    devtools::load_all(quiet = TRUE)
  }
  if(!"package:COINr" %in% search()){
    library(COINr)
  }
})

data("unbal_iData", package = "COINr")
data("unbal_iMeta", package = "COINr")

# Scenario 1: Aggregate set directly in a higher level without any Indicator first level
idata1 <- unbal_iData
meta1 <- unbal_iMeta
idata1[["AggSolo"]] <- rowMeans(cbind(idata1[["IndA1"]], idata1[["IndA2"]]))
agg_row <- data.frame(
  iCode = "AggSolo",
  Level = 2L,
  Parent = "Index",
  Direction = 1L,
  Weight = 0.5,
  Type = "Aggregate",
  stringsAsFactors = FALSE
)
meta1 <- rbind(meta1, agg_row)
meta1 <- meta1[order(meta1$Level, meta1$iCode), ]

print('CONFIRM UNBALANCEDNESS')
print(meta1)
coin1 <- new_unbalanced_coin(idata1, meta1, quietly = TRUE)
balanced1 <- coin1[["Meta"]][["Unbalanced"]][["BalancedMeta"]]
balanced1 <- balanced1[order(balanced1$Level, balanced1$iCode), ]

print('CONFIRM BALANCEDNESS')
print(balanced1)

# Scenario 2: Indicator set at the first level but used directly in a higher level (bypassing intermediate levels)
idata2 <- unbal_iData
meta2 <- unbal_iMeta

idata2[["SkipIndicator"]] <- idata2[["IndB"]] * 1.1
top_row <- data.frame(
  iCode = "Mega",
  Level = 4L,
  Parent = NA_character_,
  Direction = 1L,
  Weight = 1,
  Type = "Aggregate",
  stringsAsFactors = FALSE
)
meta2 <- rbind(meta2, top_row)
idx_index <- meta2[["iCode"]] == "Index"
meta2[["Parent"]][idx_index] <- "Mega"
meta2[["Weight"]][idx_index] <- 0.5
skip_row <- data.frame(
  iCode = "SkipIndicator",
  Level = 1L,
  Parent = "Mega",
  Direction = 1L,
  Weight = 0.5,
  Type = "Indicator",
  stringsAsFactors = FALSE
)
meta2 <- rbind(meta2, skip_row)
meta2 <- meta2[order(meta2$Level, meta2$iCode), ]

print('CONFIRM UNBALANCEDNESS')
print(meta2)

coin2 <- new_unbalanced_coin(idata2, meta2, quietly = TRUE)
balanced2 <- coin2[["Meta"]][["Unbalanced"]][["BalancedMeta"]]
balanced2 <- balanced2[order(balanced2$Level, balanced2$iCode), ]

print('PLACEHOLDERS ARE SET')
print(balanced2)

# Scenario 3: Scenario 1 extended with more direct children of the index (more placeholders needed)
idata3 <- unbal_iData
meta3 <- unbal_iMeta

idata3[["IndC"]] <- idata3[["IndA1"]] * 0.8
idata3[["IndD"]] <- idata3[["IndA2"]] * 1.2
idata3[["IndE"]] <- idata3[["IndB"]] * 0.9
idata3[["AggWide"]] <- rowMeans(cbind(idata3[["IndC"]], idata3[["IndD"]]))

meta3$Weight[meta3$iCode == "SubA"] <- 0.2
meta3$Weight[meta3$iCode == "IndB"] <- 0.2

meta3 <- rbind(
  meta3,
  data.frame(
    iCode = "IndC",
    Level = 1L,
    Parent = "Index",
    Direction = 1L,
    Weight = 0.2,
    Type = "Indicator",
    stringsAsFactors = FALSE
  ),
  data.frame(
    iCode = "IndD",
    Level = 1L,
    Parent = "Index",
    Direction = 1L,
    Weight = 0.2,
    Type = "Indicator",
    stringsAsFactors = FALSE
  ),
  data.frame(
    iCode = "IndE",
    Level = 1L,
    Parent = "Index",
    Direction = 1L,
    Weight = 0.2,
    Type = "Indicator",
    stringsAsFactors = FALSE
  ),
  data.frame(
    iCode = "AggWide",
    Level = 2L,
    Parent = "Index",
    Direction = 1L,
    Weight = 0.2,
    Type = "Aggregate",
    stringsAsFactors = FALSE
  )
)
meta3 <- meta3[order(meta3$Level, meta3$iCode), ]

print('SCENARIO 3 - UNBALANCED META')
print(meta3)

coin3 <- new_unbalanced_coin(idata3, meta3, quietly = TRUE)
balanced3 <- coin3[["Meta"]][["Unbalanced"]][["BalancedMeta"]]
balanced3 <- balanced3[order(balanced3$Level, balanced3$iCode), ]

print('SCENARIO 3 - BALANCED META WITH PLACEHOLDERS')
print(balanced3)
