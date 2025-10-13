#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(COINr)
  library(usethis)
})

data("unbal_iData", package = "COINr")
data("unbal_iMeta", package = "COINr")

base_meta <- unbal_iMeta
base_meta$Denominator <- NA_character_

# Scenario 1: Aggregate supplied directly in the data
unbal_scen1_iData <- unbal_iData
unbal_scen1_iMeta <- base_meta
unbal_scen1_iData$AggSolo <- rowMeans(unbal_scen1_iData[c("IndA1", "IndA2")])
agg_row <- data.frame(
  iCode = "AggSolo",
  Level = 2L,
  Parent = "Index",
  Direction = 1L,
  Weight = 0.5,
  Type = "Aggregate",
  Denominator = NA_character_,
  stringsAsFactors = FALSE
)
denom_row <- data.frame(
  iCode = "DenomA",
  Level = NA_integer_,
  Parent = NA_character_,
  Direction = 1L,
  Weight = NA_real_,
  Type = "Denominator",
  Denominator = NA_character_,
  stringsAsFactors = FALSE
)
unbal_scen1_iMeta <- rbind(unbal_scen1_iMeta, agg_row, denom_row)
unbal_scen1_iMeta$Parent[unbal_scen1_iMeta$Parent == ""] <- NA_character_
unbal_scen1_iMeta <- unbal_scen1_iMeta[order(unbal_scen1_iMeta$Level, unbal_scen1_iMeta$iCode), ]
rownames(unbal_scen1_iMeta) <- NULL
unbal_scen1_iMeta$Denominator[unbal_scen1_iMeta$iCode %in% c("IndA1", "IndA2", "IndB")] <- "DenomA"

# add denominator column to data
unbal_scen1_iData$DenomA <- c(100, 120, 90)

# Scenario 2: Indicator bypassing intermediate levels
unbal_scen2_iData <- unbal_iData
unbal_scen2_iMeta <- base_meta
unbal_scen2_iData$SkipIndicator <- unbal_scen2_iData$IndB * 1.1
top_row <- data.frame(
  iCode = "Mega",
  Level = 4L,
  Parent = NA_character_,
  Direction = 1L,
  Weight = 1,
  Type = "Aggregate",
  Denominator = NA_character_,
  stringsAsFactors = FALSE
)
unbal_scen2_iMeta <- rbind(unbal_scen2_iMeta, top_row)
unbal_scen2_iMeta$Parent[unbal_scen2_iMeta$Parent == ""] <- NA_character_
idx_index <- unbal_scen2_iMeta$iCode == "Index"
unbal_scen2_iMeta$Parent[idx_index] <- "Mega"
unbal_scen2_iMeta$Weight[idx_index] <- 0.5
skip_row <- data.frame(
  iCode = "SkipIndicator",
  Level = 1L,
  Parent = "Mega",
  Direction = 1L,
  Weight = 0.5,
  Type = "Indicator",
  Denominator = NA_character_,
  stringsAsFactors = FALSE
)
unbal_scen2_iMeta <- rbind(unbal_scen2_iMeta, skip_row)
unbal_scen2_iMeta$Parent[unbal_scen2_iMeta$Parent == ""] <- NA_character_
unbal_scen2_iMeta <- unbal_scen2_iMeta[order(unbal_scen2_iMeta$Level, unbal_scen2_iMeta$iCode), ]
rownames(unbal_scen2_iMeta) <- NULL
if(!"DenomA" %in% unbal_scen2_iMeta$iCode){
  denom_row2 <- data.frame(
    iCode = "DenomA",
    Level = NA_integer_,
    Parent = NA_character_,
    Direction = 1L,
    Weight = NA_real_,
    Type = "Denominator",
    Denominator = NA_character_,
    stringsAsFactors = FALSE
  )
  unbal_scen2_iMeta <- rbind(unbal_scen2_iMeta, denom_row2)
}
unbal_scen2_iMeta$Denominator[unbal_scen2_iMeta$iCode %in% c("IndA1", "IndA2", "IndB", "SkipIndicator")] <- "DenomA"
unbal_scen2_iMeta <- unbal_scen2_iMeta[order(unbal_scen2_iMeta$Level, unbal_scen2_iMeta$iCode), ]
rownames(unbal_scen2_iMeta) <- NULL

unbal_scen2_iData$DenomA <- c(100, 120, 90)

# Scenario 3: Wider branch requiring placeholders
unbal_scen3_iData <- unbal_iData
unbal_scen3_iMeta <- base_meta
unbal_scen3_iData$IndC <- unbal_scen3_iData$IndA1 * 0.8
unbal_scen3_iData$IndD <- unbal_scen3_iData$IndA2 * 1.2
unbal_scen3_iData$IndE <- unbal_scen3_iData$IndB * 0.9
unbal_scen3_iData$AggWide <- rowMeans(unbal_scen3_iData[c("IndC", "IndD")])

unbal_scen3_iMeta$Weight[unbal_scen3_iMeta$iCode == "SubA"] <- 0.2
unbal_scen3_iMeta$Weight[unbal_scen3_iMeta$iCode == "IndB"] <- 0.2

new_rows <- rbind(
  data.frame(iCode = "IndC", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", Denominator = NA_character_, stringsAsFactors = FALSE),
  data.frame(iCode = "IndD", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", Denominator = NA_character_, stringsAsFactors = FALSE),
  data.frame(iCode = "IndE", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", Denominator = NA_character_, stringsAsFactors = FALSE),
  data.frame(iCode = "AggWide", Level = 2L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Aggregate", Denominator = NA_character_, stringsAsFactors = FALSE)
)
unbal_scen3_iMeta <- rbind(unbal_scen3_iMeta, new_rows)
unbal_scen3_iMeta$Parent[unbal_scen3_iMeta$Parent == ""] <- NA_character_
unbal_scen3_iMeta <- unbal_scen3_iMeta[order(unbal_scen3_iMeta$Level, unbal_scen3_iMeta$iCode), ]
rownames(unbal_scen3_iMeta) <- NULL
if(!"DenomA" %in% unbal_scen3_iMeta$iCode){
  denom_row3 <- data.frame(
    iCode = "DenomA",
    Level = NA_integer_,
    Parent = NA_character_,
    Direction = 1L,
    Weight = NA_real_,
    Type = "Denominator",
    Denominator = NA_character_,
    stringsAsFactors = FALSE
  )
  unbal_scen3_iMeta <- rbind(unbal_scen3_iMeta, denom_row3)
}
unbal_scen3_iMeta$Denominator[unbal_scen3_iMeta$iCode %in% c("IndA1", "IndA2", "IndB", "IndC", "IndD", "IndE")] <- "DenomA"
unbal_scen3_iMeta <- unbal_scen3_iMeta[order(unbal_scen3_iMeta$Level, unbal_scen3_iMeta$iCode), ]
rownames(unbal_scen3_iMeta) <- NULL

unbal_scen3_iData$DenomA <- c(100, 120, 90)

# Scenario 4 meta: same data as scenario 3 but aggregate left at Level 1
unbal_scen4_iMeta <- unbal_scen3_iMeta
unbal_scen4_iMeta$Type[unbal_scen4_iMeta$iCode == "IndC"] <- "Aggregate"
rownames(unbal_scen4_iMeta) <- NULL

usethis::use_data(
  unbal_scen1_iData, unbal_scen1_iMeta,
  unbal_scen2_iData, unbal_scen2_iMeta,
  unbal_scen3_iData, unbal_scen3_iMeta,
  unbal_scen4_iMeta,
  overwrite = TRUE
)
