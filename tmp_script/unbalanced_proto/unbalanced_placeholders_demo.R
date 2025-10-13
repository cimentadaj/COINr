#!/usr/bin/env Rscript

devtools::install_github("cimentadaj/COINr@unbalanced-coin-prototype")
library(COINr)
library(dplyr)

data("unbal_iData", package = "COINr")
data("unbal_iMeta", package = "COINr")

# Scenario 1: Aggregate supplied directly in the data
idata1 <- unbal_iData
meta1 <- unbal_iMeta
idata1$AggSolo <- rowMeans(idata1[c("IndA1", "IndA2")])
agg_row <- data.frame(iCode = "AggSolo", Level = 2L, Parent = "Index", Direction = 1L,
                      Weight = 0.5, Type = "Aggregate", stringsAsFactors = FALSE)
meta1 <- rbind(meta1, agg_row)
meta1$Parent[meta1$Parent == ""] <- NA_character_

cat("\nSCENARIO 1 - ORIGINAL META\n")
print(meta1 %>% arrange(Level))
coin1 <- new_unbalanced_coin(idata1, meta1, quietly = TRUE)

cat("SCENARIO 1 - BALANCED META\n")
print(coin1$Meta$Unbalanced$BalancedMeta %>% arrange(Level))

# Scenario 2: Indicator skipping intermediate levels
idata2 <- unbal_iData
meta2 <- unbal_iMeta
idata2$SkipIndicator <- idata2$IndB * 1.1
top_row <- data.frame(iCode = "Mega", Level = 4L, Parent = NA_character_, Direction = 1L,
                      Weight = 1, Type = "Aggregate", stringsAsFactors = FALSE)
meta2 <- rbind(meta2, top_row)
meta2$Parent[meta2$Parent == ""] <- NA_character_
meta2$Parent[meta2$iCode == "Index"] <- "Mega"
meta2$Weight[meta2$iCode == "Index"] <- 0.5
skip_row <- data.frame(iCode = "SkipIndicator", Level = 1L, Parent = "Mega", Direction = 1L,
                       Weight = 0.5, Type = "Indicator", stringsAsFactors = FALSE)
meta2 <- rbind(meta2, skip_row)
meta2$Parent[meta2$Parent == ""] <- NA_character_

cat("\nSCENARIO 2 - ORIGINAL META\n")
print(meta2 %>% arrange(Level))
coin2 <- new_unbalanced_coin(idata2, meta2, quietly = TRUE)
cat("SCENARIO 2 - BALANCED META\n")
print(coin2$Meta$Unbalanced$BalancedMeta %>% arrange(Level))

# Scenario 3: Wider branch with multiple placeholders needed
idata3 <- unbal_iData
meta3 <- unbal_iMeta
idata3$IndC <- idata3$IndA1 * 0.8
idata3$IndD <- idata3$IndA2 * 1.2
idata3$IndE <- idata3$IndB * 0.9
idata3$AggWide <- rowMeans(idata3[c("IndC", "IndD")])
meta3$Weight[meta3$iCode == "SubA"] <- 0.2
meta3$Weight[meta3$iCode == "IndB"] <- 0.2
new_rows <- rbind(
  data.frame(iCode = "IndC", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2,
             Type = "Indicator", stringsAsFactors = FALSE),
  data.frame(iCode = "IndD", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2,
             Type = "Indicator", stringsAsFactors = FALSE),
  data.frame(iCode = "IndE", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2,
             Type = "Indicator", stringsAsFactors = FALSE),
  data.frame(iCode = "AggWide", Level = 2L, Parent = "Index", Direction = 1L, Weight = 0.2,
             Type = "Aggregate", stringsAsFactors = FALSE)
)
meta3 <- rbind(meta3, new_rows)
meta3$Parent[meta3$Parent == ""] <- NA_character_

cat("\nSCENARIO 3 - ORIGINAL META\n")
print(meta3 %>% arrange(Level))
coin3 <- new_unbalanced_coin(idata3, meta3, quietly = TRUE)
cat("SCENARIO 3 - BALANCED META\n")
print(coin3$Meta$Unbalanced$BalancedMeta %>% arrange(Level))

# Scenario 4: Level-1 aggregate should trigger an error
meta4 <- meta3
meta4$Type[meta4$iCode == "IndC"] <- "Aggregate"

cat("\nSCENARIO 4 - EXPECT ERROR\n")
print(meta4)
tryCatch(
  {
    new_unbalanced_coin(idata3, meta4, quietly = TRUE)
    message("ERROR: Expected failure did not occur.")
  },
  error = function(e){
    message("EXPECTED ERROR: ", e$message)
  }
)
