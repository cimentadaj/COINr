#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
  library(COINr)
})

load("data/ASEM_iData.rda")
load("data/ASEM_iMeta.rda")

iData <- ASEM_iData

iMeta <- ASEM_iMeta %>%
  mutate(Parent = str_trim(Parent))

# Lift aggregate levels by one to make room for new intermediate nodes
# (Indicators remain at Level 1, aggregates move up a level.)
iMeta <- iMeta %>%
  mutate(
    Level = case_when(
      Type == "Indicator" ~ 1L,
      Type == "Aggregate" ~ Level + 1L,
      TRUE ~ Level
    )
  )

# Define new aggregate nodes that will create uneven depth in selected pillars
aggregate_specs <- tribble(
  ~agg_code,      ~agg_name,                                 ~parent,      ~children,
  "PhysTrans",    "Physical transport connectivity",          "Physical",  list(c("Flights", "Ship", "Bord")),
  "PhysEnergy",   "Physical cross-border energy",            "Physical",  list(c("Elec", "Gas")),
  "PhysDigital",  "Physical digital infrastructure",         "Physical",  list(c("ConSpeed", "Cov4G")),
  "ConTrade",     "Con: trade integration",                  "ConEcFin",  list(c("Goods", "Services")),
  "ConFinance",   "Con: financial interconnectedness",       "ConEcFin",  list(c("FDI", "PRemit", "ForPort")),
  "P2PKnow",      "P2P knowledge exchange",                  "P2P",       list(c("StMob", "Research", "Pat")),
  "P2PCulture",   "P2P cultural flows",                      "P2P",       list(c("CultServ", "CultGood")),
  "EnvClimate",   "Environmental climate performance",       "Environ",   list(c("Renew", "CO2")),
  "EnvResources", "Environmental resource stewardship",      "Environ",   list(c("PrimEner", "MatCon", "Forest")),
  "SocEquity",    "Social equity outcomes",                  "Social",    list(c("Poverty", "Palma", "TertGrad")),
  "SocCivics",    "Social civic fabric",                     "Social",    list(c("FreePress", "NGOs", "CPI")),
  "SusDebt",      "Sustainable finance: debt burden",        "SusEcFin",  list(c("PubDebt", "PrivDebt")),
  "SusResilience","Sustainable growth resilience",            "SusEcFin",  list(c("GDPGrow", "RDExp"))
)

stopifnot(all(unlist(aggregate_specs$children) %in% iMeta$iCode))

add_intermediate_node <- function(meta, agg_code, agg_name, parent_code, children_codes){
  if(agg_code %in% meta$iCode){
    stop(sprintf("Aggregate code '%s' already exists in metadata.", agg_code))
  }
  parent_row <- meta %>% filter(iCode == parent_code)
  if(nrow(parent_row) != 1){
    stop(sprintf("Parent code '%s' not found or duplicated.", parent_code))
  }
  match_count <- sum(meta$iCode %in% children_codes)
  new_row <- parent_row
  new_row$iCode <- agg_code
  new_row$iName <- agg_name
  new_row$Parent <- parent_code
  new_row$Level <- parent_row$Level - 1L
  if(new_row$Level <= 1){
    stop(sprintf("Derived level for '%s' is not above indicator level.", agg_code))
  }
  new_row$Type <- "Aggregate"
  new_row$Direction <- 1L
  new_row$Weight <- 1
  new_row$Target <- NA_real_
  new_row$Denominator <- NA_character_
  meta <- bind_rows(meta, new_row)
  idx <- which(meta$iCode %in% children_codes)
  if(length(idx) == 0){
    warning(sprintf("No matching children found for '%s'.", agg_code))
  } else {
    meta$Parent[idx] <- agg_code
  }
  meta
}

for(i in seq_len(nrow(aggregate_specs))){
  spec <- aggregate_specs[i,]
  children_codes <- unlist(spec$children[[1]])
  iMeta <- add_intermediate_node(
    meta = iMeta,
    agg_code = spec$agg_code,
    agg_name = spec$agg_name,
    parent_code = spec$parent,
    children_codes = children_codes
  )
}

iMeta <- iMeta %>% arrange(Level, Parent, iCode)

message("New aggregate nodes introduced:")
print(aggregate_specs %>%
        mutate(children = vapply(children, paste, collapse = ", ", character(1))) %>%
        select(agg_code, parent, children))

message("Level distribution after transformation:")
print(table(iMeta$Level, useNA = "ifany"))

message("Aggregate level check:")
print(iMeta %>% filter(Type == "Aggregate") %>% count(Level))
problem_aggs <- iMeta %>% filter(Type == "Aggregate", Level == 1)
if(nrow(problem_aggs) > 0){
  message("Aggregates incorrectly set to Level 1:")
  print(problem_aggs)
}

message("Parent assignments spot-check:")
print(iMeta %>% filter(iCode %in% c("Goods", "Services", "FDI", "PRemit", "ForPort", "Renew", "CO2")) %>%
        select(iCode, Parent, Level))

ASEM_unbal_iData <- iData
ASEM_unbal_iMeta <- iMeta
iMeta_coin <- ASEM_unbal_iMeta %>% select(-iName)

write.csv(iMeta, "tmp_script/unbalanced_proto/iMeta_unbalanced_preview.csv", row.names = FALSE)

# Build unbalanced coin and inspect key structures
unbal_coin <- new_unbalanced_coin(ASEM_unbal_iData, iMeta_coin, quietly = TRUE)

unbal_coin <- Impute(unbal_coin, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
unbal_coin <- Denominate(unbal_coin, dset = "Imputed", write_to = "Denominated")
unbal_coin <- Treat(unbal_coin, dset = "Denominated", write_to = "Treated")
unbal_coin <- Normalise(unbal_coin, dset = "Treated", write_to = "Normalised")
unbal_coin <- Aggregate(unbal_coin, dset = "Normalised")

message("Placeholder helper nodes introduced internally:")
print(head(unbal_coin$Meta$Unbalanced$PlaceholderCodes, 10))

message("Sample lineage (unbalanced view):")
print(head(unbal_coin$Meta$Lineage_unbalanced, 12))

message("Aggregated data set columns:")
print(names(unbal_coin$Data$Aggregated))
level3_codes <- iMeta$iCode[iMeta$Level == 3]
level3_codes <- level3_codes[!is.na(level3_codes)]
missing_lvl3 <- setdiff(level3_codes, names(unbal_coin$Data$Aggregated))
message("Level 3 codes missing from aggregated data:")
print(missing_lvl3)

test_physical <- tryCatch(
  get_data(unbal_coin, dset = "Aggregated", iCodes = "Physical", also_get = "none"),
  error = function(e){
    message("Test fetch for 'Physical' failed: ", e$message)
    NULL
  }
)
if(!is.null(test_physical)){
  message("Physical aggregate (first 5 rows):")
  print(head(test_physical, 5))
}

message("Aggregated scores (Level 3) preview:")
agg_lvl3 <- tryCatch(
  get_data(unbal_coin, dset = "Aggregated", Level = 3, also_get = "none"),
  error = function(e){
    message("Could not fetch Level 3 aggregated data: ", e$message)
    NULL
  }
)
if(!is.null(agg_lvl3)){
  print(head(agg_lvl3, 5))
}

message("Correlation flags check on Normalised data:")
print(head(get_corr_flags(unbal_coin, dset = "Normalised", cor_thresh = 0.75, grouplev = 3)))

message("Effective weights snapshot:")
print(head(get_eff_weights(unbal_coin, out2 = "df"), 10))

usethis::use_data(ASEM_unbal_iData, ASEM_unbal_iMeta, overwrite = TRUE)

message("Helper complete.")
