#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  if(requireNamespace("devtools", quietly = TRUE)){
    devtools::load_all(quiet = TRUE)
  } else {
    library(COINr)
  }
  library(usethis)
})

load("data/ASEM_iData.rda")
load("data/ASEM_iMeta.rda")

# start from the published structures
ASEM_unbal_iData <- ASEM_iData
ASEM_unbal_iMeta <- ASEM_iMeta %>%
  mutate(
    Parent = trimws(Parent),
    Parent = na_if(Parent, "")
  )

# ------------------------------------------------------------------
# Build one compact example that mixes the unbalanced situations
# we exercise elsewhere:
#   * Level 1 indicators (Cov4G, ConSpeed) feed a Level 3 parent
#     directly, skipping the Level 2 stage.
#   * PhysAccess is stored directly in iData and mirrored by a
#     simple Level 1 input so the helper can consume it.
#   * A new Level 3 aggregate (ConnectPlus) combines the above so
#     placeholders are needed internally.
# ------------------------------------------------------------------

ASEM_unbal_iData <- ASEM_unbal_iData %>%
  mutate(
    PhysAccess = rowMeans(select(., Flights, Ship), na.rm = TRUE)
  )

connect_plus_row <- tibble(
  Level = 3L,
  iCode = "ConnectPlus",
  iName = "Connectivity plus pilot pillar",
  Direction = 1L,
  Weight = 0.2,
  Unit = "Index",
  Target = NA_real_,
  Denominator = NA_character_,
  Parent = "Index",
  Type = "Aggregate"
)

phys_access_row <- tibble(
  Level = 2L,
  iCode = "PhysAccess",
  iName = "Physical access composite",
  Direction = 1L,
  Weight = 0.3,
  Unit = "Index",
  Target = NA_real_,
  Denominator = NA_character_,
  Parent = "ConnectPlus",
  Type = "Aggregate"
)

ASEM_unbal_iMeta <- bind_rows(
  ASEM_unbal_iMeta,
  connect_plus_row,
  phys_access_row
) %>%
  mutate(
    Parent = case_when(
      iCode %in% c("Cov4G", "ConSpeed", "Physical") ~ "ConnectPlus",
      TRUE ~ Parent
    ),
    Weight = case_when(
      iCode == "Conn" ~ 0.4,
      iCode == "Sust" ~ 0.4,
      iCode == "ConnectPlus" ~ 0.2,
      iCode == "Physical" ~ 0.4,
      iCode == "PhysAccess" ~ 0.3,
      iCode == "Cov4G" ~ 0.15,
      iCode == "ConSpeed" ~ 0.15,
      TRUE ~ Weight
    )
  ) %>%
  arrange(Level, Parent, iCode)

# preview the new branch
message("Children of ConnectPlus (unbalanced mix):")
print(
  ASEM_unbal_iMeta %>%
    filter(Parent == "ConnectPlus") %>%
    select(iCode, Level, Type, Weight)
)

iMeta_coin <- ASEM_unbal_iMeta

unbal_coin <- new_unbalanced_coin(
  iData = ASEM_unbal_iData,
  iMeta = iMeta_coin,
  quietly = TRUE
)
unbal_coin <- Impute(unbal_coin, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
unbal_coin <- Denominate(unbal_coin, dset = "Imputed", write_to = "Denominated")
unbal_coin <- Treat(unbal_coin, dset = "Denominated", write_to = "Treated")
unbal_coin <- Normalise(unbal_coin, dset = "Treated", write_to = "Normalised")
unbal_coin <- Aggregate(unbal_coin, dset = "Normalised")

message("Placeholder helper nodes introduced internally:")
print(head(unbal_coin$Meta$Unbalanced$PlaceholderCodes, 6))

message("Aggregated data preview (ConnectPlus branch):")
print(
  get_data(unbal_coin, dset = "Aggregated", iCodes = "ConnectPlus", also_get = "none") %>%
    head(3)
)
print(
  get_data(unbal_coin, dset = "Aggregated", iCodes = "PhysAccess", also_get = "none") %>%
    head(3)
)

usethis::use_data(
  ASEM_unbal_iData,
  ASEM_unbal_iMeta,
  overwrite = TRUE
)

message("ASEM unbalanced helper complete.")
