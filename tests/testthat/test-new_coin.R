# tests for new_coin()
# just class checks for now

test_that("class check",{
  expect_s3_class(new_coin(ASEM_iData, ASEM_iMeta, quietly = TRUE), "coin")
  expect_s3_class(new_coin(ASEM_iData_p, ASEM_iMeta, split_to = "all", quietly = TRUE), c("purse", "data.frame"))
})

test_that("check_iData", {

  # spaces
  iData <- ASEM_iData
  names(iData)[10] <- "spa ce"
  expect_error(check_iData(iData))

  # number start
  iData <- ASEM_iData
  names(iData)[10] <- "1number"
  expect_error(check_iData(iData))

})

test_that("check_iMeta", {

  # spaces
  iMeta <- ASEM_iMeta
  iMeta$iCode[10] <- "spa ce"
  expect_error(check_iMeta(iMeta))

  # number start
  iMeta <- ASEM_iMeta
  iMeta$iCode[10] <- "1number"
  expect_error(check_iMeta(iMeta))

  # duplicate codes
  iMeta <- ASEM_iMeta
  iMeta <- rbind(iMeta[1,], iMeta)
  expect_error(check_iMeta(iMeta))

})

restore_original_meta <- function(balanced_meta, original_meta){
  original <- original_meta
  original$Parent[original$Parent == ""] <- NA_character_
  cols <- names(original)
  cleaned <- balanced_meta[balanced_meta$IsPlaceholder != TRUE, cols, drop = FALSE]
  cleaned <- cleaned[match(original$iCode, cleaned$iCode), , drop = FALSE]
  cleaned$Parent <- original$Parent
  cleaned$Level <- original$Level
  cleaned$Weight <- original$Weight
  cleaned$Direction <- original$Direction
  cleaned$Type <- original$Type
  rownames(cleaned) <- NULL
  rownames(original) <- NULL
  cleaned
}

test_that("new_unbalanced_coin balances hierarchy", {

  coin <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)

  expect_s3_class(coin, c("unbalanced_coin", "coin"))
  expect_true(length(coin$Meta$Unbalanced$PlaceholderCodes) > 0)

  # public lineage should match the original unbalanced structure
  expect_equal(coin$Meta$Lineage, coin$Meta$Lineage_unbalanced)

  # balanced metadata retains placeholders internally, but raw data stays clean
  expect_true(any(coin$Meta$Unbalanced$BalancedMeta$IsPlaceholder))
  expect_setequal(names(coin$Data$Raw), c("uCode", "IndA1", "IndA2", "IndB"))

})

test_that("new_unbalanced_coin handles aggregate column supplied in data", {

  data("unbal_scen1_iData", package = "COINr")
  data("unbal_scen1_iMeta", package = "COINr")

  coin <- new_unbalanced_coin(unbal_scen1_iData, unbal_scen1_iMeta, quietly = TRUE)

  balanced <- coin$Meta$Unbalanced$BalancedMeta
  restored <- restore_original_meta(balanced, unbal_scen1_iMeta)

  expect_identical(restored, unbal_scen1_iMeta)
  expect_true(any(coin$Meta$Unbalanced$BalancedMeta$IsPlaceholder))
})

test_that("new_unbalanced_coin handles indicators skipping intermediate levels", {

  data("unbal_scen2_iData", package = "COINr")
  data("unbal_scen2_iMeta", package = "COINr")

  coin <- new_unbalanced_coin(unbal_scen2_iData, unbal_scen2_iMeta, quietly = TRUE)
  balanced <- coin$Meta$Unbalanced$BalancedMeta
  restored <- restore_original_meta(balanced, unbal_scen2_iMeta)

  expect_identical(restored, unbal_scen2_iMeta)
  expect_true(length(coin$Meta$Unbalanced$PlaceholderCodes) >= 1)
})

test_that("new_unbalanced_coin handles multiple branches requiring placeholders", {

  data("unbal_scen3_iData", package = "COINr")
  data("unbal_scen3_iMeta", package = "COINr")

  coin <- new_unbalanced_coin(unbal_scen3_iData, unbal_scen3_iMeta, quietly = TRUE)
  balanced <- coin$Meta$Unbalanced$BalancedMeta
  restored <- restore_original_meta(balanced, unbal_scen3_iMeta)

  expect_identical(restored, unbal_scen3_iMeta)
  expect_true(length(coin$Meta$Unbalanced$PlaceholderCodes) >= 1)
})

test_that("new_unbalanced_coin errors when aggregates remain at level 1", {

  expect_error(
    new_unbalanced_coin(unbal_scen3_iData, unbal_scen4_iMeta, quietly = TRUE),
    "Aggregates must have level 2 or higher",
    fixed = TRUE
  )
})

test_that("new_unbalanced_coin handles user-specified levels differing from computed", {
  # Scenario: Indicator L1 -> Aggregate L3 (user specifies L3, computed would be L2)
  # This mimics the client's Food/FoodSecure case where user wants pillars at same
  # conceptual level even though they have different tree depths

  iData <- data.frame(
    uCode = c("U1", "U2", "U3"),
    IndA = c(10, 20, 30),
    IndB = c(15, 25, 35),
    stringsAsFactors = FALSE
  )

  iMeta <- data.frame(
    iCode = c("IndA", "IndB", "PillarA", "Index"),
    Level = c(1, 1, 3, 4),
    Parent = c("PillarA", "PillarA", "Index", NA),
    Direction = c(1, 1, 1, 1),
    Weight = c(1, 1, 1, 1),
    Type = c("Indicator", "Indicator", "Aggregate", "Aggregate"),
    stringsAsFactors = FALSE
  )

  coin <- new_unbalanced_coin(iData, iMeta, quietly = TRUE)

  expect_s3_class(coin, c("unbalanced_coin", "coin"))

  # Should have created placeholder(s) to bridge L1 -> L3 gap
  placeholders <- coin$Meta$Unbalanced$PlaceholderCodes
  expect_true(length(placeholders) >= 1)

  # Balanced metadata should have L2 placeholders
  balanced <- coin$Meta$Unbalanced$BalancedMeta
  placeholder_levels <- balanced$Level[balanced$IsPlaceholder == TRUE]
  expect_true(2 %in% placeholder_levels)
})

test_that("new_unbalanced_coin handles multi-level gaps with user-specified levels", {
  # Scenario: Indicator L1 -> Aggregate L5 (4-level gap, needs 3 placeholders)

  iData <- data.frame(
    uCode = c("U1", "U2", "U3"),
    Ind1 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  iMeta <- data.frame(
    iCode = c("Ind1", "TopLevel"),
    Level = c(1, 5),
    Parent = c("TopLevel", NA),
    Direction = c(1, 1),
    Weight = c(1, 1),
    Type = c("Indicator", "Aggregate"),
    stringsAsFactors = FALSE
  )

  coin <- new_unbalanced_coin(iData, iMeta, quietly = TRUE)

  expect_s3_class(coin, c("unbalanced_coin", "coin"))

  # Should have created 3 placeholders (L2, L3, L4)
  placeholders <- coin$Meta$Unbalanced$PlaceholderCodes
  expect_true(length(placeholders) >= 3)

  balanced <- coin$Meta$Unbalanced$BalancedMeta
  placeholder_levels <- sort(unique(balanced$Level[balanced$IsPlaceholder == TRUE]))
  expect_true(all(c(2, 3, 4) %in% placeholder_levels))
})

test_that("new_unbalanced_coin handles mixed user-specified and computed levels", {
  # Scenario: Some branches use computed levels, others need placeholders
  # PillarA: IndA1, IndA2 -> SubA (L2) -> PillarA (L3) - natural depth
  # PillarB: IndB -> PillarB (L3) - needs L2 placeholder

  iData <- data.frame(
    uCode = c("U1", "U2", "U3"),
    IndA1 = c(10, 20, 30),
    IndA2 = c(11, 21, 31),
    IndB = c(15, 25, 35),
    stringsAsFactors = FALSE
  )

  iMeta <- data.frame(
    iCode = c("IndA1", "IndA2", "IndB", "SubA", "PillarA", "PillarB", "Index"),
    Level = c(1, 1, 1, 2, 3, 3, 4),
    Parent = c("SubA", "SubA", "PillarB", "PillarA", "Index", "Index", NA),
    Direction = c(1, 1, 1, 1, 1, 1, 1),
    Weight = c(1, 1, 1, 1, 1, 1, 1),
    Type = c("Indicator", "Indicator", "Indicator", "Aggregate", "Aggregate", "Aggregate", "Aggregate"),
    stringsAsFactors = FALSE
  )

  coin <- new_unbalanced_coin(iData, iMeta, quietly = TRUE)

  expect_s3_class(coin, c("unbalanced_coin", "coin"))

  # Should have placeholder for IndB -> PillarB gap
  placeholders <- coin$Meta$Unbalanced$PlaceholderCodes
  expect_true(length(placeholders) >= 1)

  # PillarA branch should NOT have extra placeholders (natural L2 SubA exists)
  balanced <- coin$Meta$Unbalanced$BalancedMeta
  pillarA_children <- balanced$iCode[balanced$Parent == "PillarA" & !is.na(balanced$Parent)]
  expect_true("SubA" %in% pillarA_children)
})
