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
