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
