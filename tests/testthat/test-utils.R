test_that("directionalise", {

  iData <- ASEM_iData[c("LPI", "Flights", "CO2")]
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  iData_ <- directionalise(iData, coin)

  iData2 <- iData
  iData2$CO2 <- -iData2$CO2

  expect_equal(iData_, iData2)

})

test_that("get_iCodes_in_group works with balanced coins", {

  # Build standard balanced coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # Test finding level 2 codes (pillars) within a level 3 group (sub-index)
  pillars_in_conn <- get_iCodes_in_group(coin, iCode_group = "Conn", at_level = 2)
  expect_type(pillars_in_conn, "character")
  expect_true(all(c("Political", "Instit", "P2P", "Physical", "ConEcFin") %in% pillars_in_conn))
  expect_equal(length(pillars_in_conn), 5)

  # Test finding level 1 codes (indicators) within a level 2 group (pillar)
  indicators_in_political <- get_iCodes_in_group(coin, iCode_group = "Political", at_level = 1)
  expect_type(indicators_in_political, "character")
  expect_true(all(c("Embs", "IGOs", "UNVote") %in% indicators_in_political))
  expect_equal(length(indicators_in_political), 3)

  # Test finding level 3 codes (sub-indices) within level 4 group (index)
  subindices_in_index <- get_iCodes_in_group(coin, iCode_group = "Index", at_level = 3)
  expect_type(subindices_in_index, "character")
  expect_true(all(c("Conn", "Sust") %in% subindices_in_index))
  expect_equal(length(subindices_in_index), 2)

})

test_that("get_iCodes_in_group works with unbalanced coins", {

  # Build unbalanced coin from scenario 1
  data("unbal_scen1_iData", package = "COINr")
  data("unbal_scen1_iMeta", package = "COINr")

  coin <- new_unbalanced_coin(unbal_scen1_iData, unbal_scen1_iMeta, quietly = TRUE)

  # Test finding level 1 codes within SubA (which has different structure)
  indicators_in_subA <- get_iCodes_in_group(coin, iCode_group = "SubA", at_level = 1)
  expect_type(indicators_in_subA, "character")
  expect_true("IndA1" %in% indicators_in_subA)
  expect_true("IndA2" %in% indicators_in_subA)

  # Test finding level 2 codes within Index
  level2_in_index <- get_iCodes_in_group(coin, iCode_group = "Index", at_level = 2)
  expect_type(level2_in_index, "character")
  expect_true("SubA" %in% level2_in_index)

})

test_that("get_iCodes_in_group handles edge cases", {

  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # Test with invalid iCode - should error
  expect_error(get_iCodes_in_group(coin, iCode_group = "NonExistent", at_level = 1))

  # Test with valid iCode at level 1 looking at same level
  result <- get_iCodes_in_group(coin, iCode_group = "Embs", at_level = 1)
  expect_type(result, "character")
  expect_equal(result, "Embs")

})

test_that("get_iCodes_in_group returns unique values", {

  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # Get codes and check for uniqueness
  result <- get_iCodes_in_group(coin, iCode_group = "Conn", at_level = 2)
  expect_equal(length(result), length(unique(result)))

})
