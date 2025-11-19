test_that("a_dea basic aggregation", {

  # Create test data frame, 10 units, 3 indicators
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Test basic DEA aggregation (no weight restrictions)
  result <- a_dea(X, wr_type = 0)

  # Check return type
  expect_true(is.numeric(result))
  expect_equal(length(result), 10)

  # All values should be between 0 and 1
  expect_true(all(result >= 0 & result <= 1, na.rm = TRUE))

  # At least one efficient unit (score = 1)
  expect_true(any(result == 1, na.rm = TRUE))

})


test_that("a_dea with details", {

  # Create test data frame
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Test with_details = TRUE
  result <- a_dea(X, wr_type = 0, with_details = TRUE)

  # Check return type is a list
  expect_true(is.list(result))
  expect_true("Agg" %in% names(result))
  expect_true("dea_weights" %in% names(result))
  expect_true("dea_intensities" %in% names(result))

  # Check dimensions
  expect_equal(length(result$Agg), 10)
  expect_equal(nrow(result$dea_weights), 10)
  expect_equal(ncol(result$dea_weights), 3)
  expect_equal(nrow(result$dea_intensities), 10)
  expect_equal(ncol(result$dea_intensities), 10)

})


test_that("a_dea with weight restriction type 1", {

  # Create test data frame
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Type 1: ordering restriction (w2 >= w1)
  # Rewritten as: -w1 + w2 + 0*w3 <= 0 is NOT correct
  # It should be: w1 - w2 <= 0
  wr_bounds <- matrix(c(1, -1, 0), byrow = TRUE, ncol = 3)

  result <- a_dea(X, wr_type = 1, wr_bounds = wr_bounds, wr_rhs = c(0), with_details = TRUE)

  # Check that weights follow the restriction
  weights <- result$dea_weights
  # For each unit, w2 should be >= w1 (within numerical tolerance)
  for(i in 1:nrow(weights)){
    if(!is.na(result$Agg[i])){
      expect_true(weights[i, 2] >= weights[i, 1] - 1e-6)
    }
  }

})


test_that("a_dea with weight restriction type 2", {

  # Create test data frame without zeros
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Type 2: contribution restrictions (10% to 75%)
  wr_bounds <- c(0.1, 0.75)

  result <- a_dea(X, wr_type = 2, wr_bounds = wr_bounds, with_details = TRUE)

  # Check that all scores are between 0 and 1
  expect_true(all(result$Agg >= 0 & result$Agg <= 1, na.rm = TRUE))

})


test_that("a_dea with weight restriction type 3", {

  # Create test data frame
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Type 3: VEA-BoD model (use unit 5 as benchmark)
  wr_bounds <- 5

  result <- a_dea(X, wr_type = 3, wr_bounds = wr_bounds, with_details = TRUE)

  # Check that results are valid
  expect_true(is.list(result))
  expect_true(all(result$Agg >= 0 & result$Agg <= 1, na.rm = TRUE))

  # In VEA-BoD, all units are evaluated using the MPS constraint
  # This doesn't mean weights are identical, but scores should be valid
  expect_true(all(!is.na(result$Agg)))

})


test_that("a_dea with weight restriction type 4", {

  # Create test data frame
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Type 4: weight share restrictions (10% to 75%)
  wr_bounds <- c(0.1, 0.75)

  result <- a_dea(X, wr_type = 4, wr_bounds = wr_bounds, with_details = TRUE)

  # Check that all scores are between 0 and 1
  expect_true(all(result$Agg >= 0 & result$Agg <= 1, na.rm = TRUE))

})


test_that("a_dea with cross efficiency", {

  # Create test data frame
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Test cross efficiency
  result <- a_dea(X, cross = TRUE)

  # Check return type
  expect_true(is.numeric(result))
  expect_equal(length(result), 10)

  # Cross efficiency scores should generally be lower than or equal to self-evaluation
  result_self <- a_dea(X, cross = FALSE)

  # Most units should have lower or equal cross efficiency
  expect_true(mean(result <= result_self + 1e-6, na.rm = TRUE) >= 0.5)

})


test_that("a_dea handles NA values", {

  # Create test data frame with NAs
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))
  colnames(X) <- c("Ind1", "Ind2", "Ind3")

  # Add some NAs
  X[1, 1] <- NA
  X[5, 2] <- NA

  # Test DEA with NAs
  result <- a_dea(X, wr_type = 0)

  # Units with NAs should have NA scores
  expect_true(is.na(result[1]))
  expect_true(is.na(result[5]))

  # Other units should have valid scores
  expect_false(is.na(result[2]))
  expect_false(is.na(result[3]))

})


test_that("a_dea in Aggregate() function", {

  # Build example coin up to normalised dataset
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Use DEA in the last aggregation level
  coin <- Aggregate(coin, dset = "Normalised",
                    f_ag = c("a_amean", "a_amean", "a_dea"),
                    by_df = c(FALSE, FALSE, TRUE),
                    w = list(NULL, NULL, "none"))

  # Check that aggregated dataset exists
  expect_true("Aggregated" %in% names(coin$Data))

  # Check that Index column exists
  Xagg <- get_dset(coin, "Aggregated")
  expect_true("Index" %in% names(Xagg))

  # Check that Index values are between 0 and 1
  expect_true(all(Xagg$Index >= 0 & Xagg$Index <= 1, na.rm = TRUE))

})


test_that("a_dea in multiple aggregation levels", {

  # Build example coin up to normalised dataset
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Use DEA in both second and third aggregation levels
  coin <- Aggregate(coin, dset = "Normalised",
                    f_ag = c("a_amean", "a_dea", "a_dea"),
                    by_df = c(FALSE, TRUE, TRUE),
                    w = list(NULL, "none", "none"))

  # Check that aggregated dataset exists
  expect_true("Aggregated" %in% names(coin$Data))

  # Check that Index column exists and has valid values
  Xagg <- get_dset(coin, "Aggregated")
  expect_true("Index" %in% names(Xagg))
  expect_true(all(Xagg$Index >= 0 & Xagg$Index <= 1, na.rm = TRUE))

})


test_that("a_dea with parameters in Aggregate()", {

  # Build example coin up to normalised dataset
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Use DEA with type 2 restrictions
  coin <- Aggregate(coin, dset = "Normalised",
                    f_ag = c("a_amean", "a_amean", "a_dea"),
                    f_ag_para = list(NULL, NULL, list(wr_type = 2, wr_bounds = c(0.1, 0.75))),
                    by_df = c(FALSE, FALSE, TRUE),
                    w = list(NULL, NULL, "none"))

  # Check that aggregated dataset exists
  Xagg <- get_dset(coin, "Aggregated")
  expect_true("Index" %in% names(Xagg))
  expect_true(all(Xagg$Index >= 0 & Xagg$Index <= 1, na.rm = TRUE))

})


test_that("get_DEA on coin", {

  # Build example coin up to aggregated dataset
  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

  # Re-aggregate with DEA
  dea_result <- get_DEA(coin)

  # Check return type
  expect_true(is.list(dea_result))
  expect_true("DEA_CI" %in% names(dea_result))
  expect_true("DEA_CI_ranks" %in% names(dea_result))
  expect_true("dea_weights" %in% names(dea_result))
  expect_true("norm.weights" %in% names(dea_result))

  # Check DEA_CI
  expect_true(is.data.frame(dea_result$DEA_CI))
  expect_true("uCode" %in% names(dea_result$DEA_CI))
  expect_true("Dea" %in% names(dea_result$DEA_CI))

  # Check DEA scores are between 0 and 1
  expect_true(all(dea_result$DEA_CI$Dea >= 0 & dea_result$DEA_CI$Dea <= 1, na.rm = TRUE))

})


test_that("get_DEA with weight restrictions", {

  # Build example coin up to aggregated dataset
  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

  # Re-aggregate with DEA and type 2 restrictions
  dea_result <- get_DEA(coin, wr_type = 2, wr_bounds = c(0.05, 0.75))

  # Check that results exist
  expect_true("Dea" %in% names(dea_result$DEA_CI))
  expect_true(all(dea_result$DEA_CI$Dea >= 0 & dea_result$DEA_CI$Dea <= 1, na.rm = TRUE))

})


test_that("get_DEA at different levels", {

  # Build example coin up to aggregated dataset
  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

  # Re-aggregate at level 2
  dea_result <- get_DEA(coin, level = 2)

  # Check that results exist
  expect_true("Dea" %in% names(dea_result$DEA_CI))
  expect_true(all(dea_result$DEA_CI$Dea >= 0 & dea_result$DEA_CI$Dea <= 1, na.rm = TRUE))

})


test_that("a_dea error handling", {

  # Test with invalid wr_type
  set.seed(123)
  X <- as.data.frame(matrix(runif(30, min = 0.1, max = 1), 10, 3))

  expect_error(a_dea(X, wr_type = 5), "wr_type must be a numeric value between 0 and 4")

  # Test with invalid wr_bounds for type 1
  expect_error(a_dea(X, wr_type = 1, wr_bounds = c(1, 2)),
               "wr_bounds must be a numeric matrix")

  # Test with invalid wr_bounds for type 2
  expect_error(a_dea(X, wr_type = 2, wr_bounds = c(0.5)),
               "wr_bounds must be a column of length=2")

  # Test with invalid bounds that violate constraints
  expect_error(a_dea(X, wr_type = 2, wr_bounds = c(0.5, 0.6)),
               "the specified value for the upper bound")

})


test_that("a_dea with unbalanced coin", {

  # Load unbalanced coin data
  data("unbal_scen1_iData", package = "COINr")
  data("unbal_scen1_iMeta", package = "COINr")

  # Create unbalanced coin
  coin <- new_unbalanced_coin(unbal_scen1_iData, unbal_scen1_iMeta, quietly = TRUE)

  # Aggregate with DEA  # For unbalanced coins, by_df length should match number of aggregation levels
  coin <- Normalise(coin, dset = "Raw", write_to = "Normalised")
  coin <- Aggregate(coin, dset = "Normalised",
                    f_ag = c("a_amean", "a_dea"),
                    by_df = c(FALSE, TRUE),
                    w = list(NULL, "none"))

  # Check that aggregated dataset exists
  expect_true("Aggregated" %in% names(coin$Data))

  # Check that Index exists and has valid values
  Xagg <- get_dset(coin, "Aggregated")
  expect_true("Index" %in% names(Xagg))
  expect_true(all(Xagg$Index >= 0 & Xagg$Index <= 1, na.rm = TRUE))

})


test_that("get_DEA with unbalanced coin", {

  # Load unbalanced coin data
  data("unbal_scen1_iData", package = "COINr")
  data("unbal_scen1_iMeta", package = "COINr")

  # Create unbalanced coin
  coin <- new_unbalanced_coin(unbal_scen1_iData, unbal_scen1_iMeta, quietly = TRUE)
  coin <- Normalise(coin, dset = "Raw", write_to = "Normalised")
  coin <- Aggregate(coin, dset = "Normalised")

  # Re-aggregate with DEA
  dea_result <- get_DEA(coin)

  # Check that results exist
  expect_true(is.list(dea_result))
  expect_true("DEA_CI" %in% names(dea_result))
  expect_true("Dea" %in% names(dea_result$DEA_CI))

  # Check DEA scores are valid
  expect_true(all(dea_result$DEA_CI$Dea >= 0 & dea_result$DEA_CI$Dea <= 1, na.rm = TRUE))

  # Check that placeholders are properly filtered out
  placeholders <- coin$Meta$Unbalanced$PlaceholderCodes
  if(length(placeholders) > 0 && !isTRUE(getOption("COINr.keep_placeholders"))){
    # Placeholders should not be in the results
    expect_false(any(names(dea_result$DEA_CI) %in% placeholders))
    expect_false(any(names(dea_result$dea_weights) %in% placeholders))
  }

})
