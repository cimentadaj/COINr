test_that("basic_functionality", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Test with single dimension
  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    verbose = FALSE,
    warnings = FALSE
  )

  # Check return structure
  expect_type(results, "list")
  expect_named(results, c("Info", "Combinations", "Successful", "Correlations"))

  # Check Info data frame
  expect_s3_class(results$Info, "data.frame")
  expect_equal(nrow(results$Info), 1)
  expect_true(results$Info$combinations > 0)

  # Check Combinations list
  expect_type(results$Combinations, "list")
  expect_true("Physical" %in% names(results$Combinations))
  expect_s3_class(results$Combinations$Physical, "data.frame")

  # Check Successful list
  expect_type(results$Successful, "list")

  # Check Correlations list
  expect_type(results$Correlations, "list")
  expect_named(results$Correlations$Physical, c("All", "Low", "High"))

})

test_that("multiple_dimensions", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Test with multiple dimensions
  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = c("Physical", "Political"),
    verbose = FALSE,
    warnings = FALSE
  )

  # Check that both dimensions are in results
  expect_equal(nrow(results$Info), 2)
  expect_true(all(c("Physical", "Political") %in% results$Info$dimension))
  expect_true(all(c("Physical", "Political") %in% names(results$Combinations)))

})

test_that("parameter_variations", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Test with different correlation threshold
  results1 <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    exclude_corr = 0,
    verbose = FALSE,
    warnings = FALSE
  )

  results2 <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    exclude_corr = 0.5,
    verbose = FALSE,
    warnings = FALSE
  )

  # Higher correlation threshold should result in fewer or equal combinations
  expect_true(results2$Info$combinations <= results1$Info$combinations)

  # Test with different quality thresholds
  results3 <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    PCA_ref = 0.5,
    cronbach_alpha_ref = 0.6,
    verbose = FALSE,
    warnings = FALSE
  )

  results4 <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    PCA_ref = 0.8,
    cronbach_alpha_ref = 0.9,
    verbose = FALSE,
    warnings = FALSE
  )

  # Lower thresholds should result in more or equal successful combinations
  if (!is.null(results3$Successful$Physical) && !is.null(results4$Successful$Physical)) {
    expect_true(nrow(results3$Successful$Physical) >= nrow(results4$Successful$Physical))
  }

})

test_that("global_min_max", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Test with global min/max constraints
  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    global_min_max = c(2, 3),
    verbose = FALSE,
    warnings = FALSE
  )

  # Check that all combinations respect the size constraints
  if (!is.null(results$Combinations$Physical)) {
    expect_true(all(results$Combinations$Physical$num_Indcs >= 2))
    expect_true(all(results$Combinations$Physical$num_Indcs <= 3))
  }

  # Verify Info reflects the constraints
  expect_equal(results$Info$min, 2)
  expect_equal(results$Info$max, 3)

})

test_that("indiv_min_max", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Test with individual dimension min/max
  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = c("Physical", "Political"),
    global_min_max = c(2, 10),
    indiv_min_max = list('Physical' = c(2, 3)),
    verbose = FALSE,
    warnings = FALSE
  )

  # Physical should respect individual constraints
  expect_equal(results$Info[results$Info$dimension == "Physical", "min"], 2)
  expect_equal(results$Info[results$Info$dimension == "Physical", "max"], 3)

  # Political should use global constraints
  expect_equal(results$Info[results$Info$dimension == "Political", "min"], 2)
  expect_true(results$Info[results$Info$dimension == "Political", "max"] >= 3)

})

test_that("add_drop_elements", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Get original indicators for Physical
  iMeta <- coin$Meta$Ind
  original_elements <- iMeta$iCode[!is.na(iMeta$Parent) & iMeta$Parent == "Physical" & iMeta$Type == "Indicator"]
  original_count <- length(original_elements)

  # Test adding elements from another dimension
  political_ind <- iMeta$iCode[!is.na(iMeta$Parent) & iMeta$Parent == "Political" & iMeta$Type == "Indicator"][1]

  results_add <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    add_elements = list('Physical' = political_ind),
    verbose = FALSE,
    warnings = FALSE
  )

  # Current length should be original + 1
  expect_equal(results_add$Info$current_length, original_count + 1)
  expect_equal(results_add$Info$added_indic, 1)

  # Test dropping elements
  drop_ind <- original_elements[1]

  results_drop <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    drop_elements = list('Physical' = drop_ind),
    verbose = FALSE,
    warnings = FALSE
  )

  # Current length should be original - 1
  expect_equal(results_drop$Info$current_length, original_count - 1)
  expect_equal(results_drop$Info$dropped_indic, 1)

})

test_that("statistical_validation", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    global_min_max = c(3, 4),  # Limit combinations for faster testing
    verbose = FALSE,
    warnings = FALSE
  )

  # Check that all combinations have valid statistical measures
  combs <- results$Combinations$Physical

  if (!is.null(combs) && nrow(combs) > 0) {
    # PCA variance should be between 0 and 1
    expect_true(all(combs$PCA >= 0 & combs$PCA <= 1))

    # Cronbach's alpha should be between 0 and 1
    expect_true(all(combs$cronbach >= 0 & combs$cronbach <= 1, na.rm = TRUE))

    # Eigenvalue count should be positive integer
    expect_true(all(combs$eigen_dim > 0))

    # Mean correlation should be within [-1, 1]
    expect_true(all(combs$mean >= -1 & combs$mean <= 1))
  }

  # Check that successful combinations meet the criteria
  successful <- results$Successful$Physical

  if (!is.null(successful) && nrow(successful) > 0) {
    # All successful should have PCA >= 0.65
    expect_true(all(successful$PCA >= 0.65))

    # All successful should have eigenvalues < 2
    expect_true(all(successful$eigen_dim < 2))

    # All successful should have cronbach >= 0.7
    expect_true(all(successful$cronbach >= 0.7, na.rm = TRUE))
  }

})

test_that("correlation_matrices", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    exclude_corr = 0.3,
    verbose = FALSE,
    warnings = FALSE
  )

  corr_mats <- results$Correlations$Physical

  # Check structure
  expect_named(corr_mats, c("All", "Low", "High"))

  # Check that All is a valid correlation matrix
  expect_s3_class(corr_mats$All, "data.frame")
  expect_equal(nrow(corr_mats$All), ncol(corr_mats$All))

  # Check diagonal elements are 1
  diag_vals <- diag(as.matrix(corr_mats$All))
  expect_true(all(abs(diag_vals - 1) < 1e-10))

  # Check Low correlations are indeed below threshold
  if (nrow(corr_mats$Low) > 0) {
    expect_true(all(corr_mats$Low$corr < 0.3))
  }

  # Check High correlations are indeed above 0.92
  if (nrow(corr_mats$High) > 0) {
    expect_true(all(corr_mats$High$corr > 0.92))
  }

})

test_that("s3_method_coin_class", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Verify it's a coin object
  expect_true(is.coin(coin))

  # Call method directly
  results <- get_combinations.coin(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    verbose = FALSE,
    warnings = FALSE
  )

  # Should return valid results
  expect_type(results, "list")
  expect_named(results, c("Info", "Combinations", "Successful", "Correlations"))

})

test_that("s3_method_unbalanced_coin_class", {

  # Load unbalanced coin data
  data("unbal_scen1_iData", package = "COINr")
  data("unbal_scen1_iMeta", package = "COINr")

  # Build unbalanced coin
  coin <- new_unbalanced_coin(unbal_scen1_iData, unbal_scen1_iMeta, quietly = TRUE)
  coin <- Normalise(coin, dset = "Raw", write_to = "Normalised", quietly = TRUE)

  # Verify it's an unbalanced_coin object
  expect_true(inherits(coin, "unbalanced_coin"))

  # Get a dimension that has at least 2 indicators (required for combinations)
  available_dims <- unique(coin$Meta$Ind$Parent[coin$Meta$Ind$Level == 1 & !is.na(coin$Meta$Ind$Parent)])

  # Find dimension with >= 2 indicators
  valid_dim <- NULL
  for (dim in available_dims) {
    n_indicators <- sum(coin$Meta$Ind$Parent == dim & coin$Meta$Ind$Type == "Indicator", na.rm = TRUE)
    if (n_indicators >= 2) {
      valid_dim <- dim
      break
    }
  }

  if (!is.null(valid_dim)) {
    results <- get_combinations(
      coin = coin,
      dset = "Normalised",
      dimension = valid_dim,
      verbose = FALSE,
      warnings = FALSE
    )

    # Should return valid results
    expect_type(results, "list")
    expect_named(results, c("Info", "Combinations", "Successful", "Correlations"))
  } else {
    skip("No dimension with >= 2 indicators found in test data")
  }

})

test_that("error_handling", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Test with invalid coin object (S3 dispatch error expected)
  expect_error(
    get_combinations(
      coin = list(not_a_coin = TRUE),
      dset = "Normalised",
      dimension = "Physical"
    )
  )

  # Test with NULL dimension
  expect_error(
    get_combinations(
      coin = coin,
      dset = "Normalised",
      dimension = NULL
    ),
    'argument "dimension" is not a character or factor'
  )

  # Test with non-existent dimension
  expect_error(
    get_combinations(
      coin = coin,
      dset = "Normalised",
      dimension = "NonExistentDimension",
      warnings = FALSE
    ),
    "No level 1 indicators have been found"
  )

  # Test with insufficient indicators (needs at least 2)
  # This is harder to test without modifying the coin structure
  # Skip for now

})

test_that("aggregation_function_parameter", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Test with different aggregation functions
  results1 <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    f_ag = "a_amean",
    global_min_max = c(3, 3),
    verbose = FALSE,
    warnings = FALSE
  )

  # Geometric mean will error with zeros in normalized data - this is expected
  expect_error(
    get_combinations(
      coin = coin,
      dset = "Normalised",
      dimension = "Physical",
      f_ag = "a_gmean",
      global_min_max = c(3, 3),
      verbose = FALSE,
      warnings = FALSE
    ),
    "Negative or zero values"
  )

  # Arithmetic mean should return results
  expect_type(results1, "list")
  expect_true(results1$Info$combinations > 0)

})

test_that("output_structure_details", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    global_min_max = c(3, 3),
    verbose = FALSE,
    warnings = FALSE
  )

  # Check Info columns
  expect_true(all(c("dimension", "combinations", "successful", "min", "max",
                   "original_length", "current_length", "added_indic", "dropped_indic") %in%
                   colnames(results$Info)))

  # Check Combinations columns
  if (!is.null(results$Combinations$Physical)) {
    expected_cols <- c("ID", "dimension", "num_Indcs", "original", "indicators",
                      "corr_values", "mean", "sd", "min", "max", "negative_corr",
                      "PCA", "PCA_NAs", "eigen_dim", "cronbach",
                      "rotated_loadings_threshold", "rotated_loadings_direction",
                      "rotated_indic", "suggested_indc", "corr_suggested_indc")

    expect_true(all(expected_cols %in% colnames(results$Combinations$Physical)))
  }

})

test_that("original_combination_flagged", {

  # Build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # Get original indicators for Physical (filtering out NA like the function does)
  iMeta <- coin$Meta$Ind
  original_elements <- iMeta[iMeta$Parent == "Physical" & iMeta$Level == 1, ]$iCode
  original_elements <- original_elements[!is.na(original_elements)]

  results <- get_combinations(
    coin = coin,
    dset = "Normalised",
    dimension = "Physical",
    exclude_corr = -1,  # Include all combinations to ensure original is not filtered out
    verbose = FALSE,
    warnings = FALSE
  )

  # At least one combination should be flagged as original
  if (!is.null(results$Combinations$Physical)) {
    expect_true(any(results$Combinations$Physical$original == 1))

    # The original combination should match the original indicators (non-NA)
    original_combo <- results$Combinations$Physical[results$Combinations$Physical$original == 1, ]
    if (nrow(original_combo) > 0) {
      expect_equal(original_combo$num_Indcs[1], length(original_elements))
    }
  }

})
