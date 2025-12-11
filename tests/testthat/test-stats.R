test_that("stats", {

  # stats of ASEM data
  # data with only indicator cols (all numeric)
  iData_ <- ASEM_iData[na.omit(ASEM_iMeta$iCode[ASEM_iMeta$Level == 1])]
  df1 <- get_stats(iData_, nsignif = 4)

  # not going to test all stats, just a selection
  expect_equal(nrow(df1), ncol(iData_))
  expect_setequal(df1$iCode, names(iData_))
  # check min (note, have to round)
  expect_equal(df1$Min[df1$iCode == "LPI"], signif(min(iData_$LPI, na.rm = TRUE), 4))
  # std dev
  expect_equal(df1$Std[df1$iCode == "LPI"], signif(sd(iData_$LPI, na.rm = TRUE), 4))
  # num avail
  expect_equal(df1$N.Avail[df1$iCode == "LPI"], signif(sum(!is.na(iData_$LPI)), 4))
  # n unique
  expect_equal(df1$N.Unique[df1$iCode == "LPI"], signif(length(unique(iData_$LPI)), 4))

  # last, do skew/kurt flags as these are important
  expect_equal(df1$Skew[df1$iCode == "Flights"], signif(skew(iData_$Flights, na.rm = TRUE), 4))
  expect_equal(df1$Kurt[df1$iCode == "Flights"], signif(kurt(iData_$Flights, na.rm = TRUE), 4))

  # flights should be OUT
  expect_true( (abs(df1$Skew[df1$iCode == "Flights"]) > 2) & (df1$Kurt[df1$iCode == "Flights"] > 3.5) )
  expect_equal(df1$Flag.SkewKurt[df1$iCode == "Flights"], "OUT")


  ## COIN METHOD ##
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  df2 <- get_stats(coin, dset = "Raw", nsignif = 4, out2 = "df")
  expect_equal(df2, df1)
  coin <- get_stats(coin, dset = "Raw", nsignif = 4, out2 = "coin")
  expect_equal(coin$Analysis$Raw$Stats, df1)
})

test_that("get_stats.unbalanced_coin strips placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  unbal <- Aggregate(unbal, dset = "Raw")
  placeholders <- unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  stats_unbal <- get_stats(unbal, dset = "Aggregated", out2 = "df")
  if(length(placeholders) > 0){
    expect_false(any(stats_unbal$iCode %in% placeholders))
  }

  stats_coin <- get_stats(unbal, dset = "Aggregated", out2 = "coin")
  expect_true(inherits(stats_coin, "unbalanced_coin"))
  if(length(placeholders) > 0){
    expect_false(any(stats_coin$Analysis$Aggregated$Stats$iCode %in% placeholders))
  }

  balanced <- structure(unbal, class = setdiff(class(unbal), "unbalanced_coin"))
  if(length(placeholders) > 0 && !is.null(balanced$Meta$Ind)){
    balanced$Meta$Ind <- balanced$Meta$Ind[balanced$Meta$Ind$iCode %nin% placeholders, , drop = FALSE]
  }
  if(length(placeholders) > 0){
    balanced$Meta$Lineage <- COINr:::`.sanitize_lineage`(balanced$Meta$Lineage, placeholders)
  }

  stats_bal <- get_stats.coin(balanced, dset = "Aggregated", out2 = "df")
  expect_equal(stats_unbal, stats_bal)
})


test_that("get_statistics basic functionality", {

  # Build example coin with aggregation
  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

  # Run get_statistics for levels 2 and 3
  stats <- get_statistics(coin, dset = "Aggregated", level = c(2, 3))

  # Check output structure
  expect_type(stats, "list")
  expect_setequal(names(stats), c("Statistics", "Correlations"))

  # Check Statistics data frame
  expect_s3_class(stats$Statistics, "data.frame")
  expect_true(nrow(stats$Statistics) > 0)

  # Check expected columns in Statistics
  expected_cols <- c("dimension", "level", "num_items", "items", "low_corr_pairs",
                    "PCA_variance", "PCA_NAs", "eigen_dim", "cronbach",
                    "rotated_loadings_thrs", "rotated_loadings_direction", "rotated_items")
  expect_true(all(expected_cols %in% names(stats$Statistics)))

  # Check Correlations is a list
  expect_type(stats$Correlations, "list")
  expect_true(length(stats$Correlations) > 0)

  # Each correlation entry should have All, Low, High
  for(dim in names(stats$Correlations)) {
    expect_setequal(names(stats$Correlations[[dim]]), c("All", "Low", "High"))
    expect_s3_class(stats$Correlations[[dim]]$All, "data.frame")
    expect_s3_class(stats$Correlations[[dim]]$Low, "data.frame")
    expect_s3_class(stats$Correlations[[dim]]$High, "data.frame")
  }

})


test_that("get_statistics PCA and Cronbach calculations", {

  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)
  stats <- get_statistics(coin, dset = "Aggregated", level = 2, warnings = FALSE)

  # Check PCA variance is between 0 and 1
  expect_true(all(stats$Statistics$PCA_variance >= 0 & stats$Statistics$PCA_variance <= 1))

  # Check Cronbach's alpha is numeric
  expect_type(stats$Statistics$cronbach, "double")

  # Check eigenvalues count is positive integer
  expect_true(all(stats$Statistics$eigen_dim >= 0))
  expect_true(all(stats$Statistics$eigen_dim == floor(stats$Statistics$eigen_dim)))

  # Check rotated loadings flags are logical
  expect_type(stats$Statistics$rotated_loadings_thrs, "logical")
  expect_type(stats$Statistics$rotated_loadings_direction, "logical")

})


test_that("get_statistics correlation thresholds", {

  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

  # Test with default threshold (0.3)
  stats1 <- get_statistics(coin, dset = "Aggregated", level = 2, warnings = FALSE)

  # Test with different threshold
  stats2 <- get_statistics(coin, dset = "Aggregated", level = 2, exclude_corr = 0.5, warnings = FALSE)

  # Low correlation pairs should differ based on threshold
  # (unless all correlations are above both thresholds)
  expect_type(stats1$Statistics$low_corr_pairs, "integer")
  expect_type(stats2$Statistics$low_corr_pairs, "integer")

  # Check that High correlations are > 0.92
  for(dim in names(stats1$Correlations)) {
    if(nrow(stats1$Correlations[[dim]]$High) > 0) {
      expect_true(all(abs(stats1$Correlations[[dim]]$High$corr) > 0.92))
    }
  }

})


test_that("get_statistics handles different levels", {

  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

  # Test single level
  stats_single <- get_statistics(coin, dset = "Aggregated", level = 2, warnings = FALSE)
  expect_true(all(stats_single$Statistics$level == 2))

  # Test multiple levels
  stats_multi <- get_statistics(coin, dset = "Aggregated", level = c(2, 3), warnings = FALSE)
  expect_true(all(stats_multi$Statistics$level %in% c(2, 3)))
  expect_true(nrow(stats_multi$Statistics) >= nrow(stats_single$Statistics))

})


test_that("get_statistics error handling", {

  coin <- build_example_coin(up_to = "Aggregate", quietly = TRUE)

  # Test invalid level (level 1 should give error)
  expect_error(
    get_statistics(coin, dset = "Aggregated", level = 1),
    "not a list of positive integers greater than 1"
  )

  # Test invalid correlation threshold
  expect_message(
    get_statistics(coin, dset = "Aggregated", level = 2, exclude_corr = 2),
    "0.3 will be used"
  )

  # Test non-coin object
  expect_error(
    get_statistics(list(), dset = "Aggregated", level = 2),
    "not a coin object"
  )

})


test_that("cronbach_alpha helper function", {

  # Test with simple data
  test_data <- data.frame(
    x1 = c(1, 2, 3, 4, 5),
    x2 = c(2, 3, 4, 5, 6),
    x3 = c(1.5, 2.5, 3.5, 4.5, 5.5)
  )

  alpha <- cronbach_alpha(test_data)

  # Should be numeric and between -1 and 1 (typically positive)
  expect_type(alpha, "double")
  expect_length(alpha, 1)

  # Test with mtcars subset
  alpha_mtcars <- cronbach_alpha(mtcars[, 1:4])
  expect_type(alpha_mtcars, "double")

  # Test error with non-numeric data
  expect_error(
    cronbach_alpha(data.frame(a = c("a", "b", "c"))),
    "is.numeric"
  )

})


test_that("get_statistics handles unbalanced coins with placeholders", {

  data("unbal_scen1_iData", package = "COINr")
  data("unbal_scen1_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_scen1_iData, unbal_scen1_iMeta, quietly = TRUE)
  unbal <- Impute(unbal, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
  unbal <- Normalise(unbal, dset = "Imputed", write_to = "Normalised")
  unbal <- Aggregate(unbal, dset = "Normalised")

  # This should work now - placeholders are excluded automatically
  stats_unbal <- get_statistics(unbal, dset = "Normalised", level = 2, warnings = FALSE)

  # Check output structure
  expect_type(stats_unbal, "list")
  expect_setequal(names(stats_unbal), c("Statistics", "Correlations"))
  expect_s3_class(stats_unbal$Statistics, "data.frame")

  # Should have analyzed at least SubA (which has real indicators IndA1, IndA2)
  expect_true(nrow(stats_unbal$Statistics) > 0)
  expect_true("SubA" %in% stats_unbal$Statistics$dimension)

  # Check that SubA has 2 indicators (IndA1, IndA2)
  suba_row <- stats_unbal$Statistics[stats_unbal$Statistics$dimension == "SubA", ]
  expect_equal(suba_row$num_items, 2)

  # Check correlations are available
  expect_true("SubA" %in% names(stats_unbal$Correlations))
  expect_setequal(names(stats_unbal$Correlations$SubA), c("All", "Low", "High"))

})
