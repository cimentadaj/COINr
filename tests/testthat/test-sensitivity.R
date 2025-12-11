test_that("sensitivity_works", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # test case where we know one sensitivity index should be zero
  # we let winmax vary
  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:5,
                   Type = "discrete")

  # we keep normalisation method FIXED
  # we trick the function by using two lists that are the same
  norm_alts <- list(
    list(f_n = "n_minmax", f_n_para = list(c(1,100))),
    list(f_n = "n_minmax", f_n_para = list(c(1,100)))
  )

  # now put this in a list
  l_norm <- list(Address = "$Log$Normalise$global_specs",
                 Distribution = norm_alts,
                 Type = "discrete")

  # create overall specification list
  SA_specs <- list(
    Winmax = l_winmax,
    Normalisation = l_norm
  )

  # run sensitivity analysis
  # we can run at fairly low sample size because very few possibilities (five in fact)
  SA_res <- get_sensitivity(coin, SA_specs = SA_specs, N = 20, SA_type = "SA",
                            dset = "Aggregated", iCode = "Index", Nboot = 100)

  # test general format of output
  expect_setequal(names(SA_res), c("Scores", "Ranks", "RankStats", "Para", "Sensitivity", "Nominal"))
  expect_equal(ncol(SA_res$Scores), 20*4+2)

  # the Si and STi of the normalisation assumption should be zero
  expect_equal(SA_res$Sensitivity$Si[2], 0)
  expect_equal(SA_res$Sensitivity$STi[2], 0)

  p1 <- plot_sensitivity(SA_res, ptype = "bar")
  p2 <- plot_sensitivity(SA_res, ptype = "pie")
  p3 <- plot_sensitivity(SA_res, ptype = "box")
  # test plotting
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")

  p4 <- plot_uncertainty(SA_res)
  p5 <- plot_uncertainty(SA_res, plot_units = "top10")
  p6 <- plot_uncertainty(SA_res, plot_units = "bottom10")
  expect_s3_class(p4, "ggplot")
  expect_s3_class(p5, "ggplot")
  expect_s3_class(p6, "ggplot")

})

test_that("get_sensitivity.unbalanced_coin strips placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  unbal <- Impute(unbal, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
  unbal <- Treat(unbal, dset = "Imputed", write_to = "Treated")
  unbal <- Normalise(unbal, dset = "Treated", write_to = "Normalised")
  unbal <- Aggregate(unbal, dset = "Normalised")

  placeholders <- unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:5,
                   Type = "discrete")

  norm_alts <- list(
    list(f_n = "n_minmax", f_n_para = list(c(1, 100))),
    list(f_n = "n_minmax", f_n_para = list(c(1, 100)))
  )

  l_norm <- list(Address = "$Log$Normalise$global_specs",
                 Distribution = norm_alts,
                 Type = "discrete")

  SA_specs <- list(
    Winmax = l_winmax,
    Normalisation = l_norm
  )

  set.seed(123)
  SA_unbal <- get_sensitivity(unbal, SA_specs = SA_specs, N = 10, SA_type = "SA",
                              dset = "Aggregated", iCode = "Index", Nboot = 10,
                              quietly = TRUE)

  if(length(placeholders) > 0){
    expect_false(any(names(SA_unbal$Scores) %in% placeholders))
    expect_false(any(names(SA_unbal$Ranks) %in% placeholders))
  }

  balanced <- structure(unbal, class = setdiff(class(unbal), "unbalanced_coin"))
  set.seed(123)
  SA_bal <- get_sensitivity.coin(balanced, SA_specs = SA_specs, N = 10, SA_type = "SA",
                                 dset = "Aggregated", iCode = "Index", Nboot = 10,
                                 quietly = TRUE)

  expect_equal(SA_unbal, SA_bal)
})

test_that("sampling_works", {

  # make a sample
  N <- 100
  d <- 3
  X <- SA_sample(N, d)

  expect_equal(nrow(X), N*(d+2))
  expect_equal(ncol(X), d)
  expect_true(all(X >= 0))
  expect_true(all(X <= 1))

  # test function (note X3 not used)
  y <- X[,1] + 10*X[,2]

  # get SA estimates
  t1 <- SA_estimate(y, N, d)

  # can't really check the individual sensitivity indices due to estimation error, but some should be zero
  expect_equal(t1$SensInd$Si[3], 0)
  expect_equal(t1$SensInd$STi[3], 0)

})

test_that("noisy_weights", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # get nominal weights
  w_nom <- coin$Meta$Weights$Original

  # build data frame specifying the levels to apply the noise at
  # here we vary at levels 2 and 3
  noise_specs = data.frame(Level = c(2,3),
                           NoiseFactor = c(0.25, 0.25))

  # get 10 replications
  noisy_wts <- get_noisy_weights(w = w_nom, noise_specs = noise_specs, Nrep = 10)

  # tests
  expect_type(noisy_wts, "list")
  expect_length(noisy_wts, 10)
  expect_true(all(sapply(noisy_wts, is.data.frame)))
  # df dims are the same
  expect_true(all(sapply(noisy_wts, function(X){
    nrow(X) == nrow(w_nom)
  })))
  expect_true(all(sapply(noisy_wts, function(X){
    ncol(X) == ncol(w_nom)
  })))
  # rows with unperturbed weights are the same
  expect_true(all(sapply(noisy_wts, function(X){
    all(X$Weight[X$Level == 1] == w_nom$Weight[w_nom$Level == 1])
  })))
  # names the same
  expect_true(all(sapply(noisy_wts, function(X){
    setequal(names(X), names(w_nom))
  })))

})

test_that("get_perturbed_weight_samples", {

  # test with four equal weights
  w <- c(0.25, 0.25, 0.25, 0.25)

  Xw <- get_perturbed_weight_samples(w, pert_by = 0.1, Nrep = 10,
                                     quietly = FALSE, tolerance = 0.01)

  expect_type(Xw, "double")
  expect_equal(nrow(Xw), 10)

  # check max/min of columns within bounds
  mincols <- apply(Xw, 2, min)
  maxcols <- apply(Xw, 2, max)
  expect_true(all(mincols >= 0.25*0.9))
  expect_true(all(maxcols <= 0.25*1.1))

  # check weight sum within tolerance
  wsums <- apply(Xw, 1, sum)
  expect_true(all(wsums >= 0.99))
  expect_true(all(wsums <= 1.01))

})

test_that("get_noisy_weights.unbalanced_coin strips placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  ph_codes <- unbal$Meta$Unbalanced$PlaceholderCodes

  noise_specs <- data.frame(Level = c(2, 3), NoiseFactor = c(0.25, 0.25))

  set.seed(123)
  noisy <- get_noisy_weights(unbal, noise_specs = noise_specs, Nrep = 5)
  expect_type(noisy, "list")
  expect_length(noisy, 5)
  expect_true(all(vapply(noisy, is.data.frame, logical(1))))
  if(length(ph_codes) > 0){
    expect_true(all(vapply(noisy, function(df){
      !any(df$iCode %in% ph_codes)
    }, logical(1))))
  }

  balanced <- structure(unbal, class = setdiff(class(unbal), "unbalanced_coin"))
  set.seed(123)
  noisy_bal <- get_noisy_weights.data.frame(balanced$Meta$Weights$Original,
                                            noise_specs = noise_specs, Nrep = 5)
  if(length(ph_codes) > 0){
    noisy_bal <- lapply(noisy_bal, function(df){
      df[!(df$iCode %in% ph_codes), , drop = FALSE]
    })
  }

  expect_equal(noisy, noisy_bal)
})

test_that("get_noisy_weights2", {

  # get a set of nominal weights
  imeta <- ASEM_iMeta
  col_names <- c("iCode", "Weight", "Level", "Parent")
  w_nom <- imeta[imeta$Type %in% c("Indicator", "Aggregate"), col_names]

  # test basic perturbation
  noise_specs = data.frame(Level = c(2,3), NoiseFactor = c(0.25, 0.5))
  noisy_wts <- get_noisy_weights2(w = w_nom, noise_specs = noise_specs, Nrep = 10)

  expect_type(noisy_wts, "list")
  expect_length(noisy_wts, 10)

  # check data frame
  l_type <- sapply(noisy_wts, class)
  expect_true(all(l_type == "data.frame"))

  # check col names
  correct_cols <- sapply(noisy_wts, function(X) {
    all(col_names %in% names(X))
  })
  expect_true(all(correct_cols))

  # check weight perturbation
  # just take one data frame here...
  w <- noisy_wts[[1]]

  # expect equal weights at level 1 (no perturbation)
  l2_groups <- imeta$iCode[which(imeta$Level == 2)] |> unique()
  for(icode in l2_groups){
    icodes <- imeta$iCode[which(imeta$Parent == icode)]
    expect_equal(w$Weight[w$iCode %in% icodes], rep(1/length(icodes), length(icodes)))
  }

  # expect 25% perturbation at level 2
  l3_groups <- imeta$iCode[which(imeta$Level == 3)] |> unique()
  for(icode in l3_groups){
    icodes <- imeta$iCode[which(imeta$Parent == icode)]
    wts <- w$Weight[w$iCode %in% icodes]
    wnom <- 1/length(wts)
    expect_true(all(wts >= wnom*0.75))
    expect_true(all(wts <= wnom*1.25))
  }

  # expect 50% perturbation at level 3
  icodes <- l3_groups
  wts <- w$Weight[w$iCode %in% icodes]
  wnom <- 1/length(wts)
  expect_true(all(wts >= wnom*0.5))
  expect_true(all(wts <= wnom*1.5))

  # Test individual_specs override
  individual_specs <- list(Physical = 1.0, P2P = 0.75)
  noisy_wts2 <- get_noisy_weights2(w = w_nom, noise_specs = noise_specs,
                                   individual_specs = individual_specs, Nrep = 5)
  expect_type(noisy_wts2, "list")
  expect_length(noisy_wts2, 5)

  # Test correct_uniform_dist mode
  noisy_wts3 <- get_noisy_weights2(w = w_nom, noise_specs = noise_specs,
                                   Nrep = 5, correct_uniform_dist = TRUE, uniform_tol = 0.01)
  expect_type(noisy_wts3, "list")
  expect_length(noisy_wts3, 5)

})


test_that("get_sensitivity2_basic_functionality", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # test case where we vary winmax
  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:5,
                   Type = "discrete")

  # create specification list
  SA_specs <- list(Winmax = l_winmax)

  # run sensitivity analysis with progress bar
  SA_res_bar <- get_sensitivity2(coin, SA_specs = SA_specs, N = 10, SA_type = "UA",
                                 dset = "Aggregated", iCode = "Index", quietly = TRUE,
                                 report_progress = "bar", monitor_convergence = FALSE)

  # test general format of output (when monitor_convergence = FALSE, est_err will be NULL)
  expect_true("Scores" %in% names(SA_res_bar))
  expect_true("Ranks" %in% names(SA_res_bar))
  expect_true("RankStats" %in% names(SA_res_bar))
  expect_true("Para" %in% names(SA_res_bar))
  expect_true("Nominal" %in% names(SA_res_bar))
  expect_equal(ncol(SA_res_bar$Scores), 10+2)
  expect_type(SA_res_bar$Para, "list")
  expect_equal(length(SA_res_bar$Para), 1)

  # run again with text progress
  SA_res_text <- get_sensitivity2(coin, SA_specs = SA_specs, N = 10, SA_type = "UA",
                                  dset = "Aggregated", iCode = "Index", quietly = TRUE,
                                  report_progress = "text", monitor_convergence = FALSE)

  # results should be similar (structure-wise)
  expect_setequal(names(SA_res_text), names(SA_res_bar))

})


test_that("get_sensitivity2_convergence_monitoring", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # test case where we vary winmax
  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:5,
                   Type = "discrete")

  SA_specs <- list(Winmax = l_winmax)

  # run with convergence monitoring
  SA_res <- get_sensitivity2(coin, SA_specs = SA_specs, N = 20, SA_type = "UA",
                             dset = "Aggregated", iCode = "Index", quietly = TRUE,
                             monitor_convergence = TRUE)

  # check that est_err is present and has values at every 5th iteration
  expect_true("est_err" %in% names(SA_res))
  expect_type(SA_res$est_err, "double")

  # check that est_err has values at positions 5, 10, 15, 20
  expect_false(is.na(SA_res$est_err[5]))
  expect_false(is.na(SA_res$est_err[10]))
  expect_false(is.na(SA_res$est_err[15]))
  expect_false(is.na(SA_res$est_err[20]))

  # test plot_convergence function
  expect_silent(plot_convergence(SA_res))

})


test_that("get_sensitivity2_early_stopping", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # test case where we vary winmax with limited alternatives (quick convergence)
  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:3,
                   Type = "discrete")

  SA_specs <- list(Winmax = l_winmax)

  # run with early stopping - set very high convergence threshold so it should stop early
  SA_res <- get_sensitivity2(coin, SA_specs = SA_specs, N = 50, SA_type = "UA",
                             dset = "Aggregated", iCode = "Index", quietly = TRUE,
                             monitor_convergence = TRUE, converge_on = 0.5)

  # check that the function stopped early - est_err should be shorter than 50
  expect_true(length(SA_res$est_err) < 50)

  # check that the last non-NA value is < 0.5
  last_err <- SA_res$est_err[!is.na(SA_res$est_err)]
  expect_true(last_err[length(last_err)] < 0.5)

})


test_that("get_sensitivity2_sensitivity_analysis", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # create two parameters to vary
  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:5,
                   Type = "discrete")

  norm_alts <- list(
    list(f_n = "n_minmax", f_n_para = list(c(1,100))),
    list(f_n = "n_zscore", f_n_para = list(c(10,2)))
  )

  l_norm <- list(Address = "$Log$Normalise$global_specs",
                 Distribution = norm_alts,
                 Type = "discrete")

  SA_specs <- list(
    Winmax = l_winmax,
    Normalisation = l_norm
  )

  # run sensitivity analysis (SA_type = "SA")
  SA_res <- get_sensitivity2(coin, SA_specs = SA_specs, N = 15, SA_type = "SA",
                             dset = "Aggregated", iCode = "Index", Nboot = 50,
                             quietly = TRUE, monitor_convergence = FALSE)

  # test general format of output (when monitor_convergence = FALSE, est_err will be NULL)
  expect_true("Scores" %in% names(SA_res))
  expect_true("Ranks" %in% names(SA_res))
  expect_true("RankStats" %in% names(SA_res))
  expect_true("Para" %in% names(SA_res))
  expect_true("Sensitivity" %in% names(SA_res))
  expect_true("Nominal" %in% names(SA_res))

  # check sensitivity indices are present
  expect_true("Si" %in% names(SA_res$Sensitivity))
  expect_true("STi" %in% names(SA_res$Sensitivity))
  expect_equal(nrow(SA_res$Sensitivity), 2)

  # check bootstrap confidence intervals are present
  expect_true("Si_q5" %in% names(SA_res$Sensitivity))
  expect_true("Si_q95" %in% names(SA_res$Sensitivity))
  expect_true("STi_q5" %in% names(SA_res$Sensitivity))
  expect_true("STi_q95" %in% names(SA_res$Sensitivity))

})


test_that("get_sensitivity2_returns_same_structure_as_get_sensitivity", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # Simple specs
  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:5,
                   Type = "discrete")

  SA_specs <- list(Winmax = l_winmax)

  # Run both versions
  set.seed(123)
  SA_res1 <- get_sensitivity(coin, SA_specs = SA_specs, N = 10, SA_type = "UA",
                             dset = "Aggregated", iCode = "Index", quietly = TRUE)

  set.seed(123)
  SA_res2 <- get_sensitivity2(coin, SA_specs = SA_specs, N = 10, SA_type = "UA",
                              dset = "Aggregated", iCode = "Index", quietly = TRUE,
                              monitor_convergence = FALSE)

  # Check that core output structures match (excluding est_err which is only in get_sensitivity2)
  expect_setequal(names(SA_res1), names(SA_res2)[names(SA_res2) != "est_err"])

  # Check column counts match
  expect_equal(ncol(SA_res1$Scores), ncol(SA_res2$Scores))
  expect_equal(ncol(SA_res1$Ranks), ncol(SA_res2$Ranks))
  expect_equal(nrow(SA_res1$RankStats), nrow(SA_res2$RankStats))

})


# Tests for pipeline validation functions
test_that("get_sensitivity_pipeline_works", {

  # Build example coin
  coin <- build_example_coin(quietly = TRUE)

  # Create SA specs
  SA_specs <- list(
    Winmax = list(
      Address = "$Log$Treat$global_specs$f1_para$winmax",
      Distribution = 1:3,
      Type = "discrete"
    ),
    Normalisation = list(
      Address = "$Log$Normalise$global_specs$f_n",
      Distribution = c("n_minmax", "n_zscore"),
      Type = "discrete"
    )
  )

  # Test basic pipeline extraction
  pipeline <- get_sensitivity_pipeline(coin, SA_specs, validate = TRUE)

  # Check output structure
  expect_s3_class(pipeline, "sa_pipeline")
  expect_s3_class(pipeline, "data.frame")
  expect_true(nrow(pipeline) > 0)

  # Check required columns exist
  required_cols <- c("step_id", "order", "input_dset", "output_dset",
                     "log_args", "spec_args", "modified",
                     "has_issues", "issue_type", "issue_severity", "issue_message")
  expect_true(all(required_cols %in% names(pipeline)))

  # Check that some steps are marked as modified
  expect_true(any(pipeline$modified))

  # For valid pipeline, should have no issues
  expect_false(any(pipeline$has_issues))

})


test_that("get_sensitivity_pipeline_focus_modes", {

  coin <- build_example_coin(quietly = TRUE)

  SA_specs <- list(
    Winmax = list(
      Address = "$Log$Treat$global_specs$f1_para$winmax",
      Distribution = 1:3,
      Type = "discrete"
    )
  )

  # Test focus = "all"
  pipeline_all <- get_sensitivity_pipeline(coin, SA_specs, focus = "all")
  expect_true(nrow(pipeline_all) > 0)

  # Test focus = "modified"
  pipeline_modified <- get_sensitivity_pipeline(coin, SA_specs, focus = "modified")
  expect_true(all(pipeline_modified$modified))
  expect_true(nrow(pipeline_modified) < nrow(pipeline_all))

  # Test focus = "issues" (should have none for valid pipeline)
  expect_message(
    pipeline_issues <- get_sensitivity_pipeline(coin, SA_specs, focus = "issues"),
    "No pipeline issues detected"
  )
  expect_equal(nrow(pipeline_issues), 0)

})


test_that("pipeline_validation_detects_dataset_mismatch", {

  coin <- build_example_coin(quietly = TRUE)

  # Create problematic log where Impute reads from wrong dataset
  coin_bad <- coin
  coin_bad$Log$Impute$dset <- "Normalised"  # Should be "Denominated"

  SA_specs <- list(
    Winmax = list(
      Address = "$Log$Treat$global_specs$f1_para$winmax",
      Distribution = 1:3,
      Type = "discrete"
    )
  )

  # Get pipeline with validation
  pipeline <- get_sensitivity_pipeline(coin_bad, SA_specs, validate = TRUE)

  # Should detect issues
  expect_true(any(pipeline$has_issues))

  # Should have dataset_mismatch error
  expect_true(any(pipeline$issue_type == "dataset_mismatch", na.rm = TRUE))
  expect_true(any(pipeline$issue_severity == "error", na.rm = TRUE))

})


test_that("pipeline_validation_detects_data_overwriting", {

  coin <- build_example_coin(quietly = TRUE)

  # Create problematic log where two steps write to same dataset
  coin_bad <- coin
  coin_bad$Log$Normalise$write_to <- "Treated"  # Overwrites Treat output

  SA_specs <- list(
    Winmax = list(
      Address = "$Log$Treat$global_specs$f1_para$winmax",
      Distribution = 1:3,
      Type = "discrete"
    )
  )

  # Get pipeline with validation
  pipeline <- get_sensitivity_pipeline(coin_bad, SA_specs, validate = TRUE)

  # Should detect issues
  expect_true(any(pipeline$has_issues))

  # Should have data_overwrite warning
  expect_true(any(pipeline$issue_type == "data_overwrite", na.rm = TRUE))
  expect_true(any(pipeline$issue_severity == "warning", na.rm = TRUE))

})


test_that("get_pipeline_issues_works", {

  coin <- build_example_coin(quietly = TRUE)

  # Create problematic log
  coin_bad <- coin
  coin_bad$Log$Impute$dset <- "Normalised"  # Wrong dataset

  SA_specs <- list(
    Winmax = list(
      Address = "$Log$Treat$global_specs$f1_para$winmax",
      Distribution = 1:3,
      Type = "discrete"
    )
  )

  # Get only issues
  issues <- get_pipeline_issues(coin_bad, SA_specs)

  # Should have issues
  expect_true(nrow(issues) > 0)
  expect_true(all(issues$has_issues))

  # For valid pipeline, should return empty
  expect_message(
    issues_valid <- get_pipeline_issues(coin, SA_specs),
    "No pipeline issues detected"
  )
  expect_equal(nrow(issues_valid), 0)

})


test_that("plot_sensitivity_pipeline_refactored", {

  coin <- build_example_coin(quietly = TRUE)

  SA_specs <- list(
    Winmax = list(
      Address = "$Log$Treat$global_specs$f1_para$winmax",
      Distribution = 1:3,
      Type = "discrete"
    )
  )

  # Test basic plotting
  p <- plot_sensitivity_pipeline(coin, SA_specs, focus = "all", show_issues = TRUE)
  expect_s3_class(p, "ggplot")

  # Check pipeline_df is attached as attribute
  expect_true(!is.null(attr(p, "pipeline_df")))
  pipeline_df <- attr(p, "pipeline_df")
  expect_true(is.data.frame(pipeline_df))

  # Test focus = "modified"
  p_modified <- plot_sensitivity_pipeline(coin, SA_specs, focus = "modified")
  expect_s3_class(p_modified, "ggplot")

  # Test with problematic pipeline
  coin_bad <- coin
  coin_bad$Log$Impute$dset <- "Normalised"

  p_issues <- plot_sensitivity_pipeline(coin_bad, SA_specs, focus = "issues", show_issues = TRUE)
  expect_s3_class(p_issues, "ggplot")

})


test_that("print.sa_pipeline_works", {

  coin <- build_example_coin(quietly = TRUE)

  SA_specs <- list(
    Winmax = list(
      Address = "$Log$Treat$global_specs$f1_para$winmax",
      Distribution = 1:3,
      Type = "discrete"
    )
  )

  pipeline <- get_sensitivity_pipeline(coin, SA_specs)

  # Test that print method works without errors
  expect_output(print(pipeline), "Sensitivity Analysis Pipeline")
  expect_output(print(pipeline), "Total steps:")
  expect_output(print(pipeline), "Modified by SA_specs:")

})
