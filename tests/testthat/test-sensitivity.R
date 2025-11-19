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
