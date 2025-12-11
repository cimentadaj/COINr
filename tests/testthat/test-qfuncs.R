test_that("qNormalise", {

  # normalise ASEM data
  iData <- ASEM_iData[c("uCode", na.omit(ASEM_iMeta$iCode[ASEM_iMeta$Level == 1]))]
  directions <- na.omit(ASEM_iMeta[ASEM_iMeta$Level == 1, c("iCode", "Direction")])

  # df method
  dfnorm <- qNormalise(iData[names(iData) != "uCode"], directions = directions)
  expect_equal(nrow(dfnorm), nrow(iData))
  expect_equal(ncol(dfnorm), ncol(iData) - 1)
  # expect data on [0, 100] interval
  expect_true(all(sapply(dfnorm, max, na.rm = TRUE) == 100))
  expect_true(all(sapply(dfnorm, min, na.rm = TRUE) == 0))

  # coin method
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  coin <- qNormalise(coin, dset = "Raw")
  # extract data
  dfnorm_coin <- get_data(coin, dset = "Normalised")

  # to compare we need to order rows
  dfnorm <- cbind(uCode = iData$uCode, dfnorm)
  dfnorm <- dfnorm[match(dfnorm_coin$uCode, dfnorm$uCode) ,]

  # check
  expect_equal(dfnorm_coin, dfnorm, ignore_attr = TRUE)

  # purse method
  purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)
  purse <- qNormalise(purse, dset = "Raw", global = FALSE)

  # should see same dsets
  for (ii in 1:nrow(purse)){

    # get raw
    dfraw <- get_dset(purse, dset = "Raw", Time = purse$Time[ii], also_get = "none")
    # get normalised
    dfnor <- get_dset(purse, dset = "Normalised", Time = purse$Time[ii], also_get = "none")
    # manual normalisation
    dfnor2 <- qNormalise(dfraw, directions = directions)
    # check
    expect_equal(dfnor, dfnor2, ignore_attr = TRUE)

  }
})

test_that("qTreat",{

  # DF METHOD

  # select three indicators
  df1 <- ASEM_iData[c("Flights", "Goods", "Services")]

  # treat data frame, changing winmax and skew/kurtosis limits
  l_treat <- qTreat(df1, winmax = 1, skew_thresh = 1.5, kurt_thresh = 3)

  expect_setequal(names(l_treat), c("x_treat", "Dets_Table", "Treated_Points"))
  expect_equal(nrow(l_treat$x_treat), nrow(df1))
  expect_equal(ncol(l_treat$x_treat), ncol(df1))

  # mainly just test that corresponds to main Treat() function, which is tested itself
  l_treat2 <- Treat(df1, global_specs = list(f1_para = list(winmax = 1,
                                                            skew_thresh = 1.5,
                                                            kurt_thresh = 3),
                                             f_pass_para = list(skew_thresh = 1.5,
                                                                kurt_thresh = 3)))
  expect_equal(l_treat, l_treat2)

  # COIN METHOD
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  # quick treat
  coin1 <- qTreat(coin, dset = "Raw", winmax = 3)
  # full Treat
  coin2 <- Treat(coin, dset = "Raw", global_specs = list(f1_para = list(winmax = 3)))
  expect_equal(coin1$Data, coin2$Data)

  # PURSE METHOD
  purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)
  purse1 <- qTreat(purse, dset = "Raw", winmax = 2)
  purse2 <- Treat(purse, dset = "Raw", global_specs = list(f1_para = list(winmax = 3)))
  for(ii in 1:nrow(purse)){

    expect_equal(purse1$coin[[ii]]$Data$Treated, purse1$coin[[ii]]$Data$Treated)

  }

  # UNBALANCED COIN METHOD
  coin_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  coin_unbal_qt <- qTreat(coin_unbal, dset = "Raw", winmax = 3)
  expect_s3_class(coin_unbal_qt, c("unbalanced_coin", "coin"))

  treated_unbal <- get_dset(coin_unbal_qt, "Treated")
  expect_setequal(names(treated_unbal), c("uCode", "IndA1", "IndA2", "IndB"))
  expect_false(any(names(treated_unbal) %in% coin_unbal_qt$Meta$Unbalanced$PlaceholderCodes))

  manual_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  manual_unbal <- Treat(manual_unbal, dset = "Raw", global_specs = list(f1_para = list(winmax = 3)))
  expect_equal(treated_unbal, get_dset(manual_unbal, "Treated"))

})


test_that("qNormalise unbalanced coin", {

  coin_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  coin_unbal <- qNormalise(coin_unbal, dset = "Raw", f_n = "n_minmax", f_n_para = list(l_u = c(0, 100)))

  expect_s3_class(coin_unbal, c("unbalanced_coin", "coin"))

  norm_unbal <- get_dset(coin_unbal, "Normalised")
  expect_setequal(names(norm_unbal), c("uCode", "IndA1", "IndA2", "IndB"))
  expect_false(any(names(norm_unbal) %in% coin_unbal$Meta$Unbalanced$PlaceholderCodes))

  manual <- Normalise(unbal_iData, global_specs = list(f_n = "n_minmax", f_n_para = list(l_u = c(0, 100))))
  expect_equal(norm_unbal, manual)

  coin_unbal_df <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  norm_df <- qNormalise(coin_unbal_df, dset = "Raw", out2 = "df")
  expect_setequal(names(norm_df), c("uCode", "IndA1", "IndA2", "IndB"))
  expect_false(any(names(norm_df) %in% coin_unbal_df$Meta$Unbalanced$PlaceholderCodes))

  expect_error(qNormalise(new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE),
                          dset = "Raw", out2 = "coin"),
               "Set out2 = 'unbalanced_coin'")

})
