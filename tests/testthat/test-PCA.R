test_that("get_pca", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # PCA on "Sust" group of indicators
  l_pca <- get_PCA(coin, dset = "Raw", iCodes = "Sust",
                  out2 = "list", nowarnings = TRUE, by_groups = TRUE)
  # orig weights
  w <- coin$Meta$Weights$Original
  # check weights
  expect_equal(nrow(l_pca$Weights), nrow(w))
  expect_equal(ncol(l_pca$Weights), ncol(w))

  # check PCA output
  expect_setequal(names(l_pca$PCAresults),
                  unique(coin$Meta$Lineage[coin$Meta$Lineage$`Sub-index` == "Sust",2]))

  expect_s3_class(l_pca$PCAresults$Environ$PCAres, "prcomp")
  expect_s3_class(l_pca$PCAresults$Social$PCAres, "prcomp")
  expect_s3_class(l_pca$PCAresults$SusEcFin$PCAres, "prcomp")

  # quick check of coin output
  coin <- get_PCA(coin, dset = "Raw", iCodes = "Sust",
                  out2 = "coin", nowarnings = TRUE, by_groups = TRUE, weights_to = "test1")
  expect_s3_class(coin, "coin")
  expect_equal(l_pca$Weights, coin$Meta$Weights$test1)

})

test_that("get_PCA.unbalanced_coin strips placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  unbal <- Aggregate(unbal, dset = "Raw", out2 = "unbalanced_coin")
  ph_codes <- unbal$Meta$Unbalanced$PlaceholderCodes

  l_unbal <- get_PCA(unbal, dset = "Aggregated", Level = 2, by_groups = TRUE,
                     out2 = "list", nowarnings = TRUE)
  expect_true(all(!l_unbal$Weights$iCode %in% ph_codes))
  unbal_wts <- setNames(l_unbal$Weights$Weight, l_unbal$Weights$iCode)

  balanced <- structure(unbal, class = setdiff(class(unbal), "unbalanced_coin"))
  if(length(unbal$Meta$Unbalanced$PlaceholderMap) > 0 && "Aggregated" %in% names(balanced$Data)){
    ph_map <- unbal$Meta$Unbalanced$PlaceholderMap
    for(child in names(ph_map)){
      ph_code <- ph_map[[child]]
      if(!ph_code %in% names(balanced$Data$Aggregated) && child %in% names(balanced$Data$Aggregated)){
        balanced$Data$Aggregated[[ph_code]] <- balanced$Data$Aggregated[[child]]
      }
    }
  }
  l_bal <- get_PCA.coin(balanced, dset = "Aggregated", Level = 2, by_groups = TRUE,
                   out2 = "list", nowarnings = TRUE)
  bal_wts <- setNames(l_bal$Weights$Weight, l_bal$Weights$iCode)
  if(length(ph_codes) > 0){
    bal_wts <- bal_wts[!(names(bal_wts) %in% ph_codes)]
  }
  expect_equal(unbal_wts[names(bal_wts)], bal_wts)

  bal_pca_wts <- lapply(l_bal$PCAresults, function(res){
    wts <- setNames(res$wts, res$iCodes)
    if(length(ph_codes) > 0){
      wts <- wts[!(names(wts) %in% ph_codes)]
    }
    wts
  })
  unbal_pca_wts <- lapply(l_unbal$PCAresults, function(res){
    setNames(res$wts, res$iCodes)
  })
  expect_equal(unbal_pca_wts, bal_pca_wts)

  coin_out <- get_PCA(unbal, dset = "Aggregated", Level = 2, by_groups = TRUE,
                       out2 = "coin", nowarnings = TRUE, weights_to = "PCA_L2")
  expect_s3_class(coin_out, "unbalanced_coin")
  if(length(ph_codes) > 0){
    expect_false(any(coin_out$Meta$Weights$PCA_L2$iCode %in% ph_codes))
  }
})
