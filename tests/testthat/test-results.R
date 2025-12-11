test_that("results_tables", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # get simple results table
  df_results <- get_results(coin, dset = "Aggregated", tab_type = "Aggs")
  # expect all agg names avove level 1
  agg_codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Aggregate"]
  expect_setequal(names(df_results), c("uCode", "Rank", agg_codes))

  # get full results table
  df_results <- get_results(coin, dset = "Aggregated", tab_type = "Full")
  # expect all agg names avove level 1
  agg_codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type %in% c("Indicator", "Aggregate")]
  expect_setequal(names(df_results), c("uCode", "Rank", agg_codes))

})

test_that("get_results.unbalanced_coin strips placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  unbal <- Aggregate(unbal, dset = "Raw")
  placeholders <- unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  res_aggs <- get_results(unbal, dset = "Aggregated", tab_type = "Aggs")
  if(length(placeholders) > 0){
    expect_false(any(names(res_aggs) %in% placeholders))
  }

  balanced <- structure(unbal, class = setdiff(class(unbal), "unbalanced_coin"))
  if(length(placeholders) > 0){
    if(!is.null(balanced$Meta$Ind)){
      balanced$Meta$Ind <- balanced$Meta$Ind[balanced$Meta$Ind$iCode %nin% placeholders, , drop = FALSE]
    }
    balanced$Meta$Lineage <- COINr:::.sanitize_lineage(balanced$Meta$Lineage, placeholders)
  }
  res_bal <- get_results.coin(balanced, dset = "Aggregated", tab_type = "Aggs")
  expect_equal(res_aggs, res_bal)

  res_full_ranks <- get_results(unbal, dset = "Aggregated", tab_type = "Full", use = "ranks")
  if(length(placeholders) > 0){
    expect_false(any(names(res_full_ranks) %in% placeholders))
  }

  res_bal_full <- get_results.coin(balanced, dset = "Aggregated", tab_type = "Full", use = "ranks")
  expect_equal(res_full_ranks, res_bal_full)

  res_coin <- get_results(unbal, dset = "Aggregated", tab_type = "Aggs", out2 = "coin")
  expect_true(inherits(res_coin, "unbalanced_coin"))
  stored_results <- res_coin$Results$AggsScore
  if(length(placeholders) > 0 && !is.null(stored_results)){
    expect_false(any(names(stored_results) %in% placeholders))
  }

  res_coin_bal <- get_results.coin(balanced, dset = "Aggregated", tab_type = "Aggs", out2 = "coin")
  stored_bal <- res_coin_bal$Results$AggsScore
  if(length(placeholders) > 0 && !is.null(stored_bal)){
    stored_bal <- stored_bal[setdiff(names(stored_bal), placeholders)]
  }
  expect_equal(stored_results, stored_bal)
})

test_that("unit_summary", {

  # build coin
  coin <- build_example_coin(quietly = TRUE)

  # summary for a unit (round to very high precision)
  dfsum <- get_unit_summary(coin, usel = "IND", Levels = c(4,3,2), dset = "Aggregated", nround = NULL)

  # cross check
  iData <- get_data(coin, dset = "Aggregated", uCodes = "IND")
  for(ii in 1:nrow(dfsum)){
    expect_equal(dfsum$Score[ii], as.numeric(iData[dfsum$Code[ii]]))
  }

  expect_setequal(names(dfsum), c("Code", "Name", "Score", "Rank"))

})

test_that("get_unit_summary handles unbalanced placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  unbal <- Aggregate(unbal, dset = "Raw")

  placeholders <- unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  res <- get_unit_summary(unbal, usel = "U1", Levels = c(1, 2, 3), dset = "Aggregated", nround = NULL)
  expect_setequal(names(res), c("Code", "Name", "Score", "Rank"))
  expect_false(any(res$Code %in% placeholders))

  base_coin <- structure(unbal, class = setdiff(class(unbal), "unbalanced_coin"))
  if(length(placeholders) > 0){
    if(!is.null(base_coin$Meta$Ind)){
      base_coin$Meta$Ind <- base_coin$Meta$Ind[base_coin$Meta$Ind$iCode %nin% placeholders, , drop = FALSE]
    }
    base_coin$Meta$Lineage <- COINr:::.sanitize_lineage(base_coin$Meta$Lineage, placeholders)
  }

  expected <- get_unit_summary.coin(base_coin, usel = "U1", Levels = c(1, 2, 3), dset = "Aggregated", nround = NULL)
  expect_equal(res, expected)
})

test_that("str_weak", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # get strengths and weaknesses for ESP
  l <- get_str_weak(coin, dset = "Raw", usel = "ESP")

  expect_setequal(names(l), c("Strengths", "Weaknesses"))

  # get data
  iData <- get_data(coin, dset = "Raw")

  # check vals
  for(ii in 1:nrow(l$Strengths)){
    expect_equal(l$Strengths$Value[ii],
                 signif(as.numeric(iData[iData$uCode == "ESP", l$Strengths$Code[ii]]), 3))
    expect_equal(l$Weaknesses$Value[ii],
                 signif(as.numeric(iData[iData$uCode == "ESP", l$Weaknesses$Code[ii]]), 3))
  }


})
