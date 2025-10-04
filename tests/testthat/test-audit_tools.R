test_that("remove_elements", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # run function removing elements in level 2
  l_res <- remove_elements(coin, Level = 2, dset = "Aggregated", iCode = "Index")
  expect_type(l_res, "list")

  # get index data
  iData <- get_data(coin, dset = "Aggregated", iCodes = "Index")
  # get names of aggs at lev 2
  agnames <- na.omit(coin$Meta$Ind$iCode[coin$Meta$Ind$Level == 2])

  # check each entry of l_res
  # scores
  expect_s3_class(l_res$Scores, "data.frame")
  expect_equal(nrow(l_res$Scores), nrow(iData))
  expect_setequal(names(l_res$Scores), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$Scores$Nominal, iData$Index)
  expect_setequal(l_res$Scores$uCode, iData$uCode)
  # ranks
  expect_s3_class(l_res$Ranks, "data.frame")
  expect_equal(nrow(l_res$Ranks), nrow(iData))
  expect_setequal(names(l_res$Ranks), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$Scores$uCode, iData$uCode)
  expect_equal(l_res$Ranks$Nominal, rank(-1*l_res$Scores$Nominal, ties.method = "min"))
  # rankdiffs
  expect_s3_class(l_res$RankDiffs, "data.frame")
  expect_equal(nrow(l_res$RankDiffs), nrow(iData))
  expect_setequal(names(l_res$RankDiffs), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$RankDiffs$uCode, iData$uCode)
  expect_equal(l_res$RankDiffs$Physical, (l_res$Ranks$Nominal - l_res$Ranks$Physical))
  # absrankdiffs
  expect_s3_class(l_res$RankAbsDiffs, "data.frame")
  expect_equal(nrow(l_res$RankAbsDiffs), nrow(iData))
  expect_setequal(names(l_res$RankAbsDiffs), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$RankAbsDiffs$uCode, iData$uCode)
  expect_equal(l_res$RankAbsDiffs$Physical, abs(l_res$RankDiffs$Physical))
  # meanabsdiff
  expect_type(l_res$MeanAbsDiff, "double")
  expect_equal(l_res$MeanAbsDiff,
               sapply(l_res$RankAbsDiffs[names(l_res$RankAbsDiffs) != "uCode"], mean))

  # test with alt set of weights
  coin$Meta$Weights$test1 <- coin$Meta$Weights$Original
  coin$Log$Aggregate$w <- "test1"
  coin <- Regen(coin, from = "Normalise", quietly = TRUE)
  # run function removing elements in level 2
  l_res <- remove_elements(coin, Level = 3, dset = "Aggregated", iCode = "Index")
  expect_type(l_res, "list")


})

test_that("remove_elements handles unbalanced placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  unbal <- Aggregate(unbal, dset = "Raw")

  placeholders <- unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  res <- remove_elements(unbal, Level = 2, dset = "Aggregated", iCode = "Index", quietly = TRUE)
  expect_type(res, "list")
  expect_false(any(names(res$Scores) %in% placeholders))
  expect_false(any(names(res$Ranks) %in% placeholders))
  expect_false(any(names(res$RankDiffs) %in% placeholders))
  expect_false(any(names(res$RankAbsDiffs) %in% placeholders))
  if(length(res$MeanAbsDiff) > 0){
    expect_false(any(names(res$MeanAbsDiff) %in% placeholders))
  }

  base_coin <- structure(unbal, class = setdiff(class(unbal), "unbalanced_coin"))
  expected <- remove_elements.coin(base_coin, Level = 2, dset = "Aggregated", iCode = "Index", quietly = TRUE)
  drop_cols <- function(df){
    if(!is.data.frame(df)){
      return(df)
    }
    keep <- setdiff(names(df), placeholders)
    df[keep]
  }
  expected$Scores <- drop_cols(expected$Scores)
  expected$Ranks <- drop_cols(expected$Ranks)
  expected$RankDiffs <- drop_cols(expected$RankDiffs)
  expected$RankAbsDiffs <- drop_cols(expected$RankAbsDiffs)
  if(!is.null(expected$MeanAbsDiff)){
    expected$MeanAbsDiff <- expected$MeanAbsDiff[names(expected$MeanAbsDiff) %nin% placeholders]
  }
  expect_equal(res, expected)
})
