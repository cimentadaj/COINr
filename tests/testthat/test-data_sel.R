test_that("get_data", {

  coin <- build_example_coin(up_to = "new_coin", quietly = T)

  # correct col selection
  X <- get_data(coin, dset = "Raw", iCodes = "LPI")
  expect_setequal(names(X), c("uCode", "LPI"))
  # no meta
  X <- get_data(coin, dset = "Raw", iCodes = "LPI", also_get = "none")
  expect_setequal(names(X), c("LPI"))
  # named meta
  X <- get_data(coin, dset = "Raw", iCodes = "LPI", also_get = c("uName", "Pop_group"))
  expect_setequal(names(X), c("uCode", "uName", "Pop_group", "LPI"))

  # row selection
  X <- get_data(coin, dset = "Raw", uCodes = c("AUT", "AUS"))
  expect_setequal(X$uCode, c("AUT", "AUS"))
  # group targeted by unit
  X <- get_data(coin, dset = "Raw", uCodes = "AUT", use_group = "GDP_group")
  expect_equal(unique(X$GDP_group), "L")
  expect_equal(nrow(X), sum(ASEM_iData$GDP_group == "L"))

})

test_that("get_data.unbalanced_coin excludes placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  placeholders <- unbal$Meta$Unbalanced$PlaceholderCodes

  raw_df <- get_data(unbal, dset = "Raw", Level = 1, also_get = "none")
  expect_false(any(names(raw_df) %in% placeholders))

  unbal <- Aggregate(unbal, dset = "Raw")
  agg_df <- get_data(unbal, dset = "Aggregated", Level = 2, also_get = "none")
  expect_false(any(names(agg_df) %in% placeholders))

  avail_list <- get_data_avail(unbal, dset = "Aggregated", out2 = "list")
  expect_false(any(names(avail_list$Summary) %in% placeholders))
  expect_false(any(names(avail_list$ByGroup) %in% placeholders))
  expect_setequal(names(avail_list$ByGroup), c("uCode", "SubA", "Index"))

  avail_coin <- get_data_avail(unbal, dset = "Aggregated", out2 = "coin")
  expect_true(inherits(avail_coin, "unbalanced_coin"))
  expect_false(any(names(avail_coin$Analysis$Aggregated$DatAvail$Summary) %in% placeholders))
  expect_false(any(names(avail_coin$Analysis$Aggregated$DatAvail$ByGroup) %in% placeholders))
  expect_setequal(names(avail_coin$Analysis$Aggregated$DatAvail$ByGroup), c("uCode", "SubA", "Index"))
})
