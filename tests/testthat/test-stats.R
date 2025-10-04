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
