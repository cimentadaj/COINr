test_that("Denominate.df", {

  # Get a sample of indicator data (note must be indicators plus a "UnitCode" column)
  iData <- ASEM_iData[c("uCode", "Goods", "Flights", "LPI")]

  # Also get some denominator data
  denoms <- ASEM_iData[c("uCode", "GDP", "Population")]

  # specify how to denominate
  denomby <- data.frame(iCode = c("Goods", "Flights"),
                        Denominator = c("GDP", "Population"),
                        ScaleFactor = c(1, 1000))

  # Denominate one by the other
  xd <- Denominate(iData, denoms, denomby)

  # Now repeat manually...
  x_m <- merge(iData, denoms, by = "uCode", sort = FALSE)

  # check denom
  expect_equal(xd$Goods, x_m$Goods/x_m$GDP)
  # check scale fac
  expect_equal(xd$Flights, x_m$Flights/x_m$Population*1000)
  # check no denom
  expect_equal(xd$LPI, x_m$LPI)

})

test_that("Denom.coin", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # denominate (here, we only need to say which dset to use, takes
  # specs and denominators from within the coin)
  coin <- Denominate(coin, dset = "Raw")
  xd <- get_dset(coin, "Denominated")

  # Now test...

  # get raw dset
  xraw <- get_dset(coin, "Raw")

  # get specs
  iMeta <- coin$Meta$Ind
  iMeta <- iMeta[!is.na(iMeta$Denominator), ]

  # get denoms
  denoms <- coin$Meta$Unit

  # since the previous test demonstrates that df method works, we can use that
  xd2 <- Denominate(xraw, denoms = denoms, denomby = iMeta)

  expect_equal(xd, xd2)

})


test_that("Denominate unbalanced coin", {

  denoms <- data.frame(
    uCode = unbal_iData$uCode,
    DenSub = c(2, 4, 5),
    DenB = c(10, 12, 15)
  )

  denomby <- data.frame(
    iCode = c("IndA1", "IndA2", "IndB"),
    Denominator = c("DenSub", "DenSub", "DenB"),
    ScaleFactor = 1
  )

  coin_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  coin_unbal <- Denominate(coin_unbal, dset = "Raw", denoms = denoms,
                           denomby = denomby, write_to = "Denom_unbal")

  expect_s3_class(coin_unbal, c("unbalanced_coin", "coin"))
  denom_dset <- get_dset(coin_unbal, "Denom_unbal")
  denom_manual <- Denominate(unbal_iData, denoms = denoms, denomby = denomby)

  expect_equal(denom_dset, denom_manual)
  expect_setequal(names(denom_dset), c("uCode", "IndA1", "IndA2", "IndB"))

  coin_unbal_df <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  denom_df <- Denominate(coin_unbal_df, dset = "Raw", denoms = denoms,
                          denomby = denomby, out2 = "df")

  expect_setequal(names(denom_df), c("uCode", "IndA1", "IndA2", "IndB"))

})
