test_that("coin custom op", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin")

  # create function - replaces suspected unreliable point with NA
  f_NA <- function(x){ x[3, 10] <- NA; return(x)}

  # call function from Custom()
  coin <- Custom(coin, dset = "Raw", f_cust = f_NA)

  expect_equal(coin$Data$Custom[3,10], as.numeric(NA))

})

test_that("custom unbalanced coin", {

  coin_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)

  f_scale <- function(x){
    x$IndA1 <- x$IndA1 * 2
    x
  }

  coin_unbal <- Custom(coin_unbal, dset = "Raw", f_cust = f_scale, write_to = "Custom_unbal")

  expect_s3_class(coin_unbal, c("unbalanced_coin", "coin"))

  custom_unbal <- get_dset(coin_unbal, "Custom_unbal")
  expect_setequal(names(custom_unbal), c("uCode", "IndA1", "IndA2", "IndB"))
  expect_false(any(names(custom_unbal) %in% coin_unbal$Meta$Unbalanced$PlaceholderCodes))

  manual <- f_scale(unbal_iData)
  expect_equal(custom_unbal, manual)

})

test_that("purse custom op", {

  # build example coin
  purse <- build_example_purse(up_to = "new_coin")

  # create function - replaces suspected unreliable point with NA
  f_NA <- function(x){ x[x$uCode == "AUT", "Goods"] <- NA; return(x)}

  # call function from Custom()
  purse <- Custom(purse, dset = "Raw", f_cust = f_NA, global = FALSE)

  # check
  dat_AT <- get_data(purse, dset = "Custom", iCodes = "Goods", uCodes = "AUT")

  expect_equal(dat_AT$Goods, as.numeric(rep(NA, 5)))

})
