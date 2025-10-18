test_that("agg_df", {

  # test data frame, 5 cols 20 rows
  X <- as.data.frame(matrix(runif(100), 20, 5))

  # test default - means of rows
  Xag <- Aggregate(X)
  expect_equal(Xag, rowMeans(X))

  # try passing some weights
  Xag <- Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = 1:5))

  # repeat manually
  Xag2 <- apply(X, 1, weighted.mean, 1:5)
  expect_equal(Xag, Xag2)

  # test data threshold
  X_NAs <- X
  # set first row to have 4/5 NAs
  X_NAs[1, 1:4] <- NA
  # aggregate (same as last time)
  XagNA <- Aggregate(X_NAs, f_ag = "a_amean", f_ag_para = list(w = 1:5), dat_thresh = 0.5)
  # check first one is NA
  expect_equal(XagNA[1], as.numeric(NA))
  # check rest are the same
  expect_equal(XagNA[-1], Xag[-1])

})

test_that("agg_coin", {

  # build example coin up to normalised dset
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # the objective here will be to check that the aggregation follows the weights
  # and the structure
  # To check this properly we'll have to change the weights, since at the moment all equal
  coin$Meta$Weights$Alt <- coin$Meta$Weights$Original
  coin$Meta$Weights$Alt$Weight <- runif(nrow(coin$Meta$Weights$Alt))

  # now aggregate
  coin <- Aggregate(coin, dset = "Normalised", w = "Alt")

  # extract normalised dset
  Xnorm <- get_dset(coin, "Normalised")
  # extract aggregated dset (via coin method)
  Xagg <- get_dset(coin, "Aggregated")

  # NOW we have to repeat manually - this will be slightly fiddly
  # the starting point is the normalised data set
  Xagg2 <- Xnorm

  # get lineage
  lin <- coin$Meta$Lineage
  # get weights
  ws <- coin$Meta$Weights$Alt

  # aggregate each pillar manually
  pcodes <- unique(lin$Pillar)
  for(pcode in pcodes){
    # ind codes
    icodes <- lin$Indicator[lin$Pillar == pcode]
    # get inds
    X <- Xagg2[icodes]
    # get weights
    w <- ws$Weight[match(icodes, ws$iCode)]
    # aggregate
    Xagg2 <- cbind(Xagg2, Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = w)))
    # rename
    names(Xagg2)[ncol(Xagg2)] <- pcode
  }

  # aggregate each sub-pillar manually
  pcodes <- unique(lin$`Sub-index`)
  for(pcode in pcodes){
    # ind codes
    icodes <- unique(lin$Pillar[lin$`Sub-index` == pcode])
    # get inds
    X <- Xagg2[icodes]
    # get weights
    w <- ws$Weight[match(icodes, ws$iCode)]
    # aggregate
    Xagg2 <- cbind(Xagg2, Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = w)))
    # rename
    names(Xagg2)[ncol(Xagg2)] <- pcode
  }

  # aggregate to index
  # ind codes
  icodes <- unique(lin$`Sub-index`)
  # get inds
  X <- Xagg2[icodes]
  # get weights
  w <- ws$Weight[match(icodes, ws$iCode)]
  # aggregate
  Xagg2 <- cbind(Xagg2, Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = w)))
  # rename
  names(Xagg2)[ncol(Xagg2)] <- "Index"

  # COMPARE
  # make sure col orderings are the same
  Xagg2 <- Xagg2[names(Xagg)]

  # CHECK
  expect_equal(Xagg, Xagg2)

})

test_that("Aggregate supports per-parent function overrides", {

  base_coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  custom_f_ag <- list(
    Level2 = list(default = "a_amean", Physical = "a_genmean"),
    Level3 = "a_amean"
  )
  custom_f_para <- list(
    Level2 = list(Physical = list(p = 2))
  )

  coin_default <- Aggregate(base_coin, dset = "Normalised")
  coin_custom <- Aggregate(base_coin, dset = "Normalised", f_ag = custom_f_ag,
                           f_ag_para = custom_f_para)

  agg_default <- get_dset(coin_default, "Aggregated")
  agg_custom <- get_dset(coin_custom, "Aggregated")

  meta <- base_coin[["Meta"]][["Ind"]]
  idx_level1 <- which(meta[["Parent"]] == "Physical" & meta[["Level"]] == 1)
  level1_codes <- meta[["iCode"]][idx_level1]
  weights <- meta[["Weight"]][idx_level1]
  normalised <- get_dset(base_coin, "Normalised")[level1_codes]

  manual_physical <- Aggregate(normalised[level1_codes],
                               f_ag = "a_genmean",
                               f_ag_para = list(w = weights, p = 2))

  expect_equal(agg_custom$Physical, manual_physical)
  expect_false(isTRUE(all.equal(agg_custom$Physical, agg_default$Physical)))
  expect_equal(agg_custom$ConEcFin, agg_default$ConEcFin)
  expect_setequal(names(agg_custom), names(agg_default))

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  unbal <- Normalise(unbal, dset = "Raw", write_to = "Normalised")

  unbal_default <- Aggregate(unbal, dset = "Normalised")

  custom_unbal_f_ag <- list(
    Level2 = list(default = "a_amean", SubA = "a_genmean"),
    Level3 = "a_amean"
  )
  custom_unbal_para <- list(
    Level2 = list(SubA = list(p = 2))
  )

  unbal_custom <- Aggregate(unbal, dset = "Normalised",
                            f_ag = custom_unbal_f_ag,
                            f_ag_para = custom_unbal_para)

  agg_unbal <- get_dset(unbal_custom, "Aggregated")
  agg_unbal_default <- get_dset(unbal_default, "Aggregated")
  meta_unbal <- unbal[["Meta"]][["Ind"]]
  idx_level1_unbal <- which(meta_unbal[["Parent"]] == "SubA" & meta_unbal[["Level"]] == 1)
  level1_codes_unbal <- meta_unbal[["iCode"]][idx_level1_unbal]
  weights_unbal <- meta_unbal[["Weight"]][idx_level1_unbal]
  normalised_unbal <- get_dset(unbal_custom, "Normalised")[level1_codes_unbal]

  manual_unbal <- Aggregate(normalised_unbal,
                            f_ag = "a_genmean",
                            f_ag_para = list(w = weights_unbal, p = 2))

  expect_equal(agg_unbal$SubA, manual_unbal)
  expect_false(isTRUE(all.equal(agg_unbal$SubA, agg_unbal_default$SubA)))

  n_zscore_spec <- list(
    Level2 = list(default = "a_amean", Physical = "n_zscore"),
    Level3 = "a_amean",
    Level4 = "a_amean"
  )
  expect_error(
    Aggregate(base_coin, dset = "Normalised", f_ag = n_zscore_spec),
    "same length as nrow\\(x\\)",
    class = "simpleError"
  )
})

test_that("agg_purse", {

  # purse
  purse <- build_example_purse(up_to = "Normalise", quietly = TRUE)

  # aggregate via coin method
  coin1 <- Aggregate(purse$coin[[1]], dset = "Normalised")

  # aggregate via purse method
  purse <- Aggregate(purse, dset = "Normalised")

  # check
  expect_equal(purse$coin[[1]]$Data$Aggregated, coin1$Data$Aggregated)
})

test_that("Aggregate handles unbalanced coins", {

  coin_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  coin_unbal <- Aggregate(coin_unbal, dset = "Raw")

  placeholders <- coin_unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  expect_s3_class(coin_unbal, c("unbalanced_coin", "coin"))
  expect_setequal(names(coin_unbal$Data$Aggregated),
                  c("uCode", "IndA1", "IndA2", "IndB", "SubA", "Index"))

  expect_false(any(placeholders %in% names(coin_unbal$Data$Raw)))
  expect_false(any(placeholders %in% names(coin_unbal$Data$Aggregated)))

  expected_subA <- rowMeans(unbal_iData[c("IndA1", "IndA2")])
  expect_equal(coin_unbal$Data$Aggregated$SubA, expected_subA)

  expected_index <- (expected_subA * 0.5 + unbal_iData$IndB * 0.5) / (0.5 + 0.5)
  expect_equal(coin_unbal$Data$Aggregated$Index, expected_index)

})

test_that("Aggregate.unbalanced_coin returns placeholder-free data frames", {
  coin_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  placeholders <- coin_unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  agg_df <- Aggregate(coin_unbal, dset = "Raw", out2 = "df")

  expect_false(any(placeholders %in% names(agg_df)))
})

test_that("Aggregate handles unbalanced pipelines after multiple verbs", {
  coin_unbal <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)
  placeholders <- coin_unbal$Meta$Unbalanced$PlaceholderCodes
  placeholders <- placeholders[!is.na(placeholders)]

  coin_unbal <- Impute(coin_unbal, dset = "Raw", f_i = "i_mean", write_to = "Imputed")
  expect_false(any(placeholders %in% names(coin_unbal$Data$Imputed)))

  coin_unbal <- Normalise(coin_unbal, dset = "Imputed", write_to = "Normalised")
  expect_false(any(placeholders %in% names(coin_unbal$Data$Normalised)))

  expect_true(all(placeholders %in% names(coin_unbal$Meta$Unbalanced$PlaceholderData$Normalised)))

  coin_unbal <- Aggregate(coin_unbal, dset = "Normalised")

  expect_false(any(placeholders %in% names(coin_unbal$Data$Normalised)))
  expect_false(any(placeholders %in% names(coin_unbal$Data$Aggregated)))
  expect_setequal(names(coin_unbal$Data$Aggregated),
                  c("uCode", "IndA1", "IndA2", "IndB", "SubA", "Index"))
})

test_that("agg_functions", {

  # geometric
  x <- c(1, 2, 3)
  yg <- a_gmean(x)

  expect_equal(yg, (1*2*3)^(1/3))
  expect_error(a_gmean(c(1, 2, -1)))
  expect_error(a_gmean(c(1, 2, 0)))

  yg2 <- a_gmean(x, c(1,1,2))
  expect_equal(yg2, (1*2*3^2)^(1/4))

  # harmonic
  x <- c(1, 2, 3)
  yh <- a_hmean(x, c(2, 1, 1))
  expect_equal(yh, 4/(2/1 + 1/2 + 1/3))

  # generalised
  ygen <- a_genmean(x, c(2,1,1), p = -1)
  expect_equal(ygen, yh)
  ygen <- a_genmean(x, p = 1)
  expect_equal(ygen, 2) # simple arithmetic av.

})

test_that("outranking", {

  # a df
  X <- data.frame(
    x1 = 1:4,
    x2 = c(3:1, 5),
    x3 = c(2,3,1, 4)
  )

  orm <- outrankMatrix(X)

  expect_equal(nrow(orm$OutRankMatrix), 4)
  expect_equal(ncol(orm$OutRankMatrix), 4)

  # check some scores
  expect_equal(diag(orm$OutRankMatrix), c(0,0,0,0))
  expect_equal(orm$OutRankMatrix[2,1], 2/3)
  expect_equal(orm$OutRankMatrix[3,1], 1/3)
  expect_equal(orm$OutRankMatrix[4,1], 1)
  expect_equal(orm$nDominant, 3)
  expect_equal(orm$fracDominant, 3/6)

})

test_that("copeland", {

  # a df
  X <- data.frame(
    x1 = 1:4,
    x2 = c(3:1, 5),
    x3 = c(2,3,1, 4)
  )

  y <- a_copeland(X)

  orm <- outrankMatrix(X)$OutRankMatrix
  orm[orm > 0.5] <- 1
  orm[orm < 0.5] <- -1
  orm[orm == 0.5] <- 0
  diag(orm) <- 0

  expect_equal(y, rowSums(orm))

  # test equal units correctly assigned
  X <- data.frame(
    x1 = c(1,2),
    x2 = c(2,1)
  )

  orm <- outrankMatrix(X)$OutRankMatrix
  expect_equal(orm[1,2], 0.5)
  expect_equal(orm[2,1], 0.5)

  y <- a_copeland(X)
  expect_equal(y, c(0,0))

})

test_that("aggregation by level", {

  coin <- build_example_coin(up_to = "new_coin")

  # Testing:
  # - different aggregation functions by level
  # - different parameter sets by level
  # - passing vectors or data frames to functions at different levels
  # note: silly_aggregate is a function in utils.R
  coin <- Aggregate(coin, dset = "Raw", f_ag = c("a_amean", "a_gmean", "silly_aggregate"),
                    f_ag_para = list(NULL, NULL, list(start_at = 10)), by_df = c(FALSE, FALSE, TRUE)
                    )

  # check results
  X <- get_dset(coin, dset = "Aggregated")

  imeta <- coin$Meta$Ind

  # test lev 1 to 2
  imeta_grp <- imeta[which(imeta$Parent == "Physical"), ]
  x <- X[1, imeta_grp$iCode] |> as.numeric()
  y <- a_amean(x, w = imeta_grp$Weight)
  expect_equal(X[1, "Physical"], y)

  # test lev 2 to 3
  imeta_grp <- imeta[which(imeta$Parent == "Conn"), ]
  x <- X[1, imeta_grp$iCode] |> as.numeric()
  y <- a_gmean(x, w = imeta_grp$Weight)
  expect_equal(X[1, "Conn"], y)

  # test lev 3 to 4
  expect_equal(X[["Index"]], 10:(nrow(X) + 9))


  # Now test using different weight specs at different levels
  coin <- Aggregate(coin, dset = "Raw", f_ag = c("a_amean", "silly_aggregate_no_wts", "silly_aggregate"),
                    f_ag_para = list(NULL, NULL, list(start_at = 10)), by_df = c(FALSE, TRUE, TRUE), w = list(NULL, "none", NULL))

  # check results
  X <- get_dset(coin, dset = "Aggregated")

  # test lev 1 to 2
  imeta_grp <- imeta[which(imeta$Parent == "Physical"), ]
  x <- X[1, imeta_grp$iCode] |> as.numeric()
  y <- a_amean(x, w = imeta_grp$Weight)
  expect_equal(X[1, "Physical"], y)

  # test lev 2 to 3: expect the Conn group to be aggregated as simply the first indicator
  imeta_grp <- imeta[which(imeta$Parent == "Conn"), ]
  expect_equal(X[["Conn"]], X[[imeta_grp$iCode[1]]])

  # test lev 3 to 4
  expect_equal(X[["Index"]], 10:(nrow(X) + 9))

})

# test passing no weights to aggregation function...
test_that("sum_by_level", {

  coin <- build_example_coin(up_to = "new_coin")

  # test as sum of indicators in each group (weights NOT passed)
  coin <- Aggregate(coin, dset = "Raw", f_ag = "sum",
                    f_ag_para = list(na.rm = TRUE), w = "none"
  )

  # checks - pick a selected value
  CHN_phys <- get_data(coin, dset = "Aggregated", iCodes = "Physical", Level = 2, uCodes = "CHN", also_get = "none") |>
    as.numeric()

  CHN_phys_man <- get_data(coin, dset = "Raw", iCodes = "Physical", Level = 1, uCodes = "CHN", also_get = "none") |>
    as.numeric() |>
    sum(na.rm = TRUE)

  expect_equal(CHN_phys, CHN_phys_man)

  # another
  IND_conn <- get_data(coin, dset = "Aggregated", iCodes = "Conn", Level = 3, uCodes = "IND", also_get = "none") |>
    as.numeric()

  IND_conn_man <- get_data(coin, dset = "Aggregated", iCodes = "Conn", Level = 2, uCodes = "IND", also_get = "none") |>
    as.numeric() |>
    sum(na.rm = TRUE)

  expect_equal(IND_conn, IND_conn_man)

})
