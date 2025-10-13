# tests for new_coin()
# just class checks for now

test_that("class check",{
  expect_s3_class(new_coin(ASEM_iData, ASEM_iMeta, quietly = TRUE), "coin")
  expect_s3_class(new_coin(ASEM_iData_p, ASEM_iMeta, split_to = "all", quietly = TRUE), c("purse", "data.frame"))
})

test_that("check_iData", {

  # spaces
  iData <- ASEM_iData
  names(iData)[10] <- "spa ce"
  expect_error(check_iData(iData))

  # number start
  iData <- ASEM_iData
  names(iData)[10] <- "1number"
  expect_error(check_iData(iData))

})

test_that("check_iMeta", {

  # spaces
  iMeta <- ASEM_iMeta
  iMeta$iCode[10] <- "spa ce"
  expect_error(check_iMeta(iMeta))

  # number start
  iMeta <- ASEM_iMeta
  iMeta$iCode[10] <- "1number"
  expect_error(check_iMeta(iMeta))

  # duplicate codes
  iMeta <- ASEM_iMeta
  iMeta <- rbind(iMeta[1,], iMeta)
  expect_error(check_iMeta(iMeta))

})

restore_original_meta <- function(balanced_meta, original_meta){
  original <- original_meta
  original$Parent[original$Parent == ""] <- NA_character_
  cols <- names(original)
  cleaned <- balanced_meta[balanced_meta$IsPlaceholder != TRUE, cols, drop = FALSE]
  cleaned <- cleaned[match(original$iCode, cleaned$iCode), , drop = FALSE]
  cleaned$Parent <- original$Parent
  cleaned$Level <- original$Level
  cleaned$Weight <- original$Weight
  cleaned$Direction <- original$Direction
  cleaned$Type <- original$Type
  rownames(cleaned) <- NULL
  rownames(original) <- NULL
  cleaned
}

test_that("new_unbalanced_coin balances hierarchy", {

  coin <- new_unbalanced_coin(unbal_iData, unbal_iMeta, quietly = TRUE)

  expect_s3_class(coin, c("unbalanced_coin", "coin"))
  expect_true(length(coin$Meta$Unbalanced$PlaceholderCodes) > 0)

  # public lineage should match the original unbalanced structure
  expect_equal(coin$Meta$Lineage, coin$Meta$Lineage_unbalanced)

  # balanced metadata retains placeholders internally, but raw data stays clean
  expect_true(any(coin$Meta$Unbalanced$BalancedMeta$IsPlaceholder))
  expect_setequal(names(coin$Data$Raw), c("uCode", "IndA1", "IndA2", "IndB"))

})

test_that("new_unbalanced_coin handles aggregate column supplied in data", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  meta1 <- unbal_iMeta
  meta1$Parent[meta1$Parent == ""] <- NA_character_
  meta1 <- meta1[order(meta1$Level, meta1$iCode), ]

  idata1 <- unbal_iData
  idata1$AggSolo <- rowMeans(idata1[c("IndA1", "IndA2")])

  agg_row <- data.frame(
    iCode = "AggSolo",
    Level = 2L,
    Parent = "Index",
    Direction = 1L,
    Weight = 0.5,
    Type = "Aggregate",
    stringsAsFactors = FALSE
  )
  meta1_ext <- rbind(meta1, agg_row)
  meta1_ext$Parent[meta1_ext$Parent == ""] <- NA_character_
  meta1_ext <- meta1_ext[order(meta1_ext$Level, meta1_ext$iCode), ]
  rownames(meta1_ext) <- NULL

  coin <- new_unbalanced_coin(idata1, meta1_ext, quietly = TRUE)

  balanced <- coin$Meta$Unbalanced$BalancedMeta
  restored <- restore_original_meta(balanced, meta1_ext)

  expect_identical(restored, meta1_ext)
  expect_true(any(coin$Meta$Unbalanced$BalancedMeta$IsPlaceholder))
})

test_that("new_unbalanced_coin handles indicators skipping intermediate levels", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  idata2 <- unbal_iData
  meta2 <- unbal_iMeta
  meta2$Parent[meta2$Parent == ""] <- NA_character_
  meta2 <- meta2[order(meta2$Level, meta2$iCode), ]

  idata2$SkipIndicator <- idata2$IndB * 1.1

  top_row <- data.frame(
    iCode = "Mega",
    Level = 4L,
    Parent = NA_character_,
    Direction = 1L,
    Weight = 1,
    Type = "Aggregate",
    stringsAsFactors = FALSE
  )
  meta2_ext <- rbind(meta2, top_row)
  meta2_ext$Parent[meta2_ext$Parent == ""] <- NA_character_
  idx_index <- meta2_ext$iCode == "Index"
  meta2_ext$Parent[idx_index] <- "Mega"
  meta2_ext$Weight[idx_index] <- 0.5

  skip_row <- data.frame(
    iCode = "SkipIndicator",
    Level = 1L,
    Parent = "Mega",
    Direction = 1L,
    Weight = 0.5,
    Type = "Indicator",
    stringsAsFactors = FALSE
  )
  meta2_ext <- rbind(meta2_ext, skip_row)
  meta2_ext$Parent[meta2_ext$Parent == ""] <- NA_character_
  meta2_ext <- meta2_ext[order(meta2_ext$Level, meta2_ext$iCode), ]
  rownames(meta2_ext) <- NULL

  coin <- new_unbalanced_coin(idata2, meta2_ext, quietly = TRUE)
  balanced <- coin$Meta$Unbalanced$BalancedMeta
  restored <- restore_original_meta(balanced, meta2_ext)

  expect_identical(restored, meta2_ext)
  expect_true(length(coin$Meta$Unbalanced$PlaceholderCodes) >= 1)
})

test_that("new_unbalanced_coin handles multiple branches requiring placeholders", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  idata3 <- unbal_iData
  meta3 <- unbal_iMeta
  meta3$Parent[meta3$Parent == ""] <- NA_character_
  meta3 <- meta3[order(meta3$Level, meta3$iCode), ]

  idata3$IndC <- idata3$IndA1 * 0.8
  idata3$IndD <- idata3$IndA2 * 1.2
  idata3$IndE <- idata3$IndB * 0.9
  idata3$AggWide <- rowMeans(idata3[c("IndC", "IndD")])

  meta3_ext <- meta3
  meta3_ext$Weight[meta3_ext$iCode == "SubA"] <- 0.2
  meta3_ext$Weight[meta3_ext$iCode == "IndB"] <- 0.2

  new_rows <- rbind(
    data.frame(iCode = "IndC", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", stringsAsFactors = FALSE),
    data.frame(iCode = "IndD", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", stringsAsFactors = FALSE),
    data.frame(iCode = "IndE", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", stringsAsFactors = FALSE),
    data.frame(iCode = "AggWide", Level = 2L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Aggregate", stringsAsFactors = FALSE)
  )

  meta3_ext <- rbind(meta3_ext, new_rows)
  meta3_ext$Parent[meta3_ext$Parent == ""] <- NA_character_
  meta3_ext <- meta3_ext[order(meta3_ext$Level, meta3_ext$iCode), ]
  rownames(meta3_ext) <- NULL

  coin <- new_unbalanced_coin(idata3, meta3_ext, quietly = TRUE)
  balanced <- coin$Meta$Unbalanced$BalancedMeta
  restored <- restore_original_meta(balanced, meta3_ext)

  expect_identical(restored, meta3_ext)
  expect_true(length(coin$Meta$Unbalanced$PlaceholderCodes) >= 1)
})

test_that("new_unbalanced_coin errors when aggregates remain at level 1", {

  data("unbal_iData", package = "COINr")
  data("unbal_iMeta", package = "COINr")

  idata3 <- unbal_iData
  meta3 <- unbal_iMeta
  meta3$Parent[meta3$Parent == ""] <- NA_character_
  meta3 <- meta3[order(meta3$Level, meta3$iCode), ]

  idata3$IndC <- idata3$IndA1 * 0.8
  idata3$IndD <- idata3$IndA2 * 1.2
  idata3$IndE <- idata3$IndB * 0.9
  idata3$AggWide <- rowMeans(idata3[c("IndC", "IndD")])

  meta3_ext <- meta3
  meta3_ext$Weight[meta3_ext$iCode == "SubA"] <- 0.2
  meta3_ext$Weight[meta3_ext$iCode == "IndB"] <- 0.2

  new_rows <- rbind(
    data.frame(iCode = "IndC", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Aggregate", stringsAsFactors = FALSE),
    data.frame(iCode = "IndD", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", stringsAsFactors = FALSE),
    data.frame(iCode = "IndE", Level = 1L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Indicator", stringsAsFactors = FALSE),
    data.frame(iCode = "AggWide", Level = 2L, Parent = "Index", Direction = 1L, Weight = 0.2, Type = "Aggregate", stringsAsFactors = FALSE)
  )

  meta3_ext <- rbind(meta3_ext, new_rows)
  meta3_ext$Parent[meta3_ext$Parent == ""] <- NA_character_
  meta3_ext <- meta3_ext[order(meta3_ext$Level, meta3_ext$iCode), ]
  rownames(meta3_ext) <- NULL

  expect_error(
    new_unbalanced_coin(idata3, meta3_ext, quietly = TRUE),
    "Aggregates must have level 2 or higher",
    fixed = TRUE
  )
})
