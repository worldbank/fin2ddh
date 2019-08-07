library(dplyr)

context("test-classify_dataset_status.R")

# Get DDH Status Dataframe
ddh_status <- get_ddh_records_status(is_unit_test = TRUE)

test_that("Classify new datasets", {
  # Filter out new datasets
  fin_datasets_new <- dplyr::filter(ddh_status, status == "new")

  # Create dataframe of expected results
  new_datasets_df           <- matrix(ncol = 8, nrow = 1)
  new_datasets_df           <- data.frame(new_datasets_df)
  colnames(new_datasets_df) <- c("ddh_nids","ddh_created","ddh_updated","fin_internal_id",
                                 "fin_internal_updated", "status","sync_status","duplicate_status")

  new_datasets_df[1,]$fin_internal_id       <- "sfv5-tf7p"
  new_datasets_df[1,]$fin_internal_updated  <- "1554887415"
  new_datasets_df[1,]$status                <- "new"

  new_datasets_df[1,]$ddh_nids              <- as.character(NA)
  new_datasets_df[1,]$ddh_created           <- as.character(NA)
  new_datasets_df[1,]$ddh_updated           <- as.numeric(NA)
  new_datasets_df[1,]$sync_status           <- as.character(NA)
  new_datasets_df[1,]$duplicate_status      <- as.character(NA)

  expect_equal(fin_datasets_new, new_datasets_df)
})


test_that("Classify outdated datasets", {
  # Filter out outdated datasets
  fin_datasets_outated <- dplyr::filter(ddh_status, status == "current" &
           sync_status == "out of sync" &
           duplicate_status == "original")

  # Create dataframe of expected results
  outated_datasets_df           <- matrix(ncol = 8, nrow = 6)
  outated_datasets_df           <- data.frame(outated_datasets_df)
  colnames(outated_datasets_df) <- c("ddh_nids","ddh_created","ddh_updated","fin_internal_id",
                                 "fin_internal_updated", "status","sync_status","duplicate_status")

  outated_datasets_df[1,]$fin_internal_id       <- "efin-cagm"
  outated_datasets_df[1,]$fin_internal_updated  <- "1556190696"
  outated_datasets_df[1,]$status                <- "current"
  outated_datasets_df[1,]$ddh_nids              <- "139719"
  outated_datasets_df[1,]$ddh_created           <- "1509468198"
  outated_datasets_df[1,]$ddh_updated           <- 1556003019
  outated_datasets_df[1,]$sync_status           <- "out of sync"
  outated_datasets_df[1,]$duplicate_status      <- "original"

  outated_datasets_df[2,]$fin_internal_id       <- "gsdw-avpz"
  outated_datasets_df[2,]$fin_internal_updated  <- "1556175596"
  outated_datasets_df[2,]$status                <- "current"
  outated_datasets_df[2,]$ddh_nids              <- "139791"
  outated_datasets_df[2,]$ddh_created           <- "1509468850"
  outated_datasets_df[2,]$ddh_updated           <- 1554951606
  outated_datasets_df[2,]$sync_status           <- "out of sync"
  outated_datasets_df[2,]$duplicate_status      <- "original"

  outated_datasets_df[3,]$fin_internal_id       <- "cg7w-bf4s"
  outated_datasets_df[3,]$fin_internal_updated  <- "1555329112"
  outated_datasets_df[3,]$status                <- "current"
  outated_datasets_df[3,]$ddh_nids              <- "160576"
  outated_datasets_df[3,]$ddh_created           <- "1549472956"
  outated_datasets_df[3,]$ddh_updated           <- 1552980594
  outated_datasets_df[3,]$sync_status           <- "out of sync"
  outated_datasets_df[3,]$duplicate_status      <- "original"

  outated_datasets_df[4,]$fin_internal_id       <- "v84d-dq44"
  outated_datasets_df[4,]$fin_internal_updated  <- "1555225202"
  outated_datasets_df[4,]$status                <- "current"
  outated_datasets_df[4,]$ddh_nids              <- "199036"
  outated_datasets_df[4,]$ddh_created           <- "1550167559"
  outated_datasets_df[4,]$ddh_updated           <- 1554951609
  outated_datasets_df[4,]$sync_status           <- "out of sync"
  outated_datasets_df[4,]$duplicate_status      <- "original"

  outated_datasets_df[5,]$fin_internal_id       <- "b74b-t2z3"
  outated_datasets_df[5,]$fin_internal_updated  <- "1556194119"
  outated_datasets_df[5,]$status                <- "current"
  outated_datasets_df[5,]$ddh_nids              <- "213331"
  outated_datasets_df[5,]$ddh_created           <- "1554408747"
  outated_datasets_df[5,]$ddh_updated           <- 1554969301
  outated_datasets_df[5,]$sync_status           <- "out of sync"
  outated_datasets_df[5,]$duplicate_status      <- "original"

  outated_datasets_df[6,]$fin_internal_id       <- "rcx4-r7xj"
  outated_datasets_df[6,]$fin_internal_updated  <- "1556175592"
  outated_datasets_df[6,]$status                <- "current"
  outated_datasets_df[6,]$ddh_nids              <- "213341"
  outated_datasets_df[6,]$ddh_created           <- "1554409732"
  outated_datasets_df[6,]$ddh_updated           <- 1555988394
  outated_datasets_df[6,]$sync_status           <- "out of sync"
  outated_datasets_df[6,]$duplicate_status      <- "original"

  expect_equal(fin_datasets_outated, outated_datasets_df)
})

test_that("Classify old datasets", {
  # Filter out old datasets
  fin_datasets_old <- dplyr::filter(ddh_status, status == "old")

  # Create dataframe of expected results
  old_datasets_df           <- matrix(ncol = 8, nrow = 2)
  old_datasets_df           <- data.frame(old_datasets_df)
  colnames(old_datasets_df) <- c("ddh_nids","ddh_created","ddh_updated","fin_internal_id",
                                     "fin_internal_updated", "status","sync_status","duplicate_status")

  old_datasets_df[1,]$fin_internal_id       <- "2ppx-k958"
  old_datasets_df[1,]$fin_internal_updated  <- as.character(NA)
  old_datasets_df[1,]$status                <- "old"
  old_datasets_df[1,]$ddh_nids              <- "139725"
  old_datasets_df[1,]$ddh_created           <- "1509468244"
  old_datasets_df[1,]$ddh_updated           <- 1383756268
  old_datasets_df[1,]$sync_status           <- as.character(NA)
  old_datasets_df[1,]$duplicate_status      <- "original"

  old_datasets_df[2,]$fin_internal_id       <- "b4d6-42j9"
  old_datasets_df[2,]$fin_internal_updated  <- as.character(NA)
  old_datasets_df[2,]$status                <- "old"
  old_datasets_df[2,]$ddh_nids              <- "139741"
  old_datasets_df[2,]$ddh_created           <- "1509468378"
  old_datasets_df[2,]$ddh_updated           <- 1383756144
  old_datasets_df[2,]$sync_status           <- as.character(NA)
  old_datasets_df[2,]$duplicate_status      <- "original"

  expect_equal(fin_datasets_old, old_datasets_df)
})


test_that("Classify duplicate datasets", {
  # Filter out duplicate datasets
  fin_datasets_duplicate <- dplyr::filter(ddh_status, duplicate_status == "duplicate")

  # Create dataframe of expected results
  duplicate_datasets_df           <- matrix(ncol = 8, nrow = 4)
  duplicate_datasets_df           <- data.frame(duplicate_datasets_df)
  colnames(duplicate_datasets_df) <- c("ddh_nids","ddh_created","ddh_updated","fin_internal_id",
                                     "fin_internal_updated", "status","sync_status","duplicate_status")

  duplicate_datasets_df[1,]$fin_internal_id       <- "5fcd-tqcy"
  duplicate_datasets_df[1,]$fin_internal_updated  <- "1554825599"
  duplicate_datasets_df[1,]$status                <- "current"
  duplicate_datasets_df[1,]$ddh_nids              <- "214441"
  duplicate_datasets_df[1,]$ddh_created           <- "1555014641"
  duplicate_datasets_df[1,]$ddh_updated           <- 1554811199
  duplicate_datasets_df[1,]$sync_status           <- "in sync"
  duplicate_datasets_df[1,]$duplicate_status      <- "duplicate"

  duplicate_datasets_df[2,]$fin_internal_id       <- "5fcd-tqcy"
  duplicate_datasets_df[2,]$fin_internal_updated  <- "1554825599"
  duplicate_datasets_df[2,]$status                <- "current"
  duplicate_datasets_df[2,]$ddh_nids              <- "214451"
  duplicate_datasets_df[2,]$ddh_created           <- "1555014859"
  duplicate_datasets_df[2,]$ddh_updated           <- 1554811199
  duplicate_datasets_df[2,]$sync_status           <- "in sync"
  duplicate_datasets_df[2,]$duplicate_status      <- "duplicate"

  duplicate_datasets_df[3,]$fin_internal_id       <- "v84d-dq44"
  duplicate_datasets_df[3,]$fin_internal_updated  <- "1555225202"
  duplicate_datasets_df[3,]$status                <- "current"
  duplicate_datasets_df[3,]$ddh_nids              <- "216321"
  duplicate_datasets_df[3,]$ddh_created           <- "1556051034"
  duplicate_datasets_df[3,]$ddh_updated           <- 1555210802
  duplicate_datasets_df[3,]$sync_status           <- "in sync"
  duplicate_datasets_df[3,]$duplicate_status      <- "duplicate"

  duplicate_datasets_df[4,]$fin_internal_id       <- "efin-cagm"
  duplicate_datasets_df[4,]$fin_internal_updated  <- "1556190696"
  duplicate_datasets_df[4,]$status                <- "current"
  duplicate_datasets_df[4,]$ddh_nids              <- "216411"
  duplicate_datasets_df[4,]$ddh_created           <- "1556122204"
  duplicate_datasets_df[4,]$ddh_updated           <- 1556003019
  duplicate_datasets_df[4,]$sync_status           <- "out of sync"
  duplicate_datasets_df[4,]$duplicate_status      <- "duplicate"

  expect_equal(fin_datasets_duplicate, duplicate_datasets_df)
})


