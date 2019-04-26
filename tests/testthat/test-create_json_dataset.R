library(jsonlite)

context("test-create_json_dataset.R")

ddh_status    <- get_ddh_records_status(is_unit_test = TRUE)
fin_datasets_new <- dplyr::filter(ddh_status, status == "new")
fin_metadata  <- get_fin_datasets_metadata(fin_datasets_new$fin_internal_id, is_unit_test = TRUE)

test_that("Test that dataset values are mapped correctly", {
  # Create JSON of finance dataset
  metadata_list <- add_new_dataset(metadata_list = fin_metadata[[1]], is_unit_test_dataset = TRUE)

  # Create JSON of Test dataset
  test_metadata_list <- list()

  test_metadata_list$body[1]                                <- "The International Bank for Reconstruction and Development (IBRD) loans are public and publicly guaranteed debt extended by the World Bank Group. IBRD loans are made to, or guaranteed by, countries that are members of IBRD. IBRD may also make loans to IFC. IBRD lends at market rates. Data are in U.S. dollars calculated using historical rates. This dataset contains the latest available snapshot of the Statement of Loans. The World Bank complies with all sanctions applicable to World Bank transactions."
  test_metadata_list$field_ddh_harvest_src[1]               <- "Finances"
  test_metadata_list$field_ddh_harvest_sys_id[1]            <- "sfv5-tf7p"
  test_metadata_list$field_license_wbddh[1]                 <- "Creative Commons Attribution 4.0"
  test_metadata_list$field_topic[1]                         <- "Topic not specified"
  test_metadata_list$field_wbddh_acronym[1]                 <- "Not specified"
  test_metadata_list$field_wbddh_data_class[1]              <- "Public"
  test_metadata_list$field_wbddh_data_type[1]               <- "Other"
  test_metadata_list$field_wbddh_ds_source[1]               <- "World Bank Group"
  test_metadata_list$field_wbddh_languages_supported[1]     <- "English"
  test_metadata_list$field_wbddh_modified_date[1]           <- "2019-04-10 05:10:15"
  test_metadata_list$field_wbddh_release_date[1]            <-  "2011-05-16 13:38:17"
  test_metadata_list$title[1]                               <- "IBRD Statement of Loans - Latest Available Snapshot"
  test_metadata_list$field_granularity_list[1]              <- "Other"
  test_metadata_list$field_wbddh_update_frequency[1]        <- "Monthly"
  test_metadata_list$field_wbddh_update_schedule[1]         <- "By 20th of each month"
  test_metadata_list$field_wbddh_start_date[1]              <- "1947-05-09 00:00:00"
  test_metadata_list$field_wbddh_end_date[1]                <- "2019-03-29 00:00:00"
  test_metadata_list$field_wbddh_dsttl_upi[1]               <- "21482"
  test_metadata_list$field_ddh_external_contact_email[1]    <- "wbfinances@worldbank.org"
  test_metadata_list$field_tags[1]                          <- "finances.worldbank.org"
  test_metadata_list$field_wbddh_country[1]                 <- "Region/Country not specified"
  test_metadata_list$field_wbddh_economy_coverage[1]        <- "Economy Coverage not specified"
  test_metadata_list$field_frequency[1]                     <- "Periodicity not specified"
  # Check if both are equal
  expect_equal(fin_json, test_json)
})
