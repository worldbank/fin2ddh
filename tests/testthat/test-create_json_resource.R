library(jsonlite)

context("test-create_json_resource.R")

dkanr::dkanr_setup(url = "http://ddh1stg.prod.acquia-sites.com/",
                   username = Sys.getenv("ddh_username"),
                   password = Sys.getenv("ddh_stg_password"))

ddh_status        <- get_ddh_records_status(is_unit_test = TRUE)
fin_datasets_new  <- dplyr::filter(ddh_status, status == "new")
fin_metadata      <- get_fin_datasets_metadata(fin_datasets_new$fin_internal_id, is_unit_test = TRUE)

test_that("Test that resource values are mapped correctly", {
  # Create JSON of finance dataset
  metadata_list <- add_new_dataset(metadata_list = fin_metadata[[1]], is_unit_test_resource = TRUE)
  fin_json      <- ddhconnect::create_json_resource(values = metadata_list)

  # Create JSON of Test dataset
  test_metadata_list <- list()

  test_metadata_list$field_ddh_harvest_src[1]     <- "Finances"
  test_metadata_list$field_ddh_harvest_sys_id[1]  <- "sfv5-tf7p"
  test_metadata_list$field_wbddh_data_class[1]    <- "Public"
  test_metadata_list$title[1]                     <- "Visit World Bank Finances"
  test_metadata_list$field_wbddh_data_class[1]    <- "Public"
  test_metadata_list$field_link_api[1]            <- "http://finances.worldbank.org/d/sfv5-tf7p"
  test_metadata_list$field_wbddh_resource_type[1] <- "Query Tool"


  test_json      <- ddhconnect::create_json_resource(values = test_metadata_list)

  # Check if both are equal
  expect_equal(fin_json, test_json)
})


test_that("Throw error with invalid fields", {

  # Create JSON of Test dataset
  test_metadata_list <- list()
  test_metadata_list$field_ddh_harvest_src[1] <- "Finances"
  test_metadata_list$title[1]                 <- "Visit World Bank Finances"

  #invalid field
  test_metadata_list$field_invalid_test[1] <- "FAIL"

  expect_error(ddhconnect::create_json_resource(values = test_metadata_list))
})


test_that("Throw error with invalid value", {

  # Create JSON of Test dataset
  test_metadata_list <- list()
  test_metadata_list$field_ddh_harvest_src[1] <- "Finances"
  test_metadata_list$title[1]                 <- "Visit World Bank Finances"

  #invalid value
  test_metadata_list$field_wbddh_data_class[1]    <- "FAIL"

  expect_error(ddhconnect::create_json_resource(values = test_metadata_list))
})
