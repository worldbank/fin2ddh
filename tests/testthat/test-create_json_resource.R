context("test-create_json_resource.R")

metadata_list = list(field_ddh_harvest_sys_id = "kdui-wcs3")

test_that("resource fields have correct values", {
  expect_equal(jsonlite::fromJSON(create_json_resource(metadata_list))$title, "Visit World Bank Finances")
  expect_equal(jsonlite::fromJSON(create_json_resource(metadata_list))$field_wbddh_resource_type, "Query Tool")
  expect_equal(jsonlite::fromJSON(create_json_resource(metadata_list))$field_wbddh_data_class, "Public")
  expect_equal(jsonlite::fromJSON(create_json_resource(metadata_list))$field_link_api, paste0("http://finances.worldbank.org/d/", metadata_list$field_ddh_harvest_sys_id))
})
