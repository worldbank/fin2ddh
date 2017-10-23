context("test-create_json_dataset.R")

test_that("dataset has all columns in the correct format", {
  ddh_credentials <- list(cookie = dkanr::get_cookie(), token = dkanr::get_token())
  ddh_root_url <- dkanr::get_url()
  metadata_list <- jsonlite::fromJSON(file("./data-raw/metadata_test.json", "r", encoding="utf8"))

  json_dat <- create_json_dataset(metadata_list)
  resp_dat <- ddhconnect::create_dataset(credentials = ddh_credentials, body = json_dat, root_url = root_url)
  node_metadata <- ddhconnect::get_metadata(resp_dat$node_id, ddh_credentials, root_url)

  expect_equal(node_metadata$title, metadata_list$title)
  expect_equal(node_metadata$field_ddh_harvest_src$und[[1]]$tid, metadata_list$field_ddh_harvest_src)
  expect_equal()
})
