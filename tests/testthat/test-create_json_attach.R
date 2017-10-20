context("test-create_json_attach.R")

metadata_list <- list(title = "Afghanistan - World Bank Group Country Survey 2015")
resource_nid = 107008
json_template <- mdlibtoddh::json_template_resource

test_that("json attach constructed correctly", {
  json_attach <- jsonlite::fromJSON(create_json_attach(metadata_list, resource_nid, json_template))
  expect_equal(json_attach$field_resources$und$target_id, paste0(metadata_list$title, "(", resource_nid, ")"))
})
