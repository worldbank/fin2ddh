context("test-create_json_resource.R")

# httptest::start_capturing(path = './tests/testthat')
# dkanr::get_url()
# dkanr::get_cookie()
# dkanr::get_token()
# ddhconnect::get_lovs()
# httptest::stop_capturing()

# credentials = list(cookie = dkanr::get_cookie(),
#                    token = dkanr::get_token())

# TODO need to fix issue with httptest
metadata_list = list(field_ddh_harvest_sys_id = "kdui-wcs3")

httptest::with_mock_api({
  test_that("resource fields have correct values", {
    credentials = list(cookie = dkanr::get_cookie(),
                       token = dkanr::get_token())
    lovs <- ddhconnect::get_lovs(root_url = dkanr::get_url())

    json_resource <- jsonlite::fromJSON(create_json_resource(metadata_list))
    expect_equal(json_resource$title, "Visit World Bank Finances")

    tid_type <- unlist(json_resource$field_wbddh_resource_type, use.names = FALSE)
    expect_equal(lovs[lovs$machine_name == "field_wbddh_resource_type" & lovs$tid == tid_type, "list_value_name"], "Query Tool")

    tid_data_class <- unlist(json_resource$field_wbddh_data_class, use.names = FALSE)
    expect_equal(lovs[lovs$machine_name == "field_wbddh_data_class" & lovs$tid == tid_data_class, "list_value_name"], "Public")

    # expect_equal(json_resource$field_link_api, paste0("http://finances.worldbank.org/d/", metadata_list$field_ddh_harvest_sys_id))
  })
})
