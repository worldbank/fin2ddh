#' add_new_dataset
#'
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param url string: Financa data api
#'
#' @import jsonlite
#' @importFrom magrittr "%>%"
#' @return list
#' @export
#'

add_new_dataset <- function(root_url = dkanr::get_url(),
                            ddh_credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())) {
  # STEP 1: Collect data from API
  url <- 'http://finances.worldbank.org//api/search/views.json?limitTo=tables&datasetView=DATASET'
  temp <- extract_fin_metadata(url)

  # STEP 2: Filter to results in catalog
  temp <- filter_fin_metadata(temp)

  # STEP 3: MAP to keys and flatten
  # mapped_finance <- temp[["results"]] %>%
  #   purrr::map(fin_to_ddh_keys) %>%
  #   purrr::map(add_constant_metadata) %>%
  #   purrr::map(map_fin_metadata) %>%
  #   purrr::map(create_json_dataset)

 for(i in 1:length(temp$results)){
    print(i)
    metadata_temp <- fin_to_ddh_keys(temp$results[[i]])
    metadata_temp <- add_constant_metadata(metadata_temp)
    metadata_temp <- map_fin_metadata(metadata_temp)

    category <- temp[["results"]][[i]]$view$category
    metadata_temp <- add_link_to_resources(metadata_temp, category)

    json_dat <- create_json_dataset(metadata_temp)
    resp_dat <- ddhconnect::create_dataset(credentials = ddh_credentials, body = json_dat, root_url = root_url)

    json_res <- create_json_resource(metadata_temp)
    resp_res <- ddhconnect::create_resource(credentials = ddh_credentials, body = json_res, root_url = root_url)

    json_attach <- create_json_attach(metadata_list = metadata_temp, resource_nid = resp_res$nid)
    resp_attach <- attach_resource_to_dataset(credentials = ddh_credentials,
                                              dataset_nid = resp_dat$nid,
                                              body = json_attach,
                                              root_url = root_url)

    test_created_dataset(nid = resp_dat$nid,
                         metadata_list = metadata_temp,
                         credentials = ddh_credentials,
                         root_url = root_url)
    print(resp_dat$uri)
  }

}
