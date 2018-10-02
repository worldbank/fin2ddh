#' add_new_datasets
#'
#' Extract specific metadata from the Finance API JSON response
#'
#' @param metadata_lists list: list of finance metadata, from get_fin_datasets_metadata()
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#'
#' @import jsonlite
#' @importFrom magrittr "%>%"
#' @return list
#' @export
#'

add_new_datasets <- function(metadata_lists,
                             root_url = dkanr::get_url(),
                             credentials = list(cookie = dkanr::get_cookie(),
                                                token = dkanr::get_token())) {

  # add new datasets
  for(i in 1:length(metadata_lists)){
    # print(i)
    # format raw metadata
    metadata_temp <- fin_to_ddh_keys(metadata_lists[[i]])
    metadata_temp <- add_constant_metadata_dataset(metadata_temp)
    metadata_temp <- map_fin_metadata(metadata_temp)

    category <- metadata_lists[[i]]$view$category
    metadata_temp <- add_link_to_resources(metadata_temp, category)

    # create dataset
    json_dat <- ddhconnect::create_json_body(values = metadata_temp,
                                             node_type = "dataset",
                                             root_url = root_url)
    resp_dat <- ddhconnect::create_dataset(body = json_dat,
                                           root_url = root_url,
                                           credentials = credentials)

    # create resource
    metadata_temp_resource <- add_constant_metadata_resource(metadata_temp)
    json_res <- ddhconnect::create_json_body(values = metadata_temp_resource,
                                             node_type = "resource",
                                             root_url = root_url)
    resp_res <- ddhconnect::create_resource(body = json_res,
                                            root_url = root_url,
                                            credentials = credentials)

    # attach dataset to resource
    json_attach <- ddhconnect::create_json_attach(resource_nids = c(resp_res$nid),
                                                  root_url = root_url)
    resp_attach <- ddhconnect::attach_resources_to_dataset(dataset_nid = resp_dat$nid,
                                                           resource_nids = c(resp_res$nid),
                                                           root_url = root_url,
                                                           credentials = credentials)
    test_created_dataset(dataset_nid = resp_dat$nid,
                         metadata_list = metadata_temp,
                         root_url = root_url,
                         credentials = credentials)
    print(resp_dat$uri)
  }
}
