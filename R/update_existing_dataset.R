#' update_existing_datasets
#'
#' Update a full finance record in DDH (metadata + resources)
#'
#' @param metadata_lists list: list of finance metadata, from get_fin_datasets_metadata()
#' @param master dataframe: Output of fin2ddh::get_ddh_records_status()
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#'
#' @return character
#' @export
#'

update_existing_datasets <- function(metadata_lists,
                                     master = fin2ddh::get_ddh_records_status(),
                                     root_url = dkanr::get_url(),
                                     credentials = list(cookie = dkanr::get_cookie(),
                                                        token = dkanr::get_token())) {
  # update existing datasets
  for(i in 1:length(metadata_lists)){
    # print(i)
    # format raw metadata
    metadata_temp <- fin_to_ddh_keys(metadata_lists[[i]])
    metadata_temp <- add_constant_metadata_dataset(metadata_temp)
    metadata_temp <- map_fin_metadata(metadata_temp)

    category <- metadata_lists[[i]]$view$category
    metadata_temp <- add_link_to_resources(metadata_temp, category)

    # create dataset
    dataset_nid <- master[master$fin_internal_id == metadata_lists[[i]]$view$id, "ddh_nids"]
    json_dat <- ddhconnect::create_json_body(values = metadata_temp,
                                             node_type = "dataset",
                                             root_url = root_url)
    resp_dat <- ddhconnect::update_dataset(nid = dataset_nid,
                                           body = json_dat,
                                           root_url = root_url,
                                           credentials = credentials)

    # create resource
    metadata_temp_resource <- add_constant_metadata_resource(metadata_temp)
    # resource_nid <- dkanr::get_resource_nids(nid = resp_dat$nid,
    #                                              root_url = root_url,
    #                                              credentials = credentials)
    metadata_dataset <- dkanr::retrieve_node(nid = resp_dat$nid,
                                             url = root_url,
                                             as = "list",
                                             credentials = credentials)
    resource_nid <- unlist(dkanr::get_resource_nids(metadata_dataset))
    json_res <- ddhconnect::create_json_body(values = metadata_temp_resource,
                                             node_type = "resource",
                                             root_url = root_url)
    resp_res <- ddhconnect::update_resource(nid = resource_nid,
                                            body = json_res,
                                            root_url = root_url,
                                            credentials = credentials)

    test_created_dataset(dataset_nid = resp_dat$nid,
                         metadata_list = metadata_temp,
                         root_url = root_url,
                         credentials = credentials)
    print(resp_dat$uri)
  }
}
