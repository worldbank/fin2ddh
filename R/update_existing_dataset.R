#' update_existing_dataset
#'
#' Update a full finance record in DDH (metadata + resources)
#'
#' @param metadata_list list: list of finance metadata, from get_fin_datasets_metadata()
#' @param master dataframe: Output of fin2ddh::get_ddh_records_status()
#' @param ddh_fields dataframe: table of all the data catalog fields by node type
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#'
#' @return character
#' @export
#'

update_existing_dataset <- function(metadata_list,
                                    master = fin2ddh::get_ddh_records_status(),
                                    ddh_fields = ddhconnect::get_fields(),
                                    lovs = ddhconnect::get_lovs(),
                                    root_url = dkanr::get_url(),
                                    credentials = list(cookie = dkanr::get_cookie(),
                                                       token = dkanr::get_token())) {

  # format raw metadata
  metadata_temp <- fin_to_ddh_keys(metadata_list)
  metadata_temp <- add_constant_metadata_dataset(metadata_temp)
  metadata_temp <- map_fin_metadata(metadata_temp)

  category <- metadata_list$view$category
  metadata_temp <- add_link_to_resources(metadata_temp, category)

  # create dataset
  metadata_temp_dataset <- filter_dataset_fields(metadata_temp, ddh_fields)
  json_dat <- ddhconnect::create_json_dataset(values = metadata_temp_dataset,
                                              publication_status = "published",
                                              ddh_fields = ddh_fields,
                                              lovs = lovs,
                                              root_url = root_url)
  dataset_nid <- master[master$fin_internal_id == metadata_list$view$id, "ddh_nids"]
  resp_dat <- ddhconnect::update_dataset(nid = dataset_nid,
                                         body = json_dat,
                                         root_url = root_url,
                                         credentials = credentials)

  tryCatch({
    # Update Resource
    metadata_temp <- add_constant_metadata_resource(metadata_temp)
    metadata_temp_resource <- filter_resource_fields(metadata_temp, ddh_fields)
    json_res <- ddhconnect::create_json_resource(values = metadata_temp_resource,
                                                 dataset_nid = resp_dat$nid,
                                                 publication_status = "published",
                                                 ddh_fields = ddh_fields,
                                                 lovs = lovs,
                                                 root_url = root_url)

    metadata_dataset <- ddhconnect::get_metadata(nid = resp_dat$nid,
                                                 root_url = root_url,
                                                 credentials = credentials)
    resource_nid <- unlist(ddhconnect::get_resource_nids(metadata_dataset))


    # makesure resource is Finances Query Tool
    if(length(resource_nid) > 1){
      resource_nid <- resource_check(as.list(resource_nid))
    }

    resp_res <- ddhconnect::update_resource(nid = resource_nid,
                                            body = json_res,
                                            root_url = root_url,
                                            credentials = credentials)

    test_created_dataset(dataset_metadata = metadata_dataset,
                         metadata_list = metadata_temp_dataset,
                         root_url = root_url,
                         credentials = credentials)

    return(resp_dat$uri)

  }, error = function(e){

    return(paste("Error:",e,"; with creating resources for", resp_dat$uri))

  })
}

