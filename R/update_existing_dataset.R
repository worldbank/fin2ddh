#' update_existing_dataset()
#'
#' Update a full finance record in DDH (metadata + resources)
#'
#' @param fin_internal_id character: Finance portal internal ID of the dataset to be added
#' @param credentials list: DDH API authentication token and cookie
#' @param master dataframe: Master lookup table
#' @param root_url character: Root URL to use for the API (Staging or Production)
#'
#' @return character
#' @export
#'

update_existing_dataset <- function(fin_internal_id, credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token()),
                                    master, root_url = dkanr::get_url()) {

  if (root_url == ddhconnect:::production_root_url) {
    lkup_tids <- mdlibtoddh::ddh_tid_lovs
  } else {
    lkup_tids <- mdlibtoddh::ddh_tid_lovs_STG
  }

  # Get raw values from finance portal API and filter to the particular dataset
  url <- 'http://finances.worldbank.org//api/search/views.json?limitTo=tables&datasetView=DATASET'
  temp <- extract_fin_metadata(url)
  temp <- filter_fin_metadata(temp)
  metadata_lists <- temp$results
  # filter to get the metadata for the paritcular dataset
  metadata_list <- metadata_lists[sapply(metadata_lists, function(x) x$view$id == fin_internal_id)]

  # format raw metadata
  metadata_temp <- fin_to_ddh_keys(metadata_list)
  metadata_temp <- add_constant_metadata(metadata_temp)
  metadata_temp <- map_fin_metadata(metadata_temp)

  category <- metadata_list$view$category
  metadata_temp <- add_link_to_resources(metadata_temp, category)

  # Create JSON dataset
  json_dat <- create_json_dataset(metadata_temp)
  # Push dataset to DDH
  node_id <- master$ddh_nids[master$md_internal_id == fin_internal_id]
  resp_dat <- ddhconnect::update_dataset(credentials = credentials,
                                         nid = node_id,
                                         body = json_dat,
                                         root_url = root_url)

  # Create JSON resource
  nid_res <- ddhconnect::get_resource_nid(credentials = credentials,
                                          nid = resp_dat$nid,
                                          root_url = root_url)
  json_res <- create_json_resource(metadata_temp)
  # push resource to DDH
  resp_res <- ddhconnect::update_dataset(credentials = credentials,
                                         nid = nid_res,
                                         body = json_res,
                                         root_url = root_url)


  return(resp_dat$uri)
}
