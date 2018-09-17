#' add_new_datasets
#'
#' Extract specific metadata from the Finance API JSON response
#'
#' @param metadata_lists list: list of finance metadata
#'
#' @import jsonlite
#' @importFrom magrittr "%>%"
#' @return list
#' @export
#'

add_new_datasets <- function(metadata_lists,
                             root_url = dkanr::get_url(),
                             ddh_credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())) {

  # add new datasets
  for(i in 1:length(metadata_lists)){
    print(i)
    metadata_temp <- fin_to_ddh_keys(metadata_lists[[i]])
    metadata_temp <- add_constant_metadata(metadata_temp)
    metadata_temp <- map_fin_metadata(metadata_temp)

    category <- metadata_lists[[i]]$view$category
    metadata_temp <- add_link_to_resources(metadata_temp, category)

    json_dat <- create_json_dataset(metadata_temp)
    resp_dat <- ddhconnect::create_dataset(credentials = ddh_credentials, body = json_dat, root_url = root_url)

    json_res <- create_json_resource(metadata_temp)
    resp_res <- ddhconnect::create_resource(credentials = ddh_credentials, body = json_res, root_url = root_url)

    json_attach <- ddhconnect::create_json_attach(resource_nids = c(resp_res$nid),
                                                  root_url = root_url)
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
