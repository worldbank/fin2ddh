#' unpublish_existing_dataset
#'
#' Update a full finance record in DDH (metadata + resources)
#'
#' @param metadata_list list: Finance portal internal ID of the dataset to be added
#' @param credentials list: DDH API authentication token and cookie
#' @param master dataframe: Output of fin2ddh::get_ddh_records_status()
#' @param root_url character: Root URL to use for the API (Staging or Production)
#'
#' @return character
#' @export
#'

unpublish_existing_dataset <- function(dataset_nid,
                                       credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token()),
                                       root_url = dkanr::get_url()) {

  unpublish_body <- create_json_unpublish()

  out <- ddhconnect::update_dataset(nid = dataset_nid,
                                    body = unpublish_body,
                                    root_url = root_url,
                                    credentials = credentials)

  return(out)
}
