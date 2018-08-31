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

unpublish_existing_dataset <- function(fin_internal_id, credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token()),
                                    master = fin2ddh::get_ddh_records_status(), root_url = dkanr::get_url()) {
}
