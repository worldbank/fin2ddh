#' unpublish_existing_dataset
#'
#' Update a full finance record in DDH (metadata + resources)
#'
#' @param dataset_nid string: Finance dataset that needs to be unpublished
#' @param root_url character: Root URL to use for the API (Staging or Production)
#' @param credentials list: DDH API authentication token and cookie
#'
#' @return character
#' @export
#'

unpublish_existing_dataset <- function(dataset_nid,
                                       root_url = dkanr::get_url(),
                                       credentials = list(cookie = dkanr::get_cookie(),
                                                          token = dkanr::get_token())) {

  unpublish_body <- create_json_unpublish()

  out <- ddhconnect::update_dataset(nid = dataset_nid,
                                    body = unpublish_body,
                                    root_url = root_url,
                                    credentials = credentials)

  return(out)
}
