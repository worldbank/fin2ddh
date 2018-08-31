#' extract_fin_metadata
#'
#' Extract specific metadata from the Finance API JSON response
#'
#' @param url string: Financa data api
#'
#' @return list
#' @export
#'

extract_fin_metadata <- function(url){
  finance_data <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  return(finance_data)
}
