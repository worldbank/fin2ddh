#' extract_fin_metadata
#'
#' Extract specific metadata from the Finance API JSON response
#' @param is_unit_test boolean: check if unit test is being run
#'
#' @return list
#' @export
#'

extract_fin_metadata <- function(is_unit_test = FALSE) {
  if(is_unit_test){
    finance_data <- fin2ddh::fin_portal_datasets_test
  } else{
    url = "http://finances.worldbank.org//api/search/views.json?limitTo=tables&datasetView=DATASET"
    finance_data <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  }
  return(finance_data)
}
