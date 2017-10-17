#' extract_fin_metadata
#'
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param url string: Financa data api
#'
#' @return list
#' @export
#'

url <- "http://finances.worldbank.org/api/search/views.json?limitTo=tables&datasetView=DATASET"
extract_fin_metadata <- function(url){
  finance_data <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  return(finance_data)
}
