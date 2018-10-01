#' extract_fin_metadata
#'
#' Extract specific metadata from the Finance API JSON response
#'
#'
#' @return list
#' @export
#'

extract_fin_metadata <- function() {
  url = "http://finances.worldbank.org//api/search/views.json?limitTo=tables&datasetView=DATASET"
  finance_data <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  return(finance_data)
}
