#' add_new_dataset
#'
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param url string: Financa data api
#'
#' @return list
#' @export
#'

url <- "http://finances.worldbank.org/api/search/views.json?limitTo=tables&datasetView=DATASET"

add_new_dataset <- function(url) {
  # STEP 1: Collect data from API
  temp <- extract_fin_metadata(url)
  temp <- filter_fin_metadata(temp)

}
