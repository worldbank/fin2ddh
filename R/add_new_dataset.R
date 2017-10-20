#' add_new_dataset
#'
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param url string: Financa data api
#'
#' @import jsonlite
#' @importFrom magrittr "%>%"
#' @return list
#' @export
#'

url <- "http://finances.worldbank.org//api/search/views.json?limitTo=tables&datasetView=DATASET"
add_new_dataset <- function(url) {
  # STEP 1: Collect data from API
  temp <- extract_fin_metadata(url)

  # STEP 2: Filter to results in catalog
  temp <- filter_fin_metadata(temp)

  # STEP 3: MAP to keys and flatten
  mapped_finance <- temp[["results"]] %>%
    purrr::map(fin_to_ddh_keys) %>%
    purrr::map(add_constant_metadata) %>%
    purrr::map(map_fin_metadata)
  return(mapped_finance)
}