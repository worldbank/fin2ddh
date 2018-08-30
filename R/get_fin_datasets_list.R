#' get_fin_datasets_list
#'
#' @param metadata_lists list: list of metadata for all the datasets in the Finance Portal
#'
#' @import purrr
#' @return data.frame
#' @export
#'

get_fin_datasets_list <- function() {
  url <- 'http://finances.worldbank.org//api/search/views.json?limitTo=tables&datasetView=DATASET'
  temp <- extract_fin_metadata(url)
  temp <- filter_fin_metadata(temp)
  metadata_lists <- temp$results
  fin_internal_id <- purrr::map_chr(metadata_lists, c("view", "id"))
  fin_internal_updated <- purrr::map_chr(metadata_lists, c("view", "rowsUpdatedAt"))
  out <- data.frame(fin_internal_id, fin_internal_updated, stringsAsFactors = FALSE)
  return(out)
}
