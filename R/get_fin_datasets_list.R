#' get_fin_datasets_list
#'
#' @param metadata_lists list: list of metadata for all the datasets in the Finance Portal
#'
#' @import purrr
#' @return data.frame
#' @export
#'

get_fin_datasets_list <- function(metadata_lists) {
  fin_internal_id <- purrr::map_chr(metadata_lists, c("view", "id"))
  fin_internal_updated <- purrr::map_chr(metadata_lists, c("view", "rowsUpdatedAt"))
  out <- data.frame(fin_internal_id, fin_internal_updated, stringsAsFactors = FALSE)
  return(out)
}
