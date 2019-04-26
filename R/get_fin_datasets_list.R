#' get_fin_datasets_list
#' return all the finance datasets with updated time
#'
#'@param is_unit_test boolean: check if unit test is being run
#'
#' @import purrr
#' @return data.frame
#' @export
#'

get_fin_datasets_list <- function(is_unit_test = FALSE) {
  metadata_lists <- get_fin_datasets_metadata(is_unit_test = is_unit_test)
  fin_internal_id <- purrr::map_chr(metadata_lists, c("view", "id"))
  fin_internal_updated <- purrr::map_chr(metadata_lists, c("view", "rowsUpdatedAt"))
  out <- data.frame(fin_internal_id, fin_internal_updated, stringsAsFactors = FALSE)
  return(out)
}
