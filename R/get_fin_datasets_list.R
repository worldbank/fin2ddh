#' get_fin_datasets_list
#' return all the finance datasets with updated time
#'
#'
#' @import purrr
#' @return data.frame
#' @export
#'

get_fin_datasets_list <- function() {
  metadata_lists <- fin2ddh::get_fin_datasets_metadata()
  fin_internal_id <- purrr::map_chr(metadata_lists, c("view", "id"))
  fin_internal_updated <- purrr::map_chr(metadata_lists, c("view", "rowsUpdatedAt"))
  out <- data.frame(fin_internal_id, fin_internal_updated, stringsAsFactors = FALSE)
  return(out)
}
