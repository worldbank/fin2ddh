#' get_fin_datasets_metadata
#'
#' Get all finance metadata for those datasets in the finance catalog
#'
#' @param fin_internal_ids vector: list of finanace ids to subset the number of datasets added
#' @param is_unit_test boolean: check if unit test is being run
#'
#' @return list
#' @export

get_fin_datasets_metadata <- function(fin_internal_ids = NULL, is_unit_test = FALSE) {
  temp <- extract_fin_metadata(is_unit_test = is_unit_test)
  temp <- filter_fin_metadata(temp)

  if (!is.null(fin_internal_ids)) {
    new_fin <- unlist(lapply(temp$results, function(x) x$view$id %in% fin_internal_ids), use.names = FALSE)
    temp$results <- temp$results[new_fin]
  }

  out <- temp$results
  return(out)
}
