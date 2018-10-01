#' filter_fin_metadata
#'
#' Filter metadata that is in catalog
#'
#' @param finance_data list: The formatted JSON from extract_fin_metadata
#'
#' @return list
#' @export
#'

filter_fin_metadata <- function(finance_data) {
  for (i in finance_data[["count"]]:1){
    in_catalog <- is.null(finance_data[["results"]][[i]][["view"]][["metadata"]][["custom_fields"]][["Additional Information"]][["InCatalog"]])
    if (in_catalog != TRUE){
      finance_data[["results"]][[i]] <- NULL
    }
  }
  return(finance_data)
}
