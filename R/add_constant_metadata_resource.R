#' add_constant_metadata_resource
#'
#' Add metadata that have constant values across records for resources
#'
#' @param metadata_list list: List for machine names and their corresponding values
#'
#' @return list
#' @export
#'

add_constant_metadata_resource <- function(metadata_list) {

  metadata_list$title <- "Visit World Bank Finances"
  metadata_list$field_wbddh_resource_type <- "Query Tool"
  metadata_list$field_wbddh_data_class <- "Public"
  metadata_list$field_link_api <- paste0("http://finances.worldbank.org/d/", metadata_list$field_ddh_harvest_sys_id)
  metadata_list$body <- NULL
  metadata_list$type <- "resource"
  metadata_list$workflow_status <- "published"

  return(metadata_list)
}
