#' create_json_resource
#'
#' Create a json according to predefined template
#'
#' @param metadata_list list: List of metatadata values
#' @param json_template list: List generated from JSON template
#'
#' @return json
#' @export
#'

create_json_resource <- function(metadata_list, json_template = mdlibtoddh::json_template_resource) {

  json_template$title <- "Visit World Bank Finances"
  json_template$field_wbddh_resource_type <- "Query Tool"
  json_template$field_wbddh_data_class <- "Public"
  json_template$field_link_api <- paste0("http://finances.worldbank.org/d/", metadata_list$field_ddh_harvest_sys_id)
  json_template$body <- NULL

  # Add required dataset elements
  json_template$type <- "resource"
  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
