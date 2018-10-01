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

create_json_resource <- function(metadata_list,
                                 json_template = mdlibtoddh::json_template_resource) {

  json_template$title <- safe_unbox("Visit World Bank Finances")
  json_template$field_wbddh_resource_type$und$tid <- safe_unbox(lookup[which(lookup$field_lovs == "Query Tool" & lookup$ddh_machine_name == "field_wbddh_resource_type"),]$tid)
  json_template$field_wbddh_data_class$und$tid <- safe_unbox(lookup[which(lookup$field_lovs == "Public" & lookup$ddh_machine_name == "field_wbddh_data_class"),]$tid)
  json_template$field_link_api$und$url <- safe_unbox(paste0("http://finances.worldbank.org/d/", metadata_list$field_ddh_harvest_sys_id))
  json_template$body <- NULL

  # Add required dataset elements
  json_template$type <- jsonlite::unbox("resource")
  json_template$workflow_status <- jsonlite::unbox("published")

  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
