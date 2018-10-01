#' create_json_attach
#'
#' Create a json according to predefined template
#'
#' @param resource_nid character: resource node id
#' @param metadata_list list: List of metatadata values
#' @param json_template list: List generated from JSON template
#'
#' @return json
#' @export
#'

create_json_attach <- function(resource_nid, metadata_list,
                               json_template = mdlibtoddh::json_template_attach) {

  temp <- paste0(metadata_list$title, "(", resource_nid, ")")
  json_template$field_resources$und$target_id <- temp

  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
