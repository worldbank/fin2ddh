#' create_json_dataset
#'
#' Create a json according to predefined template
#'
#' @param metadata_list list: List of metatadata values
#' @param json_template list: List generated from JSON template
#'
#' @return json
#' @export
#'

create_json_dataset <- function(metadata_list, json_template = fin2ddh::ddh_schema_finance_dataset) {

  json_template <- jsonlite::fromJSON("./data-raw/ddh_schema_finance_dataset.json")
  for (field_name in names(json_template)) {
    print(field_name)
    if (is.character(json_template[[field_name]])) {
      json_template[[field_name]] <- safe_unbox(safe_assign(metadata_list[[field_name]]))
    }
    else if(is.null(names(json_template[[field_name]]$und))) {
      json_template[[field_name]]$und <- unlist(stringr::str_split(metadata_list[[field_name]], pattern = ';'))
    }
    else {
      subfield_name <- names(json_template[[field_name]]$und)
      json_template[[field_name]]$und[[subfield_name]] <- safe_unbox(safe_assign(metadata_list[[field_name]]))
    }
  }

  # remove empty elements
  to_keep <- names(metadata_list[!purrr::map_int(metadata_list, length) == 0])
  json_template <- json_template[names(json_template) %in% to_keep]

  # Add required dataset elements
  json_template$type <- jsonlite::unbox("dataset")

  return(jsonlite::toJSON(json_template, pretty = TRUE))
}
