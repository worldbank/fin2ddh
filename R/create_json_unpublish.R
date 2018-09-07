#' create_json_unpublish
#'
#' Create a json according to predefined template to unpublish dataset
#'
#' @param dataset_nid character: dataset node id
#' @param json_template list: List generated from JSON template
#'
#' @return json
#' @export
#'

create_json_unpublish <- function() {

  json_template <- jsonlite::fromJSON("{}")
  json_template$workflow_status <- jsonlite::unbox("unpublished")
  json_template$status <- jsonlite::unbox("0")
  out <- jsonlite::toJSON(json_template, pretty = TRUE)

  return(out)
}
