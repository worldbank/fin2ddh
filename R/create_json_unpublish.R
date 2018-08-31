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

create_json_unpublish <- function(dataset_nid, json_template = mdlibtoddh::json_template_attach) {

  metadata_list <- list()
  metadata_list$workflow_status <- "unpublished"
  metadata_list$nid <- dataset_nid

  out <- create_json_dataset(metadata_list)

  return(out)
}
