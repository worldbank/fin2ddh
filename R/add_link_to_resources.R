#' add_link_to_resources
#'
#' @param metadata_list list: Flattened list of metadata from the Microdata API
#' @param md_internal_id character: Microdata internal ID
#' @param master data.frame: Output of mdlibtoddh::get_ddh_records_status()
#'
#' @return list
#' @export
#'

add_link_to_resources <- function(metadata_list, category) {
    category <- gsub(" ", "-", category)
    title <- gsub(" ", "-", metadata_list$title)
    id <- metadata_list$field_ddh_harvest_sys_id
    url <- paste('https://finances.worldbank.org', category, title, id, sep='/')
    metadata_list$field_link_api <- url
    return(metadata_list)
}
