#' add_link_to_resources
#'
#' @param metadata_list list: Flattened list of metadata from the Fianace API
#' @param category string: finance category retrieved from metadata$view$category
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
