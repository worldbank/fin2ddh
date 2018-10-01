#' add_constant_metadata
#'
#' Add metadata that have constant values across records (not pulled from the Finance API)
#'
#' @param metadata_list list: List returned by fin_to_ddh_keys(), used in add_new_datasets()
#' @return list
#' @export
#'

add_constant_metadata <- function(metadata_list) {
#  metadata_list$field_ddh_harvest_src <- 'finances.worldbank.org'
  metadata_list$field_ddh_harvest_src <- 'Finances'
  metadata_list$field_wbddh_data_class <- 'Public'
  metadata_list$field_wbddh_dsttl_upi <- '21482'
  metadata_list$field_topic <- 'Topic not specified'
  metadata_list$field_wbddh_languages_supported <- 'English'
  metadata_list$field_ddh_external_contact_email <- 'wbfinances@worldbank.org'
  metadata_list$field_wbddh_source <- 'World Bank Group'
  metadata_list$field_wbddh_data_type <- 'Other'
  metadata_list$field_tags <- 'finances.worldbank.org'
  metadata_list$field_license_wbddh <- 'Creative Commons Attribution 4.0'
  return(metadata_list)
}
