#' test_created_dataset
#'
#' Test the metadata values of the dataset tranferred to DDH
#'
#' @param nid character: The dataset node id
#' @param metadata_list list: List of metatadata values
#' @param credentials list: object returned by the get_credentials() function
#' @param root_url character: API root URL
#'
#' @return character
#' @export
#'

test_created_dataset <- function(nid, metadata_list, credentials, root_url = production_root_url) {

  node_metadata = ddhconnect::get_metadata(nid, credentials, root_url)
  
  # for later looping
  #node_metadata$[[machine_name]]$und[[1]]$value
  #node_metadata$[[machine_name]]$und[[1]]$tid
  
  safe_assert(node_metadata$title, metadata_list$title)
  safe_assert(node_metadata$field_wbddh_terms_of_use$und[[1]]$value, metadata_list$field_wbddh_terms_of_use)
  safe_assert(node_metadata$field_wbddh_collaborator_upi$und[[1]]$value, metadata_list$field_wbddh_collaborator_upi)
  #safe_assert(node_metadata$field_wbddh_end_date$und[[1]]$value, metadata_list$field_wbddh_end_date)
  safe_assert(node_metadata$field_ddh_external_contact_email$und[[1]]$value, metadata_list$field_ddh_external_contact_email)
  safe_assert(node_metadata$field_wbddh_search_tags$und[[1]]$value, metadata_list$field_wbddh_search_tags)
  safe_assert(node_metadata$field_wbddh_copyright$und[[1]]$value, metadata_list$field_wbddh_copyright)
  safe_assert(node_metadata$field_wbddh_time_periods0.value$und[[1]]$value, metadata_list$field_wbddh_time_periods0)
  safe_assert(node_metadata$field_ddh_harvest_sys_id$und[[1]]$value, metadata_list$field_ddh_harvest_sys_id)
  safe_assert(node_metadata$field_frequency$und[[1]]$value, metadata_list$field_frequency)
  safe_assert(node_metadata$field_wbddh_type_of_license$und[[1]]$value, metadata_list$field_wbddh_type_of_license)
  safe_assert(node_metadata$field_wbddh_update_schedule$und[[1]]$value, metadata_list$field_wbddh_update_schedule)
  safe_assert(node_metadata$field_wbddh_ds_source$und[[1]]$value, metadata_list$field_wbddh_ds_source)
  safe_assert(node_metadata$field_wbddh_citation_text$und[[1]]$value, metadata_list$field_wbddh_citation_text)
  safe_assert(node_metadata$field_wbddh_publisher_name$und[[1]]$value, metadata_list$field_wbddh_publisher_name)
  safe_assert(node_metadata$field_wbddh_time_periods0.value2$und[[1]]$value, metadata_list$field_wbddh_time_periods0)
  #safe_assert(node_metadata$field_wbddh_start_date$und[[1]]$value, metadata_list$field_wbddh_start_date)
  safe_assert(node_metadata$field_wbddh_next_expected_update$und[[1]]$value, metadata_list$field_wbddh_next_expected_update)
  safe_assert(node_metadata$field_tags$und[[1]]$value, metadata_list$field_tags)
  safe_assert(node_metadata$field_wbddh_data_notes$und[[1]]$value, metadata_list$field_wbddh_data_notes)
  safe_assert(node_metadata$field_wbddh_reference_system$und[[1]]$value, metadata_list$field_wbddh_reference_system)
  safe_assert(node_metadata$field_wbddh_update_frequency$und[[1]]$tid, metadata_list$field_wbddh_update_frequency)
  safe_assert(node_metadata$field_license_wbddh$und[[1]]$tid, metadata_list$field_license_wbddh)
  safe_assert(node_metadata$field_wbddh_economy_coverage$und[[1]]$tid, metadata_list$field_wbddh_economy_coverage)
  safe_assert(node_metadata$field_wbddh_languages_supported$und[[1]]$tid, metadata_list$field_wbddh_languages_supported)
  safe_assert(node_metadata$field_wbddh_country$und[[1]]$tid, metadata_list$field_wbddh_country)
  safe_assert(node_metadata$field_frequency$und[[1]]$tid, metadata_list$field_frequency)
  safe_assert(node_metadata$field_wbddh_data_type$und[[1]]$tid, metadata_list$field_wbddh_data_type)
  safe_assert(node_metadata$field_granularity_list$und[[1]]$tid, metadata_list$field_granularity_list)
  safe_assert(node_metadata$field_ddh_harvest_src$und[[1]]$tid, metadata_list$field_ddh_harvest_src)
  safe_assert(node_metadata$field_wbddh_data_class$und[[1]]$tid, metadata_list$field_wbddh_data_class)
  safe_assert(node_metadata$field_topic$und[[1]]$tid, metadata_list$field_topic)
  safe_assert(node_metadata$field_wbddh_gps_ccsas$und[[1]]$tid, metadata_list$field_wbddh_gps_ccsas)
  safe_assert(node_metadata$field_wbddh_modified_date$und[[1]]$value, metadata_list$field_wbddh_modified_date)
  safe_assert(node_metadata$field_wbddh_release_date$und[[1]]$value, metadata_list$field_wbddh_release_date)
  safe_assert(node_metadata$field_wbddh_dsttl_upi$und[[1]]$target_id, metadata_list$field_wbddh_dsttl_upi)
  safe_assert(node_metadata$body$und[[1]]$value, metadata_list$body)

  # not populating
  #safe_assert(node_metadata$field_wbddh_source$und[[1]]$tid, metadata_list$field_wbddh_source)
}
