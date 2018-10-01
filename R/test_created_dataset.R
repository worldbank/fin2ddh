#' test_created_dataset
#'
#' Test the metadata values of the dataset tranferred to DDH
#'
#' @param dataset_nid character: The dataset node id
#' @param metadata_list list: List of metatadata values
#' @param root_url character: API root URL
#' @param credentials list: list with dkanr::get_cookie() and dkanr::get_token()
#'
#' @return character
#' @export
#'

test_created_dataset <- function(dataset_nid, metadata_list,
                                 root_url = dkanr::get_url(),
                                 credentials = list(cookie = dkanr::get_cookie(),
                                                    token = dkanr::get_token())) {

  node_metadata = ddhconnect::get_metadata(dataset_nid, root_url, credentials)

  # for later looping
  #node_metadata$[[machine_name]]$und$value
  #node_metadata$[[machine_name]]$und$tid

  safe_see_if(node_metadata$title, metadata_list$title)
  safe_see_if(node_metadata$field_wbddh_terms_of_use$und$value, metadata_list$field_wbddh_terms_of_use)
  safe_see_if(node_metadata$field_wbddh_collaborator_upi$und$value, metadata_list$field_wbddh_collaborator_upi)
  #safe_see_if(node_metadata$field_wbddh_end_date$und$value, metadata_list$field_wbddh_end_date)
  safe_see_if(node_metadata$field_ddh_external_contact_email$und$value, metadata_list$field_ddh_external_contact_email)
  safe_see_if(node_metadata$field_wbddh_search_tags$und$value, metadata_list$field_wbddh_search_tags)
  safe_see_if(node_metadata$field_wbddh_copyright$und$value, metadata_list$field_wbddh_copyright)
  safe_see_if(node_metadata$field_wbddh_time_periods0.value$und$value, metadata_list$field_wbddh_time_periods0)
  safe_see_if(node_metadata$field_ddh_harvest_sys_id$und$value, metadata_list$field_ddh_harvest_sys_id)
  safe_see_if(node_metadata$field_frequency$und$value, metadata_list$field_frequency)
  safe_see_if(node_metadata$field_wbddh_type_of_license$und$value, metadata_list$field_wbddh_type_of_license)
  safe_see_if(node_metadata$field_wbddh_update_schedule$und$value, metadata_list$field_wbddh_update_schedule)
  safe_see_if(node_metadata$field_wbddh_ds_source$und$value, metadata_list$field_wbddh_ds_source)
  safe_see_if(node_metadata$field_wbddh_citation_text$und$value, metadata_list$field_wbddh_citation_text)
  safe_see_if(node_metadata$field_wbddh_publisher_name$und$value, metadata_list$field_wbddh_publisher_name)
  safe_see_if(node_metadata$field_wbddh_time_periods0.value2$und$value, metadata_list$field_wbddh_time_periods0)
  #safe_see_if(node_metadata$field_wbddh_start_date$und$value, metadata_list$field_wbddh_start_date)
  safe_see_if(node_metadata$field_wbddh_next_expected_update$und$value, metadata_list$field_wbddh_next_expected_update)
  #safe_see_if(node_metadata$field_tags$und$tid, metadata_list$field_tags)
  safe_see_if(node_metadata$field_wbddh_data_notes$und$value, metadata_list$field_wbddh_data_notes)
  safe_see_if(node_metadata$field_wbddh_reference_system$und$value, metadata_list$field_wbddh_reference_system)
  safe_see_if(node_metadata$field_wbddh_update_frequency$und$tid, metadata_list$field_wbddh_update_frequency)
  safe_see_if(node_metadata$field_license_wbddh$und$tid, metadata_list$field_license_wbddh)
  safe_see_if(node_metadata$field_wbddh_economy_coverage$und$tid, metadata_list$field_wbddh_economy_coverage)
  safe_see_if(node_metadata$field_wbddh_languages_supported$und$tid, metadata_list$field_wbddh_languages_supported)
  safe_see_if(node_metadata$field_wbddh_country$und$tid, metadata_list$field_wbddh_country)
  #safe_see_if(node_metadata$field_frequency$und$tid, metadata_list$field_frequency)
  safe_see_if(node_metadata$field_wbddh_data_type$und$tid, metadata_list$field_wbddh_data_type)
  safe_see_if(node_metadata$field_granularity_list$und$tid, metadata_list$field_granularity_list)
  safe_see_if(node_metadata$field_ddh_harvest_src$und$tid, metadata_list$field_ddh_harvest_src)
  safe_see_if(node_metadata$field_wbddh_data_class$und$tid, metadata_list$field_wbddh_data_class)
  safe_see_if(node_metadata$field_topic$und$tid, metadata_list$field_topic)
  safe_see_if(node_metadata$field_wbddh_gps_ccsas$und$tid, metadata_list$field_wbddh_gps_ccsas)
  safe_see_if(node_metadata$field_wbddh_modified_date$und$value, metadata_list$field_wbddh_modified_date)
  safe_see_if(node_metadata$field_wbddh_release_date$und$value, metadata_list$field_wbddh_release_date)
  safe_see_if(node_metadata$field_wbddh_dsttl_upi$und$target_id, metadata_list$field_wbddh_dsttl_upi)
  safe_see_if(node_metadata$body$und$value, metadata_list$body)

  # not populating
  #safe_see_if(node_metadata$field_wbddh_source$und$tid, metadata_list$field_wbddh_source)
}
