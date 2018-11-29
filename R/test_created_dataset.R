#' test_created_dataset
#'
#' Test the metadata values of the dataset tranferred to DDH
#'
#' @param dataset_metadata character: The dataset node id
#' @param metadata_list list: List of metatadata values
#' @param lovs dataframe: lookup table of data catalog machine names and tids, return object of get_lovs()
#' @param root_url character: API root URL
#' @param credentials list: list with dkanr::get_cookie() and dkanr::get_token()
#'
#' @return character
#' @export
#'

test_created_dataset <- function(dataset_metadata, metadata_list,
                                 lovs = ddhconnect::get_lovs(),
                                 root_url = dkanr::get_url(),
                                 credentials = list(cookie = dkanr::get_cookie(),
                                                    token = dkanr::get_token())) {

  safe_see_if(dataset_metadata$workbench_moderation$current$published, "1", "status")
  safe_see_if(dataset_metadata$title, metadata_list$title, "title")
  safe_see_if(dataset_metadata$field_wbddh_dsttl_upi$und[[1]]$target_id, metadata_list$field_wbddh_dsttl_upi, "field_wbddh_dsttl_upi")

  machine_names_value <- c(
    "body",
    "field_ddh_harvest_sys_id",
    "field_wbddh_acronym",
    "field_wbddh_ds_source",
    "field_wbddh_modified_date",
    "field_wbddh_release_date",
    "field_ddh_external_contact_email"
  )

  sapply(machine_names_value, check_value,
         dataset_metadata = dataset_metadata,
         input_metadata = metadata_list
         )

  machine_names_tid <- c(
    "field_ddh_harvest_src",
    "field_frequency",
    "field_granularity_list",
    "field_license_wbddh",
    # "field_tags",
    "field_topic",
    "field_wbddh_country",
    "field_wbddh_economy_coverage",
    "field_wbddh_data_class",
    "field_wbddh_data_type",
    "field_wbddh_languages_supported",
    "field_wbddh_update_frequency"
  )

  sapply(machine_names_tid, check_lov,
         dataset_metadata = dataset_metadata,
         input_metadata = metadata_list,
         lovs = lovs
         )
}

# use unlist instead of indexing, $field_frequency doesn't have $tid but uses a tid structure
check_lov <- function(dataset_metadata, input_metadata, machine_name, lovs) {
  ddh_input <- unlist(c(dataset_metadata[[machine_name]]$und[[1]]$tid,
                        dataset_metadata[[machine_name]]$und[[1]]$value))
  ddh_value <- lovs$list_value_name[lovs$tid == ddh_input & lovs$machine_name == machine_name]
  safe_see_if(ddh_value, input_metadata[[machine_name]], machine_name)
}

check_value <- function(dataset_metadata, input_metadata, machine_name) {
  ddh_value <- dataset_metadata[[machine_name]]$und[[1]]$value
  safe_see_if(ddh_value, input_metadata[[machine_name]], machine_name)
}
