#' fin_to_ddh_keys
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param metadata_in list: The output of mdlibconnect::get_survey_metadata
#' @param metadata_out list: Package object: mdlibtoddh::md_placeholder
#' @param lookup data.frame: Package object: mdlibtoddh::lookup
#'
#' @importFrom magrittr "%>%"
#' @return list
#'
#' @export
#'

fin_to_ddh_keys <- function(metadata_in,
                                metadata_out = mdlibtoddh::md_placeholder,
                                lookup = fin2ddh::lookup) {
  machine_names <- sort(names(metadata_out))
  fin_keys_lookup <- lookup %>% dplyr::select(machine_name, finance_json_key, list_value_name, finance_value, tid) %>% dplyr::filter(!is.na(finance_json_key))
  lookup_unique <- dplyr::distinct(fin_keys_lookup, machine_name, finance_json_key)
  fin_machine_names <- lookup_unique$machine_name
  h <- hash::hash(keys = lookup_unique$machine_name, values = lookup_unique$finance_json_key)

  for(machine_name in fin_machine_names) {
    fin_name <- h[[machine_name]]
    metadata_out[[machine_name]] <- metadata_in[[fin_name]]
  }

  for(machine_name in ddhconnect:::mandatory_text_fields) {
    if(is.null(metadata_out[[machine_name]])){
      metadata_out[[machine_name]] <- "Not specified"
    }
  }

  return(metadata_out)
}
