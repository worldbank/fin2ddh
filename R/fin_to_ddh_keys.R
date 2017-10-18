#' fin_to_ddh_keys
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param metadata_in list: The output of mdlibconnect::get_survey_metadata
#' @param metadata_out list: Package object: mdlibtoddh::md_placeholder
#' @param lookup data.frame: Package object: mdlibtoddh::lookup
#'
#' @importFrom hash hash
#' @importFrom magrittr "%>%"
#' @return list
#'
#' @export
#'

fin_to_ddh_keys <- function(metadata_in,
                                metadata_out = mdlibtoddh::md_placeholder,
                                lookup = fin2ddh::lookup) {
  machine_names <- sort(names(metadata_out))
  fin_keys_lookup <- lookup %>% dplyr::filter(!is.na(finance_json_key))
  lookup_unique <- dplyr::distinct(fin_keys_lookup, ddh_machine_name, finance_json_key)
  fin_machine_names <- lookup_unique$ddh_machine_name
  h <- hash::hash(keys = lookup_unique$ddh_machine_name, values = lookup_unique$finance_json_key)

  for(machine_name in fin_machine_names) {
    fin_name_raw <- h[[machine_name]]
    metadata_out[[machine_name]] <- format_lookup(metadata_in, fin_name_raw)
  }

  for(machine_name in ddhconnect:::mandatory_text_fields) {
    if(is.null(metadata_out[[machine_name]])){
      metadata_out[[machine_name]] <- "Not specified"
    }
  }

  return(metadata_out)
}

format_lookup <- function(metadata_in, lookup_name) {
  keys <- unlist(strsplit(lookup_name, "/"))
  values <- purrr::at_depth(metadata_in, .depth = 0, .f = keys)
  return(values$view)
}
