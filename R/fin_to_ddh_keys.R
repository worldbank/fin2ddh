#' fin_to_ddh_keys
#' Extract specific metadata from the Finance API JSON response
#'
#' @param metadata_in list: The output of get_fin_datasets_metadata()
#' @param metadata_out list: Package object: fin_placeholder
#' @param lookup data.frame: Package object: fin2ddh::lookup
#'
#' @importFrom hash hash
#' @importFrom magrittr "%>%"
#' @import purrr
#' @import dplyr
#' @importFrom rlang .data
#' @return list
#'
#' @export
#'

fin_to_ddh_keys <- function(metadata_in,
                            metadata_out = fin2ddh::fin_placeholder,
                            lookup = fin2ddh::lookup) {
  fin_keys_lookup <- dplyr::filter(lookup,!is.na(.data$finance_json_key))
  lookup_unique <- dplyr::distinct(fin_keys_lookup, .data$ddh_machine_name, .data$finance_json_key)
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

  date_list <- recursive_date_read(metadata_in)
  metadata_out[["field_wbddh_start_date"]] <- date_list$start_date
  metadata_out[["field_wbddh_end_date"]] <- date_list$end_date
  metadata_out[["field_wbddh_modified_date"]] <- date_list$modified_date
  metadata_out[["field_wbddh_release_date"]] <- date_list$release_date

  return(metadata_out)
}

format_lookup <- function(metadata_in, lookup_name) {
  keys <- unlist(strsplit(lookup_name, "/"))
  values <- purrr::modify_depth(metadata_in, .depth = 0, .f = keys)
  return(values$view)
}
