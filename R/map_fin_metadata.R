#' map_fin_metadata
#'
#' Extract specific metadata from the Finance API JSON response
#'
#' @param metadata_list list: output of add_constant_metadata()
#'
#' @importFrom purrr flatten
#' @importFrom magrittr "%>%"
#' @return list
#' @export
#'

map_fin_metadata <- function(metadata_list) {

    survey_fields <- names(metadata_list)
    lkup_values <- finance_lovs
    lkup_tids <- ddh_lovs

    # Map values to DDH controlled vocabulary ---------------------------------
    controlled_variables <- survey_fields[survey_fields %in% names(lkup_values)]
    metadata_list[controlled_variables] <- purrr::map(controlled_variables, function(x) {
      map_valid_lovs(metadata_list[[x]], lkup_values[[x]])
    })

    #current mapping not matching for expected fields and expected list value fields
    # removed periodicity and license
    default_fields <- c("field_wbddh_country", "field_wbddh_economy_coverage", "field_license_wbddh", "field_frequency")
    default_values <- c("Region/Country not specified", "Economy Coverage not specified", "Creative Commons Attribution 4.0", "Periodicity not specified")
    default_value_lookup <- hash::hash(keys = default_fields, values = default_values)
    for(machine_name in default_fields) {
      if(is.null(metadata_list[[machine_name]])) {
        metadata_list[[machine_name]] <- default_value_lookup[[machine_name]]
      }
    }

    metadata_list[["body"]] <- gsub("[\n]", "", metadata_list[["body"]])

    survey_fields <- names(metadata_list)

    # Map values to DDH controlled tids
    # controlled_variables <- survey_fields[survey_fields %in% names(lkup_tids)]
    # controlled_variables <- names(Filter(Negate(is.null), metadata_list[controlled_variables]))
    # metadata_list[controlled_variables] <- purrr::map(controlled_variables, function(x) {
    #
    #   map_valid_lovs(metadata_list[[x]], lkup_tids[[x]])
    # })

    to_keep <- names(metadata_list[!purrr::map_int(metadata_list, is_blank)])
    metadata_list <- metadata_list[names(metadata_list) %in% to_keep]

    return(metadata_list)
  }
