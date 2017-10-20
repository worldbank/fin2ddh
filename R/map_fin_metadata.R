#' map_fin_metadata.R
#'
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param metadata list:
#'
#' @import flatten
#' @importFrom magrittr "%>%"
#' @return list
#' @export
#'

map_fin_metadata <- function(metadata_list, lkup_tids) {
# iterate one at a time
    survey_fields <- names(metadata_list)

    lkup_values <- unique(finance_lovs$machine_name) %>%
                purrr::map(function(x){
                  fin_vals <- finance_lovs[finance_lovs$machine_name==x,]$finance_value
                  return(fin_vals)
                })
    names(lkup_values) <- unique(finance_lovs$machine_name)
    # Map values to DDH controlled vocabulary ---------------------------------
    controlled_variables <- survey_fields[survey_fields %in% finance_lovs$machine_name]
    metadata_list[controlled_variables] <- purrr::map(controlled_variables, function(x) {
      map_valid_lovs(metadata_list[[x]], lkup_values[[x]])
    })

    # Map values to DDH controlled tids
    controlled_variables <- survey_fields[survey_fields %in% names(lkup_tids)]
    metadata_list[controlled_variables] <- purrr::map(controlled_variables, function(x) {
      map_valid_lovs(metadata_list[[x]], lkup_tids[[x]])
    })


    metadata_list %>%
      map_if(if.NULL(type)) %>%
      map_if()


      if (metadata_list == NULL){
      }

    return(metadata_list)
  }

}
