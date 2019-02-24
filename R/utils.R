map_valid_lovs <- function(values, lkup_vector) {
  names(lkup_vector) <- tolower(names(lkup_vector))
  # Deal with multiple values
  values <- stringr::str_split(values, pattern = ';', simplify = FALSE)
  values <- unlist(values)
  values <- values[values != ""]
  values <- stringr::str_trim(values, side = 'both')
  out <- purrr::map_chr(values, function(x) unname(lkup_vector[tolower(x)]))

  out[is.na(out)] <- values[is.na(out)] # replace non-mapped values, by original values
  out <- unique(stringr::str_trim(out))

  purrr::map(out, function(x) assertthat::assert_that(are_valid_lovs(x, lkup_vector),
                                                      msg = paste0("No valid mapping found in lkup_vector for: ", x)))

  out <- paste(out, collapse = ';')

  return(out)
}

are_valid_lovs <- function(values, accepted_values) {
  all(values %in% accepted_values)
}

safe_unbox <- purrr::possibly(jsonlite::unbox, otherwise = '')
safe_assign <- function(x) {if (length(x) > 0) {x} else {""}}

safe_assert <- function(file_value, orig_value) {
  assertthat::assert_that((is_blank(file_value) && is_blank(orig_value)) || (file_value == gsub("[\n]", "", orig_value)))
}

safe_see_if <- function(file_value, orig_value, field_name) {
  assert_result <- assertthat::see_if(is.same(file_value, orig_value, field_name))
  if(!assert_result){
    warning(paste0(field_name, ": The updated value is not equal to the passed value."))
  }
}

is.same <- function(file_value, orig_value, field_name) {
  is.empty(file_value) && is.empty(orig_value) ||
    is.character(file_value) && is.character(orig_value) && (gsub("[\n]", "", file_value) == gsub("[\n]", "", orig_value))

}

is.empty <- function(s) {
  is.null(s) || s == ""
}

is_blank <- function(input){
  return(gtools::invalid(input) || all(input == ""))
}

filter_dataset_fields <- function(metadata_temp,
                                  ddh_fields = ddhconnect::get_fields()) {
  dataset_fields <- ddh_fields$machine_name[ddh_fields$node_type == "dataset"]
  dataset_fields <- unique(dataset_fields)
  metadata_temp <- metadata_temp[names(metadata_temp) %in% dataset_fields]
  return(metadata_temp)
}

filter_resource_fields <- function(metadata_temp,
                                   ddh_fields = ddhconnect::get_fields()) {
  resource_fields <- ddh_fields$machine_name[ddh_fields$node_type == "resource"]
  resource_fields <- unique(resource_fields)
  metadata_temp <- metadata_temp[names(metadata_temp) %in% resource_fields]
  return(metadata_temp)
}


resource_check <- function(nid_list){
  nids <- unique(nid_list)
  for(i in 1:length(nid_list)){
    nid <- nids[[i]]
    resource_meta <- get_metadata(nid)
    if((resource_meta$field_wbddh_resource_type$und[[1]]$tid == 631) & (resource_meta$title == "Visit World Bank Finances")){
      return(nid)
    }
  }
}
