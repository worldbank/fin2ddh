#' format_metadata
#'
#' Format metadata fields to the format accepted by ddh
#'
#' @param metadata_in list: flattened list of metadata
#' @param date_fields character: variables to be processed as dates
#' @param text_fields character: variables to be processed as text
#'
#' @return list
#' @export
#'

format_metadata <- function(metadata_in,
                            timestamp_fields = c("field_wbddh_modified_date",
                                            "field_wbddh_release_date"),
                            date_fields = c("field_wbddh_start_date",
                                            "field_wbddh")) {
  # Format date fields
  metadata_in[timestamp_fields] <- purrr::map(metadata_in[timestamp_fields], timestamp_to_ddhdate)
  metadata_in[date_fields] <- purrr::map(metadata_in[date_fields], format_date)
  return(metadata_in)
}

format_date <- function(date_string) {
  if(is.null(date_string)){
    return(date_string)
  }
  if(is_year(date_string)){
    date_string <- paste(date_string, "01", "01", sep="-")
  }
  else{
    date_string <- gsub("T", " ", date_string)
  }
  return(date_string)
}

is_year <- function(date_string) {
  return(!is.null(date_string) && !is.na(as.numeric(date_string)) && as.numeric(date_string) > 1900 && as.numeric(date_string) < 2100)
}
