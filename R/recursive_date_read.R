#' read values from the date fields which require a recursive read
#'
#' @param metadata_in list: a dataset extracted from the finances data portal
#'
#' @import jsonlite
#' @importFrom gtools invalid
#' @return list
#' @export
#'

recursive_date_read <- function(metadata_in) {
  start_date <- NULL
  end_date <- NULL

  column_name <- metadata_in$view$metadata$custom_fields$`Additional Information`$`Range Column`
  if(is_valid_column(column_name)){
    df_columns <- jsonlite::fromJSON(toJSON(metadata_in$view$columns))
    row <- subset(df_columns, df_columns$fieldName == column_name)
    start_date <- unlist(row$cachedContents$smallest)
    end_date <- unlist(row$cachedContents$largest)
    # deal with date formats that are hard to parse
    data_type_name <- unlist(row$dataTypeName)
    # TODO: correct the double check
    if(!gtools::invalid(data_type_name)) {
      start_date <- clean_date(start_date)
      end_date <- clean_date(end_date)
    }
    return(list(start_date = start_date, end_date = end_date))
  }
}

is_valid_column <- function(column_name) {
  return(!is.null(column_name) && !is.na(column_name) && nchar(column_name)>0)
}

clean_date <- function(dt) {
  if(nchar(dt) == 4){
    dt <- gsub("FY", "20", dt)
  }
  if(nchar(dt) == 6){
    dt <- gsub("FY", "", dt)
  }
  if(is_year(dt)) {
    dt <- paste(dt, "01", "01", sep="-")
  }
  else{
    dt <- gsub("T", " ", dt)
  }
  return(dt)
}

is_year <- function(dt) {
  return(!is.na(as.numeric(dt)) && as.numeric(dt) >= 1900 && as.numeric(dt) <= 2100)
}
