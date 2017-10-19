#' read values from the date fields which require a recursive read
#'
#' @param finance_list list: object returned by reading the json file from the API using jsonlite::fromJSON()
#' @param i numeric: index of the specific dataset
#'
#' @import jsonlite
#' @return list
#' @export
#'

recursive_date_read <- function(metadata_in) {
  start_date <- NULL
  end_date <- NULL

  column_name <- metadata_in$view$metadata$custom_fields$`Additional Information`$`Range Column`
  if(is_valid_column(column_name)){
    df_columns <- fromJSON(toJSON(metadata_in$view$columns))
    column <- subset(df_columns, df_columns$fieldName == column_name)
    start_date <- unlist(row$cachedContents$smallest)
    end_date <- unlist(row$cachedContents$largest)
    # deal with date formats that are hard to parse
    data_type_name <- unlist(row$dataTypeName)
    if(data_type_name == "text") {
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
    clean_dt <- gsub("FY", "20", dt)
  }
  if(nchar(dt) == 6){
    clean_dt <- gsub("FY", "", dt)
  }
  return(clean_dt)
}
