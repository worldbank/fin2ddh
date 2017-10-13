#' read values from the date fields which require a recursive read
#'
#' @param finance_list list: object returned by reading the json file from the API using jsonlite::fromJSON()
#' @param i numeric: index of the specific dataset
#'
#' @return list
#' @export
#'

recursive_read <- function(finance_list, i) {
  start_date <- NULL
  end_date <- NULL

  column_name <- finance_list$results$view$metadata$custom_fields$`Additional Information`$`Range Column`[[i]]
  if(is_valid_column(column_name)){
    if(column_name == "transaction_posting_date"){
      column_name <- "fund_allocation_date"
    }
    if(i == 76){
      column_name <- "fiscal_year"
    }
    idx <- match(column_name, finance_list$results$view$columns[[i]]$fieldName)
    start_date <- finance_list$results$view$columns[[i]]$cachedContents$smallest[[idx]]
    end_date <- finance_list$results$view$columns[[i]]$cachedContents$largest[[idx]]
    # deal with date formats that are hard to parse
    data_type_name <- finance_list$results$view$columns[[i]]$dataTypeName[[idx]]
    if(data_type_name == "text") {
      start_date <- clean_date(start_date)
      end_date <- clean_date(end_date)
    }
    return(list(start_date = start_date, end_date = end_date))
  }
}

is_valid_column <- function(column_name) {
  return(!is.na(column_name) && nchar(column_name)>0)
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
