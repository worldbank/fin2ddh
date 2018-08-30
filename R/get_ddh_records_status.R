#' get_ddh_records_status
#'
#' Compare DDH and finance portal records
#'
#' @param root_url character: API root URL
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return data frame
#' @export
#'

get_ddh_records_status <- function(root_url = dkanr::get_url(),
                                   credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())) {

  # ddh
  # subset the ddh catalog for the finance datasets
  ddh_list <- get_finance_datasets(root_url = root_url, credentials = credentials)
  ddh_list$ddh_updated <- as.numeric(lubridate::ymd_hms(ddh_list$ddh_updated))

  # finance harvest
  fin_list <- get_fin_datasets_list()

  # Combine datasets
  full_list <- dplyr::full_join(ddh_list, fin_list, by = "fin_internal_id")
  full_list$status <- NA
  full_list$status[is.na(full_list$ddh_nids)] <- "new"
  full_list$status[!is.na(full_list$ddh_nids) & !is.na(full_list$fin_internal_id)] <- "current"
  full_list$status[!is.na(full_list$ddh_nids) & is.na(full_list$fin_internal_updated)] <- "old"

  # Identify Current / New / Old datasets based on timestamps
  full_list$time_diff <- abs(as.numeric(full_list$fin_internal_updated) - as.numeric(full_list$ddh_updated)) - 14400
  full_list$sync_status <- NA
  full_list$sync_status[full_list$status == "current" & full_list$time_diff <= 3600] <- "in sync"
  full_list$sync_status[full_list$status == "current" & full_list$time_diff > 3600] <- "out of sync"
  full_list$time_diff <- NULL

  # Identify change of versions
  full_list$sync_status[full_list$md_refids != full_list$md_internal_refid] <- "out of sync"
  return(full_list)
}



get_finance_datasets <- function(root_url = dkanr::get_url(),
                                 credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())) {
  finance_datasets <- ddhconnect::search_catalog(
    fields = c(
      "nid",
      "field_ddh_harvest_src",
      "field_ddh_harvest_sys_id",
      "field_wbddh_modified_date"
    ),
    filters = c(
      "field_ddh_harvest_src" = "1015",
      "type" = "dataset"
    ),
    credentials = credentials,
    root_url = root_url
  )

  ddh_nids <- as.character(purrr::map(finance_datasets, "nid"))
  ddh_updated <- as.character(purrr::map(finance_datasets, function(x) x[["field_wbddh_modified_date"]][["und"]][[1]][["value"]]))
  fin_internal_id <- as.character(purrr::map(finance_datasets, function(x) x[["field_ddh_harvest_sys_id"]][["und"]][[1]][["value"]]))

  out <- data.frame(ddh_nids, ddh_updated, fin_internal_id, stringsAsFactors = FALSE)
  return(out)
}
