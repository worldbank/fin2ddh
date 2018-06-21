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
  ddh_list <- mdlibtoddh::get_ddh_datasets_list(root_url = root_url, credentials = credentials)
  ddh_list$ddh_updated <- as.numeric(lubridate::ymd_hms(ddh_list$ddh_updated))
  ddh_list$ddh <- 'ddh'

  # mdlib
  fin_list <- get_fin_datasets_list()

  # Combine datasets
  full_list <- dplyr::full_join(ddh_list, fin_list, by = 'md_internal_id')
  full_list$status <- NA
  full_list$status[is.na(full_list$ddh_nids)] <- 'new'
  full_list$status[!is.na(full_list$ddh_nids) & !is.na(full_list$md_internal_id)] <- 'current'
  full_list$status[!is.na(full_list$ddh_nids) & is.na(full_list$md_internal_id)] <- 'old'

  # Identify Current / New / Old datasets based on timestamps
  full_list$time_diff <- abs(full_list$fin_internal_updated - full_list$ddh_updated) - 14400
  full_list$sync_status <- NA
  full_list$sync_status[full_list$status == 'current' & full_list$time_diff <= 3600] <- 'in sync'
  full_list$sync_status[full_list$status == 'current' & full_list$time_diff > 3600] <- 'out of sync'
  full_list$time_diff <- NULL

  # Identify change of versions
  full_list$sync_status[full_list$md_refids != full_list$md_internal_refid] <- 'out of sync'

  return(full_list)
}
