#' timestamp_to_ddhdate
#'
#' @param time_stamp numeric: A linux time stamp
#' @param origin character the date of origin
#'
#' @return character vector
#' @export
#'
#' @examples
#' timestamp_to_ddhdate(time_stamp = 1497978729, origin = "1970-01-01")

timestamp_to_ddhdate <- function(time_stamp, origin = "1970-01-01") {
  
  # CHECK inputs
  if (!is.numeric(time_stamp)) {time_stamp <- as.numeric(time_stamp)}
  assertthat::assert_that(!is.na(time_stamp), msg = "Please ensure that time_stamp has a valid value")
  
  date <- as.POSIXct(time_stamp, origin = origin, tz = "")
  date <- strftime(date, format = "%Y-%m-%d %H:%M:%S", tz = "")
  
  return(date)
}
