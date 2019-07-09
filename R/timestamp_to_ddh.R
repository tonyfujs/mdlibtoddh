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
  date <- clean_date(time_stamp)
  date <- as.POSIXct(date, origin = origin, tz = "")
  date <- strftime(date, format = "%Y-%m-%d %H:%M:%S", tz = "")

  return(date)
}
