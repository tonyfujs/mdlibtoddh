#' timestamp_to_ddhdate
#'
#' @param time_stamp numeric: A linux time stamp
#' @param origin character the date of origin
#'
#' @return named vector
#' @export
#'
#' @examples
#' timestamp_to_ddhdate(time_stamp = 1497978729, origin = "1970-01-01")

timestamp_to_ddhdate <- function(time_stamp, origin = "1970-01-01") {

  # CHECK inputs
  assertthat::is.number(time_stamp)

  out <- vector(mode = 'character', length = 6)
  names(out) <- c('year', 'month', 'day', 'hour', 'minute', 'ampm')

  date <- as.POSIXct(time_stamp, origin = origin)
  out['year'] <- lubridate::year(date)
  out['month'] <- lubridate::month(date)
  out['day'] <- lubridate::day(date)
  out['hour'] <- lubridate::hour(date)
  out['minute'] <- lubridate::minute(date)
  out['ampm'] <- if (lubridate::am(date)) {'am'} else {'pm'}

  return(out)
}
