#' get_md_metadata
#'
#' Fetch survey metadata from Microdata internal API
#'
#' @param id character: Microdata internal survey id
#' @param token character: Microdata API authentication token
#'
#' @return list
#' @export
#'

get_md_metadata <- function(id, token) {

  out_1 <- mdlibconnect::get_survey_metadata(id = id, token = token)
  out_2 <- mdlibconnect::get_survey(id = id, token = token)
  out <- c(out_1, out_2)

  return(out)
}
