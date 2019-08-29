#' get_md_dictionary
#'
#' return a list of survey variables and list of values as a single, semi-colon separated string (required format to be passed to the DDH API)
#'
#' @param id character: survey unique id (internal id)
#' @param token character: Microdata API authentication token
#' @param limit numeric: Maximum number of variables to be returned
#'
#' @return character vector
#' @export
#'

get_md_dictionary <- function(id, token, limit = 10000) {

  out <- mdlibconnect::get_variables_by_study(id,token)
  out <- purrr::map_chr(out, "labl")
  out <- unique(out)
  out <- stringr::str_trim(out)
  out <- paste(out, collapse = ';')

  return(out)
}
