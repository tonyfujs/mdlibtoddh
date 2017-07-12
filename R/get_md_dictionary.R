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
  path <- paste0('index.php/api/v2/metadata/list_variables/', id, '/', limit)
  out <- mdlibconnect::connect_mdlib(path = path, token = token)
  out <- out$content$items
  out <- unlist(out)
  out <- unique(out)
  out <- stringr::trim(out)
  out <- paste(out, collapse = ';')

  return(out)
}
