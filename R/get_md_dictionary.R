#' get_md_dictionary
#'
#' return a list of survey variables and list of values as a single, semi-colon separated string (required format to be passed to the DDH API)
#'
#' @param id character: survey unique id (internal id)
#' @param token character: Microdata API authentication token
#'
#' @return character vector
#' @export
#'

get_md_dictionary <- function(id, token) {

  out <- mdlibconnect::get_variables_by_study(id,token)
  out_1 <- lapply(out, function(x){

      ifelse(!is.null(x[["labl"]]), x[["labl"]], "")

    })

  out_2 <- lapply(out, function(x){

    ifelse(!is.null(x[["name"]]), x[["name"]], "")

  })

  out <- c(out_1,out_2)
  out <- unique(out)
  out <- stringr::str_trim(out)
  out <- paste(out, collapse = ';')

  return(out)
}
