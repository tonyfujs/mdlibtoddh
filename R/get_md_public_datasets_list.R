#' get_md_public_datasets_list
#'
#' @param token character: Microdatalib API authentication token
#'
#' @return data.frame
#' @export
#'

get_md_public_datasets_list <- function(token) {
  resp <- mdlibconnect::get_public_survey_list(token = token)
  md_external_refid <- purrr::map_chr(resp, 'surveyid')

  out <- data.frame(md_external_refid, stringsAsFactors = FALSE)

  return(out)
}
