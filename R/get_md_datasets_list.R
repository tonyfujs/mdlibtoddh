#' get_md_datasets_list
#'
#' @param token character: Microdatalib API authentication token
#'
#' @return data.frame
#' @export
#'

get_md_datasets_list <- function(token) {
  resp <- mdlibconnect::get_survey_list(token = token)
  md_internal_id <- as.numeric(purrr::map_chr(resp, 'id'))
  md_internal_refid <- purrr::map_chr(resp, 'surveyid')
  md_internal_update <- as.numeric(purrr::map_chr(resp, 'changed'))

  out <- data.frame(md_internal_id, md_internal_refid, md_internal_update, stringsAsFactors = FALSE)

  return(out)
}
