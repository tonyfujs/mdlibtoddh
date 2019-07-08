#' get_md_datasets_list
#'
#' @param token character: Microdatalib API authentication token
#'
#' @return data.frame
#' @export
#'

get_md_datasets_list <- function(token) {
  resp <- mdlibconnect::get_survey_list(token = token)
  md_internal_id <- purrr::map_chr(resp, 'id')
  md_internal_refid <- purrr::map_chr(resp, 'surveyid')
  md_updated <- as.numeric(purrr::map_chr(resp, 'changed'))

  # Clean date format
  md_updated <- purrr::map_chr(md_updated, clean_date)

  out <- data.frame(md_internal_id, md_internal_refid, md_updated, stringsAsFactors = FALSE)

  return(out)
}
