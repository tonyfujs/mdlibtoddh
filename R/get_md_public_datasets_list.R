#' get_md_public_datasets_list
#'
#' @param token character: Microdatalib API authentication token
#'
#' @return data.frame
#' @export
#'

get_md_public_datasets_list <- function(token) {
  resp <- mdlibconnect::get_public_survey_list(token = token)
  md_external_refid <- purrr::map_chr(resp, "idno")
  md_updated <- purrr::map_chr(resp, "changed")
  md_external_id <- purrr::map_chr(resp, "id")

  out <- data.frame(md_external_refid, md_updated, md_external_id, stringsAsFactors = FALSE)

  return(out)
}
