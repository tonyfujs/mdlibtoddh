#' get_md_public_datasets_list
#'
#' @param token character: Microdatalib API authentication token
#'
#' @return data.frame
#' @export
#'

get_md_public_datasets_list <- function(token) {
  resp <- mdlibconnect::get_public_survey_list(token = token)

  #TODO: Have to revert back, temporary change
  # md_external_refid <- purrr::map_chr(resp, "idno")
  # md_external_changed <- purrr::map_chr(resp, "changed")
  # md_external_id <- purrr::map_chr(resp, "id")

  # out <- data.frame(md_external_refid, md_external_changed, md_external_id, stringsAsFactors = FALSE)

  md_internal_refid <- purrr::map_chr(resp, "idno")
  md_internal_updated <- purrr::map_chr(resp, "changed")
  md_internal_id <- purrr::map_chr(resp, "id")

  out <- data.frame(md_internal_refid, md_internal_updated, md_internal_id, stringsAsFactors = FALSE)

  return(out)
}
