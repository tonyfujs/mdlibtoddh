#' add_constant_metadata
#'
#' Add metadata that have constant values across records (not pulled from Microdata API)
#' Currently adds values for the following fields:
#' field_wbddh_languages_supported ('English')
#' field_wbddh_data_type ('Microdata')
#' field_contact_email ('microdata@worldbank.org')
#'
#' @return list
#' @export
#'

add_constant_metadata <- function(metadata_list) {

  metadata_list$field_wbddh_languages_supported <- 'English'
  metadata_list$field_wbddh_data_type <- 'Microdata'
  metadata_list$field_contact_email <- 'microdata@worldbank.org'

  return(metadata_list)
}
