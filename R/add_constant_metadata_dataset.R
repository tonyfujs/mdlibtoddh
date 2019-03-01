#' add_constant_metadata_dataset
#'
#' Add metadata that have constant values across records (not pulled from Microdata API)
#' Currently adds values for the following fields:
#' field_wbddh_languages_supported ('English')
#' field_wbddh_data_type ('Microdata')
#' field_contact_email ('microdata@worldbank.org')
#' field_ddh_harvest_src ("Microdata")
#'
#' @param metadata_list list: List returned by flatten_md_metadata()
#' @return list
#' @export
#'

add_constant_metadata_dataset <- function(metadata_list) {

  metadata_list$field_wbddh_languages_supported <- 'English'
  metadata_list$field_wbddh_data_type <- 'Microdata'
  # metadata_list$field_contact_email <- 'microdata@worldbank.org'
  metadata_list$field_ddh_harvest_src <- "Microdata"
  metadata_list$field_topic <- 'Topic not specified'
  metadata_list$field_wbddh_responsible <- "No"
  metadata_list$field_wbddh_economy_coverage <- 'Economy Coverage not specified'
  return(metadata_list)
}
