#' add_link_to_resources
#'
#' @param metadata_list list: Flattened list of metadata from the Microdata API
#' @param md_internal_id character: Microdata internal ID
#' @param master data.frame: Output of mdlibtoddh::get_ddh_records_status()
#'
#' @return list
#' @export
#'

add_link_to_resources <- function(metadata_list, md_internal_id, master) {

  data_class <- master[master$md_internal_id == md_internal_id, 'data_classification']
  # For STG
  # data_class <- master[master$md_internal_id == md_internal_id,][1,'data_classification']
  assertthat::assert_that(length(data_class) == 1)

  # CHECK that input is correct
  assertthat::validate_that(data_class %in% c('public', 'official'))

  # Add correct data classification
  if (data_class == 'public') {
    md_external_id <- master$md_external_id[master$md_internal_id == md_internal_id]
    url <- paste0('https://microdata.worldbank.org/index.php/catalog/', md_external_id)

    metadata_list$field_link_api <- url
    return(metadata_list)

  } else if (data_class == 'official') {
    url <- paste0('https://microdatalib.worldbank.org/index.php/catalog/', md_internal_id)

    metadata_list$field_link_api <- url
    return(metadata_list)

  }
}
