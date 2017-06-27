#' add_link_to_resources
#'
#' @param metadata_list list: Flattened list of metadata from the Microdata API
#' @param refid character: Microdata reference ID (common to Internal and External Microdata records)
#' @param master data.frame: Output of mdlibtoddh::get_ddh_records_status()
#'
#' @return list
#' @export
#'

add_link_to_resources <- function(metadata_list, refid, master) {

  md_internal_id <- master[['md_internal_id']][master[['md_internal_refid']] == refid]
  assertthat::assert_that(length(md_internal_id) == 1)
  master <- master[master$md_internal_refid == refid, 'data_classification']
  assertthat::assert_that(length(master) == 1)

  # CHECK that input is correct
  assertthat::validate_that(master %in% c('public', 'official'))
  # Add correct data classification
  if (master == 'public') {
    url <- paste0('http://microdata.worldbank.org/index.php/catalog/study/', refid)
    metadata_list$field_link_api <- url
    return(metadata_list)
  } else if (master == 'official') {
    url <- paste0('http://microdatalib.worldbank.org/index.php/catalog/', md_internal_id)
    metadata_list$field_link_api <- url
    return(metadata_list)
  }

}
