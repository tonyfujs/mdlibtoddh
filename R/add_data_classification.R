#' add_data_classification
#'
#' @param metadata_list list: Flattened list of metadata from the Microdata API
#' @param refid character: Microdata reference ID (common to Internal and External Microdata records)
#' @param master data.frame: Output of mdlibtoddh::get_ddh_records_status()
#'
#' @return list
#' @export
#'

add_data_classification <- function(metadata_list, refid, master) {

  master <- master[master$md_internal_refid == refid, 'data_classification']
  assertthat::assert_that(length(master) == 1)

  # CHECK that input is correct
  assertthat::validate_that(master %in% c('public', 'official'))
  # Add correct data classification
  if (master == 'public') {
    metadata_list$field_wbddh_data_class <- '358'
    metadata_list$field_exception_s_ <- ''
    return(metadata_list)
  } else if (master == 'official') {
    metadata_list$field_wbddh_data_class <- '359'
    metadata_list$field_exception_s_ <- '1194'
    return(metadata_list)
  }

}
