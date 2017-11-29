#' add_data_classification
#'
#' @param metadata_list list: Flattened list of metadata from the Microdata API
#' @param md_internal_id character: Microdata internal ID
#' @param master data.frame: Output of mdlibtoddh::get_ddh_records_status()
#'
#' @return list
#' @export
#'

add_data_classification <- function(metadata_list, md_internal_id, master) {

  master <- master[master$md_internal_id == md_internal_id, 'data_classification']
  assertthat::assert_that(length(master) == 1)

  # CHECK that input is correct
  assertthat::validate_that(master %in% c('public', 'official'))
  # Add correct data classification
  if (master == 'public') {
    metadata_list$field_wbddh_data_class <- 'Public'
    # metadata_list$field_exception_s_ <- ''
    return(metadata_list)
  } else if (master == 'official') {
    metadata_list$field_wbddh_data_class <- 'Official Use Only'
    metadata_list$field_exception_s_ <- '7. Member Countries/Third Party Confidence'
    return(metadata_list)
  }

}
