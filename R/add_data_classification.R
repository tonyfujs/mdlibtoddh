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

  # CHECK that input is correct
  assertthat::validate_that(all(unique(master$data_classification) %in% c('public', 'official')))
  if (master$data_classification == 'public') {
    metadata_list$field_wbddh_data_class <- '358'
    return(metadata_list)
  } else if (master$data_classification == 'official') {
    metadata_list$field_wbddh_data_class <- '359'
    metadata_list$field_exception_s_ <- '1194'
    return(metadata_list)
  }

}
