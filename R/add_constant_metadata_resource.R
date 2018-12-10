#' add_constant_metadata_resource
#'
#' Add metadata that have constant values across records for resources
#'
#' @param metadata_list list: List for machine names and their corresponding values
#'
#' @return list
#' @export
#'

add_constant_metadata_resource <- function(metadata_list) {

  metadata_list$title <- "Related materials (Questionnaires, reports, tables, technical documents, and data files)"
  metadata_list$field_wbddh_resource_type <- "Landing page"
  metadata_list$type <- "resource"
  metadata_list$workflow_status <- "published"

  return(metadata_list)
}
