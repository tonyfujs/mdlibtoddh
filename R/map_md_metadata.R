#' map_md_metadata
#'
#' @param metadata_list list: Named list containing all values extracted from the Microdata API response
#'
#' @return list
#' @export
#'

map_md_metadata <- function(metadata_list) {

  survey_fields <- names(metadata_list)
  lkup_values <- mdlibtoddh::md_ddh_lovs
  lkup_tids <- mdlibtoddh::ddh_tid_lovs

  # Map values to DDH controlled vocabulary ---------------------------------
  controlled_variables <- survey_fields[survey_fields %in% names(lkup_values)]
  metadata_list[controlled_variables] <- purrr::map(controlled_variables, function(x) {
    map_valid_lovs(metadata_list[[x]], lkup_values[[x]])
  })

  # Map values to DDH controlled tids
  # controlled_variables <- survey_fields[survey_fields %in% names(lkup_tids)]
  # metadata_list[controlled_variables] <- purrr::map(controlled_variables, function(x) {
  #   map_valid_lovs(metadata_list[[x]], lkup_tids[[x]])
  # })

  return(metadata_list)
}
