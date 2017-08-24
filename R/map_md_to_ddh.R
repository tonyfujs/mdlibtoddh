#' map_md_to_ddh
#'
#' Map response from Microdata to DDH variables and LOVs
#'
#' @param metadata_list list: List of Microdata metadata (output from get_md_metadata())
#' @param lkup_tids named vector
#'
#' @return list
#' @export
#'

map_md_to_ddh <- function(metadata_list, lkup_tids) {

  # Extract values to predefined list
  metadata_list <- extract_md_metadata(metadata_list)
  # if (length(metadata_list) == 0) {next}
  metadata_list <- flatten_md_metadata(metadata_list)
  metadata_list <- add_constant_metadata(metadata_list)
  # Format values
  metadata_list <- format_md_metadata(metadata_list)
  # Map values
  metadata_list <- map_md_metadata(metadata_list, lkup_tids)

  return(metadata_list)

}
