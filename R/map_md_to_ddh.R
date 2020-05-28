#' map_md_to_ddh
#'
#' Map response from Microdata to DDH variables and LOVs
#'
#' @param metadata_list list: List of Microdata metadata (output from get_md_metadata())
#' @param iso3_lkup data.frame: ddhconnect::get_iso3()
#' @return list
#' @export
#'

map_md_to_ddh <- function(metadata_list, iso3_lkup) {

  # Extract values to predefined list
  out_1         <- extract_md_metadata(metadata_list)
  out_2         <- handle_array_metadata(metadata_list)
  metadata_list <- c(out_1,out_2)

  metadata_list <- add_country(metadata_in = metadata_list,
                               iso3_lkup = iso3_lkup)
  # Format values
  metadata_list <- format_md_metadata(metadata_list)

  # Map values
  metadata_list <- map_md_metadata(metadata_list)

  return(metadata_list)

}
