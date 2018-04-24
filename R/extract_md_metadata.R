#' extract_md_metadata
#'
#' Extract specific metadata from the Microdata API JSON response
#'
#' @param metadata_in list: The output of mdlibconnect::get_survey_metadata
#' @param metadata_out list: Package object: mdlibtoddh::md_placeholder
#' @param lookup data.frame: Package object: mdlibtoddh::lookup
#'
#' @return list
#' @export
#'

extract_md_metadata <- function(metadata_in,
                                metadata_out = mdlibtoddh::md_placeholder,
                                lookup = mdlibtoddh::lookup) {

  machine_names <- sort(names(metadata_out))
  keep <- metadata_in[names(metadata_in) %in% machine_names]

  for (i in seq_along(machine_names)) {
    mdlib_json_key <- unique(lookup$mdlib_json_field[lookup$ddh_machine_name == machine_names[i]])
    mdlib_json_key <- mdlib_json_key[!is.na(mdlib_json_key)]
    if (length(mdlib_json_key) == 0) {next}

    if (is.null(metadata_in[[mdlib_json_key]]) & machine_names[i] %in% ddhconnect:::mandatory_text_fields) {
      metadata_out[[machine_names[i]]] <- "Not specified"
    } else {
      metadata_out[[machine_names[i]]] <- metadata_in[[mdlib_json_key]]
    }

    if (machine_names[i] %in% mdlibtoddh:::microdata_date_fields) {
      metadata_out[[machine_names[i]]] <- expand_date(metadata_out[[machine_names[i]]])
    }
  }

  metadata_out <- metadata_out[!purrr::map_lgl(metadata_out, is.null)]
  metadata_out <- c(metadata_out, keep)

  return(metadata_out)
}
