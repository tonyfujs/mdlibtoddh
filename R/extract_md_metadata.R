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

  # Filter out machine names that aren't arrays from Microdata API
  non_array_machine_names <- unique(lookup[lookup$is_array == FALSE | is.na(lookup$is_array) == TRUE, "ddh_machine_name"])
  machine_names <- machine_names[machine_names %in% non_array_machine_names]

  for (i in seq_along(machine_names)) {
    mdlib_json_key <- unique(lookup$mdlib_json_field[lookup$ddh_machine_name == machine_names[i]])
    mdlib_json_key <- mdlib_json_key[!is.na(mdlib_json_key)]
    if (length(mdlib_json_key) == 0) {next}

    metadata_value <- find_metadata_value(mdlib_json_key, metadata_in)

    if (is.null(metadata_value) & machine_names[i] %in% ddhconnect:::mandatory_text_fields) {
      metadata_out[[machine_names[i]]] <- "Not specified"
    }

    else if(!is.null(metadata_value)) {
      metadata_out[[machine_names[i]]] <- trimws(stringr::str_replace_all(metadata_value, pattern = '^; ?|;$|;| $|\\n', replacement = ''))
    }

    if (!is.null(metadata_out[[machine_names[i]]]) & machine_names[i] %in% mdlibtoddh:::microdata_date_fields) {
      metadata_out[[machine_names[i]]] <- expand_date(metadata_out[[machine_names[i]]])
    }
  }

  metadata_out <- metadata_out[!purrr::map_lgl(metadata_out, is.null)]

  return(metadata_out)
}
