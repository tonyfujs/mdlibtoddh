#' handle_array_metadata
#'
#' Handle list metadata
#'
#' @param metadata_in list: The output of mdlibconnect::get_survey_metadata
#' @param metadata_out list: Package object: mdlibtoddh::md_placeholder
#' @param lookup data.frame: Package object: mdlibtoddh::lookup
#'
#' @return list
#' @export
#'

handle_array_metadata <- function(metadata_in,
                                  metadata_out = mdlibtoddh::md_placeholder,
                                  lookup = mdlibtoddh::lookup){

  machine_names <- sort(names(metadata_out))

  # Filter out machine names that are arrays from Microdata API
  array_machine_names <- unique(lookup[lookup$is_array == TRUE, "ddh_machine_name"])
  machine_names <- machine_names[machine_names %in% array_machine_names]

  for (i in seq_along(machine_names)) {
    mdlib_json_key <- unique(lookup$mdlib_json_field[lookup$ddh_machine_name == machine_names[i]])
    mdlib_json_key <- mdlib_json_key[!is.na(mdlib_json_key)]
    if (length(mdlib_json_key) == 0) {next}

    metadata_value <- find_metadata_value(mdlib_json_key, metadata_in)

      if (is.null(metadata_value) & machine_names[i] %in% ddhconnect:::mandatory_text_fields) {
      metadata_out[[machine_names[i]]] <- "Not specified"
    } else if (!is.null(metadata_value)){

      # Handle for single Key
      if(is.na(unique(lookup$key_2[lookup$ddh_machine_name == machine_names[i]])[[1]])){

        key <- unique(lookup$key_1[lookup$ddh_machine_name == machine_names[i]])[[1]]
        metadata_value <- unlist(lapply(metadata_value, function(x){
          x[[key]]
        }))

        # Handle Email fields
        if(machine_names[i] == "field_contact_email"){
          metadata_value <- paste(metadata_value, collapse = "; ")
          metadata_value <- stringr::str_replace_all(metadata_value, pattern = '^; ?|;$|; $', replacement = '')
          metadata_value <- stringr::str_replace_all(metadata_value, pattern = ' ', replacement = '')
          metadata_value <- stringr::str_trim(metadata_value, side = 'both')
          metadata_out[[machine_names[i]]] <- stringr::str_replace_all(metadata_value, pattern = ';', replacement = '; ')
        } else{
          metadata_value <- paste(metadata_value, collapse = ", ")
          metadata_out[[machine_names[i]]] <- trimws(stringr::str_replace_all(metadata_value, pattern = '^; ?|;$|; $', replacement = ''))
        }

      } else {

        # Handle for multiple keys
        keys <- unique(lookup[lookup$ddh_machine_name == machine_names[i], c("key_1", "key_2")])
        keys <- keys[!is.na(keys)]

        metadata_value <- unlist(lapply(metadata_value, function(x)
          {
            quick_map(input = x, keys = keys)
          }))

        metadata_value <- paste(metadata_value, collapse = ", ")
        metadata_out[[machine_names[i]]] <- trimws(stringr::str_replace_all(metadata_value, pattern = '^; ?|;$|; $', replacement = ''))
      }
    }

    if (!is.null(metadata_out[[machine_names[i]]]) & machine_names[i] %in% mdlibtoddh:::microdata_date_fields) {
      metadata_out[[machine_names[i]]] <- expand_date(metadata_out[[machine_names[i]]])
    }
  }
  metadata_out <- metadata_out[!purrr::map_lgl(metadata_out, is.null)]
}

