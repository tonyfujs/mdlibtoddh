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
      key <- unique(lookup$key[lookup$ddh_machine_name == machine_names[i]])[[1]]
      
      metadata_value <- unlist(lapply(metadata_value, function(x){
        x[[key]]
      }))
      
      metadata_out[[machine_names[i]]] <- paste(metadata_value, collapse = ", ")
    }
    
    if (!is.null(metadata_out[[machine_names[i]]]) & machine_names[i] %in% mdlibtoddh:::microdata_date_fields) {
      metadata_out[[machine_names[i]]] <- expand_date(metadata_out[[machine_names[i]]])
    }
  }
  metadata_out <- metadata_out[!purrr::map_lgl(metadata_out, is.null)]
}