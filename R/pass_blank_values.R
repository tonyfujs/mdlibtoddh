#' pass_blank_values()
#'
#' Pass blank values for values which are not present in updated version of dataset
#'
#' @param node_id character: DDH node ID of the dataset to be updated
#' @param credentials list: DDH API authentication token and cookie
#' @param master dataframe: Master lookup table
#' @param root_url character: Root URL to use for the API (Staging or Production)
#'
#' @return list

pass_blank_values <- function(node_id, dataset_metadata, metadata_list, root_url, credentials){

  # List of non-mandatory values to check
  machine_names <- c(
    "field_contact_email",
    "field_ddh_external_contact_email",
    "field_ddh_harvest_sys_id",
    "field_wbddh_access_authority",
    "field_wbddh_acronym",
    "field_wbddh_citation_text",
    "field_wbddh_disclaimer",
    "field_wbddh_ds_source",
    "field_wbddh_estimate_sampling",
    "field_wbddh_funding_name",
    "field_wbddh_modified_date",
    "field_wbddh_primary_investigator",
    "field_wbddh_questionnaires",
    "field_wbddh_reference_id",
    "field_wbddh_release_date",
    "field_wbddh_response_rates",
    "field_wbddh_sampling_procedure",
    "field_wbddh_series_information",
    "field_wbddh_study_type",
    "field_wbddh_supervision",
    "field_wbddh_unit_of_analysis",
    "field_wbddh_universe",
    "field_wbddh_version_description",
    "field_wbddh_kind_of_data",
    "field_wbddh_mode_data_collection",
    "field_wbddh_authority_name",
    "field_wbddh_data_editing",
    "field_wbddh_dstranslated_title",
    "field_wbddh_note_data_collection",
    "field_wbddh_other_acknowledgment",
    "field_wbddh_other_forms_of_data",
    "field_wbddh_other_processing",
    "field_wbddh_aggregation_method",
    "field_wbddh_version_notes",
    "field_wbddh_authority_name",
    "field_wbddh_copyright",
    "field_wbddh_data_collector",
    "field_wbddh_data_editing",
    "field_wbddh_deviations_sample",
    "field_wbddh_dstranslated_title",
    "field_wbddh_note_data_collection",
    "field_wbddh_other_acknowledgment",
    "field_wbddh_other_forms_of_data",
    "field_wbddh_other_processing",
    "field_wbddh_publisher_name",
    "field_wbddh_subtitle",
    "field_wbddh_version_notes",
    "field_ddh_harvest_src",
    "field_frequency",
    "field_granularity_list",
    "field_wbddh_update_frequency"
  )

  # Filter out fields from DDH metadata
  dataset_metadata <- dataset_metadata[names(dataset_metadata) %in% machine_names]

  # Create vector of outdated fields that are only present in DDH (i.e fields not present in Microdata anymore)
  to_pass <- c()
  for(i in seq_along(dataset_metadata)){
    if(!is_blank(dataset_metadata[i])){
      if(!names(dataset_metadata[i]) %in% names(metadata_list)){
        to_pass <- c(to_pass, names(dataset_metadata[i]))
      }
    }
  }

  if(length(to_pass)>0){
    # Create JSON with blank values
    json_blank <- ddhconnect::create_blank_json_body(values = to_pass,
                                                     publication_status = "published",
                                                     type = "dataset")

    # Update dataset with blank values
    ddhconnect::update_dataset(nid = node_id, body = json_blank,
                   root_url = root_url,
                   credentials = credentials)
  }

  return(ddhconnect::get_metadata(nid = node_id,
                                  root_url = root_url,
                                  credentials = credentials))
}
